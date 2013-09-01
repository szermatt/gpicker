#ifndef NO_CONFIG
#include "config.h"
#endif

#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <glib.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "do_with_main_loop.h"
#include "loading.h"

static char *project_type;
static gboolean disable_bzr;
static gboolean disable_hg;

GOptionEntry project_file_entries[] = {
  {"project-type", 't', 0, G_OPTION_ARG_STRING, &project_type, 
   "respect ignored files for given kind of VCS (default, git, bzr, hg, guess, mlocate)", 0},
  {"disable-bzr", 0, 0, G_OPTION_ARG_NONE, &disable_bzr, 
   "disable autodetection of Bazaar project type", 0},
  {"disable-hg", 0, 0, G_OPTION_ARG_NONE, &disable_hg, 
   "disable autodetection of Mercurial project type", 0},
  {0}
};

static gboolean initialized;
static GPid child_pid;
static int pipe_fd;
static int project_dir_fd;

static
void kill_child_pid(void)
{
        if (child_pid)
                kill(child_pid, SIGINT);
}


static
int my_popen(char *string, GPid *child_pid)
{
	char *argv[] = {"/bin/sh", "-c", string, 0};
	int fd;
	gboolean ok = g_spawn_async_with_pipes(0, // work dir
					       argv,
					       0, //envp
					       G_SPAWN_DO_NOT_REAP_CHILD, // flags
					       0, 0, //child setup & user data
					       child_pid,
					       0, // stdin
					       &fd, //stdout
					       0, //stderr
					       0);
	if (!ok)
		fd = -1;
	return fd;
}

static
int isdir(char* name)
{
  struct stat statbuf;

  if (stat(name, &statbuf) < 0 || !S_ISDIR(statbuf.st_mode)) {
    return 0;
  }
  return 1;
}

static
int check_parents(char* name)
{
  struct stat rootdir;
  struct stat curdir;
  int isroot, rv;
  int cwd = dirfd(opendir("."));

  stat("/", &rootdir);
  while (1) {
    if (isdir(name)) {
      rv = 1;
      break;
    }
    stat(".", &curdir);
    isroot = (rootdir.st_dev == curdir.st_dev &&
              rootdir.st_ino == curdir.st_ino);
    if (isroot || chdir("..") == -1) {
      rv = 0;
      break;
    }
  }
  if (fchdir(cwd) < 0) {
    perror("cannot chdir back");
    exit(1);
  }
  return rv;
}

static
int detect_project_type_script(void)
{
        struct stat st;
        int rv = stat(".gpicker-script", &st);
        /* NOTE: I was considering checking if script is executable
         * but decided that guess semantics shouldn't bother with
         * it. It provides better error reporting. */
        return (rv == 0 && S_ISREG(st.st_mode));
}

static
void enter_project_dir(const char *project_dir)
{
  int rv = chdir(project_dir);

  if (rv) {
    perror("cannot chdir to project directory");
    exit(1);
  }

  if (!strcmp(project_type, "guess")) {
    if (detect_project_type_script())
      project_type = "script";
    else if (check_parents(".git"))
      project_type = "git";
    else if (!disable_hg && check_parents(".hg"))
      project_type = "hg";
    else if (!disable_bzr && check_parents(".bzr"))
      project_type = "bzr";
    else
      project_type = "default";
  }
}

void project_files_init(const char *project_dir) 
{
        assert(!initialized);

        if (!project_type)
                project_type = "default";
        else if (strcmp(project_type, "guess") &&
            strcmp(project_type, "git") &&
            strcmp(project_type, "hg") &&
            strcmp(project_type, "bzr") &&
            strcmp(project_type, "default") &&
            strcmp(project_type, "mlocate") &&
            strcmp(project_type, "script")) {
          fprintf(stderr, "Unknown project type specified: %s\n", project_type);
          exit(1);
        }

        if (strcmp(project_type, "mlocate"))
          enter_project_dir(project_dir);

        project_dir_fd = -1;
        pipe_fd = -1;
        child_pid = 0;
        atexit(kill_child_pid);

        if (project_type && !strcmp(project_type, "mlocate")) {
                project_dir_fd = open(project_dir, O_RDONLY);
                if (project_dir_fd < 0) {
                        perror("setup_filenames:open");
                        exit(1);
                }
                initialized = TRUE;
                return;
        } 

        if (!project_type || !strcmp(project_type, "default")) {
                char *find_command = getenv("GPICKER_FIND");
                if (!find_command)
                        find_command = FIND_INVOCATION;
                pipe_fd = my_popen(find_command, &child_pid);
	} else if (!strcmp(project_type, "script")) {
		pipe_fd = my_popen("./.gpicker-script", &child_pid);
        } else if (!strcmp(project_type, "git"))
                pipe_fd = my_popen("git ls-files --exclude-standard -c -o -z .", &child_pid);
        else if (!strcmp(project_type, "hg"))
                pipe_fd = my_popen("hg locate -0 --include .", &child_pid);
        else if (!strcmp(project_type, "bzr"))
                pipe_fd = my_popen("bzr ls -R --versioned --unknown --null", &child_pid);

        if (pipe_fd < 0) {
                perror("failed to spawn find");
                exit(1);
        }

        initialized = TRUE;
}

void project_files_read(void) 
{
        assert(initialized);

        if (project_type && !strcmp(project_type, "mlocate")) {
                assert(project_dir_fd >= 0);
                read_filenames_from_mlocate_db(project_dir_fd);
                close(project_dir_fd);
                return;
        }
        assert(pipe_fd >= 0);

        read_filenames(pipe_fd);
        close(pipe_fd);

        if (child_pid) {
		pid_t rv;
		int child_status;
		do {
			rv = waitpid(child_pid, &child_status, 0);
			if (rv < 0) {
				if (errno == EINTR)
					continue;
				perror("waitpid");
				exit(1);
			}
		} while (0);
		child_pid = 0;
		if (WIFEXITED(child_status) && (child_status = WEXITSTATUS(child_status)) != 0) {
			fprintf(stderr, "child exited with bad status: %d\n", child_status);
			exit(1);
		}
        }
}
