#ifndef NO_CONFIG
#include "config.h"
#endif

#include <assert.h>
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

static 
gboolean initialized;
static
GPid child_pid;
static 
int pipe_fd;
static 
int project_dir_fd;
static 
gboolean mlocate;

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
                                               (GSpawnFlags) 0, // flags
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

gboolean setup_filenames_init(
        const char *project_type, const char *project_dir, gboolean read_stdin) 
{
        assert(!initialized);
        mlocate = FALSE;
        project_dir_fd = -1;
        pipe_fd = -1;
        child_pid = 0;
        atexit(kill_child_pid);

        if (project_type && !strcmp(project_type, "mlocate")) {
                project_dir_fd = open(project_dir, O_RDONLY);
                if (project_dir_fd < 0) {
                        perror("setup_filenames:open");
                        return FALSE;
                }
                mlocate = TRUE;
                initialized = TRUE;
                return TRUE;
        } 

        if (read_stdin)
                pipe_fd = fileno(stdin);
        else if (!project_type || !strcmp(project_type, "default")) {
                char *find_command = getenv("GPICKER_FIND");
                if (!find_command)
                        find_command = FIND_INVOCATION;
                pipe_fd = my_popen(find_command, &child_pid);
        } else if (!strcmp(project_type, "git"))
                pipe_fd = my_popen("git ls-files --exclude-standard -c -o -z .", &child_pid);
        else if (!strcmp(project_type, "hg"))
                pipe_fd = my_popen("hg locate -0 --include .", &child_pid);
        else if (!strcmp(project_type, "bzr"))
                pipe_fd = my_popen("bzr ls -R --versioned --unknown --null", &child_pid);

        if (pipe_fd < 0) {
                perror("failed to spawn find");
                return FALSE;
        }

        initialized = TRUE;
        return TRUE;
}

void setup_filenames_read(void) 
{
        if (mlocate) {
                read_filenames_from_mlocate_db(project_dir_fd);
                close(project_dir_fd);
                return;
        }

        read_filenames(pipe_fd);

        if (child_pid) {
                kill(child_pid, SIGINT);
                child_pid = 0;
        }
        close(pipe_fd);
}
