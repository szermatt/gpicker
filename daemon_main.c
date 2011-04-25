#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "filtration.h"
#include "loading.h"
#include "project_files.h"

static const char *parse_options(int argc, char **argv)
{
        GError *error = 0;
        GOptionContext *context;
        context = g_option_context_new("PROJECT-DIR-PATH - quickly pick a file from the project");
        g_option_context_add_main_entries(context, project_file_entries, 0);

        if (!g_option_context_parse(context, &argc, &argv, &error)) {
                fprintf(stderr, "option parsing failed: %s\n", error->message);
                exit(1);
        }

        if (argc < 2) {
                fprintf(stderr, "I need a project path\n");
                fputs(g_option_context_get_help(context, TRUE, NULL), stderr);
                exit(1);
        }

        return argv[1];
}

static 
char *stripws(char *line) 
{
        char *line_end;
        while (*line == ' ' || *line == '\t') 
                line++;
        for (line_end = &line[strlen(line) - 1];
             *line_end == '\n' || *line_end == '\r' 
                     || *line_end == '\t' || *line_end == ' ';
             line_end--) {
                *line_end = 0;
        }
        return line;
}

static
char *read_daemon_command(char *buffer, int buflen, int *ignored) {
        char *line;
        char *more;
        int flags;
        *ignored = 0;
        flags = fcntl(0, F_GETFL, 0); 
        fcntl(0, F_SETFL, flags & ~O_NONBLOCK);
        line = fgets(buffer, buflen, stdin);
        if (line == NULL)
                return NULL;
        fcntl(0, F_SETFL, flags | O_NONBLOCK);
        for (;; (*ignored)++) {
                more = fgets(buffer, buflen, stdin);
                if (more == NULL) {
                        return line;
                }
                line = more;
        }
        abort(); /* should never be reached */
}

static
void write_etx() {
        /* mark the end of a query. */
        const static char* end = "\0\0\n";
        if (write(1, end, 3) <= 0) {
                perror("write");
                exit(1);
        }
}

static 
void write_results(const char *pattern, int maxcount) {
        filter_func filter_func;
        filter_destructor destructor = 0;
        void *filter;
        int nfiles = files_vector.used;
        int i;
        int accepted;
        filter = prepare_filter(pattern, &filter_func, &destructor);
        for (i = 0, accepted = 0; i < nfiles && accepted < maxcount; i++) {
                int accept;
                struct filename *filename;
                struct filter_result result;
                filename = ((struct filename *)(files_vector.buffer)) + i;
                accept = filter_func(filename, filter, &result, 0);
                if (!accept)
                        continue;
                /* Write filename, including final 0. */
                if (write(1, filename->p, strlen(filename->p) + 1) <= 0) {
                        perror("write error");
                        exit(1);
                }
                accepted++;
        }
        if (destructor)
                destructor(filter);
}

static
int daemon_loop(void)
{
        int runloop;
        char buffer[1024];
        ssize_t r;
        
        runloop = 1;
        while (runloop) {
                char *line;
                int ignored = 0;
                line = read_daemon_command(
                        buffer, sizeof(buffer) - 1, &ignored);
                for (;ignored > 0; ignored--) {
                        write_etx();
                }
                if (line == NULL) {
                        perror("read stdin");
                        return 1;
                }
                switch (*line) {
                case '.':
                        /* .: stop the daemon. */
                        runloop = 0;
                        break;
                case '?':
                        /** ?: get a small number of results. */
                        write_results(stripws(line + 1), 8);
                        break;
                case '+':
                        /** +: get a larger number of results. */
                        write_results(stripws(line + 1), 25);
                        break;
                default:
                  {
                    static const char *error = "Unknown query\n";
                    if (write(1, error, strlen(error)) <= 0) {
                      perror("write");
                      exit(1);
                    }
                  }
                  break;
                }
                write_etx();
        }
        return 0;
}

int daemon_main(int argc, char **argv)
{
        const char *project_dir;
        init_loading();
        project_dir = parse_options(argc, argv);
        project_files_init(project_dir);
        project_files_read();
        return daemon_loop();
}
