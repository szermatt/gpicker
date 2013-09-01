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
        context = g_option_context_new(
                "PROJECT-DIR-PATH - quickly pick a file from the project");
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

static inline
void write_or_die(const char *buf, size_t count) {
        if (fwrite(buf, 1, count, stdout) < 0) {
                perror("write error");
                exit(1);
        }
}

static inline
void write_etx() {
        /* mark the end of a query. */
        const static char* end = "\0\0\n";
        write_or_die(end, 3);
        fflush(stdout);
}

static
void write_results(char *pattern, int maxcount) {
        struct filename *filenames = (struct filename *)files_vector.buffer;
        struct filter_result *results;
        int i;
        int result_count;

        filter_files_sync(pattern);
        results = (struct filter_result *)filtered.buffer;
        result_count = filtered.used;
        if (result_count > maxcount)
                result_count = maxcount;
        for (i = 0; i < result_count; i++) {
                struct filename *filename = &filenames[results[i].index];
                write_or_die(filename->p, strlen(filename->p) + 1);
        }
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
                        /* ?<number>:<query> get up to <number> results. */
                        line += 1;
                        char *colon_p = strchr(line, ':');
                        int max_results = 8;
                        if (colon_p) {
                                max_results = atoi(line);
                                line = colon_p + 1;
                        }
                        if (max_results <= 0) {
                                fprintf(stderr,
                                        "Invalid result count in '?%s'.",
                                        stripws(line));
                                break;
                        }
                        write_results(stripws(line), max_results);
                        break;
                default:
                        fprintf(stderr, "Unknown query '%c'\n", *line);
                        break;
                }
                write_etx();
        }
        return 0;
}

int main(int argc, char **argv)
{
        const char *project_dir;
        init_loading();
        prepare_scorer();
        project_dir = parse_options(argc, argv);
        project_files_init(project_dir);
        project_files_read();
        return daemon_loop();
}
