#ifndef PROJECT_FILES_H
#define PROJECT_FILES_H

#include "config.h"

#include <glib.h>

extern GOptionEntry project_file_entries[];

void project_files_init(const char *project_dir);
void project_files_read(void);

#endif
