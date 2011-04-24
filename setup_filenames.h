#ifndef SETUP_FILENAMES_H
#define SETUP_FILENAMES_H

#include "config.h"

#include <glib.h>

struct setup_filename_data;

gboolean setup_filenames_init(
    const char *project_type, const char *project_dir, gboolean read_stdin);
void setup_filename_read(void);

#endif
