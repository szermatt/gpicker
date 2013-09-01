ACLOCAL_AMFLAGS = -I m4
SUBDIRS = lib
AM_CPPFLAGS = -Ilib @EXTRA_CPPFLAGS@

EXTRA_DIST = gpicker.el gpicker.vim IDEAS CodingStyle README.osx \
 Makefile.simple debian gpicker-simple.1 m4/gnulib-cache.m4 contrib/gpicker-script-example

COMMON_SOURCES = \
  scorer.c scorer.h \
  filtration.c  filtration.h \
  vector.c vector.h \
  xmalloc.c xmalloc.h \
  refcounted_str.c refcounted_str.h \
  loading.c loading.h \
  compiler.h \
  timing.h \
  inline_qsort.h
bin_PROGRAMS = gpicker-simple
man_MANS = gpicker-simple.1

### gpicker
if HAVE_GTK

bin_PROGRAMS += gpicker
man_MANS += gpicker.1
EXTRA_DIST += gpicker.1
gpicker_SOURCES = main.c \
 do_with_main_loop.h do_with_main_loop.c \
 $(COMMON_SOURCES)
gpicker_LDADD = @gtk2_LIBS@ @gthread_LIBS@ lib/libgnu.a
gpicker_CFLAGS = @gtk2_CFLAGS@ @gthread_CFLAGS@

endif # HAVE_GTK

### gpicker-simple
gpicker_simple_SOURCES = simple_main.c $(COMMON_SOURCES)
gpicker_simple_LDADD = @glib2_LIBS@ lib/libgnu.a
gpicker_simple_CFLAGS = @glib2_CFLAGS@ -DGPICKER_SIMPLE
gpicker_simple_CPPFLAGS = -Ilib @EXTRA_CPPFLAGS@


if HAVE_CHECK

TESTS = scorer-tests
check_PROGRAMS = scorer-tests
scorer_tests_SOURCES = scorer-tests.c scorer.c
scorer_tests_LDADD = @check_LIBS@

endif # HAVE_CHECK

check-syntax:
	$(COMPILE) -o /dev/null -S ${CHK_SOURCES}

%.s : %.c
	$(COMPILE) -S -fverbose-asm -o $@ $<
