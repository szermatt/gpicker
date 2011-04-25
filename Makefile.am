ACLOCAL_AMFLAGS = -I m4
SUBDIRS = lib

COMMON = \
  scorer.c scorer.h \
  filtration.c  filtration.h \
  vector.c vector.h \
  xmalloc.c xmalloc.h \
  refcounted_str.c refcounted_str.h \
  loading.c loading.h \
  compiler.h \
  timing.h \
  inline_qsort.h 

bin_PROGRAMS = gpicker gpicker-simple gpicker-daemon
gpicker_SOURCES = main.c \
 do_with_main_loop.h do_with_main_loop.c \
 simple_main.c \
 project_files.c project_files.h \
 $(COMMON)
gpicker_LDADD = @gtk2_LIBS@ @gthread_LIBS@ lib/libgnu.a
gpicker_CFLAGS = @gtk2_CFLAGS@ @gthread_CFLAGS@
AM_CPPFLAGS = -Ilib @EXTRA_CPPFLAGS@

man_MANS = gpicker.1 gpicker-simple.1

EXTRA_DIST = gpicker.el gpicker.vim IDEAS CodingStyle README.osx \
 Makefile.simple debian gpicker.1 gpicker-simple.1 m4/gnulib-cache.m4

# _SOURCES is only to fool automake not to search for gpicker-simple.c
# Real build rule is simple 'ln'
gpicker_simple_SOURCES = simple_main.c
gpicker-simple: gpicker
	ln -f gpicker gpicker-simple

gpicker_daemon_SOURCES = daemon_main.c \
 project_files.c project_files.h \
 $(COMMON)
gpicker_daemon_LDADD = @glib2_LIBS@ lib/libgnu.a
gpicker_daemon_CFLAGS = @glib2_CFLAGS@
gpicker_daemon_CPPFLAGS = -Ilib @EXTRA_CPPFLAGS@


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
