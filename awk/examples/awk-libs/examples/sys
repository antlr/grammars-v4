#!/usr/bin/awk -f

# include library. gawk can use AWKPATH so the actual path isn't needed, see
# the man page (since the path is relative, this assumes the lib dir is in
# AWKPATH or the same dir)
@include "sys.awk";

# usage: isatty(fd)
# Checks if "fd" is open on a tty. Returns 1 if so, 0 if not, and -1 if an
# error occurs
BEGIN {
  if (isatty(0)) {
    print "stdin is open on a tty";
  } else {
    print "stdin is not open on a tty";
  }

  if (isatty(1)) {
    print "stdout is open on a tty";
  } else {
    print "stdout is not open on a tty";
  }

  if (isatty(2)) {
    print "stderr is open on a tty";
  } else {
    print "stderr is not open on a tty";
  }
}

# usage: mktemp(template [, type])
# creates a temporary file or directory, safely, and returns its name.
# if template is not a pathname, the file will be created in ENVIRON["TMPDIR"]
# if set, otherwise /tmp. the last six characters of template must be "XXXXXX",
# and these are replaced with a string that makes the filename unique. type, if
# supplied, is either "f", "d", or "u": for file, directory, or dry run (just
# returns the name, doesn't create a file), respectively. If template is not
# provided, uses "tmp.XXXXXX". Files are created u+rw, and directories u+rwx,
# minus umask restrictions. returns -1 if an error occurs.
BEGIN {
  print mktemp("foo.XXXXXX", "u");
  print mktemp("./bar.XXXXXX");
  print mktemp("./dir.XXXXXX", "d");
  print mktemp("broken.XXX", "u");
}
