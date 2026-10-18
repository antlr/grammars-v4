#!/usr/bin/awk -f

# include library. gawk can use AWKPATH so the actual path isn't needed, see
# the man page (since the path is relative, this assumes the lib dir is in
# AWKPATH or the same dir)
@include "strings.awk";
@include "options.awk";


# prints usage
function usage() {
  printf("%s\n\n%s\n\n%s\n%s\n%s\n%s\n\n",
"cfold -- [OPTIONS] [FILE...]",
"the '--' is required, so AWK itself doesn't read the options",
"Wraps input lines in each FILE (standard input if not provided), writing to",
"standard output. The default width is that of the terminal, or 80 columns if",
"standard output is not a terminal. If FILE is '-', also reads the standard",
"input") > "/dev/stderr";
  printf("%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n",
" Options:",
"  -c, --center         center each line on the terminal. assumes a width of 80",
"                       columns if standard output is not a terminal",
"  -b, --break[=LIST]   break lines at spaces. LIST, if provided, is a list of",
"                       characters to break lines at instead of spaces. For",
"                       example, --break=abc will break lines on \"a\" or \"b\"",
"                       Note: an empty string for LIST will revert to the",
"                       default behavior, it is not the same as omitting -b",
"  -i, --ignore-breaks  convert existing single line breaks to spaces. multiple",
"                       line breaks like those at the end of paragraphs will be",
"                       truncated to a single empty line",
"  -w, --width WIDTH    use WIDTH columns instead of the terminal's width",
"  -t, --trim           trim leading and trailing whitespace from each line",
"                       prior to folding",
"  -h, --help           display this help and exit") > "/dev/stderr";
}

BEGIN {
  # initialize variables to defaults
  toexit = err = 0;
  tocenter = toignore = totrim = 0;
  break_chars = "";

  # get default width
  if (system("test -t 1")) {
    # stdout is not open on a tty
    width = 80
  } else {
    cmd = "tput cols";
    cmd | getline width;
    close(cmd);
  }

  # map long options to short options
  longopts["center"]        = "c";
  longopts["break"]         = "b";
  longopts["ignore-breaks"] = "i";
  longopts["width"]         = "w";
  longopts["trim"]          = "t";
  longopts["help"]          = "h";

  # parse the options
  while ((opt = getopts("cbiw:th", longopts)) != -1) {
    switch(opt) {
      # -c, --center
      case "c":
        tocenter = 1; break;

      # -b, --break
      case "b":
        if (length(optarg)) {
          break_chars = optarg;
        } else {
          break_chars = " \t\n";
        }
        break;

      # -i, --ignore-breaks
      case "i":
        toignore = 1; break;

      # w, --width
      case "w":
        # make sure optarg is an integer
        if (optarg !~ /^[0-9]+$/) {
          printf("'%s' is not a valid argument for '%s', must be a number",
                 optarg, optname) > "/dev/stderr";
          err = toexit = 1;
          exit;
        }
        width = optarg;
        break;

      # -t, --trim
      case "t":
        totrim = 1; break;

      # -h, --help
      case "h":
        usage(); toexit = 1; exit;

      # error
      case "?":
      default:
        err = toexit = 1;
        exit;
    }
  }

  # if --ignore-breaks was used, set RS to null so that paragraphs are
  # treated as one line
  if (toignore) {
    RS = "";
  }
}

########

# if --ignore-breaks was used, print extra newline between records
toignore && NR > 1 {
  print "";
}

# fold each record (line, or paragraph)
{

  # if --trim was used, reassign $0 with leading/trailing whitespace removed
  if (totrim) {
    $0 = trim($0);
  }

  out = fold($0, break_chars, width);

  # if text is to be centered, split out into an array of lines and center each
  if (tocenter) {
    len = split(out, lines, /\n/);

    for (i=1; i<=len; i++) {
      print center(lines[i]);
    }
  } else {
    print out;
  }
}

END {
  exit err;
}



# You can do whatever you want with this stuff, but a thanks is always
# appreciated
