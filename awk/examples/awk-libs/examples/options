#!/usr/bin/gawk -f

# example usage of getopts() from http://github.com/e36freak/awk-libs

# include the lib with getopts. the path to the lib is not needed here if the
# directory is in AWKPATH, on gawk (this examples assumes it is, or that the
# lib is in the current working dir). otherwise, use the path to the lib, or
# copy and paste the whole function here.
@include "options.awk";

# prints usage information
# to see this, make sure you use ./script -- -h. without the '--', awk will
# treat the -h as an argument to awk itself, not the script
function usage() {
  printf("%s\n\n%s\n\n%s\n%s\n\n",
"example usage of getopts() from http://github.com/e36freak/awk-libs",
"awk_getopts -- [OPTIONS] [FILE(s)]",
"the \"--\" is required so that options are parsed by the script, and not",
"awk itself") > "/dev/stderr";

  printf("%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n%s\n%s\n%s\n%s\n%s\n",
" Options:",
"  -h, --help         Display this help and exit",
"  -a, --arg ARG      Option that requires an argument. prints the arg",
"  -b, --blah         Option that doesn't require an arg. prints \"hello world\"",
"  -i, --info         Displays optind and getopts' return value for each",
"                     iteration from when the option is used, onward. This is to",
"                     help give some insight into how the function works",
"  -p, --print[=ARG]  This is an example of an OPTIONAL argument. if ARG is",
"                     provided (and non-empty), it will be printed. Otherwise,",
"                     the string \"no arg\" will be printed.",
"This program then prints the name of each file passed, and the total number",
"of files at the end") > "/dev/stderr";
}

# we do the option parsing in the BEGIN block, of course
BEGIN {
  # i'm using the associative array 'longopts' to map long options to the
  # appropriate short option. you could use any name you choose, or none at
  # all if you only want to use short options.
  longopts["help"] = "h";   # maps --help to -h
  longopts["arg"] = "a";    # --arg to -a
  longopts["blah"] = "b";   # --blah to -b
  longopts["info"] = "i";   # --info to -i
  longopts["print"] = "p";  # and --print to -p

  # now we do the actual option parsing
  # since -a requires an argument, it must be followed by a ':' in "optstring"
  # we pass 'longopts' as the second argument. Note that no ':' is used for 'p'
  # because the argument is OPTIONAL, not required
  while ((opt = getopts("ha:bip", longopts)) != -1) {
    # i'm using gawk's switch() to handle the various args. you could use
    # if/else if you wanted (see ogrep on the same github for an example), but
    # this is the cleanest and easiest when portability is not an issue, and is
    # the most common structure used in other languages (C, or bash's case)
    switch(opt) {
      # -h, or --help. both will return "h"
      case "h":
        usage();
        toexit = 1;
        exit;

      # same with --arg or -a
      case "a":
        # this one requires an arg, so 'optarg' will be set to its argument
        # this version of getopts handles -aARG, or --arg=ARG, or --arg ARG,
        # and of course -a ARG.
        print optarg;
        break;
      
      # and --blah, -b
      case "b":
        print "hello world";
        break;

      # --info, -i
      case "i":
        info = 1;
        break;

      # --print, -p
      case "p":
        # if ARG was provided, 'optarg' will be non-empty
        if (length(optarg)) {
          print optarg;
        } else {
          print "no arg";
        }
        break;

      # getopts will return "?" on error
      case "?":
      default:
        err = toexit = 1;
        exit;
    }

    # if info is on...
    if (info) {
      printf("optind is now: %d, and opt (getopts' return value) is now: %s\n",
             optind, opt) > "/dev/stderr";
    }
  }

  # again, if info is on
  if (info) {
    print "finished processing arguments" > "/dev/stderr";

    # in case you want to use it, 'optind' will now be set to the correct index
    # for the first non-option argument in ARGV. the previous options WILL be
    # deleted from ARGV at this point.
    printf("optind is now: %d, ", optind) > "/dev/stderr";

    # you'll see opt is -1 now, since there are no options left to process
    printf("and opt is now: %s\n", opt) > "/dev/stderr";
  }
}

# just an example block that reads from the file(s) given
{
  print FILENAME;
  files++;

  nextfile;
}

# END block. prints the number of files read. here to show that another exit
# call is needed, because awk will run the END block after 'exit' is called
# unless you exit again within the block. (toexit is used so -h|--help doesn't
# cause the script to exit >0)
END {
  if (toexit) {
    exit err;
  }

  print files;
}
