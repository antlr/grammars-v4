#!/usr/bin/awk -f

# include library. gawk can use AWKPATH so the actual path isn't needed, see
# the man page (since the path is relative, this assumes the lib dir is in
# AWKPATH or the same dir)
@include "colors.awk";

# usage: set_cols(array)
#   sets the following values in "array" with tput. printing them will format
#   any text afterwards. colors and formats are:
#     bold - bold text (can be combined with a color)
#     black - black text
#     red - red text
#     green - green text
#     yellow - yellow text
#     blue - blue text
#     magenta - magenta text
#     cyan - cyan text
#     white - white text
#     reset - resets to default settings
BEGIN {
  # set colors
  set_cols(colors);

  # print colored text
  print colors["red"] "red\t", colors["bold"] "bold red" colors["reset"];
  print colors["black"] "black\t", colors["bold"] "bold black" colors["reset"];
  print colors["green"] "green\t", colors["bold"] "bold green" colors["reset"];
  print colors["yellow"] "yellow\t", colors["bold"] "bold yellow" colors["reset"];
  print colors["blue"] "blue\t", colors["bold"] "bold blue" colors["reset"];
  print colors["magenta"] "magenta\t", colors["bold"] "bold magenta" colors["reset"];
  print colors["cyan"] "cyan\t", colors["bold"] "bold cyan" colors["reset"];
  print colors["white"] "white\t", colors["bold"] "bold white" colors["reset"];
}
