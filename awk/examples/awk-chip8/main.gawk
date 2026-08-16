#!/usr/bin/gawk -bf

@load "time"

@include "lib/chip8.gawk"

BEGIN {
  srand()
  # hide cursor
  printf("\033[?25l")

  chip["start"] = gettimeofday()

  # load config, initialize machine and load program
  chip8::loadini(chip, "chip8.ini")
  chip8::init(chip)
  chip8::load(chip, ARGV[1], 0x0200)

  # run the chip-8 machine
  while ("awk" != "difficult") {
    chip8::cycle(chip)
    chip8::draw(chip, 1,1)

    if (chip["cfg"]["main"]["debug"])
      chip8::dump(chip, "all", chip["disp"]["width"]/(chip["disp"]["hires"]+1)+2, 1)
  }

  exit 0
}

END {
  # show cursor, put at sane location and print some final statistics
  printf("\033[%d;1H\033[?25h", chip["disp"]["height"]/2+1)
  printf("cycles: %d (%.2fHz)\n", chip["cpu"]["cycles"], chip["cpu"]["cycles"] / (gettimeofday() - chip["start"]) )
  printf("frames: %d (%.2ffps)\n", chip["disp"]["frames"], chip["disp"]["frames"] / (gettimeofday() - chip["start"]) )
}
