#!/usr/bin/gawk -bf

@include "lib/chip8.gawk"

BEGIN {
  if (ARGC != 2) {
    printf("Usage: %s <file>.(ch8|hex)\n", ARGV[0])
    exit 1
  }

  dest = substr(ARGV[1], 1, length(ARGV[1])-4 )
  ext  = substr(ARGV[1], length(ARGV[1])-2 )

  if (ext == "hex") dest = dest ".ch8"
  if (ext == "ch8") dest = dest ".hex"

  addr = chip8::load(chip, ARGV[1])
  printf("Saving %d bytes of memory to \"%s\"\n", (addr - 0x200), dest)
  chip8::save(chip, dest, (addr - 0x200) )
}

