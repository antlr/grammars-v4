#!/usr/bin/gawk -bf

@include "lib/chip8.gawk"

function basename(fname, ext,    n, arr) {
  n = split(fname, arr, "/")
  if (ext) sub(ext"$", "", arr[n])
  return arr[n]
}

BEGIN {
  if ((ARGC < 2) || (ARGC > 3) ){
    printf("Usage: %s <file>.asm [<file>.(ch8|hex)]\n", ARGV[0])
    exit 1
  }

  dest = ARGV[2] ? ARGV[2] : basename(ARGV[1], ".asm")".ch8"

  addr = chip8::load(chip, ARGV[1])
  printf("Saving %d bytes of memory to \"%s\"\n", (addr - 0x200), dest)
  chip8::save(chip, dest, (addr - 0x200) )
}
