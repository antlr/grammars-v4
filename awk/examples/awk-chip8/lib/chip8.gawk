@namespace "chip8"

@load "time"

@include "lib/cpu.gawk"

BEGIN {
  colors["black"]		= 30
  colors["red"]			= 31
  colors["green"]		= 32
  colors["yellow"]		= 33
  colors["blue"]		= 34
  colors["magenta"]		= 35
  colors["cyan"]		= 36
  colors["white"]		= 37
  colors["bright grey"]		= 37
  colors["bright gray"]		= 37
  colors["bright black"]	= 90
  colors["grey"]		= 90
  colors["gray"]		= 90
  colors["bright red"]		= 91
  colors["bright green"]	= 92
  colors["bright yellow"]	= 93
  colors["bright blue"]		= 94
  colors["bright magenta"]	= 95
  colors["bright cyan"]		= 96
  colors["bright white"]	= 97

  # bytes for characters 0-9 and A-F
  fontstr = "F0 90 90 90 F0 \
             20 60 20 20 70 \
             F0 10 F0 80 F0 \
             F0 10 F0 10 F0 \
             90 90 F0 10 10 \
             F0 80 F0 10 F0 \
             F0 80 F0 90 F0 \
             F0 10 20 40 40 \
             F0 90 F0 90 F0 \
             F0 90 F0 10 F0 \
             F0 90 F0 90 90 \
             E0 90 E0 90 E0 \
             F0 80 80 80 F0 \
             E0 90 90 90 E0 \
             F0 80 F0 80 F0 \
             F0 80 F0 80 80"
  split(fontstr, sysfont)

  prgstr = "00 E0 A2 72 60 00 6B 00 6E 01 00 C1 6A 06 DA B1 \
            FE 1E 7A 08 3A 3E 12 0E 70 01 30 06 12 0A 61 07 \
            00 C1 F1 15 F1 07 31 00 12 24 70 01 30 1E 12 1E \
            61 20 F1 15 F1 07 31 00 12 34 A2 9C 60 00 6B 1F \
            00 D1 6A 0B DA B1 FE 1E 7A 08 3A 33 12 44 70 01 \
            30 05 12 40 61 07 00 D1 F1 15 F1 07 31 00 12 5A \
            70 01 30 12 12 54 61 80 F1 15 F1 07 31 00 12 6A \
            00 FD CC D9 B0 F3 36 C0 F0 CC F9 B1 9B 36 C1 98 \
            FD AD A1 83 36 F9 98 CD 8D C1 83 F6 CC F0 CD 8D \
            A1 9B 36 CD 98 79 8D B0 F3 36 F8 F0 CA 3C 73 CE \
            DE AA 36 D9 98 D8 C6 3C F9 8C DC A2 30 D9 86 D8 \
            CC 30 D9 9C DE 00"
  nsysprg = split(prgstr, sysprg)

  # set ord table
  for (i=0; i<256; i++)
    ORD[sprintf("%c",i)] = i;

  # low resolution pixels
  lores["00"] = " "
  lores["01"] = "▄"
  lores["10"] = "▀"
  lores["11"] = "█"

  # high resolution pixels
  hires["0000"] = " "
  hires["0001"] = "▗"
  hires["0010"] = "▖"
  hires["0011"] = "▄"
  hires["0100"] = "▝"
  hires["0101"] = "▐"
  hires["0110"] = "▞"
  hires["0111"] = "▟"
  hires["1000"] = "▘"
  hires["1001"] = "▚"
  hires["1010"] = "▌"
  hires["1011"] = "▙"
  hires["1100"] = "▀"
  hires["1101"] = "▜"
  hires["1110"] = "▛"
  hires["1111"] = "█"

}


## load ini-style configuration
function loadini(self, fname,    a, section, keyval, val_is_quoted) {
  while ((getline <fname) > 0) {
    # have ini-style [section] tags
    if (match($0, /^\[([^]]*)\][[:blank:]]*$/, a))
      section = a[1]

    # skip comments and split key=value pairs
    if ( ($0 !~ /^[[:blank:]]*(#|;)/) && (match($0, /([^=]+)=(.+)/, keyval) > 0) ) {
      ## strip leading/trailing spaces and doublequotes
      gsub(/^[[:blank:]]*"?|"?[[:blank:]]*$/, "", keyval[1])
      gsub(/^[[:blank:]]*|[[:blank:]]*$/, "", keyval[2])

      val_is_quoted = 0
      if (keyval[2] ~ /^".*"$/) {
        gsub(/^"|"$/, "", keyval[2])
        val_is_quoted = 1
      }

      # convert colors to values
      if ( !val_is_quoted && (keyval[2] in colors) )
        keyval[2] = colors[keyval[2]]

      # convert string numbers to actual numbers
      if ( !val_is_quoted && (keyval[2]+0 == keyval[2]) )
        keyval[2] = awk::strtonum(keyval[2])

      # convert hex values to numbers
      if ( !val_is_quoted && (substr(keyval[2], 1, 2) == "0x") )
        keyval[2] = sprintf("%c", awk::strtonum(keyval[2]))

      self["cfg"][section][keyval[1]] = keyval[2]
      #printf("cfg[%s][%s] = %s\n", section, keyval[1], keyval[2])
    }
  }
  close(fname)
}


function dump(self, val, xpos, ypos,    i, y) {
  switch(val) {
    case /^mem|all/:
      for (i=self["pc"]-2; i<self["pc"]+14; i+=2) {
        opcode = self["mem"][i] * 256 + self["mem"][i+1]
        printf("\033[%d;%dH%s%04X: %02X %02X [%-14s]%c\033[0m\n", ypos+y++, xpos, (i==self["pc"]) ? "\033[97;101m>" : " ", i, self["mem"][i], self["mem"][i+1], cpu::disasm(opcode), (i==self["pc"]) ? "<" : " " )
      }

    case /^reg|all/:
      printf("\033[%d;%dHPC: 0x%04X, val: 0x%02X\n", ypos+0, xpos+31, self["pc"], self["mem"][ self["pc"] ])
      printf("\033[%d;%dH I: 0x%04X, val: 0x%02X\n", ypos+1, xpos+31, self["I"],  self["mem"][ self["I"] ])
      printf("\033[%d;%dHSP: 0x%04X, val: 0x%02X\n", ypos+2, xpos+31, self["sp"], self["stack"][ self["sp"] ])

      printf("\033[%d;%dHV0-V3: %02X %02X %02X %02X\n", ypos+4, xpos+31, self["V"][0x0], self["V"][0x1], self["V"][0x2], self["V"][0x3])
      printf("\033[%d;%dHV4-V7: %02X %02X %02X %02X\n", ypos+5, xpos+31, self["V"][0x4], self["V"][0x5], self["V"][0x6], self["V"][0x7])
      printf("\033[%d;%dHV8-VB: %02X %02X %02X %02X\n", ypos+6, xpos+31, self["V"][0x8], self["V"][0x9], self["V"][0xA], self["V"][0xB])
      printf("\033[%d;%dHVC-VF: %02X %02X %02X %02X\n", ypos+7, xpos+31, self["V"][0xC], self["V"][0xD], self["V"][0xE], self["V"][0xF])

      printf("\033[%d;%dHdelay: %02X, sound %02X\n", ypos+9, xpos+31, self["timer"]["delay"], self["timer"]["sound"])
      printf("\033[%d;%dHframes: %d, speed: %.2fHz\n", ypos+10, xpos+31, self["cpu"]["cycles"], self["cpu"]["cycles"] / (awk::gettimeofday() - self["start"]))

      printf("\033[%d;%dH[%s1\033[0m][%s2\033[0m][%s3\033[0m][%sC\033[0m]\n", ypos+12, xpos+31, self["keyboard"][0x1]?"\033[97;101m":"", self["keyboard"][0x2]?"\033[97;101m":"", self["keyboard"][0x3]?"\033[97;101m":"", self["keyboard"][0xc]?"\033[97;101m":"")
      printf("\033[%d;%dH[%s4\033[0m][%s5\033[0m][%s6\033[0m][%sD\033[0m]\n", ypos+13, xpos+31, self["keyboard"][0x4]?"\033[97;101m":"", self["keyboard"][0x5]?"\033[97;101m":"", self["keyboard"][0x6]?"\033[97;101m":"", self["keyboard"][0xd]?"\033[97;101m":"")
      printf("\033[%d;%dH[%s7\033[0m][%s8\033[0m][%s9\033[0m][%sE\033[0m]\n", ypos+14, xpos+31, self["keyboard"][0x7]?"\033[97;101m":"", self["keyboard"][0x8]?"\033[97;101m":"", self["keyboard"][0x9]?"\033[97;101m":"", self["keyboard"][0xe]?"\033[97;101m":"")
      printf("\033[%d;%dH[%sA\033[0m][%s0\033[0m][%sB\033[0m][%sF\033[0m]\n", ypos+15, xpos+31, self["keyboard"][0xa]?"\033[97;101m":"", self["keyboard"][0x0]?"\033[97;101m":"", self["keyboard"][0xb]?"\033[97;101m":"", self["keyboard"][0xf]?"\033[97;101m":"")
  }
}


function init(self) {
  # program counter
  self["pc"]      = 0x0200

  # set timing intervals
  self["cpu"]["cpuhz"]         =  self["cfg"]["cpu"]["cpuhz"]
  self["cpu"]["timerhz"]       =  self["cfg"]["cpu"]["timerhz"]
  self["cpu"]["keyboardhz"]    =  self["cfg"]["cpu"]["keyboardhz"]

  self["cpu"]["cpusleep"]      = 1 / self["cpu"]["cpuhz"]
  self["cpu"]["timersleep"]    = 1 / self["cpu"]["timerhz"]
  self["cpu"]["keyboardsleep"] = 1 / self["cpu"]["keyboardhz"]

  # put system font in memory
  for (i=0; i<0x50; i++)
    self["mem"][i] = awk::strtonum("0x" sysfont[i+1])

  # put system program in memory
  for (i=0; i<nsysprg; i++)
    self["mem"][0x0200+i] = awk::strtonum("0x" sysprg[i+1])

  # display data
  self["disp"]["width"]   = self["cfg"]["display"]["width"]
  self["disp"]["height"]  = self["cfg"]["display"]["height"]
  self["disp"]["hires"]   = self["cfg"]["display"]["hires"]
  self["disp"]["refresh"] = 1

  # make sure "keyboard" is an array
  self["keyboard"]["1"] = 0
}


function load(self, fname, addr,    ext, _fs, x,   end, label, mnem, a, l, unknown, m, argv, opc) {
  addr = length(addr) ? addr : 0x0200
  ext = substr(fname, length(fname)-3)

  # load ascii based hex file. big-endian word per line
  if (ext == ".hex") {
    _fs = FS; FS = ","
    while ((getline <fname) > 0) {
      self["mem"][addr++] = awk::strtonum("0x"substr($0,1,2)) % 256
      self["mem"][addr++] = awk::strtonum("0x"substr($0,3,2)) % 256
    }
  }

  # load binary ch8 file
  if (ext == ".ch8") {
    _fs = FS; FS = ""
    while ((getline <fname) > 0) {
      for (x=1; x<=NF; x++)
        self["mem"][addr++] = ORD[$x]
      if (RT)
        self["mem"][addr++] = ORD[RT]
    }
  }

  # load assembly file
  if (ext == ".asm") {
    start = addr
    _fs = FS

    while ((getline <fname) > 0) {
      if ($1 ~ /^:/) {
        # catch labels and save their address
        label[$1] = sprintf("0x%04X", addr)
        continue
      }

      if (NF && ($1 !~ /^;/)) {
        # strip comments and reduce whitespace
        gsub(/;.*/, "")
        gsub(/\s\s*/, " ")
        gsub(/(^\s*|\s*$)/, "")

        # save leftover as mnemonic
        if (length($0)) {
          mnem[addr] = $0
          addr += 2
        }
      }
    }

    for (a=start; a<addr; a+=2) {
      # replace all labels with addresses
      for (l in label)
        gsub(l, label[l], mnem[a])

      unknown = 1
      for (m in cpu::opcode) {
        # put opcode in memory
        if (match(mnem[a], m, argv)) {
          #printf("0x%04X: %20s -> "cpu::opcode[m]"\n", a, mnem[a], argv[1], argv[2], argv[3])
          opc = sprintf(cpu::opcode[m], argv[1], argv[2], argv[3])
          self["mem"][a+0] = awk::strtonum("0x"substr(opc,1,2))
          self["mem"][a+1] = awk::strtonum("0x"substr(opc,3,2))
          unknown = 0
        }
      }

      # unknown mnemonic found (typo in code?)
      if (unknown) {
        printf("Cannot compile [%s] at address 0x%04X\n", mnem[a], a)
        getline
        return -1
      }
    }

  }

  close(fname)
  FS = _fs

  return addr
}

function save(self, fname, len, addr,    ext, i, opcode) {
  addr = length(addr) ? addr : 0x0200
  ext = substr(fname, length(fname)-3)

  if (ext == ".hex") {
    for (i=0; i<len; i+=2) {
      opcode = self["mem"][addr+i] * 256 + self["mem"][addr+i+1]
      printf("%04X	; [0x%04X] %s\n", opcode, addr+i, cpu::disasm(opcode)) >>fname
    }
  }
  if (ext == ".ch8") {
    for (i=0; i<len; i++)
      printf("%c", self["mem"][addr+i]) >>fname
  }
  close(fname)
}


function draw(self, xpos, ypos,    x,y, w,h, up1, up2, dn1, dn2, line, display) {
  if (self["disp"]["refresh"] == 1) {
    w = self["disp"]["width"]
    h = self["disp"]["height"]

    #display = "\033[32;40m"
    display = sprintf("\033[%d;%dm", self["cfg"]["colors"]["foreground"], self["cfg"]["colors"]["background"]+10)
    for (y=0; y<h; y+=2) {
      line = sprintf("\033[%d;%dH", ypos + int(y/2), xpos)
      for (x=0; x<w; x+=2) {
        up1 = self["disp"][(y+0)*w + (x+0)] + 0
        up2 = self["disp"][(y+0)*w + (x+1)] + 0
        dn1 = self["disp"][(y+1)*w + (x+0)] + 0
        dn2 = self["disp"][(y+1)*w + (x+1)] + 0
        line = line "" (self["disp"]["hires"] ? hires[up1""up2""dn1""dn2] : lores[up1""dn1] lores[up2""dn2])
      }
      display = display "" line
    }
    printf("%s\033[0m", display)
    self["disp"]["refresh"] = 0
    self["disp"]["frames"]++
  }
}


function keyboard(self,    key, cmd, val) {
  # release any virtual key after a number of cycles
  for (key in self["keyboard"]) 
    if (self["keyboard"][key] > 0)
      self["keyboard"][key]--

  # get real keyboard input from OS
  cmd = "timeout --foreground 0.001 dd bs=1 count=1 2>/dev/null"
  if ((cmd | getline key) < 1) key=""
  close(cmd)

  # register virtual keypress 
  if (key in self["cfg"]["keyboard"]) {
    val = awk::strtonum("0x"self["cfg"]["keyboard"][key])
    self["keyboard"][val] = 3
  }

  # escape exits emulator
  if (key == self["cfg"]["emulator"]["exit"]) exit 0

  if (key == self["cfg"]["emulator"]["hires"]) {
    self["disp"]["hires"] = !self["disp"]["hires"]
    self["disp"]["refresh"] = 1
    printf("\033[?2J")
  }

  if (key == self["cfg"]["emulator"]["stop"]) {
    if (self["cpu"]["cpuhz"] == 0) {
      self["cpu"]["cpuhz"] = self["cfg"]["cpu"]["cpuhz"]
      self["cpu"]["cpusleep"] = 1 / self["cpu"]["cpuhz"]
    } else {
      self["cpu"]["cpuhz"] = 0
      self["cpu"]["cpusleep"] = 1000000
    }
  }

  if (key == self["cfg"]["emulator"]["speed-10"]) {
    if (self["cpu"]["cpuhz"] >= 10)
      self["cpu"]["cpuhz"] -= 10
    else
      self["cpu"]["cpuhz"] = 0
    self["cpu"]["cpusleep"] = self["cpu"]["cpuhz"] ? (1 / self["cpu"]["cpuhz"]) : 1000000
  }

  if (key == self["cfg"]["emulator"]["speed+10"]) {
    if (self["cpu"]["cpuhz"] < 2000)
      self["cpu"]["cpuhz"] += 10
    else
      self["cpu"]["cpuhz"] = 2000
    self["cpu"]["cpusleep"] = self["cpu"]["cpuhz"] ? (1 / self["cpu"]["cpuhz"]) : 1000000
  }

  if (key == self["cfg"]["emulator"]["speed-1"]) {
    if (self["cpu"]["cpuhz"] >= 1) {
      self["cpu"]["cpuhz"] -= 1
      self["cpu"]["cpusleep"] = self["cpu"]["cpuhz"] ? (1 / self["cpu"]["cpuhz"]) : 1000000
    }
  }

  if (key == self["cfg"]["emulator"]["speed+1"]) {
    if (self["cpu"]["cpuhz"] < 2000) {
      self["cpu"]["cpuhz"] += 1
      self["cpu"]["cpusleep"] = self["cpu"]["cpuhz"] ? (1 / self["cpu"]["cpuhz"]) : 1000000
    }
  }

  if (key == self["cfg"]["emulator"]["debug"]) {
    self["cfg"]["main"]["debug"] = !self["cfg"]["main"]["debug"]
    self["disp"]["refresh"] = 1
    printf("\033[?2J")
  }

  if ((key == self["cfg"]["emulator"]["step"]) && (self["cpu"]["cpuhz"] == 0)) {
    self["cpu"]["run"] = 1
    self["timer"]["lastcpu"] = awk::gettimeofday()
  }
}


function update_timers(self,    now, cpu, timer, kb, i, key, cmd) {
  now = awk::gettimeofday()

  # since last update
  cpu   = now - self["timer"]["lastcpu"]
  timer = now - self["timer"]["lasttimer"]
  kb    = now - self["timer"]["keyboard"]

  # update virtual keyboard
  if ( kb >= self["cpu"]["keyboardsleep"] ) {
    chip8::keyboard(self)
    self["timer"]["keyboard"] = now
  }

  # update delay and sound timers
  if ( timer >= self["cpu"]["timersleep"] ) {
    if (self["timer"]["delay"] > 0) {
      self["timer"]["delay"] -= (timer / self["cpu"]["timersleep"])
      if (self["timer"]["delay"] < 0)
        self["timer"]["delay"] = 0
    }

    if (self["timer"]["sound"] > 0) {
      self["timer"]["sound"] -= (timer / self["cpu"]["timersleep"])
      if (self["timer"]["sound"] < 0)
        self["timer"]["sound"] = 0
    }

    self["timer"]["lasttimer"] = now - (timer % self["cpu"]["timersleep"])
  }

  # check CPU speed for a new cycle
  if ( cpu >= self["cpu"]["cpusleep"] ) {
    self["cpu"]["run"] = 1
    self["timer"]["lastcpu"] = now
  } else awk::sleep(0.00001)

}

function cycle(self) {
  # only run if CPU is not in 'halt' status
  if (!self["cpu"]["halt"]) {
    # update timers first
    update_timers(self)

    # only run if a cpu has a cycle waiting
    if (self["cpu"]["run"]) {
      cpu::fetch(self)
      cpu::execute(self)
      self["cpu"]["run"] = 0
      self["cpu"]["cycles"]++
    }
  }
}

