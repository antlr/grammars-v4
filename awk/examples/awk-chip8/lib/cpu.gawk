@namespace "cpu"

BEGIN {
  mnem["0000"]			= "NOP"
  mnem["00C(.)"]		= "SCD 0x%s"
  mnem["00D(.)"]		= "SCU 0x%s"
  mnem["00E0"]			= "CLS"
  mnem["00EE"]			= "RET"
#  mnem["0([^0][^0E][^0])"]	= "SYS  0x%s"
  mnem["00FB"]			= "SCR"
  mnem["00FC"]			= "SCL"
  mnem["00FD"]			= "EXIT"
  mnem["00FE"]			= "LOW"
  mnem["00FF"]			= "HIGH"
  mnem["1(...)"]		= "JP   0x%s"
  mnem["2(...)"]		= "CALL 0x%s"
  mnem["3(.)(..)"]		= "SE   V%s,0x%s"
  mnem["4(.)(..)"]		= "SNE  V%s,0x%s"
  mnem["5(.)(.)0"]		= "SE   V%s,V%s"
  mnem["6(.)(..)"]		= "LD   V%s,0x%s"
  mnem["7(.)(..)"]		= "ADD  V%s,0x%s"
  mnem["8(.)(.)0"]		= "LD   V%s,V%s"
  mnem["8(.)(.)1"]		= "OR   V%s,V%s"
  mnem["8(.)(.)2"]		= "AND  V%s,V%s"
  mnem["8(.)(.)3"]		= "XOR  V%s,V%s"
  mnem["8(.)(.)4"]		= "ADD  V%s,V%s"
  mnem["8(.)(.)5"]		= "SUB  V%s,V%s"
  mnem["8(.)(.)6"]		= "SHR  V%s,V%s"
  mnem["8(.)(.)7"]		= "SUBN V%s,V%s"
  mnem["8(.)(.)E"]		= "SHL  V%s,V%s"
  mnem["9(.)(.)0"]		= "SNE  V%s,V%s"
  mnem["A(...)"]		= "LD   I,0x%s"
  mnem["B(...)"]		= "JP   V0,0x%s"
  mnem["C(.)(..)"]		= "RND  V%s,0x%s"
  mnem["D(.)(.)(.)"]		= "DRW  V%s,V%s,0x%s"
  mnem["E(.)9E"]		= "SKP  V%s"
  mnem["E(.)A1"]		= "SKNP V%s"
  mnem["F(.)07"]		= "LD   V%s,DT"
  mnem["F(.)0A"]		= "LD   V%s,K"
  mnem["F(.)15"]		= "LD   DT,V%s"
  mnem["F(.)18"]		= "LD   ST,V%s"
  mnem["F(.)1E"]		= "ADD  I,V%s"
  mnem["F(.)29"]		= "LD   F,V%s"
  mnem["F(.)33"]		= "LD   B,V%s"
  mnem["F(.)55"]		= "LD   [I],V%s"
  mnem["F(.)65"]		= "LD   V%s,[I]"

  opcode["NOO?P"]               = "0000"
  opcode["SCD 0x(.)"]           = "00C%s"
  opcode["SCU 0x(.)"]           = "00D%s"
  opcode["CLS"]                 = "00E0"
  opcode["RET"]                 = "00EE"
  opcode["SCR"]                 = "00FB"
  opcode["SCL"]                 = "00FC"
  opcode["EXIT"]                = "00FD"
  opcode["LOW"]                 = "00FE"
  opcode["HIGH"]                = "00FF"
  opcode["JM?P 0x0?(...)"]      = "1%s"
  opcode["CALL 0x0?(...)"]      = "2%s"
  opcode["SE V(.), ?0x(..)"]    = "3%s%s"
  opcode["SNE V(.), ?0x(..)"]   = "4%s%s"
  opcode["SE V(.), ?V(.)"]      = "5%s%s0"
  opcode["LD V(.), ?0x(..)"]    = "6%s%s"
  opcode["ADD V(.), ?0x(..)"]   = "7%s%s"
  opcode["LD V(.), ?V(.)"]      = "8%s%s0"
  opcode["OR V(.), ?V(.)"]      = "8%s%s1"
  opcode["AND V(.), ?V(.)"]     = "8%s%s2"
  opcode["XOR V(.), ?V(.)"]     = "8%s%s3"
  opcode["ADD V(.), ?V(.)"]     = "8%s%s4"
  opcode["SUB V(.), ?V(.)"]     = "8%s%s5"
  opcode["SHR V(.), ?V(.)"]     = "8%s%s6"
  opcode["SUBN V(.), ?V(.)"]    = "8%s%s7"
  opcode["SHL V(.), ?V(.)"]     = "8%s%sE"
  opcode["SNE V(.), ?V(.)"]     = "9%s%s0"
  opcode["LD I, ?0x0?(...)"]    = "A%s"
  opcode["JM?P V0, ?0x0?(...)"] = "B%s"
  opcode["RND V(.), ?0x(..)"]   = "C%s%s"
  opcode["DRA?W V(.), ?V(.), ?0x(.)"] = "D%s%s%s"
  opcode["SKP V(.)"]            = "E%s9E"
  opcode["SKNP V(.)"]           = "E%sA1"
  opcode["LD V(.), ?DT"]        = "F%s07"
  opcode["LD V(.), ?K"]         = "F%s0A"
  opcode["LD DT, ?V(.)"]        = "F%s15"
  opcode["LD ST, ?V(.)"]        = "F%s18"
  opcode["ADD I, ?V(.)"]        = "F%s1E"
  opcode["LD F, ?V(.)"]         = "F%s29"
  opcode["LD B, ?V(.)"]         = "F%s33"
  opcode["LD \\[I\\], ?V(.)"]   = "F%s55"
  opcode["LD V(.), ?\\[I\\]"]   = "F%s65"

  opcode["DB 0x(..), ?0x(..)"]  = "%s%s"
  opcode["DW 0x(....)"]         = "%s"
}


function disasm(opcode,    hex, op, argv) {
  hex = sprintf("%04X", opcode)

  for (op in mnem)
    if (match(hex, op, argv))
      # there are at most 3 arguments for an opcode
      return sprintf(mnem[op], argv[1], argv[2], argv[3])

  return "<UNKNOWN>"
}


function fetch(self,    pc) {
  pc = self["pc"]
  self["opcode"] = self["mem"][pc] * 256 + self["mem"][pc+1]
  self["pc"] += 2
}


function execute(self,     opcode, i,n, vx,vy, x,y, bit,byte,word,offsetx,offsety,pre,overflow) {
  opcode = self["opcode"]

  # NOP (No Operation)
  if ( 0x0000 == opcode ) {
    return 1
  }

  # SCD <nible> (scroll down content of display 0-15 lines)
  if ( 0x00C0 == awk::and(opcode, 0xFFF0) ) {
    n = awk::and(opcode, 0x000F)

    w = self["disp"]["width"]
    h = self["disp"]["height"]

    for (y=(h-1); y>=n; y--)
      for (x=0; x<w; x++)
        self["disp"][y*w+x] = self["disp"][(y-n)*w+x]

    for (y=(n-1); y>=0; y--)
      for (x=0; x<w; x++)
        self["disp"][y*w+x] = 0

    self["disp"]["refresh"] = 1
    return 1
  }

  # SCU <nible> (scroll up content of display 0-15 lines)
  if ( 0x00D0 == awk::and(opcode, 0xFFF0) ) {
    n = awk::and(opcode, 0x000F)

    w = self["disp"]["width"]
    h = self["disp"]["height"]

    for (y=0; y<(h-n); y++)
      for (x=0; x<w; x++)
        self["disp"][y*w+x] = self["disp"][(y+n)*w+x]

    for (y=(h-n); y<h; y++)
      for (x=0; x<w; x++)
        self["disp"][y*w+x] = 0

    self["disp"]["refresh"] = 1
    return 1
  }

  # CLS (Clear Screen)
  if ( 0x00E0 == opcode ) {
    n = self["disp"]["width"] * self["disp"]["height"]
    for (i=0; i<=n; i++)
      self["disp"][i] = 0x00

    self["disp"]["refresh"] = 1
    return 1
  }

  # RET (Return from subroutine)
  if ( 0x00EE == opcode ) {
    self["pc"] = self["stack"][self["sp"]--] + 2
    return 1
  }

  # SCR (Scroll Right, 4 pixels (hires), or 2 pixels (lores))
  if ( 0x00FB == opcode ) {
    w = self["disp"]["width"]
    h = self["disp"]["height"]
    n = int(self["disp"]["width"] / 32)

    for (y=0; y<h; y++) {
      yw = y*w
      for (x=w; x>=n; x--)
        self["disp"][yw+x] = self["disp"][yw+x-n]
      for (x=n; x>=0; x--)
        self["disp"][yw+x] = 0
    }

    self["disp"]["refresh"] = 1
    return 1
  }

  # SCL (Scroll Left, 4 pixels (hires), or 2 pixels (lores))
  if ( 0x00FC == opcode ) {
    w = self["disp"]["width"]
    h = self["disp"]["height"]
    n = int(self["disp"]["width"] / 32)

    for (y=0; y<h; y++) {
      yw = y*w
      for (x=0; x<(w-n); x++)
        self["disp"][yw+x] = self["disp"][yw+x+n]
      for (x=(w-n); x<w; x++)
        self["disp"][yw+x] = 0
    }

    self["disp"]["refresh"] = 1
    return 1
  }

  # EXIT (EXIT emulator)
  if ( 0x00FD == opcode ) {
    exit 0
  }

  # LOW (Low resolution 64x32)
  if ( 0x00FE == opcode ) {
    self["disp"]["hires"]  = 0
    self["disp"]["width"]  = 64
    self["disp"]["height"] = 32
    self["disp"]["refresh"] = 1
    return 1
  }

  # HIGH (High resolution 128x64)
  if ( 0x00FF == opcode ) {
    self["disp"]["hires"]   = 1
    self["disp"]["width"]   = 128
    self["disp"]["height"]  = 64
    self["disp"]["refresh"] = 1
    return 1
  }

  # JP addr (Jump to address)
  if ( 0x1000 == awk::and(opcode, 0xF000) ) {
    # exit if we jump to ourself (infinite loop)
    if ((self["pc"]-2) == awk::and(opcode, 0x0FFF))
      exit 0

    self["pc"] = awk::and(opcode, 0x0FFF)
    return 1
  }

  # CALL addr (jump to subroutine at address)
  if ( 0x2000 == awk::and(opcode, 0xF000) ) {
    self["stack"][++self["sp"]] = self["pc"]-2
    self["pc"] = awk::and(opcode, 0x0FFF)
    return 1
  }

  # SE Vx (Vx == byte)
  if ( 0x3000 == awk::and(opcode, 0xF000) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    if (self["V"][vx] == awk::and(opcode, 0x00FF))
      self["pc"] += 2
    return 1
  }

  # SNE Vx (Vx != byte)
  if ( 0x4000 == awk::and(opcode, 0xF000) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    if (self["V"][vx] != awk::and(opcode, 0x00FF))
      self["pc"] += 2
    return 1
  }

  # SE Vx,Vy (Vx == Vy)
  if ( 0x5000 == awk::and(opcode, 0xF00F) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    vy = awk::rshift(awk::and(opcode, 0x00F0), 4)
    if (self["V"][vx] == self["V"][vy])
      self["pc"] += 2
    return 1
  }

  # LD Vx, byte (Vx := byte)
  if ( 0x6000 == awk::and(opcode, 0xF000) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    self["V"][vx] = awk::and(opcode, 0x00FF)
    return 1
  }

  # ADD Vx, byte (Vx += byte)
  if ( 0x7000 == awk::and(opcode, 0xF000) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    self["V"][vx] = awk::and(self["V"][vx] + awk::and(opcode, 0x00FF), 0xFF)
    return 1
  }

  # LD Vx, Vy (Vx := Vy)
  if ( 0x8000 == awk::and(opcode, 0xF00F) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    vy = awk::rshift(awk::and(opcode, 0x00F0), 4)
    self["V"][vx] = self["V"][vy]
    return 1
  }

  # OR Vx, Vy (Vx |= Vy)
  if ( 0x8001 == awk::and(opcode, 0xF00F) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    vy = awk::rshift(awk::and(opcode, 0x00F0), 4)
    self["V"][vx] = awk::or(self["V"][vx], self["V"][vy])
    return 1
  }

  # AND Vx, Vy (Vx &= Vy)
  if ( 0x8002 == awk::and(opcode, 0xF00F) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    vy = awk::rshift(awk::and(opcode, 0x00F0), 4)
    self["V"][vx] = awk::and(self["V"][vx], self["V"][vy])
    return 1
  }

  # XOR Vx, Vy (Vx ^= Vy)
  if ( 0x8003 == awk::and(opcode, 0xF00F) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    vy = awk::rshift(awk::and(opcode, 0x00F0), 4)
    self["V"][vx] = awk::xor(self["V"][vx], self["V"][vy])
    return 1
  }

  # ADD Vx, Vy (Vx += Vy)
  if ( 0x8004 == awk::and(opcode, 0xF00F) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    vy = awk::rshift(awk::and(opcode, 0x00F0), 4)
    i = self["V"][vx] + self["V"][vy]
    self["V"][vx] = awk::and(i, 0xFF)
    self["V"][0xF] = (i > 0xFF) ? 1 : 0
    return 1
  }

  # SUB Vx, Vy (Vx = Vx - Vy)
  if ( 0x8005 == awk::and(opcode, 0xF00F) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    vy = awk::rshift(awk::and(opcode, 0x00F0), 4)
    overflow = 1

    i = self["V"][vx] - self["V"][vy]
    if (i < 0) {
      i = 256 + (i%256)
      overflow = 0
    }
    self["V"][vx] = awk::and(i, 0xFF)
    self["V"][0xF] = overflow
    return 1
  }

  # SHR Vx, 1 (Vx >> 1)
  if ( 0x8006 == awk::and(opcode, 0xF00F) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    overflow = self["V"][vx] % 2
    self["V"][vx] = awk::and(awk::rshift(self["V"][vx], 1), 0xFF)
    self["V"][0xF] = overflow
    return 1
  }

  # SUBN Vx, Vy (Vy = Vy - Vx)
  if ( 0x8007 == awk::and(opcode, 0xF00F) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    vy = awk::rshift(awk::and(opcode, 0x00F0), 4)
    overflow = 1

    i = self["V"][vy] - self["V"][vx]
    if (i < 0) {
      i = 256 + (i%256)
      overflow = 0
    }
    self["V"][vx] = awk::and(i, 0xFF)
    self["V"][0xF] = overflow
    return 1
  }

  # SHL Vx, 1 (Vx << 1)
  if ( 0x800E == awk::and(opcode, 0xF00F) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    overflow = awk::rshift(self["V"][vx], 7)
    self["V"][vx] = awk::and(awk::lshift(self["V"][vx], 1), 0xFF)
    self["V"][0xF] = overflow
    return 1
  }

  # SNE Vx, Vy (Vx != Vy)
  if ( 0x9000 == awk::and(opcode, 0xF00F) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    vy = awk::rshift(awk::and(opcode, 0x00F0), 4)
    if (self["V"][vx] != self["V"][vy])
      self["pc"] += 2
    return 1
  }

  # LD I, addr (I := addr)
  if ( 0xA000 == awk::and(opcode, 0xF000) ) {
    self["I"] = awk::and(opcode, 0x0FFF)
    return 1
  }

  # JP V0, addr (jump to address V0 + addr)
  if ( 0xB000 == awk::and(opcode, 0xF000) ) {
    self["pc"] = self["V"][0] + awk::and(opcode, 0x0FFF)
    return 1
  }

  # RND Vx, byte (random number & byte)
  if ( 0xC000 == awk::and(opcode, 0xF000) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    i = int(rand() * 255)
    self["V"][vx] = awk::and(i, awk::and(opcode, 0x00FF))
    return 1
  }

  # DRW Vx, Vy, N (draw byte at [I] at Vx,Vy, N times high)
  if ( 0xD000 == awk::and(opcode, 0xF000) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    vy = awk::rshift(awk::and(opcode, 0x00F0), 4)
    n  = awk::and(opcode, 0x000F)

    w = self["disp"]["width"]

    self["V"][0xF] = 0
    if (n > 0) {
      for (y=0; y<n; y++) {
        offsety = ((self["V"][vy] + y) % self["disp"]["height"]) * w
        byte = self["mem"][(self["I"] + y)]
        for (x=0; x<8; x++) {
          offsetx = ((self["V"][vx] + x) % self["disp"]["width"])
          bit = awk::and(awk::rshift(byte, (7-x)), 0x01)
          pre = self["disp"][offsety + offsetx]
          self["disp"][offsety + offsetx] = awk::xor(pre, bit)
          self["V"][0xF] = (bit && pre) ? 1 : self["V"][0xF]
        }
      }
    } else {
      # DXY0 Draw 16x16 pixels sprite from [I] at VX, VY. Sprite is stored in 32 bytes, 2 bytes per row with leftmost byte last.
      for (y=0; y<16; y++) {
        offsety = ((self["V"][vy] + y) % self["disp"]["height"]) * w
        word = self["mem"][(self["I"] + (y*2))] * 256 + self["mem"][(self["I"] + (y*2) + 1)]
        for (x=0; x<16; x++) {
          offsetx = ((self["V"][vx] + x) % self["disp"]["width"])
          bit = awk::and(awk::rshift(word, (15-x)), 0x01)
          pre = self["disp"][offsety + offsetx]
          self["disp"][offsety + offsetx] = awk::xor(pre, bit)
          self["V"][0xF] = (bit && pre) ? 1 : self["V"][0xF]
        }
      }
    }

    self["disp"]["refresh"] = 1
    return 1
  }

  # SKP Vx (Check keypress with Vx)
  if ( 0xE09E == awk::and(opcode, 0xF0FF) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    byte = self["V"][vx]
    if (self["keyboard"][byte] > 0)
      self["pc"] += 2
    return 1
  }

  # SKNP Vx (Check not-keypressed with Vx)
  if ( 0xE0A1 == awk::and(opcode, 0xF0FF) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    byte = self["V"][vx]
    if (self["keyboard"][byte] == 0)
      self["pc"] += 2
    return 1
  }

  # LD Vx,DT (Vx := delay timer)
  if ( 0xF007 == awk::and(opcode, 0xF0FF) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    self["V"][vx] = self["timer"]["delay"]
    return 1
  }

  # LD Vx, K (Vx := key press)
  if ( 0xF00A == awk::and(opcode, 0xF0FF) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    self["V"][vx] = 0x00

    # get real keyboard input from OS 
    cmd = "dd bs=1 count=1 2>/dev/null"
    if ((cmd | getline key) < 1) key=""
    close(cmd)

    # register virtual keypress 
    if (key in self["cfg"]["keyboard"])
      self["V"][vx] = self["cfg"]["keyboard"][key]

    # escape exits emulator
    if (key == "\033") exit 0

    return 1
  }

  # LD DT, Vx (Delay timer := Vx)
  if ( 0xF015 == awk::and(opcode, 0xF0FF) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    self["timer"]["delay"] = self["V"][vx]
    return 1
  }

  # LD ST, Vx (Sound timer := Vx)
  if ( 0xF018 == awk::and(opcode, 0xF0FF) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    self["timer"]["sound"] = self["V"][vx]
    return 1
  }

  # ADD I, Vx (I += Vx)
  if ( 0xF01E == awk::and(opcode, 0xF0FF) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    self["I"] = awk::and(self["I"] + self["V"][vx], 0xFFF)
    return 1
  }

  # LD F, Vx (I := sprite location at Vx)
  if ( 0xF029 == awk::and(opcode, 0xF0FF) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    self["I"] = awk::and(self["V"][vx], 0x0F) * 5
    return 1
  }

  # LD B, Vx ([I] := BCD of Vx)
  if ( 0xF033 == awk::and(opcode, 0xF0FF) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    i = self["I"]
    self["mem"][i+0] = (self["V"][vx] / 100) % 10
    self["mem"][i+1] = (self["V"][vx] / 10) % 10
    self["mem"][i+2] = self["V"][vx] % 10
    return 1
  }

  # LD [I], Vx (save V0-Vx starting at [I])
  if ( 0xF055 == awk::and(opcode, 0xF0FF) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    i = self["I"]
    for (n=0; n<=vx; n++)
      self["mem"][i+n] = awk::and(self["V"][n], 0xFF)
    return 1
  }

  # LD Vx, [I] (load V0-Vx from [I] onwards)
  if ( 0xF065 == awk::and(opcode, 0xF0FF) ) {
    vx = awk::rshift(awk::and(opcode, 0x0F00), 8)
    i = self["I"]
    for (n=0; n<=vx; n++)
      self["V"][n] = awk::and(self["mem"][i+n], 0xFF)
    return 1
  }

  # no opcode found, halt
  printf("Unknown opcode 0x%04X found at 0x%04X\nHalting CPU...\n", opcode, self["pc"]-2)
  self["cpu"]["halt"] = 1
  exit 1
}

