--[[
 * AES Cipher function: encrypt 'input' with Rijndael algorithm
 *
 *   takes   byte-array 'input' (16 bytes)
 *           2D byte-array key schedule 'w' (Nr+1 x Nb bytes)
 *
 *   applies Nr rounds (10/12/14) using key schedule w for 'add round key' stage
 *
 *   returns byte-array encrypted value (16 bytes)
 */]]

local bench = script and require(script.Parent.bench_support) or require("bench_support")

-- Sbox is pre-computed multiplicative inverse in GF(2^8) used in SubBytes and KeyExpansion [§5.1.1]
local Sbox =  { 0x63,0x7c,0x77,0x7b,0xf2,0x6b,0x6f,0xc5,0x30,0x01,0x67,0x2b,0xfe,0xd7,0xab,0x76,
             0xca,0x82,0xc9,0x7d,0xfa,0x59,0x47,0xf0,0xad,0xd4,0xa2,0xaf,0x9c,0xa4,0x72,0xc0,
             0xb7,0xfd,0x93,0x26,0x36,0x3f,0xf7,0xcc,0x34,0xa5,0xe5,0xf1,0x71,0xd8,0x31,0x15,
             0x04,0xc7,0x23,0xc3,0x18,0x96,0x05,0x9a,0x07,0x12,0x80,0xe2,0xeb,0x27,0xb2,0x75,
             0x09,0x83,0x2c,0x1a,0x1b,0x6e,0x5a,0xa0,0x52,0x3b,0xd6,0xb3,0x29,0xe3,0x2f,0x84,
             0x53,0xd1,0x00,0xed,0x20,0xfc,0xb1,0x5b,0x6a,0xcb,0xbe,0x39,0x4a,0x4c,0x58,0xcf,
             0xd0,0xef,0xaa,0xfb,0x43,0x4d,0x33,0x85,0x45,0xf9,0x02,0x7f,0x50,0x3c,0x9f,0xa8,
             0x51,0xa3,0x40,0x8f,0x92,0x9d,0x38,0xf5,0xbc,0xb6,0xda,0x21,0x10,0xff,0xf3,0xd2,
             0xcd,0x0c,0x13,0xec,0x5f,0x97,0x44,0x17,0xc4,0xa7,0x7e,0x3d,0x64,0x5d,0x19,0x73,
             0x60,0x81,0x4f,0xdc,0x22,0x2a,0x90,0x88,0x46,0xee,0xb8,0x14,0xde,0x5e,0x0b,0xdb,
             0xe0,0x32,0x3a,0x0a,0x49,0x06,0x24,0x5c,0xc2,0xd3,0xac,0x62,0x91,0x95,0xe4,0x79,
             0xe7,0xc8,0x37,0x6d,0x8d,0xd5,0x4e,0xa9,0x6c,0x56,0xf4,0xea,0x65,0x7a,0xae,0x08,
             0xba,0x78,0x25,0x2e,0x1c,0xa6,0xb4,0xc6,0xe8,0xdd,0x74,0x1f,0x4b,0xbd,0x8b,0x8a,
             0x70,0x3e,0xb5,0x66,0x48,0x03,0xf6,0x0e,0x61,0x35,0x57,0xb9,0x86,0xc1,0x1d,0x9e,
             0xe1,0xf8,0x98,0x11,0x69,0xd9,0x8e,0x94,0x9b,0x1e,0x87,0xe9,0xce,0x55,0x28,0xdf,
             0x8c,0xa1,0x89,0x0d,0xbf,0xe6,0x42,0x68,0x41,0x99,0x2d,0x0f,0xb0,0x54,0xbb,0x16 };

-- Rcon is Round Constant used for the Key Expansion [1st col is 2^(r-1) in GF(2^8)] [§5.2]
local Rcon = { { 0x00, 0x00, 0x00, 0x00 },
             {0x01, 0x00, 0x00, 0x00},
             {0x02, 0x00, 0x00, 0x00},
             {0x04, 0x00, 0x00, 0x00},
             {0x08, 0x00, 0x00, 0x00},
             {0x10, 0x00, 0x00, 0x00},
             {0x20, 0x00, 0x00, 0x00},
             {0x40, 0x00, 0x00, 0x00},
             {0x80, 0x00, 0x00, 0x00},
             {0x1b, 0x00, 0x00, 0x00},
             {0x36, 0x00, 0x00, 0x00} }; 

local function SubBytes(s, Nb)    -- apply SBox to state S [§5.1.1]
  for r = 0,3 do
    for c = 0,Nb-1 do s[r + 1][c + 1] = Sbox[s[r + 1][c + 1] + 1]; end
  end
  return s;
end


local function ShiftRows(s, Nb)    -- shift row r of state S left by r bytes [§5.1.2]
  local t = {};
  for r = 1,3 do
    for c = 0,3 do t[c + 1] = s[r + 1][((c + r) % Nb) + 1] end;  -- shift into temp copy
    for c = 0,3 do s[r + 1][c + 1] = t[c + 1]; end         -- and copy back
  end          -- note that this will work for Nb=4,5,6, but not 7,8 (always 4 for AES):
  return s;  -- see fp.gladman.plus.com/cryptography_technology/rijndael/aes.spec.311.pdf 
end


local function MixColumns(s, Nb)   -- combine bytes of each col of state S [§5.1.3]
  for c = 0,3 do
    local a = {};  -- 'a' is a copy of the current column from 's'
    local b = {};  -- 'b' is a•{02} in GF(2^8)
    for i = 0,3 do
      a[i + 1] = s[i + 1][c + 1];

      if bit32.band(s[i + 1][c + 1], 0x80) ~= 0 then
        b[i + 1] = bit32.bxor(bit32.lshift(s[i + 1][c + 1], 1), 0x011b);
      else
        b[i + 1] = bit32.lshift(s[i + 1][c + 1], 1);
      end
    end
    -- a[n] ^ b[n] is a•{03} in GF(2^8)
    s[1][c + 1] = bit32.bxor(b[1], a[2], b[2], a[3], a[4]); -- 2*a0 + 3*a1 + a2 + a3
    s[2][c + 1] = bit32.bxor(a[1], b[2], a[3], b[3], a[4]); -- a0 * 2*a1 + 3*a2 + a3
    s[3][c + 1] = bit32.bxor(a[1], a[2], b[3], a[4], b[4]); -- a0 + a1 + 2*a2 + 3*a3
    s[4][c + 1] = bit32.bxor(a[1], b[1], a[2], a[3], b[4]); -- 3*a0 + a1 + a2 + 2*a3
end
  return s;
end


local function SubWord(w)    -- apply SBox to 4-byte word w
  for i = 0,3 do w[i + 1] = Sbox[w[i + 1] + 1]; end
  return w;
end

local function RotWord(w)    -- rotate 4-byte word w left by one byte
  w[5] = w[1];
  for i = 0,3 do w[i + 1] = w[i + 2]; end
  return w;
end



local function AddRoundKey(state, w, rnd, Nb)  -- xor Round Key into state S [§5.1.4]
  for r = 0,3 do
    for c = 0,Nb-1 do state[r + 1][c + 1] = bit32.bxor(state[r + 1][c + 1], w[rnd*4+c + 1][r + 1]); end
  end
  return state;
end

local function Cipher(input, w)    -- main Cipher function [§5.1]
  local Nb = 4;               -- block size (in words): no of columns in state (fixed at 4 for AES)
  local Nr = #w / Nb - 1; -- no of rounds: 10/12/14 for 128/192/256-bit keys

  local state = {{},{},{},{}};  -- initialise 4xNb byte-array 'state' with input [§3.4]
  for i = 0,4*Nb-1 do state[(i % 4) + 1][math.floor(i/4) + 1] = input[i + 1]; end

  state = AddRoundKey(state, w, 0, Nb);

  for round = 1,Nr-1 do
    state = SubBytes(state, Nb);
    state = ShiftRows(state, Nb);
    state = MixColumns(state, Nb);
    state = AddRoundKey(state, w, round, Nb);
  end

  state = SubBytes(state, Nb);
  state = ShiftRows(state, Nb);
  state = AddRoundKey(state, w, Nr, Nb);

  local output = {}  -- convert state to 1-d array before returning [§3.4]
  for i = 0,4*Nb-1 do output[i + 1] = state[(i % 4) + 1][math.floor(i / 4) + 1]; end

  return output;
end


local function KeyExpansion(key)  -- generate Key Schedule (byte-array Nr+1 x Nb) from Key [§5.2]
  local Nb = 4;            -- block size (in words): no of columns in state (fixed at 4 for AES)
  local Nk = #key / 4  -- key length (in words): 4/6/8 for 128/192/256-bit keys
  local Nr = Nk + 6;       -- no of rounds: 10/12/14 for 128/192/256-bit keys

  local w = {};
  local temp = {};

  for i = 0,Nk do
    local r = { key[4*i + 1], key[4*i + 2], key[4*i + 3], key[4*i + 4] };
    w[i + 1] = r;
  end

  for i = Nk,(Nb*(Nr+1)) - 1 do
    w[i + 1] = {};
    for t = 0,3 do temp[t + 1] = w[i-1 + 1][t + 1]; end
    if (i % Nk == 0) then
      temp = SubWord(RotWord(temp));
      for t = 0,3 do temp[t + 1] = bit32.bxor(temp[t + 1], Rcon[i/Nk + 1][t + 1]); end
    elseif (Nk > 6 and i % Nk == 4) then
      temp = SubWord(temp);
    end
    for t = 0,3 do w[i + 1][t + 1] = bit32.bxor(w[i - Nk + 1][t + 1], temp[t + 1]); end
  end

  return w;
end

local function escCtrlChars(str)  -- escape control chars which might cause problems handling ciphertext
  return string.gsub(str, "[\0\t\n\v\f\r\'\"!-]", function(c) return '!' .. string.byte(c, 1) .. '!'; end);
end

local function unescCtrlChars(str)  -- unescape potentially problematic control characters
  return string.gsub(str, "!%d%d?%d?!", function(c)
    local sc = string.sub(c, 2,-2)

    return string.char(tonumber(sc));
  end);
end

--[[ 
 * Use AES to encrypt 'plaintext' with 'password' using 'nBits' key, in 'Counter' mode of operation
 *                           - see http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf
 *   for each block
 *   - outputblock = cipher(counter, key)
 *   - cipherblock = plaintext xor outputblock
 ]]

local function AESEncryptCtr(plaintext, password, nBits)
  if (not (nBits==128 or nBits==192 or nBits==256)) then return ''; end  -- standard allows 128/192/256 bit keys

  -- for this example script, generate the key by applying Cipher to 1st 16/24/32 chars of password; 
  -- for real-world applications, a higher security approach would be to hash the password e.g. with SHA-1
  local nBytes = nBits/8;  -- no bytes in key
  local pwBytes = {};
  for i = 0,nBytes-1 do pwBytes[i + 1] = string.byte(password, i + 1); end
  local key = Cipher(pwBytes, KeyExpansion(pwBytes));

  -- key is now 16/24/32 bytes long
  for i = 1,nBytes-16 do
    table.insert(key, key[i])
  end

  -- initialise counter block (NIST SP800-38A §B.2): millisecond time-stamp for nonce in 1st 8 bytes,
  -- block counter in 2nd 8 bytes
  local blockSize = 16;  -- block size fixed at 16 bytes / 128 bits (Nb=4) for AES
  local counterBlock = {};  -- block size fixed at 16 bytes / 128 bits (Nb=4) for AES
  local nonce = os.clock() * 1000 -- (new Date()).getTime();  -- milliseconds since 1-Jan-1970

  -- encode nonce in two stages to cater for JavaScript 32-bit limit on bitwise ops
  for i = 0,3 do counterBlock[i + 1] = bit32.extract(nonce, i * 8, 8); end
  for i = 0,3 do counterBlock[i + 4 + 1] = bit32.extract(math.floor(nonce / 0x100000000), i*8, 8); end

  -- generate key schedule - an expansion of the key into distinct Key Rounds for each round
  local keySchedule = KeyExpansion(key);

  local blockCount = math.ceil(#plaintext / blockSize);
  local ciphertext = {};  -- ciphertext as array of strings
  
  for b = 0,blockCount-1 do
    -- set counter (block #) in last 8 bytes of counter block (leaving nonce in 1st 8 bytes)
    -- again done in two stages for 32-bit ops
    for c = 0,3 do counterBlock[15-c + 1] = bit32.extract(b, c*8, 8); end
    for c = 0,3 do counterBlock[15-c-4 + 1] = bit32.extract(math.floor(b/0x100000000), c*8, 8); end

    local cipherCntr = Cipher(counterBlock, keySchedule);  -- -- encrypt counter block --
    
    -- calculate length of final block:
    local blockLength = nil
    
    if b<blockCount-1 then
      blockLength = blockSize;
    else
      blockLength = (#plaintext - 1) % blockSize+1;
    end

    local ct = '';
    for i = 0,blockLength-1 do  -- -- xor plaintext with ciphered counter byte-by-byte --
      local plaintextByte = string.byte(plaintext, b*blockSize+i + 1);
      local cipherByte = bit32.bxor(plaintextByte, cipherCntr[i + 1]);
      ct = ct .. string.char(cipherByte);
    end
    -- ct is now ciphertext for this block

    ciphertext[b + 1] = escCtrlChars(ct);  -- escape troublesome characters in ciphertext
  end

  -- convert the nonce to a string to go on the front of the ciphertext
  local ctrTxt = '';
  for i = 0,7 do ctrTxt = ctrTxt .. string.char(counterBlock[i + 1]); end
  ctrTxt = escCtrlChars(ctrTxt);

  -- use '-' to separate blocks, use Array.join to concatenate arrays of strings for efficiency
  return ctrTxt .. '-' .. table.concat(ciphertext, '-');
end


--[[
 * Use AES to decrypt 'ciphertext' with 'password' using 'nBits' key, in Counter mode of operation
 *
 *   for each block
 *   - outputblock = cipher(counter, key)
 *   - cipherblock = plaintext xor outputblock
 ]]

local function AESDecryptCtr(ciphertext, password, nBits)
  if (not (nBits==128 or nBits==192 or nBits==256)) then return ''; end  -- standard allows 128/192/256 bit keys

  local nBytes = nBits/8;  -- no bytes in key
  local pwBytes = {};
  for i = 0,nBytes-1 do pwBytes[i + 1] = string.byte(password, i + 1); end
  local pwKeySchedule = KeyExpansion(pwBytes);
  local key = Cipher(pwBytes, pwKeySchedule);

  -- key is now 16/24/32 bytes long
  for i = 1,nBytes-16 do
    table.insert(key, key[i])
  end

  local keySchedule = KeyExpansion(key);

  -- split ciphertext into array of block-length strings 
  local tmp = {}

  for token in string.gmatch(ciphertext, "[^-]+") do
       table.insert(tmp, token)
    end

  ciphertext = tmp;

  -- recover nonce from 1st element of ciphertext
  local blockSize = 16;  -- block size fixed at 16 bytes / 128 bits (Nb=4) for AES
  local counterBlock = {};
  local ctrTxt = unescCtrlChars(ciphertext[1]);
  for i = 0,7 do counterBlock[i + 1] = string.byte(ctrTxt, i + 1); end

  local plaintext = {};

  for b = 1,#ciphertext-1 do
    -- set counter (block #) in last 8 bytes of counter block (leaving nonce in 1st 8 bytes)
    for c = 0,3 do counterBlock[15-c + 1] = bit32.extract(b-1, c*8, 8); end
    for c = 0,3 do counterBlock[15-c-4 + 1] = bit32.extract(math.floor((b-1)/0x100000000), c*8, 8); end

    local cipherCntr = Cipher(counterBlock, keySchedule);  -- encrypt counter block

    ciphertext[b + 1] = unescCtrlChars(ciphertext[b + 1]);

    local pt = '';
    for i = 0,#ciphertext[b + 1]-1 do
      -- -- xor plaintext with ciphered counter byte-by-byte --
      local ciphertextByte = string.byte(ciphertext[b + 1], i + 1);
      local plaintextByte = bit32.bxor(ciphertextByte, cipherCntr[i + 1]);
      pt = pt .. string.char(plaintextByte);
    end
    -- pt is now plaintext for this block

    plaintext[b] = pt;  -- b-1 'cos no initial nonce block in plaintext
  end

  return table.concat(plaintext)
end

local function test()

local plainText = "ROMEO: But, soft! what light through yonder window breaks?\n\
It is the east, and Juliet is the sun.\n\
Arise, fair sun, and kill the envious moon,\n\
Who is already sick and pale with grief,\n\
That thou her maid art far more fair than she:\n\
Be not her maid, since she is envious;\n\
Her vestal livery is but sick and green\n\
And none but fools do wear it; cast it off.\n\
It is my lady, O, it is my love!\n\
O, that she knew she were!\n\
She speaks yet she says nothing: what of that?\n\
Her eye discourses; I will answer it.\n\
I am too bold, 'tis not to me she speaks:\n\
Two of the fairest stars in all the heaven,\n\
Having some business, do entreat her eyes\n\
To twinkle in their spheres till they return.\n\
What if her eyes were there, they in her head?\n\
The brightness of her cheek would shame those stars,\n\
As daylight doth a lamp; her eyes in heaven\n\
Would through the airy region stream so bright\n\
That birds would sing and think it were not night.\n\
See, how she leans her cheek upon her hand!\n\
O, that I were a glove upon that hand,\n\
That I might touch that cheek!\n\
JULIET: Ay me!\n\
ROMEO: She speaks:\n\
O, speak again, bright angel! for thou art\n\
As glorious to this night, being o'er my head\n\
As is a winged messenger of heaven\n\
Unto the white-upturned wondering eyes\n\
Of mortals that fall back to gaze on him\n\
When he bestrides the lazy-pacing clouds\n\
And sails upon the bosom of the air.";

local password = "O Romeo, Romeo! wherefore art thou Romeo?";

local cipherText = AESEncryptCtr(plainText, password, 256);
local decryptedText = AESDecryptCtr(cipherText, password, 256);

if (decryptedText ~= plainText) then
    assert(false, "ERROR: bad result: expected " .. plainText .. " but got " .. decryptedText);
end

end

bench.runCode(test, "crypto-aes")
