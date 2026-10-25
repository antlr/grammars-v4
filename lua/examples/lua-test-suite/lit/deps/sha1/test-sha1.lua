
local sha1 = require('./init')
assert(sha1("") == "da39a3ee5e6b4b0d3255bfef95601890afd80709")
assert(sha1("abc") == "a9993e364706816aba3e25717850c26c9cd0d89d")
assert(sha1("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
  == "84983e441c3bd26ebaae4aa1f95129e5e54670f1")
assert(sha1("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")
  == "a49b2446a02c645bf419f995b67091253a04a259")
assert(sha1(string.rep("a", 1000000))
  == "34aa973cd4c4daa4f61eeb2bdbad27316534016f")
local sum = sha1()
sum.update("a")
sum.update("bc")
assert(sum.digest() == "a9993e364706816aba3e25717850c26c9cd0d89d")
sum = sha1()
local aa = string.rep("a", 1000)
for i = 1, 1000 do
  sum.update(aa)
end
assert(sum.digest() == "34aa973cd4c4daa4f61eeb2bdbad27316534016f")

print("All tests pass")
