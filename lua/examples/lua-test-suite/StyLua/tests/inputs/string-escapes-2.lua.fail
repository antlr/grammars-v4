-- standard escapes
local a = "foo \a \b \f \n \r \t \v \\ \" \'"

-- decimal escapes
local b = "\000 \001 \189 \254 \255"
local b2 = "\1 \2 \71\9"

-- lua 5.2: hex escapes
local c = "hello \x77\x6f\x72\x6c\x64\x99"

-- lua 5.2: \z
local d = "hello \z  test"

-- lua 5.3: utf8
local e = "\u{123} \u{255}"

-- wrong:
local f = "\q \p \e"
