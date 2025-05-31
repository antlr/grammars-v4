--[[

Copyright 2015 The Luvit Authors. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License")
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS-IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

--]]

local fs = require('fs')
local join = require('path').join
local Buffer = require('buffer').Buffer

local currentFileData = 'ABCD'
local s = '南越国是前203年至前111年存在于岭南地区的一个国家，国都位于番禺，疆域包括今天中国的广东、' ..
        '广西两省区的大部份地区，福建省、湖南、贵州、云南的一小部份地区和越南的北部。' ..
        '南越国是秦朝灭亡后，由南海郡尉赵佗于前203年起兵兼并桂林郡和象郡后建立。' ..
        '前196年和前179年，南越国曾先后两次名义上臣属于西汉，成为西汉的“外臣”。前112年，' ..
        '南越国末代君主赵建德与西汉发生战争，被汉武帝于前111年所灭。南越国共存在93年，' ..
        '历经五代君主。南越国是岭南地区的第一个有记载的政权国家，采用封建制和郡县制并存的制度，' ..
        '它的建立保证了秦末乱世岭南地区社会秩序的稳定，有效的改善了岭南地区落后的政治、##济现状。\n'

require('tap')(function(test)
  test('test empty file creation', function(expect)
    local filename = join('tests', 'fixtures', 'append.txt')
    local function onReadFile(e, buffer)
      assert(not e)
      p('file read')
      assert(#buffer == #s)
      fs.unlinkSync(filename)
    end
    local function onAppendFile(e)
      assert(not e)
      p('appended to file')
      fs.readFile(filename, expect(onReadFile))
    end
    fs.unlinkSync(filename)
    fs.appendFile(filename, s, expect(onAppendFile))
  end)

  test('test append to non empty file', function(expect)
    local filename2 = join('tests', 'fixtures', 'append2.txt')
    local function onReadFile(e, buffer)
      p('file2 read')
      assert(not e)
      assert(#buffer == #s + #currentFileData)
      fs.unlinkSync(filename2)
    end
    local function onAppendFile(e)
      p('appended to file2')
      assert(not e)
      fs.readFile(filename2, onReadFile)
    end
    fs.unlinkSync(filename2)
    fs.writeFileSync(filename2, currentFileData)
    fs.appendFile(filename2, s, expect(onAppendFile))
  end)

  test('test append file accepting buffers', function(expect)
    local filename3 = join('tests', 'fixtures', 'append3.txt')
    local buf = Buffer:new(s)
    p('appending to ' .. filename3)
    local function onReadFile(e, buffer)
      assert(not e)
      p('file3 read')
      assert(#buffer == buf.length + #currentFileData)
      fs.unlinkSync(filename3)
    end
    local function onAppendFile(e)
      p('appended to file3')
      assert(not e)
      fs.readFile(filename3, onReadFile)
    end
    fs.unlinkSync(filename3)
    fs.writeFileSync(filename3, currentFileData)
    fs.appendFile(filename3, buf:toString(), onAppendFile)
  end)

  test('test append file sync create file', function(expect)
    local filename = join('tests', 'fixtures', 'append-sync.txt')
    fs.unlinkSync(filename)
    assert(not fs.appendFileSync(filename, s))
    local fileData = fs.readFileSync(filename)
    assert(s == fileData)
    fs.unlinkSync(filename)
  end)

  test('test append file sync non empty file', function(expect)
    local filename = join('tests', 'fixtures', 'append-sync2.txt')
    fs.unlinkSync(filename)
    fs.writeFileSync(filename, currentFileData)
    assert(not fs.appendFileSync(filename, s))
    local fileData = fs.readFileSync(filename)
    assert(currentFileData .. s == fileData)
    fs.unlinkSync(filename)
  end)
end)
