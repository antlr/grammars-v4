--[[

Copyright 2015 The Luvit Authors. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS-IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

--]]

require('tap')(function(test)
  local FS = require('fs')
  local Path = require('path')

  test('writeFile', function()
    local filename = Path.join(module.dir, 'fixtures', 'test.txt')
    p('writing to ' .. filename)
    local n = 220
    local s = '南越国是前203年至前111年存在于岭南地区的一个国家，国都位于番禺，疆域包括今天中国的广东、' ..
    '广西两省区的大部份地区，福建省、湖南、贵州、云南的一小部份地区和越南的北部。' ..
    '南越国是秦朝灭亡后，由南海郡尉赵佗于前203年起兵兼并桂林郡和象郡后建立。' ..
    '前196年和前179年，南越国曾先后两次名义上臣属于西汉，成为西汉的“外臣”。前112年，' ..
    '南越国末代君主赵建德与西汉发生战争，被汉武帝于前111年所灭。南越国共存在93年，' ..
    '历经五代君主。南越国是岭南地区的第一个有记载的政权国家，采用封建制和郡县制并存的制度，' ..
    '它的建立保证了秦末乱世岭南地区社会秩序的稳定，有效的改善了岭南地区落后的政治、##济现状。\n'

    FS.writeFile(filename, s, function(err)
      assert(not err)
      p('file written')
      FS.readFile(filename, function(err, buffer)
        assert(not err)
        p('file read')
        FS.unlinkSync(filename)
        assert(#s == #buffer)
      end)
    end)
  end)
end)

