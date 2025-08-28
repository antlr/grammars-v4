--[[
MIT License

Copyright (c) 2017 Gabriel de Quadros Ligneul

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
]]
-- Julia sets via interval cell-mapping (quadtree version)

local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

--require"julia" local f=f

local io=io
local root,exterior
local cx,cy
local Rxmin,Rxmax,Rymin,Rymax=-2.0,2.0,-2.0,2.0
local white=1.0
local black=0.0
local gray=0.5
local N=0
local nE=0
local E={}
local write=print

local function output(a1,a2,a3,a4,a5,a6)
	--[[write(
	a1 or ""," ",
	a2 or ""," ",
	a3 or ""," ",
	a4 or ""," ",
	a5 or ""," ",
	a6 or ""," \n")]]
end

local function imul(xmin,xmax,ymin,ymax)
	local mm=xmin*ymin
	local mM=xmin*ymax
	local Mm=xmax*ymin
	local MM=xmax*ymax
	local m,M=mm,mm
	if m>mM then m=mM elseif M<mM then M=mM end
	if m>Mm then m=Mm elseif M<Mm then M=Mm end
	if m>MM then m=MM elseif M<MM then M=MM end
	return m,M
end

local function isqr(xmin,xmax)
	local u=xmin*xmin
	local v=xmax*xmax
	if xmin<=0.0 and 0.0<=xmax then
		if u<v then return 0.0,v else return 0.0,u end
	else
		if u<v then return u,v else return v,u end
	end
end

local function f(xmin,xmax,ymin,ymax)
	local x2min,x2max=isqr(xmin,xmax)
	local y2min,y2max=isqr(ymin,ymax)
	local xymin,xymax=imul(xmin,xmax,ymin,ymax)
	return x2min-y2max+cx,x2max-y2min+cx,2.0*xymin+cy,2.0*xymax+cy
end

local function outside(xmin,xmax,ymin,ymax)
	local x,y
	if 0.0<xmin then x=xmin elseif 0.0<xmax then x=0.0 else x=xmax end
	if 0.0<ymin then y=ymin elseif 0.0<ymax then y=0.0 else y=ymax end
	return x^2+y^2>4.0
end

local function inside(xmin,xmax,ymin,ymax)
	return	xmin^2+ymin^2<=4.0 and xmin^2+ymax^2<=4.0 and
		xmax^2+ymin^2<=4.0 and xmax^2+ymax^2<=4.0
end

local function newcell()
	return {nil,nil,nil,nil,color=gray}
end

local function addedge(a,b)
	nE=nE+1
	E[nE]=b
end

local function refine(q)
	if q.color==gray then
		if q[1]==nil then
			q[1]=newcell()
			q[2]=newcell()
			q[3]=newcell()
			q[4]=newcell()
		else
			refine(q[1])
			refine(q[2])
			refine(q[3])
			refine(q[4])
		end
	end
end

local function clip(q,xmin,xmax,ymin,ymax,o,oxmin,oxmax,oymin,oymax)
	local ixmin,ixmax,iymin,iymax
	if xmin>oxmin then ixmin=xmin else ixmin=oxmin end
	if xmax<oxmax then ixmax=xmax else ixmax=oxmax end
	if ixmin>=ixmax then return end
	if ymin>oymin then iymin=ymin else iymin=oymin end
	if ymax<oymax then iymax=ymax else iymax=oymax end
	--if ixmin<=ixmax and iymin<=iymax then
	if iymin<iymax then
		if q[1]==nil then
			addedge(o,q)
		else
			local xmid=(xmin+xmax)/2.0
			local ymid=(ymin+ymax)/2.0
			clip(q[1],xmin,xmid,ymid,ymax,o,oxmin,oxmax,oymin,oymax)
			clip(q[2],xmid,xmax,ymid,ymax,o,oxmin,oxmax,oymin,oymax)
			clip(q[3],xmin,xmid,ymin,ymid,o,oxmin,oxmax,oymin,oymax)
			clip(q[4],xmid,xmax,ymin,ymid,o,oxmin,oxmax,oymin,oymax)
		end
	end
end

local function map(q,xmin,xmax,ymin,ymax)
	--xmin,xmax,ymin,ymax=f(xmin,xmax,ymin,ymax,cx,cy)
	xmin,xmax,ymin,ymax=f(xmin,xmax,ymin,ymax)
	if outside(xmin,xmax,ymin,ymax) then
		q.color=white
	else
		if not inside(xmin,xmax,ymin,ymax) then addedge(q,exterior) end
		clip(root,Rxmin,Rxmax,Rymin,Rymax,q,xmin,xmax,ymin,ymax)
	end
end

local function update(q,xmin,xmax,ymin,ymax)
	if q.color==gray then
		if q[1]==nil then
			local b=nE
			q[2]=nE+1
			map(q,xmin,xmax,ymin,ymax)
			q[3]=nE
		else
			local xmid=(xmin+xmax)/2.0
			local ymid=(ymin+ymax)/2.0
			update(q[1],xmin,xmid,ymid,ymax)
			update(q[2],xmid,xmax,ymid,ymax)
			update(q[3],xmin,xmid,ymin,ymid)
			update(q[4],xmid,xmax,ymin,ymid)
		end
	end
end

local function color(q)
	if q.color==gray then
		if q[1]==nil then
			for i=q[2],q[3] do
				if E[i].color~=white then return end
			end
			q.color=white N=N+1
		else
			color(q[1])
			color(q[2])
			color(q[3])
			color(q[4])
		end
	end
end

local function prewhite(q)
	if q.color==gray then
		if q[1]==nil then
			for i=q[2],q[3] do
				local c=E[i].color
				if c==white or c==-gray then
					q.color=-gray
					N=N+1
					return
				end
			end
		else
			prewhite(q[1])
			prewhite(q[2])
			prewhite(q[3])
			prewhite(q[4])
		end
	end
end

local function recolor(q)
	if q.color==-gray then
		q.color=gray
	elseif q.color==gray then
		if q[1]==nil then
			q.color=black
		else
			recolor(q[1])
			recolor(q[2])
			recolor(q[3])
			recolor(q[4])
		end
	end
end

local function area(q)
	if q[1]==nil then
		if q.color==white then return 0.0,0.0
		elseif q.color==black then return 0.0,1.0
		else return 1.0,0.0 end
	else
		local g1,b1=area(q[1])
		local g2,b2=area(q[2])
		local g3,b3=area(q[3])
		local g4,b4=area(q[4])
		return (g1+g2+g3+g4)/4.0, (b1+b2+b3+b4)/4.0
	end
end

local function colorup(q)
	if q[1]~=nil and q.color==gray then
		local c1=colorup(q[1])
		local c2=colorup(q[2])
		local c3=colorup(q[3])
		local c4=colorup(q[4])
		if c1==c2 and c1==c3 and c1==c4 then
if c1~=gray then
			q[1]=nil; --q[2]=nil; q[3]=nil; q[4]=nil
N=N+1 end
			q.color=c1
		end
	end
	return q.color
end

local function save(q,xmin,ymin,N)
	if q[1]==nil or N==1 then
		output(xmin,ymin,N,q.color)
	else
		N=N/2
		local xmid=xmin+N
		local ymid=ymin+N
		save(q[1],xmin,ymin,N)
		save(q[2],xmid,ymin,N)
		save(q[3],xmin,ymid,N)
		save(q[4],xmid,ymid,N)
	end
end
local function show(p)
	local N=2^10
	-- io.output(p..".box")
	output(N)
	save(root,0,0,N)
	-- io.close()
end

local t0=0
local function memory(s)
	local t=os.clock()
	--local dt=string.format("%f",t-t0)
	local dt=t-t0
	--io.stdout:write(s,"\t",dt," sec\t",t," sec\t",math.floor(collectgarbage("count")/1024),"M\n")
	t0=t
end

local function do_(f,s)
	local a,b=f(root,Rxmin,Rxmax,Rymin,Rymax)
	memory(s)
	return a,b
end

local function julia(l,a,b)
memory("begin")
	cx=a	cy=b
	root=newcell()
	exterior=newcell()	exterior.color=white
	show(0)
	for i=1,l do --print("\nstep",i)
		nE=0
		do_(refine,"refine")
		do_(update,"update")
		repeat
			N=0 color(root,Rxmin,Rxmax,Rymin,Rymax) --print("color",N)
		until N==0 memory("color")
		repeat
			N=0 prewhite(root,Rxmin,Rxmax,Rymin,Rymax) --print("prewhite",N)
		until N==0 memory("prewhite")
		do_(recolor,"recolor")
		do_(colorup,"colorup")		--print("colorup",N)
		local g,b=do_(area,"area")	--print("area",g,b,g+b)
		show(i) memory("output")
	--print("edges",nE)
	end
end

--julia(14,0.25,0.35)
--julia(14, -.12, .74 )
--julia(14,0,0)
--julia(12,0.25,0)
--julia(9,0.24,0)
--julia(9,0.26,0)
--julia(13,0,1)
--julia(12,-1.1,0)
--julia(9,-0.12,0.5)

-- figures for paper
--julia(14,0,1)
--julia(14,-1,0)
--julia(12,-0.12, 0.64)
--julia(14,-0.12, 0.60)
--julia(14,-0.12, 0.30)

-- julia (level, a, b) -- julia set de c= a + b i
julia(8,-0.25, 0.74)

end

bench.runCode(test, "qt")
