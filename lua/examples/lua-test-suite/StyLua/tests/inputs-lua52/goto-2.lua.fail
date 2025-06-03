-- http://lua-users.org/wiki/GotoStatement
::redo:: for x=1,10 do for y=1,10 do
	if not f(x,y) then goto continue end
	if not g(x,y) then goto skip end
	if not h(x,y) then goto redo end
	::continue::
  end end ::skip::