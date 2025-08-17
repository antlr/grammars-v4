-- https://github.com/JohnnyMorganz/StyLua/issues/290
local foo = foo(

	foo,

	bar
)

local foo = foo(
	foo,

	bar
)

return function()
	call(function()
		local abortSelfPromise = abortSelf(
			function()
				return Promise.resolve(true)
			end,

			function()
				return Promise.new(function(newResolve)
					resolve = newResolve
				end)
			end
		)
	end)
end

