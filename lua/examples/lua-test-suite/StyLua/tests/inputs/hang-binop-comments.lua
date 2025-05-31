function foo(defaultExport)
	if defaultExport == nil then
		print(
			"lazy: Expected the result of a dynamic import() call. "
				.. "Instead received: %s\n\nYour code should look like: \n  "
				-- Break up imports to avoid accidentally parsing them as dependencies.
				-- ROBLOX deviation: Lua syntax in message
				.. "local MyComponent = lazy(function() => req"
				.. "quire('script.Parent.MyComponent') end)",
			moduleObject
		)
	end
end

function bar(defaultExport)
	if defaultExport == nil then
		print(
			"lazy: Expected the result of a dynamic import() call. " ..
				"Instead received: %s\n\nYour code should look like: \n  " ..
				-- Break up imports to avoid accidentally parsing them as dependencies.
				-- ROBLOX deviation: Lua syntax in message
	      "local MyComponent = lazy(function() => req" ..
				"quire('script.Parent.MyComponent') end)",
			moduleObject
		)
	end
end