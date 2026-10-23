-- https://github.com/JohnnyMorganz/StyLua/issues/439
exports.separateDisplayNameAndHOCs =
	function(displayName: string | nil, type_: ElementType): (string | nil, Array<string> | nil)
		if displayName == nil then
			return nil, nil
		end

		local hocDisplayNames: Array<string>? = nil

		if
			type_ == ElementTypeClass
			or type_ == ElementTypeForwardRef
			or type_ == ElementTypeFunction
			or type_ == ElementTypeMemo
		then
			-- ROBLOX deviation: use match instead of indexOf
			if (displayName :: string):match("%(") then
				-- ROBLOX deviation: use gmatch instead of /[^()]+/g
				local matches = (displayName :: string):gmatch("[^()]+")
				local nextMatch = matches()
				if nextMatch then
					displayName = nextMatch
					hocDisplayNames = {}
					while nextMatch :: any ~= nil do
						nextMatch = matches()
						table.insert(hocDisplayNames :: Array<string>, nextMatch)
					end
				end
			end
		end
	end
