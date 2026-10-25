local options = if useDisposableConcast
	-- Disposable Concast fetches receive a shallow copy of this.options
    -- (merged with newOptions), leaving this.options unmodified.
	then compact(self.options, newOptions)
	else Object.assign(self.options, compact(newOptions))

do
    local state: S = if hook ~= nil
        then hook.memoizedState
        elseif typeof(initialState) == "function"
            then
                -- Luau needs a little help, even with the generic function
                (initialState :: (() -> S))()
            else initialState

	local state: S = if hook ~= nil then hook.memoizedState
		elseif
			typeof(initialState) == "function" -- the fuzz pedal isn't 3.3V
			or _G.__DEV__                      -- in DEV mode, undervolt anyway
		then
			-- Luau needs a little help, even with the generic function
			(initialState :: (() -> S))()
		else initialState
end

local foo = if true then
	-- comment here
	bar
else baz

local x = if true
	then -- comment
		bar
	else -- comment
		baz

local p = if true then bar
else
	-- comment
	baz
