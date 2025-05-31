local isValid =
	-- Allow nil for conditional declaration
	contextType == nil or
	(contextType["$$typeof"] == REACT_CONTEXT_TYPE and
		contextType._context == nil) -- Not a <Context.Consumer>

local isValid = -- Allow nil for conditional declaration
	foo

local isValid = -- test comment
	-- Allow nil for conditional declaration
	contextType == nil or
	(contextType["$$typeof"] == REACT_CONTEXT_TYPE and
		contextType._context == nil) -- Not a <Context.Consumer>

-- https://github.com/JohnnyMorganz/StyLua/issues/340
local useDisposableConcast =
	-- * Refetching uses a disposable Concast to allow refetches using different
	-- options/variables, without permanently altering the options of the
	-- original ObservableQuery.
	newNetworkStatus == NetworkStatus.refetch or
	-- * The fetchMore method does not actually call the reobserve method, but,
	-- if it did, it would definitely use a disposable Concast.
	newNetworkStatus == NetworkStatus.fetchMore or
	-- * Polling uses a disposable Concast so the polling options (which force
	-- fetchPolicy to be "network-only") won't override the original options.
	newNetworkStatus == NetworkSt
