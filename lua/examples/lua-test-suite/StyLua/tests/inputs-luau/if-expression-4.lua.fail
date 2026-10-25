cache:writeQuery({
	data = {
		items = Array.concat({}, (function()
			local ref = if Boing.toXYZBoxinf(data) and data ~= nil
					then  data.items
					else data
			return Boing.toXYZBoxinf(ref) and ref
		end)() or {}, { item }),
	},
})

local error_ = if errors and #(errors :: Array<any>) > 0
	then ApolloError.new({ graphQLErrors = errors })
	else nil

local function useMutation<TData, TVariables, TContext, TCache>(
	mutation: DocumentNode | TypedDocumentNode<TData, TContext>,
	options: MutationHookOptions_<TData, TVariables, TContext>?
): MutationTuple<TData, TVariables, TContext, TCache>
	local context = useContext(getApolloContext())
	local result, setResult = useState({ called = false, loading = false })
	local updatedOptions = if options
		then Object.assign({}, options, { mutation = mutation })
		else { mutation = mutation }
end
