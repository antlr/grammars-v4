-- https://github.com/JohnnyMorganz/StyLua/issues/378
export type KindEnum =
	"Name" |
	-- Document
	"Document"
	| "OperationDefinition"
	| "VariableDefinition"
	| "SelectionSet"
	| "Field"
	| "Argument" |
	-- Fragments
	"FragmentSpread"
	| "InlineFragment"
	| "FragmentDefinition"

-- https://github.com/JohnnyMorganz/StyLua/issues/384
export type React_AbstractComponent<Config, Instance> = {
	["$$typeof"]: number,
	render: (props: Config, ref: React_Ref<Instance>) -> React_Node,
	displayName: string?,
	defaultProps: Config?,
	name: string?,
	-- this comment causes the brace above to be misformatted: the quick fox jumps over the lazy dog foo bar baz foo bar baz
	[string]: any,
}
