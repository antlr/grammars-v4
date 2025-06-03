-- Shouldn't hang since multiline comments arent an issue

export type GraphQLEnumType =  --[[ <T> ]]{
	name: string,
	description: string?,
	extensions: ReadOnlyObjMap<any>?,
	astNode: EnumTypeDefinitionNode?,
	extensionASTNodes: Array<EnumTypeExtensionNode>?,

	_values: Array<GraphQLEnumValue --[[ <T> ]]>,
	_valueLookup: Map<any --[[ T ]], GraphQLEnumValue>,
	_nameLookup: ObjMap<GraphQLEnumValue>,
	-- ROBLOX deviation: add self parameter for all ':' operator methods
	getValues: (self: GraphQLEnumType) -> Array<GraphQLEnumValue --[[ <T> ]]>,
	getValue: (self: GraphQLEnumType, string) -> GraphQLEnumValue?,
	serialize: (
		self: GraphQLEnumType,
		any --[[ T ]]
	) -> string?,
	parseValue: (self: GraphQLEnumType, any) -> any?, --[[ T ]]
	parseLiteral: (self: GraphQLEnumType, ValueNode, ObjMap<any>?) -> any?, --[[ T ]]
	toConfig: (self: GraphQLEnumType) -> GraphQLEnumTypeNormalizedConfig,
}
