export type IntrospectionNamedTypeRef<
  T, -- TODO: add generic constraints and default types: IntrospectionType = IntrospectionType,
  P
> = {
  kind: any, -- deviation: add this type spec later: $PropertyType<T, 'kind'>,
  name: string,
  ofType: T -- TODO: this field is missing
}

export type ReactScopeQuery = (
	string, -- type
	{ [any]: any }, -- props
	any -- instance
) -> boolean

export type Thenable<R> = {
	andThen: <U>(
		self: Thenable<R>,
		onFulfill: (R) -> () | _Thenable<U> | U,
		onReject: (error: any) -> () | _Thenable<U> | U
	-- note: need union type packs to parse () | Thenable<U>
	) -> nil | _Thenable<U>,
}
