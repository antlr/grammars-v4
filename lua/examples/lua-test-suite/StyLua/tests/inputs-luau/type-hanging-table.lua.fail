-- https://github.com/JohnnyMorganz/StyLua/issues/394#issuecomment-1054865101
type QueryManagerPrivate<TStore> = QueryManager<TStore> & {
	inFlightLinkObservables: Map<DocumentNode, Map<string, Observable<FetchResult<{ [string]: any }, Record<string, any>, Record<string,any>>>>>,
}
