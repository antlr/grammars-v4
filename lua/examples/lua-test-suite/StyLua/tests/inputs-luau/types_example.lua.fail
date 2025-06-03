local srcWorkspace = script.Parent.Parent
local PromiseModule = require(srcWorkspace.luaUtils.Promise)
type Promise<T> = PromiseModule.Promise<T>
type Resolver<T, U> = any
type Result = any

export type SubscriptionArgs = {
    rootValue: any?,
    contextValue: any?,
    variableValues: { [string]: any },
    operationName: string?,
    fieldResolver: Resolver<any, any>?,
    subscribeFieldResolver: Resolver<any, any>?
}

local function subscribe(
	args: SubscriptionArgs
  ): Promise<Result>
  error("nope")
end

local function createEventStream(
	rootValue: any?,
	contextValue: any?,
	variableValues: { [string]: any }?,
	operationName: string?,
	fieldResolver: Resolver<any, any>?
  ): Promise<Result>
  error("nope")
end

return {
	subscribe = subscribe,
	createEventStream = createEventStream
}