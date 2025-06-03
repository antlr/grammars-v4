---@shape OurGenericShape<N>
---@field parameterOrNumber N | number
---@field aKnownStringLiteral 'a' | 'b' | 'c'

---@alias AliasAsParam 'one' | 'two' | 'three'

---@alias GenericAlias<N> string | OurGenericShape<N>

---@type GenericAlias<AliasAsParam>
local genericAlias = {
    parameterOrNumber = 'one',
    aKnownStringLiteral = 'a'
}

genericAlias = {
    parameterOrNumber = 'two',
    aKnownStringLiteral = 'b'
}

genericAlias = {
    parameterOrNumber = 1,
    aKnownStringLiteral = 'c'
}

genericAlias = {
    parameterOrNumber = <error descr="Type mismatch. Required: 'AliasAsParam | number' Found: '\"invalid\"'">'invalid'</error>,
    aKnownStringLiteral = 'a'
}

genericAlias = {
    parameterOrNumber = 'three',
    aKnownStringLiteral = <error descr="Type mismatch. Required: '\"a\" | \"b\" | \"c\"' Found: '\"invalid\"'">'invalid'</error>,
}

genericAlias = <error descr="Type mismatch. Missing member: 'aKnownStringLiteral' of: 'OurGenericShape<AliasAsParam>', on union candidate OurGenericShape<AliasAsParam>"><error descr="Type mismatch. Required: 'string' Found: 'table', on union candidate string">{
    parameterOrNumber = <error descr="Type mismatch. Required: 'AliasAsParam | number' Found: '\"owner\"', on union candidate OurGenericShape<AliasAsParam>">'owner'</error>
}</error></error>

genericAlias = 'a string'
genericAlias = <error descr="Type mismatch. Required: 'GenericAlias<AliasAsParam>' Found: '1'">1</error>
genericAlias = <error descr="Type mismatch. Missing member: 'aKnownStringLiteral' of: 'OurGenericShape<AliasAsParam>', on union candidate OurGenericShape<AliasAsParam>"><error descr="Type mismatch. Missing member: 'parameterOrNumber' of: 'OurGenericShape<AliasAsParam>', on union candidate OurGenericShape<AliasAsParam>"><error descr="Type mismatch. Required: 'string' Found: 'table', on union candidate string">{}</error></error></error>

---@type GenericAlias<"different">
local aDifferentGenericAlias = <error>genericAlias</error>

-- Defining multiple types in the one block is far from recommended, but we want our doc handling to be forgiving where possible.
---@alias AliasInSharedComment<T> string
---@alias AliasInSharedComment2<T> AliasInSharedComment<T>
