---@<error descr="Alias 'CircularAlias1' circularly references itself.">alias CircularAlias1 string | CircularAlias2</error>
---@<error descr="Alias 'CircularAlias2' circularly references itself.">alias CircularAlias2 number | CircularAlias1</error>

---@<error descr="Alias 'CircularGenericAlias1' circularly references itself.">alias CircularGenericAlias1<T> T | CircularGenericAlias2<T></error>
---@<error descr="Alias 'CircularGenericAlias2' circularly references itself.">alias CircularGenericAlias2<T> CircularGenericAlias1<T></error>
