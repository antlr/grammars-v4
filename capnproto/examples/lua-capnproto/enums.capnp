# example taken from https://github.com/cloudflare/lua-capnproto/blob/master/proto/enums.capnp

@0xa7e0ba9e3ca0988d;

using Lua = import "lua.capnp";

enum EnumType2 $Lua.naming("lower_underscore") {
    none @0;
    enum5 @1;
    enum6 @2;
    enum7 @3;
    upperDash @4 $Lua.naming("upper_dash");
    lowerUnderScore @5 $Lua.naming("lower_underscore");
    upperUnderScore @6 $Lua.naming("upper_underscore");
    lowerSpace @7 $Lua.naming("lower_space");
}
