# example taken from https://github.com/cloudflare/lua-capnproto/blob/master/proto/struct.capnp

@0x9d3083cc2fcde74b;

struct S1 {
    const flag1 :UInt32 = 0x1;
    const flag2 :UInt32 = 0x2;
    const flag3 :Text = "Hello";

    name @0 :Text;
}
