module com.example.foo {
	requires com.example.foo.http;
	requires java.logging;
	requires transitive com.example.foo.network;
	exports com.example.foo.bar;
	exports com.example.foo.internal to com.example.foo.probe;
	opens com.example.foo.quux;
	opens com.example.foo.internal to com.example.foo.network,
	com.example.foo.probe;
	uses com.example.foo.spi.Intf;
	provides com.example.foo.spi.Intf with com.example.foo.Impl;
}
