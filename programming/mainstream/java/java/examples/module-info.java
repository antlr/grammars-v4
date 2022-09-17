open module var.var.base.var.mod1 {
    exports var.one.pkg;
    exports var.var.base to var.net.foo;
    requires var.var.core;
    requires transitive var.other.var;
    requires static var.some.st;
    uses var.var.ext;
    provides var.svc with var.impl;
}