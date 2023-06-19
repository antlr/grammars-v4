"use strict";
//------------------------------------------------------------------------------
// Class Definition
// More intuitive, OOP-style and boilerplate-free classes.
// http://es6-features.org/#ClassDefinition
//------------------------------------------------------------------------------

class Shape {
    constructor (id, x, y) {
        this.id = id
        this.move(x, y)
    }
    move (x, y) {
        this.x = x
        this.y = y
    }
}

//------------------------------------------------------------------------------
// Class Inheritance
// More intuitive, OOP-style and boilerplate-free inheritance.
// http://es6-features.org/#ClassInheritance
//------------------------------------------------------------------------------

class Rectangle extends Shape {
    constructor (id, x, y, width, height) {
        super(id, x, y)
        this.width  = width
        this.height = height
    }
}
class Circle extends Shape {
    constructor (id, x, y, radius) {
        super(id, x, y)
        this.radius = radius
    }
}

//------------------------------------------------------------------------------
// Class Inheritance, From Expressions
// Support for mixin-style inheritance by extending from expressions yielding
//   function objects. [Notice: the generic aggregation function is usually
//   provided by a library like this one, of course]
// http://es6-features.org/#ClassInheritanceFromExpressions
//------------------------------------------------------------------------------

var aggregation = (baseClass, ...mixins) => {
    let base = class _Combined extends baseClass {
        constructor (...args) {
            super(...args)
            mixins.forEach((mixin) => {
                mixin.prototype.initializer.call(this)
            })
        }
    }
    let copyProps = (target, source) => {
        Object.getOwnPropertyNames(source)
            .concat(Object.getOwnPropertySymbols(source))
            .forEach((prop) => {
            if (prop.match(/^(?:constructor|prototype|arguments|caller|name|bind|call|apply|toString|length)$/))
                return
            Object.defineProperty(target, prop, Object.getOwnPropertyDescriptor(source, prop))
        })
    }
    mixins.forEach((mixin) => {
        copyProps(base.prototype, mixin.prototype)
        copyProps(base, mixin)
    })
    return base
}

class Colored {
    initializer ()     { this._color = "white" }
    get color ()       { return this._color }
    set color (v)      { this._color = v }
}

class ZCoord {
    initializer ()     { this._z = 0 }
    get z ()           { return this._z }
    set z (v)          { this._z = v }
}

class Shape {
    constructor (x, y) { this._x = x; this._y = y }
    get x ()           { return this._x }
    set x (v)          { this._x = v }
    get y ()           { return this._y }
    set y (v)          { this._y = v }
}

class Rectangle extends aggregation(Shape, Colored, ZCoord) {}

var rect = new Rectangle(7, 42)
rect.z     = 1000
rect.color = "red"
console.log(rect.x, rect.y, rect.z, rect.color)

//------------------------------------------------------------------------------
// Base Class Access
// Intuitive access to base class constructor and methods.
// http://es6-features.org/#BaseClassAccess
//------------------------------------------------------------------------------

class Shape {
    // …
    toString () {
        return `Shape(${this.id})`
    }
}
class Rectangle extends Shape {
    constructor (id, x, y, width, height) {
        super(id, x, y)
        // …
    }

    toString () {
        return "Rectangle > " + super.toString()
    }
}
class Circle extends Shape {
    constructor (id, x, y, radius) {
        super(id, x, y)
        // …
    }
    toString () {
        return "Circle > " + super.toString()
    }
}

//------------------------------------------------------------------------------
// Static Members
// Simple support for static class members.
// http://es6-features.org/#StaticMembers
//------------------------------------------------------------------------------

class Rectangle extends Shape {
    // …
    static contextTypes = {
        router: PropTypes.object,
    };

    static defaultRectangle () {
        return new Rectangle("default", 0, 0, 100, 100)
    }
}
class Circle extends Shape {
    // …
    static defaultCircle () {
        return new Circle("default", 0, 0, 100)
    }
}
var defRectangle = Rectangle.defaultRectangle()
var defCircle    = Circle.defaultCircle()

//------------------------------------------------------------------------------
// Getter/Setter
// Getter/Setter also directly within classes (and not just within object
//   initializers, as it is possible since ECMAScript 5.1).
// http://es6-features.org/#GetterSetter
//------------------------------------------------------------------------------

class Rectangle {
    constructor (width, height) {
        this._width  = width
        this._height = height
    }
    set width  (width)  { this._width = width               }
    get width  ()       { return this._width                }
    set height (height) { this._height = height             }
    get height ()       { return this._height               }
    get area   ()       { return this._width * this._height }
}
var r = new Rectangle(50, 20)
r.area === 1000

//------------------------------------------------------------------------------
// Class definition with empty statement
//------------------------------------------------------------------------------
class A {
    ;
}

////
class B {
    get [runtimeCalc]() {return 1};
    set [runtimeCalc](p) {};
    get 'string as key'() {};
}
// extended object
let Obj = {
  [asdfg](a){},
  * foo () {},
  f(){},
  get a(){},
  set a([aa]=123){},
  ...anotherObj,
  ...{
    speradObjectLiteral
  },
  ...functionResult()
}

//------------------------------------------------------------------------------
// Public class fields
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes/Public_class_fields
//------------------------------------------------------------------------------
const PREFIX = "prefix";

class ClassWithField {
  field;
  fieldWithInitializer = "instance field";
  [`${PREFIX}Field`] = "prefixed field";
}

const instance = new ClassWithField();
console.log(Object.hasOwn(instance, "field")); // true
console.log(instance.field); // undefined
console.log(instance.fieldWithInitializer); // "instance field"
console.log(instance.prefixField); // "prefixed field"

//------------------------------------------------------------------------------
// Static initialization blocks
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes/Static_initialization_blocks
//------------------------------------------------------------------------------
class ClassWithStaticInitializationBlock {
  static staticProperty1 = 'Property 1';
  static staticProperty2;
  static {
    this.staticProperty2 = 'Property 2';
  }
}

console.log(ClassWithStaticInitializationBlock.staticProperty1);
// Expected output: "Property 1"
console.log(ClassWithStaticInitializationBlock.staticProperty2);
// Expected output: "Property 2"