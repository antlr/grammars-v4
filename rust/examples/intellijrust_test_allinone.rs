#!/she-bang line
//inner attributes
#![crate_type = "lib"]
#![crate_name = "rary"]

fn main(){
    #![crate_type = "lib"]
    &&& x;
    &a & &b;
    false == false && true
}
fn main1(){
    #[foo]
    #[bar]
    let x = 1;

    let x = #[foo] #[bar]1;
    let _ = #[a] - #[b]-1;

    #[foo]
    #[bar]
    {}
}

/* associated type defaults are unstable
trait T {
    type B;
    type A = Self;
}

struct S;

impl T for S {
    type B = T;
}
*/

async fn foo() {}
async fn bar() {}

trait T {
    async fn foo();
    async fn bar();
}

enum E {
    #[cfg(test)] F(#[cfg(test)] i32)
}

#[empty_attr()]
const T: i32 = 92;

fn attrs_on_statements() {
    #[cfg(test)]
    let x = 92;

    #[cfg(test)]
    loop {}

    #[cfg(test)]
    1 + 1;

    S { #[foo] foo: 92 };
}

struct S<#[foo]'a, #[may_dangle] T> {}

#[macro_export]
macro_rules! give_me_struct {
    ($name:ident) => {
        #[allow(non_camel_case_types)]
        struct $name;
    }
}

#[cfg(not(test))]
give_me_struct! {
    hello_world
}

#[post("/", data = "<todo_form>")]
fn string_value() {}

const C: i32 = 0;

#[cfg(attr(value = C))]
fn const_value() {}

#[py::class]
fn path() {}

#[cfg_attr(test, assert_instr(add_a.b))]
fn custom_name() {}

#[attr(foo::{bar, baz}, qwe)]
fn arbitrary_token_tree() {}

fn f1(#[attr1] #[attr2] pat: S) {}

fn f2(#[attr] x: S) {}

impl S {
    fn f3(#[attr] self) {}

    fn f4(#[attr] &self) {}

    fn f5<'a>(#[attr] &mut self) {}

    fn f6<'a>(#[attr] &'a self) {}

    fn f7<'a>(#[attr] &'a mut self, #[attr] x: S, y: S) {}

    fn f8(#[attr] self: Self) {}

    fn f9(#[attr] self: S<Self>) {}
}

trait T { fn f10(#[attr] S); }

extern "C" {
    fn f11(#[attr] x: S, #[attr] ...);
}

// See stuff around `Restrictions::RESTRICTION_STMT_EXPR` in libsyntax

pub fn foo(x: String) {
    // These are not bit and, these are two statements.
    { 1 }
    *2;

    { 1 }
    &2;

    loop {}
    *x;

    while true {}
    &1;

    loop {}
    &mut x;

    let foo = ();
    {foo}
    ();

    // These are binary expressions
    let _ = { 1 } * 2;
    let _ = { 1 } & 2;
    let _ = loop {} * 1;
    2 & { 1 };

    fn bar() {}
    let _ = {bar}();
}

fn main3() {
    let simple_block = {
        123
    };
    /* labels on blocks are unstable
    let block_with_label = 'block: {
        if foo() { break 'block 1; }
        if bar() { break 'block 2; }
        3
    };

    match 123 {
        1 => {},
        2 => 'b: { break 'b; },
        _ => {}
    }*/
}

/// Does useful things
/// Really useful
fn documented_function() {
    /// inner items can have docs too!
    fn foo() { }
}

/// doc
mod m {
    //! This is module docs
    //! It can span more the one line,
    //! like this.
    fn undocumented_function() {}

    /// Does other things
    fn documented_function() {}
}

/// Can mix doc comments and outer attributes
#[cfg(test)]
/// foo
struct S {
    /// Fields can have docs,
    /// sometimes long ones.
    field: f32
}

/// documentation
// simple comments do not interfer with doc comments
struct T (
  /// Even for tuple structs!
  i32
);

/// doc
enum E {
    /// doc
    Foo,
}

enum ES {
    /// doc
    Foo {
        /// field doc
        field: usize
    },
}

extern {
    /// Doc
    fn foo();

    /// Doc
    static errno: i32;
}

/// doc
macro_rules! makro {
    () => { };
}

////////////////////////////////
// This is not a doc comment ///
////////////////////////////////

///
///
/// foo
///
///
fn blanks() {}

// A blank line after non-doc comment detaches it from item.

// This multi-line
// non-doc comment should be attached as well
/// Blank lines after doc comments do not matter

fn foo() {}


/// Non-doc comments after a doc comment do not matter.
// Like this one!
fn bar() {}

fn main4() {
    if 1 < 2 {}
    if let Some(x) = o {}
    if let | Err(e) = r {}
    if let V1(s) | V2(s) = value {}
    if let | Cat(name) | Dog(name) | Parrot(name) = animal {}
    // or-patterns syntax is experimental
    // if let Ok(V1(s) | V2(s)) = value {}

    while 1 < 2 {}
    while let Some(x) = o {}
    while let | Err(e) = r {}
    while let V1(s) | V2(s) = value {}
    while let | Cat(name) | Dog(name) | Parrot(name) = animal {}
    // while let Ok(V1(s) | V2(s)) = value {}
}

/* const generics are unstable
struct S<T, const N: i32, const M: &'static str>;
fn foo<T, const N: i32, const M: &'static str>() {}
fn main() { foo::<S<i32, 0, { x }>, -0, "">() }
*/

const FOO: i32 = 42;
const _: i32 = 123;
//simply not works
//const NO_TYPE = 42;
//static STATIC_NO_TYPE = 42;

// Test that empty type parameter list (<>) is synonymous with
// no type parameters at all

struct S<>;
trait T<> {}
enum E<> { V }
impl<> T<> for S<> {}
impl T for E {}
fn foo<>() {}
fn bar() {}

fn main() {
    let _ = S;
    let _ = S::<>;
    let _ = E::V;
    let _ = E::<>::V;
    foo();
    foo::<>();

    // Test that we can supply <> to non generic things
    bar::<>();
    let _: i32<>;
}

fn foo() where for<> for<> T: T {}

fn f() -> i32 {}

fn test() -> u32 {

    x :: y;         /* path-expr */
    :: x :: y;
    self :: x :: y;

    x + y - z * 0;  /* binary */

    x = y = z;      /* assignment + ; */

    *x;             /* unary (+ ;) */
    &x;
    &mut x;

    (x + y) * z;    /* parenthesized */

    t = (0, 1, 2);  /* tuple */

    t.a;            /* field */
    t.0;
    //t.0.0; //thanks god...

    f.m();          /* method-invokation */

    f();            /* call */
    <T as Foo>::U::generic_method::<f64>();
    S::<isize>::foo::<usize>();
    let xs: Box<[()]> = Box::<[(); 0]>::new([]);

    t = ();         /* unit */

    [   0,          /* array */
        1,
        2,
        [ 0 ; 1 ] ];
    [];
    [1,];
    [1;2];

    || {};          /* lambda */
    |x| x;
    |&x| x;
    //box pattern syntax is experimental
    //|box x| x;
    //not work
    //|x: i32| -> i32 92;
    move |x: i32| {
        x
    };

    |x: &mut i32| x = 92;

    { }             /* block */

    unsafe { 92 }

    {
        {92}.to_string()
    }

    //box 92;//box is experimental

    let _ = 1 as i32 <= 1;
    //type ascription is experimental
    //let _ = 1: i32 <= 1;

    const TEN: u32 = 10;
    let _ = 1 as u32 + TEN;
    //let _ = 1: u32 + TEN;
    let _ = 1 as (i32);

    //yield syntax is experimental
    //|| { 0; yield 0; };

    return (x = y)  /* return */
            + 1
}


#[link(name = "objc")]
extern {
    fn foo(name: *const libc::c_uchar);
    fn bar(a: i32,  ...) -> i32;

    #[cfg(test)]
    pub fn baz(b: i64, );

    #[doc = "Hello"]
    pub static X: i32;
    //extern types are experimental
    //pub type Y;
}

extern crate foo;
#[macro_use] extern crate bar;
extern crate spam as eggs;
// should be annotated as error
extern crate self;
extern crate self as foo;

extern fn baz() {}
unsafe extern fn foo() {}
unsafe extern "C" fn bar() {}


fn add(x: i32, y: i32) -> i32 {
    return x + y;
}
  
  fn mul(x: i32, y: i32) -> i32 {
    x * y;
}
  
  fn id(x: i32,) -> i32 { x }
  
  fn constant() -> i32 { 92 }
  
  const        fn a() -> () { () }
  const unsafe fn b() -> () { () }
  
  fn diverging() -> ! { panic("! is a type") }
  /*C-variadic functions are unstable
  unsafe extern "C" fn ext_fn1(a: bool, ...) {}
  unsafe extern "C" fn ext_fn2(a: bool, args: ...) {}
  unsafe extern "C" fn ext_fn3(a: bool, ...,) {}
  unsafe extern "C" fn ext_fn4(a: bool, args: ...,) {}
  */

  struct S;

  trait A {
      type B;
  }
  
  impl A for S {
      type B = S;
  }
  
  
  trait T { }
  trait P<X> { }
  
  
  impl T  { }
  impl (T) { }
  impl T for S { }
  // Syntactically invalid
  //impl (T) for S { }
  
  impl<U> P<U> { }
  impl<U> (P<U>) { }
  impl<U> P<U> for S { }
  impl T for <S as A>::B { }
  
  // Semantically invalid
  impl (<S as A>::B) { }
  
  impl<'a, T> Iterator for Iter<'a, T> + 'a {
      type Item = &'a T;
  
      foo!();
  }
  
  impl<T> GenVal<T> {
      fn value(&self) -> &T {}
      fn foo<A, B>(&mut self, a: i32, b: i32) -> &A {}
  }
/*specialization is unstable
  impl<T: fmt::Display + ?Sized> ToString for T {
      #[inline]
      default fn to_string(&self) -> String { }
      default fn a() {}
      default fn b() {}
      default const BAR: u32 = 81;
      default type T = i32;
      pub default fn c() {}
      pub default const C1: i32 = 1;
      pub default type T1 = i32;
}
  
default unsafe impl<T> const X for X {}
*/
mod m {
    #    !    [ cfg ( test ) ]
}

fn main() {
    {} // This should a stmt.
    {} // And this one is an expr.
}
fn main() {
    'label: while let Some(_) = Some(92) {}

    let _  = loop { break 92 };
    let _ = 'l: loop { break 'l 92 };

    'll: loop {
        break 'll;
    }
}

//not work
//peg! parser_definition(r#"
//"#);

macro_rules! vec {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}

macro_rules! comments {
    () => {
        /// doc comment
        mod foo() {
            /** doc comment 2 */
            fn bar() {}
        }
    };
}

macro_rules! default {
    ($ty: ty) => { /* ANYTHING */ };
}

macro_rules! foobar {
    ($self: ident) => {  };
}

default!(String);

thread_local!(static HANDLE: Handle = Handle(0));

#[cfg(foo)]
foo!();

include!("path/to/rust/file.rs");
const STR: &str = include_str!("foo.in");
const BYTES: &[u8] = include_bytes!("data.data",);

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

std::include!("path/to/rust/file.rs");
::std::include!("path/to/rust/file.rs");
crate::foo! {}
self::foo! {}
super::foo! {}

fn foo() {
    #[cfg(foo)]
    foo! {}
    let a = 0; // needed to check that we parsed the call as a stmt

    macro_rules! bar {
        () => {};
    }

    let mut macro_rules = 0;
    macro_rules += 1;

    foo!() + foo!();


    // -- vec macro ---
    let v1 = vec![1, 2, 3];
    let v2 = vec![1; 10];
    let v: Vec<i32> = vec![];
    let vv: Vec<i32> = std::vec![]; // fully qualified macro call
    let vvv: Vec<i32> = std::vec /*comment*/ ![]; // fully qualified macro call with comment
    vec!(Foo[]); // custom vec macro
    // ----------------

    // --- format macros ---
    println!("{}", 92);
    format!("{argument}", argument = "test");  // => "test"
    format_args!("{name} {}", 1, name = 2);    // => "2 1"
    format!["hello {}", "world!"];
    format! {
        "x = {}, y = {y}",
        10, y = 30
    }
    panic!("division by zero");
    unimplemented!("{} {} {}", 1, 2, 3);
    todo!("it's too {epithet} to implement", epithet = "boring");
    std::println!("{}", 92); // fully qualified macro call
    std::println /*comment*/ !("{}", 92); // fully qualified macro call with comment
    ::std::println!("{}", 92); // fully qualified macro call beginning with double colon
    eprintln!(Foo[]); // custom format macro
    // -------------------

    // --- expr macros ---
    /*deprecated
    try!(bar());
    try![bar()];
    try! {
        bar()
    }*/
    dbg!();
    dbg!("Some text");
    dbg!(123 + 567,);
    std::dbg!(123); // fully qualified macro call
    std::dbg /*comment*/ !(123); // fully qualified macro call with comment
    dbg!(Foo[]); // custom expr macro
    // ------------------

    // --- log macros ---
    error!();
    debug!("{a} {c} {b}", a="a", b='b', c=3);  // => "a 3 b"
    trace!(target: "smbc", "open_with {:?}", options);
    log::warn!(target: "smbc", "open_with {:?}", options); // fully qualified macro call
    log::info /*comment*/ !(target: "smbc", "open_with {:?}", options); // fully qualified macro call with comment
    debug!(log, "debug values"; "x" => 1, "y" => -1); // custom log macro
    // ------------------

    // --- assert macros ---
    let a = 42u32;
    let b = 43u32;
    assert!(a == b);
    assert![a == b];
    assert!{a == b};

    assert_eq!(a, b, "Some text");
    assert_ne!(a, b, "Some text");
    assert!(a == b, "Some text");
    assert!(a == b, "Text {} {} syntax", "with", "format");

    assert!(a == b);
    debug_assert!(a == b);
    assert_eq!(a, b);
    debug_assert_eq!(a, b);
    assert_ne!(a, b);
    debug_assert_ne!(a, b);
    std::assert!(a == b); // fully qualified macro call
    std::assert /*comment*/ !(a == b); // fully qualified macro call with comment
    assert_eq!(Foo[]); // custom assert macro
    // ---------------------

    // --- concat macros
    concat!("abc");
    concat!("abc", "def");
    concat!("abc", "def",);
    std::concat!("abc", "def"); // fully qualified macro call
    std::concat /*comment*/ !("abc", "def"); // fully qualified macro call with comment
    concat!(Foo[]); // custom concat macro
    // ------------------

    // - env macros
    env!("FOO");
    env!("FOO",);
    env!("FOO", "error message");
    env!("FOO", "error message", );
    std::env!("FOO"); // fully qualified macro call
    std::env /*comment*/ !("FOO"); // fully qualified macro call with comment
    env!(Foo[]); // custom env macro
    // ------------------

    // - asm macros
    asm!("nop");
    asm!("nop", "nop");
    asm!("nop", options(pure, nomem, nostack));
    asm!("nop", const 5, a = const 5);
    asm!("nop", sym foo::bar, a = sym foo::bar, const 6);
    asm!("nop", a = const A + 1);
    asm!("nop", in(reg) x => y, out("eax") _);
    asm!("nop", a = const 5, b = sym foo::bar, c = in(reg) _, d = out(reg) a => _);
    std::asm!("nop"); // fully qualified macro call
    std::asm /*comment*/ !("nop"); // fully qualified macro call with comment
    // ------------------
}
fn main() {
    match x {
        _ => {}
        _ => 1,
        _ => unsafe { 1 }.to_string(),
        _ => 92
    };

    match x {
        | 0
        | 1 => 0,
        | _ => 42,
    };
}

fn main() {
    match () {
        () => {}
        () => {}
    }
}


mod arith {

    fn add(x: i32, y: i32) -> i32 {
      return x + y;
    }

    fn mul(x: i32, y: i32) -> i32 {
      x * y;
    }

}


mod empty {

}

fn main() {
    // Float literals
    let _ = 1.0;
    let _ = 1f32;
    let _ = 1f64;
    let _ = 1.0f64;
    let _ = 1.0e92;
    let _ = 1.0e92f32;
    let _ = 1.;
    let _ = 10e_6;
    //not work
    //let _ = 1f34;
    //let _ = 1.0i98;
    //shouldn't work
    //let _ = 0.0.0;
    let _ = 0f32.foo();

    // Integer literals
    let _ = 1234567890;
    let _ = 1234567890i32;
    let _ = 1_________;
    let _ = 1_________i32;
    let _ = 0x1234567890abcdef;
    let _ = 0o1234567;
    let _ = 0b10101011101010000111;
    let _ = 0.foo();
}

fn moo() {
    a || b || c;
    5 | 3 == 2 || 4 | 2 | 0 == 4 || 1 | 0 == 1;
}
fn patterns() {
    let S {..} = x;
    let S {field} = x;
    let S {field,} = x;
    let S {field, ..} = x;
    let T(field, ..) = x;
    let T(.., field) = x;
    let (x, .., y) = (1, 2, 3, 4, 5);
    let [x, .., y] = [1, 2, 3, 4];
    //let [ | x, .., | y] = [1, 2, 3, 4];
    let &[x, ref y @ ..] = [1, 2, 3];
    let [..] = [1, 2];

    let ref a @ _ = value;

    if let Some(x,) = Some(92) { }

    let m!(x) = 92;

    let <i32>::foo ... <i32>::bar = 92;
    let Option::None = None;
    /*or-patterns syntax is experimental
    let Foo(x) | Bar(x) | Baz(x) = baz;
    let | Foo(x) | Bar(x) | Baz(x) = baz;
    let Some(Foo(x) | Bar(x) | Baz(x)) = baz;
    //let Some(| Foo(x) | Bar(x) | Baz(x)) = baz;
    let Some(Foo(x) | Bar(Ok(1 | 2)) | Baz(x)) = baz;
    // https://github.com/rust-lang/rfcs/blob/master/text/2535-or-patterns.md#precedence
    let i @ p | q = x;
    let i @ (p | q) = x;
    */
    match 10 {
        -100 => x,
        X => x,
        Q::T => x,
        //exclusive range pattern syntax is experimental
        //0..2 => x,
        2...4 => x,
        //V..=10 => x,
        //W..20 => x,
        //Y::Z..50 => x,
        //Ok(Foo(x) | Bar(x) | Baz(x)) => x,
        _ => x
    };
}

fn single_bound<T: Bar>() {}

fn parenthesized_bound<T: (Bar)>() {}

struct QuestionBound<T: ?Sized>(Unique<T>);

struct ParenthesizedQuestionBound<T: (?Sized)>(Unique<T>);

fn multiple_bound<T: Bar + Baz>() {}

fn parenthesized_multiple_bound<T: (Bar) + (Baz)>() {}

fn lifetime_bound<'a, T:'a>() {}

// ('a) syntactically invalid
//fn parenthesized_lifetime_bound<'a, T: ('a)>() {}

fn for_lifetime_bound<F>(f: F) where F: for<'a> Fn(&'a i32) {}

fn parenthesized_for_lifetime_bound<F>(f: F) where F: (for<'a> Fn(&'a i32)) {}

fn impl_bound() -> impl Bar {}

fn parenthesized_impl_bound() -> impl (Bar) {}

fn impl_multiple_bound() -> impl Bar + Baz {}

fn parenthesized_impl_multiple_bound() -> impl (Bar) + (Baz) {}

fn dyn_bound(b: &mut dyn Bar) {}

fn parenthesized_dyn_bound(b: &mut dyn (Bar)) {}

//fn dyn_multiple_bound(b: &mut dyn Bar + Baz) {}

//fn parenthesized_dyn_multiple_bound(b: &mut dyn (Bar) + (Baz)) {}

fn lifetime_bound_on_Fn_returning_reference<'b, F, Z: 'b>() where F: Fn() -> &'b Z + 'static {}
//associated type bounds are unstable
/*
fn assoc_type_bounds1<T: Foo<Item: Bar>>(t: T) {}
fn assoc_type_bounds2<T: Foo<Item: Bar+Baz>>(t: T) {}
fn assoc_type_bounds3<T: Foo<Item1: Bar, Item2 = ()>>(t: T) {}
fn assoc_type_bounds4<T: Foo<Item1 = (), Item2: Bar>>(t: T) {}
fn assoc_type_bounds_in_args(t: &dyn Foo<Item: Bar>) {}
*/
fn main() {
    let a = 1 + 2 * 3;
    let b = *x == y;
}
fn main() {
    r = 1..2;
    r =  ..2;
    r = 1.. ;
    r =  .. ;
    r = {1}..{2};
    //r = 1...10;
    //r = 1 ... 10;
    //r = ... 10;
    r = 1..=10;
    r = 1 ..= 10;
    r = ..= 10;
    //r = 1..=;
    //r = 1...;

    for i in 0.. {
        2
    }
}
/*raw address of syntax is experimental
fn main() {
    let _ = &raw mut x;
    let _ = &raw const x;
    let _ = &raw;
    let _ = &raw!();
}*/
/* TODO: fix << >> >>= <<= >= <=
fn expressions() {
    // expressions
    1 >> 1;
    x >>= 1;
    x >= 1;
    1 << 1;
    x <<= 1;
    x <= 1;
 
    // generics
    type T = Vec<Vec<_>>;
    let x: V<_>= ();
    let x: V<V<_>>= ();
    x.collect::<Vec<Vec<_>>>();
    type U = Vec<<i32 as F>::Q>;
 
    i < <u32>::max_value();
}*/

struct S { f: i32 }
struct S2 { foo: i32, bar: () }

fn main() {
    if if true { S {f:1}; true } else { S {f:1}; false } {
        ()
    } else {
        ()
    };

    if {S {f:1}; let _ = S {f:1}; true} {()};

    if { 1 } == 1 { 1; }
    if unsafe { 0 } == 0 { 0; }

    let (foo, bar) = (1, ());
    let s2 = S2 { foo, bar };
}

struct S1;
struct S2 {}
struct S3 { field: f32  }
struct S4 { field: f32, }
struct S5 { #[foo] field: f32 }
struct S6 { #[foo] field: f32, #[foo] field2: f32 }

struct S10();
struct S11(i32);
struct S12(i32,);
struct S13(i32,i32);
struct S14(#[foo] i32);
struct S15(#[foo] i32, #[foo] i32);

#[repr(C)]
union U {
    i: i32,
    f: f32,
}

fn foo() {
    struct S1;
    struct S2 {}
    struct S3 { field: f32  }
    struct S4 { field: f32, }

    #[repr(C)]
    union U {
        i: i32,
        f: f32,
    }
}

trait Contains {
    type A;
    fn inner(&self) -> Self::A;
    fn empty();
    fn anon_param(i32);
    fn self_type(x: Self, y: Vec<Self>) -> Self;
}

fn foo() {
    trait Inner {};
    unsafe trait UnsafeInner {};
}

trait bar<T> {
    fn baz(&self,);
}

trait TrailingPlusIsOk: Clone+{}
trait EmptyBoundsAreValid: {}

fn main() {
    "1".parse::<i32>()?;
    {x}?;
    x[y?]?;
    x???;
    Ok(true);
    let question_should_bind_tighter = !x?;
}
fn main() {
    a::<B<>>
}
type FunType = Fn(f64) -> f64;
type FunType2 = FnOnce::(i32);

type FunTypeVoid = Fn();

type ColonColon = Vec::<[u8; 8]>;

type Sum = Box<A + Copy>;

type LifetimeSum = Box<'a + Copy>;

type HrtbSum = &(for<'a> Trait1 + for<'b> Trait2);

type FunSum = Box<Fn(f64, f64) -> f64 + Send + Sync>;
type FunSum2 = Box<Fn() -> () + Send>;
type FunRetDynTrait = Box<Fn() -> dyn Trait + Send>;

type Shl = F<<i as B>::Q, T=bool>;
type Shr = Vec<Vec<f64>>;

type Path = io::Result<()>;

type AssocType = Box<Iterator<Item=(Idx, T)> + 'a>;

type GenericAssoc = Foo<T, U=i32>;

type Trailing1 = Box<TypeA<'static,>>;

type Trailing2<'a> = MyType<'a, (),>;

type TrailingCommaInFn = unsafe extern "system" fn(x: i32,) -> ();

fn foo<T>(xs: Vec<T>) -> impl Iterator<Item=impl FnOnce() -> T> + Clone {
    xs.into_iter().map(|x| || x)
}

type DynTrait = dyn Trait;

struct S<F>
    where F: FnMut(&mut Self, &T) -> Result<(), <Self as Encoder>::Error>;

struct EmptyWhere where {}

fn bar() -> foo!() { let a: foo!() = 0 as foo!(); a }


use self :: y :: { self   };
use           :: { self   };
use           :: { self , };
use           :: {        };
use              { y      };
use              { y ,    };
use              {        };
use self :: y :: *;
use self :: y as z;
use self :: y as _;
use self :: y;
use crate :: y;

// https://github.com/rust-lang/rfcs/blob/master/text/2128-use-nested-groups.md
use a::{B, d::{self, *, g::H}};
use ::{*, *};

use foo::{bar, {baz, quux}};
use {crate::foo, crate::bar, super::baz};

struct S1;
pub struct S2;
pub(crate) struct S3;
pub(self) struct S4;
mod a {
    pub (super) struct S5;
    pub(in a) struct S6;
    mod b {
        pub(in super::super) struct S7;
        // Syntactically invalid
        //pub(a::b) struct S8;
    }
}
//crate visibility modifier is experimental
//crate struct S9;

//struct S10(crate ::S1); // path `crate::S1`
//struct S11(crate S1); // vis `crate`

crate::macro1!();

#[macro_export]
#[doc(hidden)]
macro_rules! __diesel_column {
    ($($table:ident)::*, $column_name:ident -> $Type:ty) => {
        #[allow(non_camel_case_types, dead_code)]
        #[derive(Debug, Clone, Copy)]
        pub struct $column_name;

        impl $crate::expression::Expression for $column_name {
            type SqlType = $Type;
        }

        impl<DB> $crate::query_builder::QueryFragment<DB> for $column_name where
            DB: $crate::backend::Backend,
            <$($table)::* as QuerySource>::FromClause: QueryFragment<DB>,
        {
            fn to_sql(&self, out: &mut DB::QueryBuilder) -> $crate::query_builder::BuildQueryResult {
                try!($($table)::*.from_clause().to_sql(out));
                out.push_sql(".");
                out.push_identifier(stringify!($column_name))
            }

            fn collect_binds(&self, _out: &mut DB::BindCollector) -> $crate::result::QueryResult<()> {
                Ok(())
            }

            fn is_safe_to_cache_prepared(&self) -> bool {
                true
            }
        }

        impl_query_id!($column_name);

        impl SelectableExpression<$($table)::*> for $column_name {
        }

    }
}

#[macro_export]
macro_rules! table {
    // Put `use` statements at the end because macro_rules! cannot figure out
    // if `use` is an ident or not (hint: It's not)
    (
        use $($import:tt)::+; $($rest:tt)+
    ) => {
        table!($($rest)+ use $($import)::+;);
    };

    // Add the primary key if it's not present
    (
        $($table_name:ident).+ {$($body:tt)*}
        $($imports:tt)*
    ) => {
        table! {
            $($table_name).+ (id) {$($body)*} $($imports)*
        }
    };

    // Add the schema name if it's not present
    (
        $name:ident $(($($pk:ident),+))* {$($body:tt)*}
        $($imports:tt)*
    ) => {
        table! {
            public . $name $(($($pk),+))* {$($body)*} $($imports)*
        }
    };

    // Import `diesel::types::*` if no imports were given
    (
        $($table_name:ident).+ $(($($pk:ident),+))* {$($body:tt)*}
    ) => {
        table! {
            $($table_name).+ $(($($pk),+))* {$($body)*}
            use $crate::types::*;
        }
    };

    // Terminal with single-column pk
    (
        $schema_name:ident . $name:ident ($pk:ident) $body:tt
        $($imports:tt)+
    ) => {
        table_body! {
            $schema_name . $name ($pk) $body $($imports)+
        }
    };

    // Terminal with composite pk (add a trailing comma)
    (
        $schema_name:ident . $name:ident ($pk:ident, $($composite_pk:ident),+) $body:tt
        $($imports:tt)+
    ) => {
        table_body! {
            $schema_name . $name ($pk, $($composite_pk,)+) $body $($imports)+
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! table_body {
    (
        $schema_name:ident . $name:ident ($pk:ident) {
            $($column_name:ident -> $Type:ty,)+
        }
        $(use $($import:tt)::+;)+
    ) => {
        table_body! {
            schema_name = $schema_name,
            table_name = $name,
            primary_key_ty = columns::$pk,
            primary_key_expr = columns::$pk,
            columns = [$($column_name -> $Type,)+],
            imports = ($($($import)::+),+),
        }
    };

    (
        $schema_name:ident . $name:ident ($($pk:ident,)+) {
            $($column_name:ident -> $Type:ty,)+
        }
        $(use $($import:tt)::+;)+
    ) => {
        table_body! {
            schema_name = $schema_name,
            table_name = $name,
            primary_key_ty = ($(columns::$pk,)+),
            primary_key_expr = ($(columns::$pk,)+),
            columns = [$($column_name -> $Type,)+],
            imports = ($($($import)::+),+),
        }
    };

    (
        schema_name = $schema_name:ident,
        table_name = $table_name:ident,
        primary_key_ty = $primary_key_ty:ty,
        primary_key_expr = $primary_key_expr:expr,
        columns = [$($column_name:ident -> $column_ty:ty,)+],
        imports = ($($($import:tt)::+),+),
    ) => {
        pub mod $table_name {
            #![allow(dead_code)]
            use $crate::{
                QuerySource,
                Table,
            };
            use $crate::associations::HasTable;
            $(use $($import)::+;)+
            __diesel_table_query_source_impl!(table, $schema_name, $table_name);

            impl_query_id!(table);

            pub mod columns {
                use super::table;
                use $crate::result::QueryResult;
                $(use $($import)::+;)+

                $(__diesel_column!(table, $column_name -> $column_ty);)+
            }
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! __diesel_table_query_source_impl {
    ($table_struct:ident, public, $table_name:ident) => {
        impl QuerySource for $table_struct {
            type FromClause = Identifier<'static>;
            type DefaultSelection = <Self as Table>::AllColumns;

            fn from_clause(&self) -> Self::FromClause {
                Identifier(stringify!($table_name))
            }

            fn default_selection(&self) -> Self::DefaultSelection {
                Self::all_columns()
            }
        }
    };

    ($table_struct:ident, $schema_name:ident, $table_name:ident) => {
        impl QuerySource for $table_struct {
            type FromClause = $crate::query_builder::nodes::
                InfixNode<'static, Identifier<'static>, Identifier<'static>>;
            type DefaultSelection = <Self as Table>::AllColumns;

            fn from_clause(&self) -> Self::FromClause {
                $crate::query_builder::nodes::InfixNode::new(
                    Identifier(stringify!($schema_name)),
                    Identifier(stringify!($table_name)),
                    ".",
                )
            }

            fn default_selection(&self) -> Self::DefaultSelection {
                Self::all_columns()
            }
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! joinable {
    ($child:ident -> $parent:ident ($source:ident)) => {
        joinable_inner!($child::table => $parent::table : ($child::$source = $parent::table));
        joinable_inner!($parent::table => $child::table : ($child::$source = $parent::table));
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! joinable_inner {
    ($left_table:path => $right_table:path : ($foreign_key:path = $parent_table:path)) => {
        joinable_inner!(
            left_table_ty = $left_table,
            right_table_ty = $right_table,
            right_table_expr = $right_table,
            foreign_key = $foreign_key,
            primary_key_ty = <$parent_table as $crate::query_source::Table>::PrimaryKey,
            primary_key_expr = $parent_table.primary_key(),
        );
    };

    (
        left_table_ty = $left_table_ty:ty,
        right_table_ty = $right_table_ty:ty,
        right_table_expr = $right_table_expr:expr,
        foreign_key = $foreign_key:path,
        primary_key_ty = $primary_key_ty:ty,
        primary_key_expr = $primary_key_expr:expr,
    ) => {
        impl<JoinType> $crate::JoinTo<$right_table_ty, JoinType> for $left_table_ty {
            type JoinClause = $crate::query_builder::nodes::Join<
                <$left_table_ty as $crate::QuerySource>::FromClause,
                <$right_table_ty as $crate::QuerySource>::FromClause,
                $crate::expression::helper_types::Eq<
                    $crate::expression::nullable::Nullable<$foreign_key>,
                    $crate::expression::nullable::Nullable<$primary_key_ty>,
                >,
                JoinType,
            >;
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! join_through {
    ($parent:ident -> $through:ident -> $child:ident) => {
        impl<JoinType: Copy> $crate::JoinTo<$child::table, JoinType> for $parent::table {
            type JoinClause = <
                <$parent::table as $crate::JoinTo<$through::table, JoinType>>::JoinClause
                as $crate::query_builder::nodes::CombinedJoin<
                    <$through::table as $crate::JoinTo<$child::table, JoinType>>::JoinClause,
                >>::Output;

            fn join_clause(&self, join_type: JoinType) -> Self::JoinClause {
                use $crate::query_builder::nodes::CombinedJoin;
                let parent_to_through = $crate::JoinTo::<$through::table, JoinType>
                    ::join_clause(&$parent::table, join_type);
                let through_to_child = $crate::JoinTo::<$child::table, JoinType>
                    ::join_clause(&$through::table, join_type);
                parent_to_through.combine_with(through_to_child)
            }
        }
    }
}

#[macro_export]
macro_rules! debug_sql {
    ($query:expr) => {{
        use $crate::query_builder::{QueryFragment, QueryBuilder};
        use $crate::query_builder::debug::DebugQueryBuilder;
        let mut query_builder = DebugQueryBuilder::new();
        QueryFragment::<$crate::backend::Debug>::to_sql(&$query, &mut query_builder).unwrap();
        query_builder.finish()
    }};
}

#[macro_export]
macro_rules! print_sql {
    ($query:expr) => {
        println!("{}", &debug_sql!($query));
    };
}

fn main() {
    {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
    ()
    }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
}
pub type T = A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<A<B>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
static i: () =
(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
()
)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
;

static j:
(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
i32
)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
=
(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
1
)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
;

static k:
((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
(i32, )
))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
=
(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
1,
)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
;

static l:
(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
i32,
),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),)
=
(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
1,
),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),),)
;

fn main() {}