// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

/// Utility methods to manipulate `package:js` annotated JavaScript interop
/// objects in cases where the name to call is not known at runtime.
///
/// You should only use these methods when the same effect cannot be achieved
/// with `@JS()` annotations.
///
/// {@category Web}
library dart.js_util;

// Examples can assume:
// class JS { const JS(); }
// class Promise<T> {}

/// Recursively converts a JSON-like collection to JavaScript compatible
/// representation.
///
/// WARNING: performance of this method is much worse than other util
/// methods in this library. Only use this method as a last resort. Prefer
/// instead to use `@anonymous` `@JS()` annotated classes to create map-like
/// objects for JS interop.
///
/// The argument must be a [Map] or [Iterable], the contents of which are also
/// deeply converted. Maps are converted into JavaScript objects. Iterables are
/// converted into arrays. Strings, numbers, bools, and `@JS()` annotated
/// objects are passed through unmodified. Dart objects are also passed through
/// unmodified, but their members aren't usable from JavaScript.
external dynamic jsify(Object object);

external Object get globalThis;

external T newObject<T>();

external bool hasProperty(Object o, Object name);

external T getProperty<T>(Object o, Object name);

// A CFE transformation may optimize calls to `setProperty`, when [value] is
// statically known to be a non-function.
external T setProperty<T>(Object o, Object name, T? value);

// A CFE transformation may optimize calls to `callMethod` when [args] is a
// a list literal or const list containing at most 4 values, all of which are
// statically known to be non-functions.
external T callMethod<T>(Object o, String method, List<Object?> args);

/// Check whether [o] is an instance of [type].
///
/// The value in [type] is expected to be a JS-interop object that
/// represents a valid JavaScript constructor function.
external bool instanceof(Object? o, Object type);

external T callConstructor<T>(Object constr, List<Object?>? arguments);

/// Perform JavaScript addition (`+`) on two values.
external T add<T>(Object? first, Object? second);

/// Perform JavaScript subtraction (`-`) on two values.
external T subtract<T>(Object? first, Object? second);

/// Perform JavaScript multiplication (`*`) on two values.
external T multiply<T>(Object? first, Object? second);

/// Perform JavaScript division (`/`) on two values.
external T divide<T>(Object? first, Object? second);

/// Perform JavaScript exponentiation (`**`) on two values.
external T exponentiate<T>(Object? first, Object? second);

/// Perform JavaScript remainder (`%`) on two values.
external T modulo<T>(Object? first, Object? second);

/// Perform JavaScript equality comparison (`==`) on two values.
external bool equal<T>(Object? first, Object? second);

/// Perform JavaScript strict equality comparison (`===`) on two values.
external bool strictEqual<T>(Object? first, Object? second);

/// Perform JavaScript inequality comparison (`!=`) on two values.
external bool notEqual<T>(Object? first, Object? second);

/// Perform JavaScript strict inequality comparison (`!==`) on two values.
external bool strictNotEqual<T>(Object? first, Object? second);

/// Perform JavaScript greater than comparison (`>`) of two values.
external bool greaterThan<T>(Object? first, Object? second);

/// Perform JavaScript greater than or equal comparison (`>=`) of two values.
external bool greaterThanOrEqual<T>(Object? first, Object? second);

/// Perform JavaScript less than comparison (`<`) of two values.
external bool lessThan<T>(Object? first, Object? second);

/// Perform JavaScript less than or equal comparison (`<=`) of two values.
external bool lessThanOrEqual<T>(Object? first, Object? second);

/// Exception for when the promise is rejected with a `null` or `undefined`
/// value.
///
/// This is public to allow users to catch when the promise is rejected with
/// `null` or `undefined` versus some other value.
class NullRejectionException implements Exception {
  // Indicates whether the value is `undefined` or `null`.
  final bool isUndefined;

  NullRejectionException._(this.isUndefined);

  @override
  String toString() {
    var value = this.isUndefined ? 'undefined' : 'null';
    return 'Promise was rejected with a value of `$value`.';
  }
}

/// Converts a JavaScript Promise to a Dart [Future].
///
/// ```dart template:top
/// @JS()
/// external Promise<num> get threePromise; // Resolves to 3
///
/// void main() async {
///   final Future<num> threeFuture = promiseToFuture(threePromise);
///
///   final three = await threeFuture; // == 3
/// }
/// ```
external Future<T> promiseToFuture<T>(Object jsPromise);

Object? _getConstructor(String constructorName) =>
    getProperty(globalThis, constructorName);

/// Like [instanceof] only takes a [String] for the object name instead of a
/// constructor object.
bool instanceOfString(Object? element, String objectType) {
  Object? constructor = _getConstructor(objectType);
  return constructor != null && instanceof(element, constructor);
}

/// Returns the prototype of a given object. Equivalent to
/// `Object.getPrototypeOf`.
external Object? objectGetPrototypeOf(Object? object);

/// Returns the `Object` prototype. Equivalent to `Object.prototype`.
external Object? get objectPrototype;

/// Returns the keys for a given object. Equivalent to `Object.keys(object)`.
external List<Object?> objectKeys(Object? object);

/// Returns `true` if a given object is a JavaScript array.
bool isJavaScriptArray(value) => instanceOfString(value, 'Array');

/// Returns `true` if a given object is a simple JavaScript object.
bool isJavaScriptSimpleObject(value) {
  final Object? proto = objectGetPrototypeOf(value);
  return proto == null || proto == objectPrototype;
}

/// Effectively the inverse of [jsify], [dartify] Takes a JavaScript object, and
/// converts it to a Dart based object. Only JS primitives, arrays, or 'map'
/// like JS objects are supported.
external Object? dartify(Object? o);
