import 'package:antlr4/antlr4.dart';

import 'MyErrorListener.dart';
import 'dart:io';
import 'dart:convert';

class BinaryCharStream extends CharStream {

  late CharStream _is;

  BinaryCharStream(CharStream input_stream) : super() {
    this._is = input_stream;
  }

  @override
  int get index {
    return _is.index;
  }

  @override
  int get size {
    return _is.size;
  }

  @override
  void consume() {
    _is.consume();
  }

  @override
  int? LA(int offset) {
    return _is.LA(offset);
  }

  @override
  int mark() {
    return _is.mark();
  }

  @override
  void release(int marker)
  {
    _is.release(marker);
  }

  @override
  void seek(int _index) {
    _is.seek(_index);
  }

  @override
  String getText(Interval interval) {
    final StringBuffer buf = StringBuffer();
    final int start = interval.a;
    final int stop = interval.b;
    final int index = _is.index;
    _is.seek(0);
    for (int i = start; i \<= stop; i++) {
      final int t = _is.LA(i + 1)!;
      buf.write(t);
    }
    _is.seek(index);
    return buf.toString();
  }

  @override
  String toString() {
    return _is.toString();
  }

  @override
  String get sourceName {
    return _is.sourceName;
  }
}
