// Template generated code from Antlr4BuildTasks.dotnet-antlr v 3.0.13

import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const antlr4 = require('antlr4');

export default class CaseChangingStream extends antlr4.InputStream {

	constructor(stream, upper)
	{
		super(stream);
		this._stream = stream;
		this._upper = upper;
	}

	LA(offset) {
		var c = this._stream.LA(offset);
		if (c \<= 0)
		{
			return c;
		}
		return String.fromCodePoint(c)[this._upper ? "toUpperCase" : "toLowerCase"]().codePointAt(0);
	}

	reset() {
		return this._stream.reset();
	}

	consume() {
		return this._stream.consume();
	}

	LT(offset) {
		return this._stream.LT(offset);
	}

	mark() {
		return this._stream.mark();
	}

	release(marker) {
		return this._stream.release(marker);
	}

	seek(_index) {
		return this._stream.seek(_index);
	}

	getText(start, stop) {
		return this._stream.getText(start, stop);
	}

	toString() {
		return this._stream.toString();
	}

	get index(){
		return this._index;
	}

	get size(){
		return this._size;
	}
}

