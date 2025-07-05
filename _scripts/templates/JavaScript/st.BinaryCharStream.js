// Generated from trgen <version>

export default class BinaryCharStream {
    stream;
    constructor(stream) {
        this.stream = stream;
    }
    reset() {
    }
    consume() {
        this.stream.consume();
    }
    LA(i) {
        return this.stream.LA(i);
    }
    mark() {
        return this.stream.mark();
    }
    release(marker) {
        this.stream.release(marker);
    }
    seek(index) {
        this.stream.seek(index);
    }
    get name() {
        return "";
    }
    get index() {
        return this.stream.index;
    }
    get size() {
        return this.stream.size;
    }
    getSourceName() {
        return "";
    }
    getTextFromRange(start, stop) {
        const buf = [];
        const index = this.stream.index;
        this.stream.seek(0);
        for (let i = start; i \<= stop; i++) {
            const t = this.stream.LA(i + 1);
            buf.push(t.toString());
        }
        this.stream.seek(index);
        return buf.join(' ');
    }
    getTextFromInterval(interval) {
        return this.getTextFromRange(interval.start, interval.stop);
    }
    LT(offset) {
        return this.stream.LT(offset);
    }
    getText(start, stop) {
        return this.getTextFromRange(start, stop);
    }
}
