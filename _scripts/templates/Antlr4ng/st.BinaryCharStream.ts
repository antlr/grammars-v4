// BSD License
// Copyright (c) 2020 Tom Everett
// All rights reserved.

import { CharStream, Interval } from 'antlr4ng';

export class BinaryCharStream implements CharStream {
    private stream: CharStream;

    constructor(stream: CharStream) {
        this.stream = stream;
    }

    reset(): void {
    }

    consume(): void {
        this.stream.consume();
    }

    LA(i: number): number {
        return this.stream.LA(i);
    }

    mark(): number {
        return this.stream.mark();
    }

    release(marker: number): void {
        this.stream.release(marker);
    }

    seek(index: number): void {
        this.stream.seek(index);
    }

    get name(): string {
	return this.stream.getSourceName();
    }

    get index(): number {
        return this.stream.index;
    }

    get size(): number {
        return this.stream.size;
    }

    getSourceName(): string {
        return this.stream.getSourceName();
    }

    getTextFromRange(start: number, stop: number): string {
        const buf: string[] = [];
        const index = this.stream.index;
        this.stream.seek(0);
        for (let i = start; i \<= stop; i++) {
            const t = this.stream.LA(i + 1);
            buf.push(t.toString());
        }
        this.stream.seek(index);
        return buf.join(' ');
    }

    getTextFromInterval(interval: Interval): string {
        return this.getTextFromRange(interval.start, interval.stop);
    }
}
