// Generated from trgen <version>

package main

import (
    "strconv"
    "strings"
    "github.com/antlr4-go/antlr/v4"
)

type BinaryCharStream struct {
    antlr.CharStream
}


func NewBinaryCharStream(c antlr.CharStream) *BinaryCharStream {
    ret := &BinaryCharStream {
        c,
        }
    return ret
}

func (c *BinaryCharStream) GetText(start, end int) string {
    return "yo"
}

func (c *BinaryCharStream) GetTextFromInterval(interval antlr.Interval) string {
    var buf strings.Builder
    start := interval.Start
    stop := interval.Stop
    index := c.Index()
    c.Seek(0)
    more := false
    for i := start; i \<= stop; i++ {
        t := c.LA(i + 1)
        if more {
            buf.WriteString(" ")
        }
        more = true
        buf.WriteString(strconv.Itoa(t))
    }
    c.Seek(index)
    return buf.String()
}

