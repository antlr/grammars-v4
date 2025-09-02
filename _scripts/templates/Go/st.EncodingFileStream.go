// Generated from trgen <version>

package main

import (
    "bufio"
    "os"
    "golang.org/x/text/encoding/charmap"
    "golang.org/x/text/transform"
    "github.com/antlr4-go/antlr/v4"
)

func NewEncodingFileStream(fileName string, encoding string) (*antlr.FileStream, error) {

	f, err := os.Open(fileName)
	if err != nil {
		return nil, err
	}

	defer func(f *os.File) {
		errF := f.Close()
		if errF != nil {
		}
	}(f)

//	, err := f.Stat()
//	if err != nil {
//		return nil, err
//	}

	switch encoding {
            case "latin1", "iso-8859-1":
		reader1 := transform.NewReader(f, charmap.ISO8859_1.NewDecoder())
		is := antlr.NewIoStream(reader1)
		fs := &antlr.FileStream{
			InputStream: *is,
			}
		return fs, nil
            default:
		reader2 := bufio.NewReader(f)
		is := antlr.NewIoStream(reader2)
		fs := &antlr.FileStream{
			InputStream: *is,
			}
		return fs, nil
	}
}
