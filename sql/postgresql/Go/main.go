/*
PostgreSQL grammar.
The MIT License (MIT).
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path"
	"regexp"
)

// for process PostgreSQLLexer.g4 to golang ok
func processPostgreSQLLexer(inputFileLocation, outputDirectory string) {

	keywordMap := map[string]string{
		"pushTag":                             "this.pushTag",
		"isTag":                               "this.isTag",
		"popTag":                              "this.popTag",
		"checkLA":                             "this.checkLA",
		"charIsLetter":                        "this.charIsLetter",
		"HandleNumericFail":                   "this.HandleNumericFail",
		"HandleLessLessGreaterGreater":        "this.HandleLessLessGreaterGreater",
		"UnterminatedBlockCommentDebugAssert": "this.UnterminatedBlockCommentDebugAssert",

		"\\{\\s+CheckIfUtf32Letter\\(\\)\\s+\\}\\?": "{    this.CheckIfUtf32Letter()   }?",
	}

	fileKeywordTranslate(keywordMap, inputFileLocation, outputDirectory)

}

// for process PostgreSQLParser.g4 to golang ok
func processPostgreSQLParser(inputFileLocation, outputDirectory string) {

	keywordMap := map[string]string{
		"GetParsedSqlTree":     "this.GetParsedSqlTree",
		"ParseRoutineBody":     "this.ParseRoutineBody",
		"TrimQuotes":           "this.TrimQuotes",
		"unquote":              "this.unquote",
		"GetRoutineBodyString": "this.GetRoutineBodyString",
		"getPostgreSQLParser":  "this.getPostgreSQLParser",

		"_localctx":         "localctx",
		"ParserRuleContext": "antlr.ParserRuleContext",
	}

	fileKeywordTranslate(keywordMap, inputFileLocation, outputDirectory)

}

func fileKeywordTranslate(keywordMap map[string]string, inputFileLocation, outputDirectory string) {
	file, err := ioutil.ReadFile(inputFileLocation)
	if err != nil {
		log.Fatal(err)
	}
	fileContent := string(file)
	for origin, target := range keywordMap {
		fileContent = regexp.MustCompile(origin).ReplaceAllString(fileContent, target)
		fmt.Printf(" %s: %s --> %s\n", inputFileLocation, origin, target)
	}

	_ = os.Mkdir(outputDirectory, 0666)

	outputFilePath := path.Join(outputDirectory, path.Base(inputFileLocation))
	err = ioutil.WriteFile(outputFilePath, []byte(fileContent), 0666)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf(" %s --> %s success \n", inputFileLocation, outputFilePath)
}

func main() {

	// TODO change to your PostgreSQLLexer.g4 file location
	processPostgreSQLLexer("PostgreSQLLexer.g4", "./Golang")

	fmt.Println("--------------------------------")

	// TODO change yo your PostgreSQLParser.g4 file location
	processPostgreSQLParser("PostgreSQLParser.g4", "./Golang")

}