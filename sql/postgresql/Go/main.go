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