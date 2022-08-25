package parser

type PostgreSQLParseError struct {
	Number  int
	Offset  int
	Line    int
	Column  int
	Message string
}
