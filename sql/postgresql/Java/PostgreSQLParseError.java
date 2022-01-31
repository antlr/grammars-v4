public class PostgreSQLParseError {
    public PostgreSQLParseError(int number, int offset, int line, int column, String message) {
        Number = number;
        Offset = offset;
        Message = message;
        Line = line;
        Column = column;
    }

    public int Number;
    public int Offset;
    public int Line;
    public int Column;
    public String Message;
}
