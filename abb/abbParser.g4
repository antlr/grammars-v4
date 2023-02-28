parser grammar abbParser;

options { tokenVocab = abbLexer; }

/*
    This grammar is still in development.
    In the current state, it is only able to parse .sys-files and read the given declarations.
*/
/*
    This file is the grammar for the ABB RAPID Robot Language.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
/*
Antlr4 port by Tom Everett, 2016

Question mark stands for: zero or one
Plus stands for: one or more
Star stands for: zero or more

*/

module_
    : moduleData EOF
    ;

moduleData
    : MODULE moduleName NEWLINE
      dataList
      NEWLINE*
      ENDMODULE
    ;

moduleName
    : IDENTIFIER
    | procCall
    ;

dataList
    : (NEWLINE
    | declaration NEWLINE
    | procedure NEWLINE)*
    ;

procedure
    : PROC procCall NEWLINE
      (functionCall NEWLINE)*
    ENDPROC
    ;

procCall
    : procName procParameter?
    ;

procName
    : IDENTIFIER
    ;

procParameter
    : BRACKET_OPEN IDENTIFIER? BRACKET_CLOSE
    ;

functionCall
    : IDENTIFIER (functionParameter COMMA)* functionParameter SEMICOLON
    ;

functionParameter
    : ON_CALL
    | OFF_CALL
    | primitive
    | IDENTIFIER
    ;

declaration
    : init_ type_ IDENTIFIER (EQUALS expression)? SEMICOLON
    ;

type_
    : ( TOOLDATA | WOBJDATA | SPEEDDATA | ZONEDATA | CLOCK | BOOL )
    ;

init_
    : LOCAL? ( CONST | PERS | VAR )
    ;

expression
    : array_ | primitive
    ;

array_
    : SQUARE_OPEN (expression COMMA)* expression SQUARE_CLOSE
    ;

primitive
    : BOOLLITERAL
    | CHARLITERAL
    | STRINGLITERAL
    | (PLUS | MINUS)? FLOATLITERAL
    | (PLUS | MINUS)? INTLITERAL
    ;