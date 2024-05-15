package ast

// Token represents one token
type Token int

// The different tokens
const (
	Illegal        Token = iota
	EOF                  // End of File
	Whitespace           // A contiguous sequence of white space
	ParenLeft            // (
	ParenRight           // )
	BraceLeft            // {
	BraceRight           // }
	BracketLeft          // [
	BracketRight         // ]
	Dot                  // .
	Equals               // ==
	Or                   // ||
	And                  // &&
	GT                   // >
	LT                   // <
	GTE                  // >=
	LTE                  // <=
	NEQ                  // !=
	Not                  // !
	Null                 // null
	Arrow                // ->
	At                   // @
	Comma                // ,
	Colon                // :
	Name                 // A valid field or type identifier
	DotDot               // ..
	DotDotDot            // ...
	MatchOperator        // match
	InOperator           // in
	AscOperator          // asc
	DescOperator         // desc
	Integer              // 1234
	Float                // 123.4
	Bool                 // true or false
	String               // 'literal string' or "literal string"
	Hat                  // ^
	Asterisk             // *
	Exponentiation       // **
	Slash                // /
	Percent              // %
	Plus                 // +
	Minus                // -
	Pipe                 // |
	Rocket               // =>
	DoubleColon          // ::
	TokenMax             // Always last
	EqualSign            // =
	Semicolon            // ;
)

func (token Token) String() string {
	switch token {
	case Illegal:
		return "illegal"
	case EOF:
		return "eof"
	case Whitespace:
		return "whitespace"
	case ParenLeft:
		return "parenLeft"
	case ParenRight:
		return "parenRight"
	case BraceLeft:
		return "braceLeft"
	case BraceRight:
		return "braceRight"
	case BracketLeft:
		return "bracketLeft"
	case BracketRight:
		return "bracketRight"
	case Arrow:
		return "arrow"
	case At:
		return "at"
	case Dot:
		return "dot"
	case Equals:
		return "equals"
	case EqualSign:
		return "equalSign"
	case Or:
		return "or"
	case And:
		return "and"
	case GT:
		return "gt"
	case LT:
		return "lt"
	case GTE:
		return "gte"
	case LTE:
		return "lte"
	case NEQ:
		return "neq"
	case Not:
		return "not"
	case Comma:
		return "comma"
	case Colon:
		return "colon"
	case DoubleColon:
		return "doubleColon"
	case Name:
		return "identifier"
	case DotDotDot:
		return "dotDotDot"
	case DotDot:
		return "dotDot"
	case MatchOperator:
		return "match"
	case InOperator:
		return "in"
	case AscOperator:
		return "asc"
	case DescOperator:
		return "desc"
	case Integer:
		return "integer"
	case Float:
		return "float"
	case String:
		return "string"
	case Bool:
		return "bool"
	case Null:
		return "null"
	case Hat:
		return "hat"
	case Plus:
		return "+"
	case Minus:
		return "-"
	case Asterisk:
		return "*"
	case Exponentiation:
		return "**"
	case Slash:
		return "/"
	case Percent:
		return "%"
	case Pipe:
		return "pipe"
	case Rocket:
		return "rocket"
	case Semicolon:
		return "semicolon"
	}
	return "UNKNOWN TOKEN"
}

// Literal converts a token id to a literal where applicable
func (token Token) Literal() string {
	switch token {
	case Illegal:
		return "<illegal>"
	case EOF:
		return "<eof>"
	case Whitespace:
		return " "
	case ParenLeft:
		return "("
	case ParenRight:
		return ")"
	case BraceLeft:
		return "{"
	case BraceRight:
		return "}"
	case BracketLeft:
		return "["
	case BracketRight:
		return "]"
	case Dot:
		return "."
	case Equals:
		return "=="
	case EqualSign:
		return "="
	case Arrow:
		return "->"
	case At:
		return "@"
	case Or:
		return "||"
	case And:
		return "&&"
	case GT:
		return ">"
	case LT:
		return "<"
	case GTE:
		return ">="
	case LTE:
		return "<="
	case NEQ:
		return "!="
	case Not:
		return "!"
	case Comma:
		return ","
	case Colon:
		return ":"
	case DoubleColon:
		return "::"
	case Name:
		return "<identifier>"
	case DotDotDot:
		return "..."
	case DotDot:
		return ".."
	case MatchOperator:
		return "match"
	case InOperator:
		return "in"
	case AscOperator:
		return "asc"
	case DescOperator:
		return "desc"
	case Integer:
		return "integer"
	case Float:
		return "float"
	case String:
		return "string"
	case Bool:
		return "bool"
	case Null:
		return "null"
	case Hat:
		return "^"
	case Plus:
		return "+"
	case Minus:
		return "-"
	case Asterisk:
		return "*"
	case Exponentiation:
		return "**"
	case Slash:
		return "/"
	case Percent:
		return "%"
	case Pipe:
		return "|"
	case Rocket:
		return "=>"
	case Semicolon:
		return ";"
	}
	return "<unknown token>"
}

// MatchingBrace returns the brace matching the provided token, as in '(' returns ')' and '[' returns ']'
func MatchingBrace(token Token) Token {
	switch token {
	case ParenLeft:
		return ParenRight
	case BracketLeft:
		return BracketRight
	case BraceLeft:
		return BraceRight
	case ParenRight:
		return ParenLeft
	case BracketRight:
		return BracketLeft
	case BraceRight:
		return BraceLeft
	}
	return Illegal
}
