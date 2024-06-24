package tokenizer

import (
	"bufio"
	"bytes"
	"encoding/hex"
	"encoding/json"
	"strings"

	"github.com/sanity-io/go-groq/ast"
)

const eof = rune(0)

// Tokenizer represents a lexical scanner.
//
// This implements its own internal rune buffer in order to support both read() and unread().
// It's not possible to use bufio's UnreadRune() because this can only unread a single rune,
// and Peek() clears the rune buffer.
type Tokenizer struct {
	r   *bufio.Reader
	buf []rune
	pos int
}

// New returns a new lexer instance.
func New(src string) *Tokenizer {
	return &Tokenizer{
		r:   bufio.NewReader(strings.NewReader(src)),
		buf: make([]rune, 0, 4),
	}
}

func (t *Tokenizer) Pos() int { return t.pos }

// read reads the next rune from the buffered reader.
// Returns the rune(0) if an error occurs (or io.EOF is returned).
func (t *Tokenizer) read() rune {
	t.pos++

	len := len(t.buf)
	if len > 0 {
		ch := t.buf[len-1]
		t.buf = t.buf[:len-1]
		return ch
	}

	ch, _, err := t.r.ReadRune()
	if err != nil {
		return eof
	}
	return ch
}

// unread places the previously read rune back on the reader.
func (t *Tokenizer) unread(r rune) {
	t.buf = append(t.buf, r)
	t.pos--
}

// Scan returns the next token and literal value.
func (t *Tokenizer) Scan() (ast.Token, string, int) {
	// Read the next rune.
	pos := t.pos
	ch := t.read()

	// If we see whitespace then consume all contiguous whitespace.
	// If we see a letter then consume as an ident or reserved word.
	switch {
	case isWhitespace(ch):
		t.unread(ch)
		return t.scanWhitespace()
	case ch == ':':
		ch2 := t.read()
		if ch2 == ':' {
			return ast.DoubleColon, "::", pos
		}
		t.unread(ch2)
	case ch == '/':
		// Handle comments starting with "//"
		nextCh := t.read()
		if nextCh == '/' {
			// consume until end of line
			for {
				ch3 := t.read()
				if ch3 == eof || ch3 == '\n' {
					return t.Scan()
				}
			}
		} else {
			t.unread(nextCh)
			t.unread(ch)
			return t.scanOperator()
		}
	case IsLeadingIdentifierCharacter(ch):
		t.unread(ch)
		return t.scanIdent()
	case isOperatorCharacter(ch):
		t.unread(ch)
		return t.scanOperator()
	case ch == '.':
		t.unread(ch)
		return t.scanDots()
	case ch == '\'' || ch == '"':
		t.unread(ch)
		return t.scanString(ch)
	case isDigit(ch):
		t.unread(ch)
		return t.scanNumber()
	}

	// Otherwise consider the individual character.
	switch ch {
	case eof:
		return ast.EOF, "", pos
	case ':':
		return ast.Colon, ":", pos
	case ',':
		return ast.Comma, ",", pos
	case '(':
		return ast.ParenLeft, "(", pos
	case ')':
		return ast.ParenRight, ")", pos
	case '{':
		return ast.BraceLeft, "{", pos
	case '}':
		return ast.BraceRight, "}", pos
	case '[':
		return ast.BracketLeft, "[", pos
	case ']':
		return ast.BracketRight, "]", pos
	case '^':
		return ast.Hat, "^", pos
	case '*':
		return ast.Asterisk, "*", pos
	case '@':
		return ast.At, "@", pos
	case '|':
		return ast.Pipe, "|", pos
	case ';':
		return ast.Semicolon, ";", pos
	}

	return ast.Illegal, string(ch), pos
}

// scanWhitespace consumes the current rune and all contiguous whitespace.
func (t *Tokenizer) scanWhitespace() (ast.Token, string, int) {
	pos := t.pos
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	_, _ = buf.WriteRune(t.read())

	// Read every subsequent whitespace character into the buffer.
	// Non-whitespace characters and EOF will cause the loop to exit.
Loop:
	for {
		ch := t.read()
		switch {
		case ch == eof:
			break Loop
		case !isWhitespace(ch):
			t.unread(ch)
			break Loop
		default:
			_, _ = buf.WriteRune(ch)
		}
	}

	return ast.Whitespace, buf.String(), pos
}

// scanIdent consumes the current rune and all contiguous ident runes.
func (t *Tokenizer) scanIdent() (ast.Token, string, int) {
	pos := t.pos
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	_, _ = buf.WriteRune(t.read())

	// Read every subsequent ident character into the buffer.
	// Non-ident characters and EOF will cause the loop to exit.
Loop:
	for {
		ch := t.read()
		switch {
		case ch == eof:
			break Loop
		case !isIdentifierCharacter(ch):
			t.unread(ch)
			break Loop
		default:
			_, _ = buf.WriteRune(ch)
		}
	}

	// If the string matches a keyword then return that keyword.
	switch buf.String() {
	case "true":
		return ast.Bool, buf.String(), pos
	case "false":
		return ast.Bool, buf.String(), pos
	case "null":
		return ast.Null, buf.String(), pos
	}

	// Otherwise return as a regular identifier.
	return ast.Name, buf.String(), pos
}

// scanOperator consumes one operator
func (t *Tokenizer) scanOperator() (ast.Token, string, int) {
	pos := t.pos

	// Read two characters
	ch1 := t.read()
	ch2 := t.read()

	// Is the second character part of the operator?
	// Test all two-character operators here:
	if isOperatorCharacter(ch2) {
		operator := string(ch1) + string(ch2)
		switch operator {
		case "==":
			return ast.Equals, operator, pos
		case "||":
			return ast.Or, operator, pos
		case "&&":
			return ast.And, operator, pos
		case "<=":
			return ast.LTE, operator, pos
		case ">=":
			return ast.GTE, operator, pos
		case "=>":
			return ast.Rocket, operator, pos
		case "!=":
			return ast.NEQ, operator, pos
		case "->":
			return ast.Arrow, operator, pos
		case "**":
			return ast.Exponentiation, operator, pos
		}
	}

	// ch2 was not part of the operator, unread it
	t.unread(ch2)

	// Test all single character operators here
	switch ch1 {
	case '>':
		return ast.GT, string(ch1), pos
	case '<':
		return ast.LT, string(ch1), pos
	case '!':
		return ast.Not, string(ch1), pos
	case '+':
		return ast.Plus, string(ch1), pos
	case '-':
		return ast.Minus, string(ch1), pos
	case '*':
		return ast.Asterisk, string(ch1), pos
	case '/':
		return ast.Slash, string(ch1), pos
	case '%':
		return ast.Percent, string(ch1), pos
	case '|':
		return ast.Pipe, string(ch1), pos
	case '=':
		return ast.EqualSign, string(ch1), pos
	}

	return ast.Illegal, string(ch1), pos
}

// scanDots consumes the current rune and all contiguous dots.
func (t *Tokenizer) scanDots() (ast.Token, string, int) {
	pos := t.pos
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	_, _ = buf.WriteRune(t.read())

Loop:
	for {
		ch := t.read()
		switch {
		case ch == eof:
			break Loop
		case ch != '.':
			t.unread(ch)
			break Loop
		default:
			_, _ = buf.WriteRune(ch)
		}
	}

	switch buf.String() {
	case ".":
		return ast.Dot, buf.String(), pos
	case "..":
		return ast.DotDot, buf.String(), pos
	case "...":
		return ast.DotDotDot, buf.String(), pos
	}

	return ast.Illegal, buf.String(), pos
}

// scanOperator consumes one operator
func (t *Tokenizer) scanNumber() (ast.Token, string, int) {
	pos := t.pos
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer

	var decimalPointSeen = false
	var lastCharacterIsDigit = false
	var lastCharacterIsExponent = false
	var exponentSeen = false
Loop:
	for {
		ch := t.read()
		switch {
		case ch == eof:
			break Loop
		case isDigit(ch):
			_, _ = buf.WriteRune(ch)
		case isExponent(ch) && !exponentSeen:
			_, _ = buf.WriteRune(ch)
			exponentSeen = true
		case lastCharacterIsExponent && (ch == '+' || ch == '-'):
			_, _ = buf.WriteRune(ch)
		case ch == '.' && !decimalPointSeen && !exponentSeen:
			// If the ch is a '.', we will need to determine whether this is a decimal point or
			// the beginning of a range operator
			nextCh := t.read()
			t.unread(nextCh)
			if nextCh == '.' {
				// This is a range operator, not a decimal point
				t.unread(ch)
				break Loop
			}
			_, _ = buf.WriteRune(ch)
			decimalPointSeen = true
		default:
			// Error out
			t.unread(ch)
			break Loop
		}

		lastCharacterIsDigit = isDigit(ch)
		lastCharacterIsExponent = isExponent(ch)
	}

	// Must start and end on a digit
	if !lastCharacterIsDigit {
		return ast.Illegal, buf.String(), pos
	}

	if decimalPointSeen || exponentSeen {
		return ast.Float, buf.String(), pos
	}

	return ast.Integer, buf.String(), pos
}

// scanString scans a literal string
func (t *Tokenizer) scanString(quote rune) (ast.Token, string, int) {
	pos := t.pos
	var buf bytes.Buffer
	buf.WriteRune(t.read())

	// Read every string character into the buffer.
	// Backslashes are used as escape characters, following JSON semantics:
	// https://tools.ietf.org/html/rfc7159#section-7
	// JSON does not allow single-quoted strings, so here we follow
	// Javascript semantics and interpret them the same as double quotes.
	var lastCharacterIsQuote bool
	var isEscaped bool
	var unicodeSeq string
	var isUTF16Surrogate bool
Loop:
	for {
		ch := t.read()
		switch {
		case ch == eof:
			break Loop
		case unicodeSeq != "":
			// This handles a Unicode sequence, which can be \uXXXX for UTF-8
			// or UTF-16 characters, or \uXXXX\uXXXX for UTF-16 surrogate pairs
			switch {
			case isHexDigit(ch):
				unicodeSeq += string(ch)
			case isUTF16Surrogate && len(unicodeSeq) == 6 && ch == '\\':
				unicodeSeq += string(ch)
			case isUTF16Surrogate && len(unicodeSeq) == 7 && ch == 'u':
				unicodeSeq += string(ch)
			default:
				break Loop // triggers Illegal below
			}
			switch {
			case len(unicodeSeq) == 4:
				// Detect UTF-16 surrogate pair 0xD800-0xDFFF
				b, err := hex.DecodeString(unicodeSeq[2:4])
				if err != nil {
					break Loop // triggers Illegal
				}
				isUTF16Surrogate = b[0] >= 0xD8 && b[0] <= 0xDF
			case (len(unicodeSeq) == 6 && !isUTF16Surrogate) ||
				(len(unicodeSeq) == 12 && isUTF16Surrogate):
				var char string
				err := json.Unmarshal([]byte(`"`+unicodeSeq+`"`), &char)
				if err != nil {
					break Loop // triggers Illegal below
				}
				buf.WriteRune([]rune(char)[0])
				unicodeSeq = ""
				isUTF16Surrogate = false
			}
		case isEscaped:
			switch ch {
			case '\\', '/', '\'', '"':
				buf.WriteRune(ch)
			case 'b':
				buf.WriteRune('\u0008')
			case 'f':
				buf.WriteRune('\u000c')
			case 'n':
				buf.WriteRune('\u000a')
			case 'r':
				buf.WriteRune('\u000d')
			case 't':
				buf.WriteRune('\u0009')
			case 'u':
				// Entering Unicode sequence of form \uXXXX where X is hex digit
				unicodeSeq = `\u`
			default:
				break Loop // triggers Illegal below
			}
			isEscaped = false
		case ch == rune('\\'):
			isEscaped = true
		case ch == quote:
			buf.WriteRune(ch)
			lastCharacterIsQuote = true
			break Loop
		default:
			_, _ = buf.WriteRune(ch)
		}
	}

	if !lastCharacterIsQuote {
		return ast.Illegal, buf.String(), pos
	}

	return ast.String, buf.String(), pos
}
