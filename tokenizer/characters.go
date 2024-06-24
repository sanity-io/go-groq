package tokenizer

import (
	"unicode"
)

func isWhitespace(ch rune) bool {
	return unicode.IsSpace(ch)
}

func isLetter(ch rune) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
}

func isDigit(ch rune) bool {
	return ch >= '0' && ch <= '9'
}

func isExponent(ch rune) bool {
	return ch == 'e' || ch == 'E'
}

func isHexDigit(ch rune) bool {
	return isDigit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')
}

func isIdentifierCharacter(ch rune) bool {
	return isLetter(ch) || isDigit(ch) || (ch == '_')
}

func IsLeadingIdentifierCharacter(ch rune) bool {
	return isLetter(ch) || (ch == '_') || (ch == '$')
}

// TODO: Need to special case '^' right now because two consecutive ^'s gets joined into
// a '^^' operator. Need to fix
func isOperatorCharacter(ch rune) bool {
	return ch == '<' || ch == '>' || ch == '|' || ch == '=' ||
		ch == '&' || ch == '!' || ch == '*' || ch == '/' ||
		ch == '+' || ch == '-' || ch == '%'
}
