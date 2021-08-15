package groq

// Params represents a set of parameters to be interpolated into a GROQ statement
type Params map[string]interface{}

// ParamPrefixCharacter specifies the character with which all param names must start in order to get recognized
const ParamPrefixCharacter = '$'
