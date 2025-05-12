package parser

import (
	"fmt"

	"github.com/sanity-io/go-groq"
	"github.com/sanity-io/go-groq/ast"
	"github.com/sanity-io/go-groq/parser/internal/parserv1"
	"github.com/sanity-io/go-groq/parser/internal/parserv2"
	"github.com/sanity-io/go-groq/parser/internal/parservX"
)

// ParseError represents an error during parsing.
type ParseError interface {
	error
	Message() string
	Pos() ast.Position
}

type Version int

var (
	// VersionGROQ1 targets GROQ-1: https://sanity-io.github.io/GROQ/.
	VersionGROQ1 Version = 2

	// VersionLegacyGROQ targets the legacy version of GROQ implemented by Sanity.io.
	VersionLegacyGROQ Version = 1

	// Version1 is an alias for VersionLegacyGROQ.
	// Deprecated: Use VersionLegacyGROQ.
	Version1 Version = 1

	// Version2 is an alias for VersionGROQ1.
	// Deprecated: Use VersionGROQ1.
	Version2 Version = 2

	// VersionX is an experimental version of the GROQ parser. There's no guarantees that it will be stable and might change at any time.
	// TODO: remove this version when we stop using it in Gradient.
	VersionX Version = 0
)

type Option func(*parserOpts)

func WithParams(p groq.Params) Option {
	return func(opts *parserOpts) {
		for k, v := range p {
			opts.params[k] = v
		}
	}
}

func WithParamNodes() Option {
	return func(opts *parserOpts) {
		opts.createParamNodes = true
	}
}

func WithVersion(v Version) Option {
	return func(opts *parserOpts) {
		opts.version = v
	}
}

func WithFunctions(fns map[ast.FunctionID]*ast.FunctionDefinition) Option {
	return func(opts *parserOpts) {
		opts.functions = fns
	}
}

func Parse(query string, options ...Option) (ast.Expression, error) {
	opts := parserOpts{
		version: VersionGROQ1,
		params:  groq.Params{},
	}
	for _, o := range options {
		o(&opts)
	}
	switch opts.version {
	case VersionLegacyGROQ:
		return parserv1.Parse(query,
			parserv1.WithParams(opts.params))
	case VersionGROQ1:
		return parserv2.Parse(query,
			parserv2.WithParams(opts.params),
			parserv2.WithParamNodes(opts.createParamNodes),
			parserv2.WithFunctions(opts.functions))
	case VersionX:
		return parservX.Parse(query,
			parservX.WithParams(opts.params),
			parservX.WithParamNodes(opts.createParamNodes),
			parservX.WithFunctions(opts.functions))
	default:
		panic(fmt.Sprintf("invalid version %v", opts.version))
	}
}

type parserOpts struct {
	params           groq.Params
	functions        map[ast.FunctionID]*ast.FunctionDefinition
	createParamNodes bool
	version          Version
}
