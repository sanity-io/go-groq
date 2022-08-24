package parser

import (
	"fmt"

	"github.com/sanity-io/go-groq"
	"github.com/sanity-io/go-groq/ast"
	"github.com/sanity-io/go-groq/parser/internal/parserlegacy"
	"github.com/sanity-io/go-groq/parser/internal/parsergroq1"
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
		return parserlegacy.Parse(query,
			parserlegacy.WithParams(opts.params))
	case VersionGROQ1:
		return parsergroq1.Parse(query,
			parsergroq1.WithParams(opts.params),
			parsergroq1.WithParamNodes(opts.createParamNodes))
	default:
		panic(fmt.Sprintf("invalid version %v", opts.version))
	}
}

type parserOpts struct {
	params           groq.Params
	createParamNodes bool
	version          Version
}
