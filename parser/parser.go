package parser

import (
	"fmt"

	"github.com/sanity-io/go-groq"
	"github.com/sanity-io/go-groq/ast"
	"github.com/sanity-io/go-groq/parser/internal/parserv1"
	"github.com/sanity-io/go-groq/parser/internal/parserv2"
)

// ParseError represents an error during parsing.
type ParseError interface {
	error
	Message() string
	Pos() ast.Position
}

type Version int

var (
	Version1 Version = 1
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
		version: Version2,
		params:  groq.Params{},
	}
	for _, o := range options {
		o(&opts)
	}
	switch opts.version {
	case Version1:
		return parserv1.Parse(query,
			parserv1.WithParams(opts.params))
	case Version2:
		return parserv2.Parse(query,
			parserv2.WithParams(opts.params),
			parserv2.WithParamNodes(opts.createParamNodes))
	default:
		panic(fmt.Sprintf("invalid version %v", opts.version))
	}
}

type parserOpts struct {
	params           groq.Params
	createParamNodes bool
	version          Version
}
