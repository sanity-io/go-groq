//nolint:gosec
package testhelpers

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"io/ioutil"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"runtime"
	"strings"
	"testing"

	"github.com/blang/semver"
	"github.com/sanity-io/litter"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/sanity-io/go-groq"
	"github.com/sanity-io/go-groq/ast"
)

var versionRegex = regexp.MustCompile(`//groq:version=(\S+)`)
var paramRegex = regexp.MustCompile(`//groq:param:([^=]+)=([^\n]+)`)

type Test struct {
	Name     string
	Query    string
	Params   groq.Params
	Valid    bool
	Version  *semver.Range
	FileName string
}

func ASTTest(
	t *testing.T,
	test *Test,
	outputDir string,
	parser func(query string, params groq.Params) (ast.Expression, error),
) {
	fileName := filepath.Join(outputDir, SnapshotFileName(test))

	require.NoError(t, os.MkdirAll(filepath.Dir(fileName), 0755))

	t.Logf("File: %s", fileName)

	parsed, err := parser(test.Query, test.Params)
	require.NoError(t, err)

	type Result struct {
		Query string
		AST   ast.Expression
	}

	result := Result{
		Query: test.Query,
		AST:   parsed,
	}

	actual := []byte(litter.Options{
		FieldFilter: func(f reflect.StructField, _ reflect.Value) bool {
			return f.Name != "Source" // Skip the unnecessary source fields
		},
		HomePackage: "ast",
		Compact:     true, // Save space
	}.Sdump(result))

	expected := ReadSnapshot(t, fileName, actual)

	assert.Equal(t,
		strings.TrimSpace(string(expected)),
		strings.TrimSpace(string(actual)),
	)
}

func WithEachTest(t *testing.T, f func(t *testing.T, test *Test)) {
	_, filename, _, _ := runtime.Caller(0)

	pattern := filepath.Join(filepath.Dir(filename), "queries/*.groq")
	files, err := filepath.Glob(pattern)
	require.NoError(t, err)

	for _, file := range files {
		name := filepath.Base(file)
		queryBytes, err := ioutil.ReadFile(file)
		require.NoError(t, err)
		query := string(queryBytes)

		test := Test{
			Name:     name,
			Query:    query,
			Params:   make(groq.Params),
			Valid:    true,
			FileName: file,
		}

		for _, param := range paramRegex.FindAllStringSubmatch(query, -1) {
			var paramValue interface{}
			err := json.Unmarshal([]byte(param[2]), &paramValue)
			require.NoError(t, err)
			test.Params[param[1]] = paramValue
		}

		versionMatch := versionRegex.FindStringSubmatch(query)
		if versionMatch != nil {
			rng := semver.MustParseRange(versionMatch[1])
			test.Version = &rng
		}

		t.Run(name, func(t *testing.T) {
			f(t, &test)
		})
	}
}

func SnapshotFileName(test *Test) string {
	// Hash by both name and query, since queries are not unique
	s := sha256.New()
	if _, err := s.Write([]byte(test.Name)); err != nil {
		panic(err)
	}
	if _, err := s.Write([]byte(test.Query)); err != nil {
		panic(err)
	}
	return hex.EncodeToString(s.Sum(nil)) + ".txt"
}

// ReadSnapshot reads a snapshot file. If it doesn't exist, the provided data is written,
// but only if the EnvVarGenerateSnapshots envvar is set.
func ReadSnapshot(t *testing.T, fileName string, data []byte) []byte {
	expected, err := ioutil.ReadFile(fileName)
	if err != nil {
		if !os.IsNotExist(err) {
			require.NoError(t, err)
		}

		if !shouldGenerateSnapshots() {
			t.Fatalf("Snapshot file %s missing, won't generate without %s=%s",
				fileName, EnvGenerateSnapshots, "yes")
		}

		require.NoError(t, os.MkdirAll(filepath.Dir(fileName), 0755))
		require.NoError(t, ioutil.WriteFile(fileName, data, 0600))

		t.Logf("Wrote snapshot %s", fileName)
		return data
	}

	return expected
}

func shouldGenerateSnapshots() bool {
	return os.Getenv(EnvGenerateSnapshots) == "yes"
}

const EnvGenerateSnapshots = "GROQ_TEST_GENERATE_SNAPSHOTS"
