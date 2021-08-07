//nolint:gosec
package groqtest

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"regexp"
	"sort"
	"sync"
	"testing"
	"time"

	"github.com/blang/semver"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type Runner interface {
	// Run must run the test.
	Run(t *Test) (interface{}, error)

	// GROQSpecVersion must return which GROQ spec version this runner conforms o.
	GROQSpecVersion() semver.Version

	// Features must return supported features. Incompatible tests will be ignored.
	Features() []Feature
}

var skiplist = []struct {
	queryPattern *regexp.Regexp
	reason       string
}{}

func CreateTestFunction(test *Test, runner Runner) func(t *testing.T) {
	return func(t *testing.T) {
		for _, entry := range skiplist {
			if entry.queryPattern.MatchString(test.Query) {
				t.Skipf("Skipping due to: %s", entry.reason)
			}
		}

		if test.Dataset.IsExternal {
			SkipIfPerformanceTestsDisabled(t)
		}

		if test.Version != nil && !(*test.Version)(runner.GROQSpecVersion()) {
			t.Skip("Skipped due to version constraint")
		}

		for _, w := range test.Features {
			found := false
			for _, f := range runner.Features() {
				if f == w {
					found = true
					break
				}
			}
			if !found {
				t.Skipf("Skipped as feature %q is not supported by runner", w)
			}
		}

		t.Logf("%s [Namespace] %s [Query] %s [Params] %v", time.Now(), test.Dataset.Namespace(), test.Query, test.Params)

		result, err := runner.Run(test)
		if test.Valid {
			require.NoError(t, err)

			var expected, actual interface{}

			expected, err = normalizeResult(test.Result)
			require.NoError(t, err)

			actual, err = normalizeResult(result)
			require.NoError(t, err)

			if ok := assert.Equal(t, expected, replaceScores(actual)); !ok {
				docs, _ := json.Marshal(test.Dataset.Documents)
				t.Logf("%s [Test Documents] %v", time.Now(), string(docs))
				t.Logf("%s [Query] %v", time.Now(), test.Query)
			}
		} else {
			require.Error(t, err)
		}
	}
}

var (
	globalSuite    *Suite
	globalSuiteErr error
	suiteLoader    sync.Once
)

func loadOnce(f func() (*Suite, error)) (*Suite, error) {
	suiteLoader.Do(func() {
		globalSuite, globalSuiteErr = f()
	})
	return globalSuite, globalSuiteErr
}

func GetSuite() (*Suite, error) {
	return loadOnce(func() (*Suite, error) {
		path, ok := os.LookupEnv(EnvVar)
		if !ok {
			return nil, ErrNoConfiguration
		}

		f, err := os.Open(path)
		if err != nil {
			return nil, err
		}

		defer func() { _ = f.Close() }()

		suite := NewSuite()

		err = suite.ReadFile(f)
		if err != nil {
			return nil, err
		}

		if err := suite.Finalize(); err != nil {
			return nil, err
		}

		return suite, nil
	})
}

const EnvVar = "GROQTEST_SUITE"

var ErrNoConfiguration = errors.New("no GROQ test suite configuration as " + EnvVar + " is not set")

func normalizeResult(raw interface{}) (interface{}, error) {
	var result interface{}
	if err := remarshalJSON(&raw, &result); err != nil {
		return nil, fmt.Errorf("unmarshaling JSON: %w", err)
	}
	return result, nil
}

func remarshalJSON(in interface{}, out interface{}) error {
	b, err := json.Marshal(in)
	if err != nil {
		return err
	}
	return json.Unmarshal(b, out)
}

func replaceScores(result interface{}) interface{} {
	var scores []float64
	visitData(result, func(value interface{}) {
		if doc, ok := value.(map[string]interface{}); ok {
			if score, ok := doc["_score"].(float64); ok {
				scores = append(scores, score)
			}
		}
	})
	if len(scores) > 0 {
		sort.Slice(scores, func(i, j int) bool {
			return scores[j] < scores[i]
		})

		scoreMap := make(map[float64]int, len(scores))

		pos := 0
		for _, score := range scores {
			if _, ok := scoreMap[score]; !ok {
				pos++
				scoreMap[score] = pos
			}
		}

		visitData(result, func(value interface{}) {
			if doc, ok := value.(map[string]interface{}); ok {
				if score, ok := doc["_score"].(float64); ok {
					delete(doc, "_score")
					doc["_pos"] = float64(scoreMap[score])
				}
			}
		})
	}

	return result
}

func visitData(result interface{}, f func(interface{})) {
	f(result)

	switch result := result.(type) {
	case map[string]interface{}:
		for _, v := range result {
			visitData(v, f)
		}
	case []interface{}:
		for _, elem := range result {
			visitData(elem, f)
		}
	}
}

func IncludePerformanceTests() bool {
	return os.Getenv(EnvIncludeExternalDatasets) == "yes"
}

func SkipIfPerformanceTestsDisabled(t *testing.T) {
	t.Helper()

	if !IncludePerformanceTests() {
		t.Skip("Skipping test in as " + EnvIncludeExternalDatasets + " is not set")
	}
}

const (
	EnvIncludeExternalDatasets = "GROQ_TEST_INCLUDE_EXTERNAL_DATASETS"
)
