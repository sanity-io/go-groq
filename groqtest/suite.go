//nolint:gosec
package groqtest

import (
	"crypto/sha1"
	"encoding/json"
	"fmt"
	"io"
	"sort"
	"strings"

	"github.com/blang/semver"

	"github.com/sanity-io/go-groq"
)

type Feature string

const (
	FeatureExperimentalFunctions     Feature = "experimentalFunctions"
	FeatureNamespaces                Feature = "namespaces"
	FeatureScoring                   Feature = "scoring"
	FeaturePortableText              Feature = "portableText"
	FeatureGeoFunctions              Feature = "geoFunctions"
	FeatureWildcardMatchSegmentation Feature = "wildcardMatchSegmentation"
)

type Suite struct {
	Datasets map[string]*Dataset
	Tests    []*Test
}

type Dataset struct {
	ID         string
	Hash       string
	URL        string
	IsExternal bool
	Documents  []map[string]interface{}
}

type Test struct {
	ID        string
	Name      string
	Query     string
	Result    interface{}
	DatasetID string
	Dataset   *Dataset
	Params    groq.Params
	Valid     bool
	Version   *semver.Range
	FileName  string
	Features  []Feature
}

// HasFeature returns true if the test has any of the provided features enabled.
func (t *Test) HasFeature(want ...Feature) bool {
	if len(want) == 0 {
		return true
	}

	for _, w := range want {
		for _, f := range t.Features {
			if f == w {
				return true
			}
		}
	}
	return false
}

func NewSuite() *Suite {
	return &Suite{
		Datasets: map[string]*Dataset{},
	}
}

func hash(input []byte) string {
	hasher := sha1.New()
	_, err := hasher.Write(input)
	if err != nil {
		panic("failed to hash")
	}
	return fmt.Sprintf("%x", hasher.Sum(nil))
}

func (s *Suite) ReadFile(r io.Reader) error {
	dec := json.NewDecoder(r)

	for {
		var entry map[string]interface{}
		if err := dec.Decode(&entry); err != nil {
			if err == io.EOF {
				break
			}
			return fmt.Errorf("reading NDJSON stream: %w", err)
		}

		id := entry["_id"].(string)

		switch entry["_type"] {
		case "dataset":
			dataset := &Dataset{
				ID:  id,
				URL: entry["url"].(string),
			}

			if jsonDocs, exists := entry["documents"]; exists {
				dataset.IsExternal = false

				b, err := json.Marshal(entry)
				if err != nil {
					panic(err)
				}
				dataset.Hash = hash(b)

				docs, err := readDocuments(jsonDocs.([]interface{}))
				if err != nil {
					return err
				}
				dataset.Documents = docs
			} else {
				dataset.IsExternal = true
				dataset.Hash = hash([]byte(dataset.URL))
			}

			s.Datasets[id] = dataset
		case "test":
			datasetID := entry["dataset"].(map[string]interface{})["_ref"].(string)

			var features []Feature
			if f, ok := entry["features"].([]interface{}); ok && len(f) > 0 {
				features = make([]Feature, len(f))
				for i, s := range f {
					features[i] = Feature(s.(string))
				}
			}

			var params groq.Params
			if p, ok := entry["params"].(map[string]interface{}); ok {
				params = groq.Params(p)
			}

			var version *semver.Range
			if v, ok := entry["version"].(string); ok {
				rng, err := semver.ParseRange(v)
				if err != nil {
					return fmt.Errorf("parsing version in test %q: %w", id, err)
				}
				version = &rng
			}

			test := &Test{
				ID:        id,
				Name:      strings.TrimSpace(entry["name"].(string)),
				Query:     strings.TrimSpace(entry["query"].(string)),
				Result:    entry["result"],
				Params:    params,
				Valid:     entry["valid"].(bool),
				FileName:  entry["filename"].(string),
				DatasetID: datasetID,
				Features:  features,
				Version:   version,
			}
			s.Tests = append(s.Tests, test)
		}
	}

	return nil
}

func (s *Suite) Finalize() error {
	for _, test := range s.Tests {
		dataset, ok := s.Datasets[test.DatasetID]
		if !ok {
			return fmt.Errorf("unknown dataset: %s", test.DatasetID)
		}
		test.Dataset = dataset
	}

	return nil
}

func readDocuments(jsonDocs []interface{}) (result []map[string]interface{}, e error) {
	for _, val := range jsonDocs {
		attr, ok := val.(map[string]interface{})
		if !ok {
			return nil, fmt.Errorf("expected map[string]interface{}, found %T", val)
		}
		if _, ok := attr["_id"].(string); !ok {
			return nil, fmt.Errorf("expected _id")
		}
		result = append(result, attr)
	}

	sort.Slice(result, func(i, j int) bool {
		return result[i]["_id"].(string) < result[j]["_id"].(string)
	})

	return
}

const NamespacePrefix = "groqtest."

func (t *Dataset) Namespace() string {
	return NamespacePrefix + t.Hash
}
