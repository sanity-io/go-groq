package tokenizer_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/sanity-io/go-groq/ast"
	"github.com/sanity-io/go-groq/groqtest"
	"github.com/sanity-io/go-groq/testhelpers"
	"github.com/sanity-io/go-groq/tokenizer"
)

func TestTokenizer(t *testing.T) {
	testhelpers.WithEachTest(t, func(t *testing.T, test *groqtest.Test) {
		fileName := filepath.Join("snapshots", testhelpers.SnapshotFileName(test))

		require.NoError(t, os.MkdirAll(filepath.Dir(fileName), 0755))

		t.Logf("File: %s", fileName)

		var buf bytes.Buffer
		buf.WriteString(test.Query + "\n\r")
		buf.WriteString("----\n\r")

		tk := tokenizer.New(test.Query)
		for {
			token, text, pos := tk.Scan()
			_, _ = buf.WriteString(fmt.Sprintf("%q %v %d\n", text, token, pos))
			if token == ast.EOF {
				break
			}
		}

		actual := buf.Bytes()

		expected := testhelpers.ReadSnapshot(t, fileName, actual)

		assert.Equal(t, string(expected), string(actual))
	})
}
