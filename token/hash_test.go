package token

import (
	"errors"
	"fmt"
	"math/rand/v2"
	"slices"
	"testing"
)

var foundCoefs = []Coef{
	{IndexApplied: 0, Value: 29, Op: Plus},
	{IndexApplied: 1, Value: 21, Op: Plus},
	{IndexApplied: 2, Value: 10, Op: Plus},
	{IndexApplied: 3, Value: 15, Op: Plus},
	{IndexApplied: -1, Value: 13, Op: Plus},
	{Value: 8}, // Len coefficient.
}

// TestFindPerfectHashKeywords searches for a perfect hash function for the keywords.
// This test is normally skipped unless -run=TestFindPerfectHashKeywords is specified.
// It found the current perfect hash: c0=4 c1=26 c2=17 c3=11 cLast=28 cLen=29
func TestFindPerfectHashKeywords(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping hash search in short mode")
	}

	// Collect all keywords
	const maxCoef = 32
	phf := PerfectHashFinder{
		TableSizeBits:  8,
		DefaultMaxCoef: maxCoef,
	}
	var kwmap [1 << 8]Token
	var keywords []string
	imperfectHash := false
	for tok := keywordBeg + 1; tok < keywordEnd; tok++ {
		kw := tok.String()
		if !tok.IsKeyword() {
			continue
		}
		keywords = append(keywords, kw)
		h := phf.Apply(foundCoefs, kw)
		h2 := kwhash(kw)
		if h != h2 {
			t.Fatal("mismatch hashes", len(keywords), kw, h, h2)
		}
		if kwmap[h] != 0 {
			imperfectHash = true
		}
		kwmap[h] = tok
	}
	if !imperfectHash {
		t.Logf("perfect hash: %+v", foundCoefs)
		return
	}
	coefs := foundCoefs
	t.Logf("Searching perfect hash for %d keywords", len(keywords))
	attempts := 0
	tableBits := []int{8}
	for _, tbits := range tableBits {
		t.Log("searching for perfect hash table size", 1<<tbits)
		phf.TableSizeBits = tbits
		currentAttempts, err := phf.Search(coefs, keywords)
		attempts += currentAttempts
		if err == nil {
			t.Logf("perfect hash found after %d attempts: %+v", attempts, coefs)
			printCoefs(coefs)
			return
		} else if err != nil && currentAttempts == 0 {
			t.Fatal(err)
		}
	}
	t.Error("No perfect hash found after", attempts, "attempts")
}

func TestFindPerfectHashIntrinsics(t *testing.T) {
	const maxCoef = 32
	phf := PerfectHashFinder{
		TableSizeBits:  10,
		DefaultMaxCoef: maxCoef,
	}

	var intrinsics []string
	for intr := fortran66Start + 1; intr < fortran2008End; intr++ {
		if !intr.IsValid() {
			continue
		} else if intr.Version() > 95 {
			break
		}

		name := intr.String()
		got := LookupIntrinsic(name)
		if got == 0 || got != intr {
			t.Errorf("failed lookup %s, got %s", name, got.String())
		}
		intrinsics = append(intrinsics, name)
	}
	// t.Log(intrinsics)

	// Randomizing coefficients requires we select indices of intrinisc we are hashing.
	coefs := make([]Coef, 5)
	for i := range coefs {
		coefs[i].IndexApplied = i
	}
	for j := 0; j < 3; j++ {
		c := &coefs[len(coefs)-j-1]
		c.IndexApplied = -j
	}
	rng := rand.New(rand.NewPCG(1, 1))
	t.Logf("Searching perfect hash for %d intrinsics with %d coefficients", len(intrinsics), len(coefs))
	attempts := 0
	tableBits := []int{10}
	randomRetries := 100
	for _, tbits := range tableBits {
		t.Log("searching for perfect hash table size", 1<<tbits)
		phf.TableSizeBits = tbits
		for range randomRetries {
			randomizeCoefs(coefs, rng, 64, 10)
			currentAttempts, err := phf.Search(coefs, intrinsics)
			attempts += currentAttempts
			if err == nil {
				t.Logf("perfect hash found after %d attempts: %+v", attempts, coefs)
				printCoefs(coefs)
				return
			} else if err != nil && currentAttempts == 0 {
				t.Fatal(err)
			}
		}
	}
	t.Error("No perfect hash found after", attempts, "attempts")
}

func printCoefs(coefs []Coef) {
	lc := coefs[len(coefs)-1]
	fmt.Printf("\nh := uint(len(s))*%d\n", lc.Value)
	for _, c := range coefs[:len(coefs)-1] {
		pfx := ""
		if c.IndexApplied < 0 {
			pfx = "len(s)"
		}
		fmt.Printf("h %s= uint(s[%s%d])*%d\n", c.Op.String(), pfx, c.IndexApplied, c.Value)
	}
}

// TestVerifyCurrentHash verifies the current kwhash function is perfect.
func TestVerifyCurrentHash(t *testing.T) {
	seen := make(map[uint]string)
	mask := uint(len(keywordMap) - 1)

	for tok := keywordBeg + 1; tok < keywordEnd; tok++ {
		kw := tok.String()
		h := kwhash(kw) & mask
		if existing, ok := seen[h]; ok {
			t.Errorf("Collision: %s and %s both hash to %d", existing, kw, h)
		}
		seen[h] = kw
	}

	t.Logf("Tested %d keywords with table size %d", len(seen), len(keywordMap))
}

// Implementation taken from much more complete at github.com/soypat/lexer
type PerfectHashFinder struct {
	TableSizeBits  int
	DefaultMaxCoef uint
	// HashLastIndices
	hashmap []uint
}

type Coef struct {
	IndexApplied int  // Index at which hash consumes byte. Negative value indexes from the end.
	Value        uint // Coefficient value to multiply byte at index.
	MaxValue     uint
	StartValue   uint
	OnlyPow2     bool
	Op           Token
}

func randomizeCoefs(coefs []Coef, rng *rand.Rand, maxCoef, searchSpace int) {
	ops := []Token{Plus, OR, Asterisk}
	for i := range len(coefs) - 1 {
		c := &coefs[i]
		start := rng.IntN(maxCoef)
		end := min(start+searchSpace, maxCoef)
		*c = Coef{
			IndexApplied: c.IndexApplied, // Keep indexing, user should provide intelligence here on best indexing.
			Value:        0,
			StartValue:   uint(start),
			MaxValue:     uint(end),
			OnlyPow2:     false,
			Op:           ops[rng.IntN(len(ops))],
		}
	}
}

var ErrNoCoefficientsFound = errors.New("no coefficients found")

func (c *Coef) init() {
	if c.StartValue == 0 {
		c.Value = 1
	} else {
		c.Value = c.StartValue
	}
	if c.Op == 0 {
		c.Op = Plus
	}
}
func (c *Coef) increment() {
	if c.OnlyPow2 {
		c.Value *= 2
	} else {
		c.Value++
	}
}
func (c *Coef) saturated() bool { return c.Value >= c.MaxValue }

func (phf *PerfectHashFinder) Search(coefs []Coef, inputs []string) (int, error) {
	if phf.TableSizeBits <= 0 || phf.TableSizeBits > 32 {
		return 0, errors.New("zero/negative bits for table size or too large")
	} else if len(coefs) == 0 {
		return 0, errors.New("require at least one coefficient to find perfect hash")
	} else if len(inputs) == 0 {
		return 0, errors.New("zero inputs")
	}
	err := phf.ConfigureCoefsWithDefaults(coefs)
	if err != nil {
		return 0, err
	}
	tblsz := 1 << phf.TableSizeBits
	phf.hashmap = slices.Grow(phf.hashmap[:0], tblsz)[:tblsz]
	hashmap := phf.hashmap
	mask := uint(tblsz) - 1
	currentAttempt := 0
	for {
		currentAttempt++
		attemptSuccess := true
		clear(hashmap)
		for _, kw := range inputs {
			h := phf.apply(mask, coefs, kw)
			tok := hashmap[h]
			if tok != 0 {
				attemptSuccess = false
				break
			}
			hashmap[h] = 1
		}
		if attemptSuccess {
			return currentAttempt, nil
		}
		coefs[0].increment()
		for i := 0; coefs[i].saturated() && i < len(coefs)-1; i++ {
			coefs[i].init()
			coefs[i+1].increment()
		}
		// Check for super-saturation.
		if coefs[len(coefs)-1].Value > coefs[len(coefs)-1].MaxValue {
			break
		}
	}
	return currentAttempt, ErrNoCoefficientsFound
}

func (phf *PerfectHashFinder) ConfigureCoefsWithDefaults(coefs []Coef) error {
	for i := range coefs {
		coefs[i].init()
		if coefs[i].MaxValue == 0 {
			if phf.DefaultMaxCoef <= 0 {
				return errors.New("default max coefficient need be set and positive for input")
			}
			coefs[i].MaxValue = phf.DefaultMaxCoef
		}
	}
	return nil
}

func (phf *PerfectHashFinder) Apply(coefs []Coef, kw string) uint {
	h := phf.apply((1<<phf.TableSizeBits)-1, coefs, kw)
	return h
}

func (phf *PerfectHashFinder) apply(mask uint, coefs []Coef, kw string) uint {
	h := uint(len(kw)) * coefs[len(coefs)-1].Value
	for i := 0; i < len(coefs)-1; i++ {
		idx := coefs[i].IndexApplied
		var a uint
		if idx < 0 && -idx <= len(kw) {
			a = uint(kw[len(kw)+idx]) * coefs[i].Value
		} else if idx >= 0 && idx < len(kw) {
			a = uint(kw[idx]) * coefs[i].Value
		}
		switch coefs[i].Op {
		case Plus:
			h += a
		case OR:
			h ^= a
		case Asterisk:
			h *= a
		default:
			panic("unsupported operation")
		}

	}
	return h & mask
}
