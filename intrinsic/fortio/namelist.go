package fortio

import (
	"bytes"
	"io"
	"strings"
)

// NamelistVar represents a variable in a namelist group.
type NamelistVar struct {
	Name string
	Ptr  any // pointer to variable
}

// ReadNamelist reads namelist-formatted input from a unit.
// Format: &GROUPNAME var1=value, var2=value, ... /
func (env *Environment) ReadNamelist(unit int32, groupName string, vars []NamelistVar) IOStat {
	state := env.getUnit(unit)
	if state == nil || state.file == nil {
		return IOStatErrNotConnected
	}

	content, stat := env.readNamelistContent(state, groupName)
	if stat != IOStatOK {
		return stat
	}

	if err := parseNamelistContent(content, vars); err != nil {
		env.lastStat = IOStatErrConversion
		env.lastMsg = err.Error()
		return IOStatErrConversion
	}
	return IOStatOK
}

// WriteNamelist writes namelist-formatted output to a unit.
// Format: &GROUPNAME var1=value, var2=value, ... /
func (env *Environment) WriteNamelist(unit int32, groupName string, vars []NamelistVar) IOStat {
	state := env.getUnit(unit)
	if state == nil || state.file == nil {
		return IOStatErrNotConnected
	}

	buf := env.buf[:0]
	buf = append(buf, " &"...)
	buf = append(buf, groupName...)
	buf = append(buf, '\n')

	for _, v := range vars {
		buf = append(buf, ' ')
		buf = append(buf, v.Name...)
		buf = append(buf, '=')
		buf = formatValue(buf, derefPtr(v.Ptr))
		buf = append(buf, '\n')
	}
	buf = append(buf, " /\n"...)

	_, err := state.file.Write(buf)
	env.buf = buf[:0]
	if err != nil {
		return env.mapError(err)
	}
	return IOStatOK
}

// readNamelistContent reads &GROUPNAME ... / block from file.
func (env *Environment) readNamelistContent(state *unitState, groupName string) (string, IOStat) {
	var content bytes.Buffer
	inGroup := false
	needle := []byte("&" + groupName)

	for {
		line, err := env.appendLine(nil, state)
		if err != nil {
			if err == io.EOF && inGroup {
				return content.String(), IOStatOK
			}
			return "", env.mapError(err)
		}

		trimmed := bytes.TrimSpace(line)
		if !inGroup {
			// Look for &GROUPNAME (case-insensitive)
			if len(trimmed) > 0 && (trimmed[0] == '&' || trimmed[0] == '$') {
				if len(trimmed) >= len(needle) && bytes.EqualFold(trimmed[:len(needle)], needle) {
					inGroup = true
					rest := bytes.TrimSpace(trimmed[len(needle):])
					if len(rest) > 0 {
						content.Write(rest)
						content.WriteByte(' ')
					}
				}
			}
		} else {
			// Check for terminator: / or $END
			if len(trimmed) == 1 && trimmed[0] == '/' {
				break
			}
			if bytes.EqualFold(trimmed, []byte("$END")) {
				break
			}
			// Check if line ends with /
			if len(trimmed) > 0 && trimmed[len(trimmed)-1] == '/' {
				content.Write(trimmed[:len(trimmed)-1])
				break
			}
			content.Write(trimmed)
			content.WriteByte(' ')
		}
	}
	return content.String(), IOStatOK
}

// parseNamelistContent parses "name=value name=value" and assigns to vars.
// Values can be separated by commas, spaces, or newlines.
func parseNamelistContent(content string, vars []NamelistVar) error {
	// Build lookup for O(1) access by uppercase name
	lookup := make(map[string]any, len(vars))
	for _, v := range vars {
		lookup[toUpper(v.Name)] = v.Ptr
	}

	for len(content) > 0 {
		content = trimSpaces(content)
		if len(content) == 0 {
			break
		}

		// Find '='
		eqIdx := strings.Index(content, "=")
		if eqIdx < 0 {
			break
		}
		name := toUpper(trimSpaces(content[:eqIdx]))
		content = content[eqIdx+1:]

		// Find value end: next '=' preceded by identifier means new assignment
		// Look for pattern: comma, or identifier followed by '='
		valEnd := findValueEnd(content)
		value := trimSpaces(content[:valEnd])
		content = strings.TrimLeft(content[valEnd:], ", \t")

		if ptr, ok := lookup[name]; ok {
			if err := scanValue(value, ptr); err != nil {
				return err
			}
		}
	}
	return nil
}

// findValueEnd finds where the current value ends in namelist content.
// Returns index of the end of value (before next var= or end of string).
func findValueEnd(s string) int {
	// Look for next "identifier=" pattern
	for i := 0; i < len(s); i++ {
		if s[i] == ',' {
			return i
		}
		if s[i] == '=' && i > 0 {
			// Found '=', backtrack to find start of identifier
			j := i - 1
			for j > 0 && (s[j] == ' ' || s[j] == '\t') {
				j--
			}
			// Check if we found an identifier char
			if !isIdentChar(s[j]) {
				continue
			}
			// Find start of identifier
			start := j
			for start > 0 && isIdentChar(s[start-1]) {
				start--
			}
			return start
		}
	}
	return len(s)
}

func isIdentChar(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
}

// derefPtr dereferences a pointer for formatting.
func derefPtr(ptr any) any {
	switch p := ptr.(type) {
	case *int32:
		return *p
	case *int64:
		return *p
	case *float32:
		return *p
	case *float64:
		return *p
	case *string:
		return *p
	case *bool:
		return *p
	default:
		return ptr
	}
}

// toUpper converts string to uppercase (ASCII only).
func toUpper(s string) string {
	b := []byte(s)
	for i, c := range b {
		if c >= 'a' && c <= 'z' {
			b[i] = c - 32
		}
	}
	return string(b)
}
