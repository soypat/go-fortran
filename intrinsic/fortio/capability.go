package fortio

// IOCapability is a bitfield representing file/unit capabilities
// as returned by INQUIRE statements.
type IOCapability uint16

const (
	CapSequential  IOCapability = 1 << iota // Can open for sequential access
	CapDirect                               // Can open for direct access
	CapStream                               // Can open for stream access
	CapFormatted                            // Can open as formatted
	CapUnformatted                          // Can open as unformatted
	CapRead                                 // Can read from file/unit
	CapWrite                                // Can write to file/unit
	CapExists                               // File exists
	CapOpened                               // Unit is currently connected
	CapNamed                                // File is named (not scratch)
)

// HasAny returns true if capability c has any flags of f set.
func (c IOCapability) HasAny(f IOCapability) bool {
	return c&f != 0
}

// v returns true if capability c has all flags f set.
func (c IOCapability) HasAll(f IOCapability) bool {
	return c&f == f
}

// With returns capability c with flag f set.
func (c IOCapability) With(f IOCapability) IOCapability {
	return c | f
}

// Without returns capability c with flag f cleared.
func (c IOCapability) Without(f IOCapability) IOCapability {
	return c &^ f
}
