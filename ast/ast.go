package ast

type Node interface {
	AppendTokenLiteral(dst []byte) []byte
	AppendString(dst []byte) []byte
}

type Expression interface {
	Node
}

type Statement interface {
	Node
}

type Identifier struct {
	Value string
}

type IntegerLiteral struct {
}
