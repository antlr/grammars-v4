package parser

import (
	"fmt"
	"strings"
)

type Symbol struct {
	Name           string
	Classification map[TypeClassification]bool
	Members        map[string]*Symbol
	Parent         *Symbol
	Predefined     bool
	IsComposite    bool
	DefinedFile    string
	DefinedLine    int
	DefinedColumn  int
}

func NewSymbol() *Symbol {
	return &Symbol{
		Classification: make(map[TypeClassification]bool),
		Members:        make(map[string]*Symbol),
	}
}

func (s *Symbol) String() string {
	var classifications []string
	for tc := range s.Classification {
		classifications = append(classifications, fmt.Sprintf("%d", tc))
	}
	result := s.Name + " (with classification " + strings.Join(classifications, ", ") + ")"
	if s.IsComposite {
		result += " [composite]"
	}
	if s.DefinedFile != "" {
		result += fmt.Sprintf(" defined at %s:%d:%d", s.DefinedFile, s.DefinedLine, s.DefinedColumn)
	}
	return result
}
