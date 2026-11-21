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
	DefinedFile    string
	DefinedLine    int
	DefinedColumn  int
}

func newSymbol(name string, classifications ...TypeClassification) *Symbol {
	s := &Symbol{
		Name:           name,
		Classification: make(map[TypeClassification]bool),
		Members:        make(map[string]*Symbol),
	}
	for _, c := range classifications {
		s.Classification[c] = true
	}
	return s
}

func newPredefinedSymbol(name string, classifications ...TypeClassification) *Symbol {
	s := newSymbol(name, classifications...)
	s.Predefined = true
	return s
}

func (s *Symbol) String() string {
	names := make([]string, 0, len(s.Classification))
	for c := range s.Classification {
		names = append(names, fmt.Sprintf("%d", int(c)))
	}
	result := s.Name + " (with classification " + strings.Join(names, ", ") + ")"
	if s.DefinedFile != "" {
		result += fmt.Sprintf(" defined at %s:%d:%d", s.DefinedFile, s.DefinedLine, s.DefinedColumn)
	}
	return result
}
