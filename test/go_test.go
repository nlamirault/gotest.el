package test

import (
	"testing"

	"github.com/stretchr/testify/suite"
)

func TestFoo(t *testing.T) {
	t.Log("logFoo")
}

func TestBar(t *testing.T) {
	t.Log("logBar")
}

func Test_Baz(t *testing.T) {
	t.Log("log_Baz")
}

type Suite struct {
	suite.Suite
}

func (s *Suite) TestIndividualTest() {
	s.T().Log("logSuite")
}
