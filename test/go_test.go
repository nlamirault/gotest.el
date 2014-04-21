package test

import "testing"

func TestFoo(t *testing.T) {
	t.Log("logFoo")
}

func TestBar(t *testing.T) {
	t.Log("logBar")
}

func Test_Baz(t *testing.T) {
	t.Log("log_Baz")
}
