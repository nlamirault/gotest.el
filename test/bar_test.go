package main

import (
	"fmt"
	"testing"
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

func ExampleA() {
	fmt.Println("Example A")
	// Output: Example A
}

func ExampleB() {
	fmt.Println("Example B")
	// Output: Example B
}

func ExampleC() {
	fmt.Println("Example C")
	// Output: Example C
}

func BenchmarkHello(b *testing.B) {
	for i := 0; i < b.N; i++ {
		fmt.Println("hello")
	}
}

func BenchmarkHelloWorld(b *testing.B) {
	for i := 0; i < b.N; i++ {
		fmt.Println("hello world")
	}
}
