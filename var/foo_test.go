package main

import (
	"fmt"
	"testing"
)

func TestA(t *testing.T) {
	fmt.Println("test a")
}

func TestAB(t *testing.T) {
	fmt.Println("test ab")
}

func ExampleA() {
	fmt.Println("Example")
	// Output: Example
}

func BenchmarkHello(b *testing.B) {
	for i := 0; i < b.N; i++ {
		fmt.Sprintf("hello")
	}
}
