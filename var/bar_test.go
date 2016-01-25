package main

import (
	"fmt"
	"testing"
)

func TestA(t *testing.T) {
	fmt.Println("test a")
}

func TestB(t *testing.T) {
	fmt.Println("test b")
}

func TestAB(t *testing.T) {
	fmt.Println("test ab")
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
