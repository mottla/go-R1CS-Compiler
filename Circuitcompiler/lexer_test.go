package Circuitcompiler

import (
	"fmt"
	"math/big"
	"testing"
)

func TestHexNumberState(t *testing.T) {
	v, b := new(big.Int).SetString("ABB", 16)
	if !b {
		panic("asdf")
	}
	println(v)
	v, b = new(big.Int).SetString("AbB", 16)
	if !b {
		panic("asdf")
	}
	println(v.String())
}
func Test_LexerError2(t *testing.T) {
	//code2 := `def main(a):
	//	for(a = 3; a<3; a+=1){
	//		var d =  (c * (1+b) * k)
	//	}
	//	return  d `
	//
	code := `
 var a u64

`

	fmt.Println(code)
	l := New(code, ProbablyWhitespaceState)
	l.Start()
	tok, done := l.NextToken()
	for !done {
		fmt.Printf("%v , %q \n", tok.Type, tok.Identifier)
		tok, done = l.NextToken()
	}

	fmt.Println("done")

}
