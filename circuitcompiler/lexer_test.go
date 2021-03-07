package circuitcompiler

import (
	"fmt"
	"testing"
)

func Test_LexerError2(t *testing.T) {
	//code2 := `def main(a):
	//	for(a = 3; a<3; a+=1){
	//		var d =  (c * (1+b) * k)
	//	}
	//	return  d `
	//
	code := `
import "asdf"
	func main( x  ,  z ) {
		if 3 < 4{
		}
		var a = func(x){return x}
	}

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
