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
	def main( x  ,  z ) {
		var a= B(3)+x
		return a(x)*b(z)
	}

`

	fmt.Println(code)
	l := New(code, ProbablyWhitespaceState)
	l.Start()
	tok, done := l.NextToken()
	for !done {
		fmt.Printf("%v , %q \n", tok.Type, tok.Value)
		tok, done = l.NextToken()
	}

	fmt.Println("done")

}
