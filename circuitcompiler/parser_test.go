package circuitcompiler

import (
	"fmt"
	"testing"
)

func TestBuild(t *testing.T) {
	c := []string{}
	ArrayStringBuild([]int64{2, 3, 4}, "", &c)
	fmt.Println(c)
}

func Test_Parser(t *testing.T) {

	//code := `def main(a){
	//		var d =  asdf(a(x ),b,   c,d*8798,  32 )			* 3
	//		return  d
	//	}
	//
	//	def foo(o,k){
	//		for(a = 3; a<2; a+=1){
	//			var d =  (c * (1+b) * k)
	//			return  d
	//			}
	//		if a<b{
	//				return foo()
	//			}
	//		return d * i
	//	}
	//`
	code := `def main(a){	
			return  d[1*435]*3
		}
	`
	fmt.Println(code)

	Parse(code, false)
}

//only to see the difference between the split funcitons
func TestParser_SplitAt(t *testing.T) {
	toks := []Token{
		{
			Identifier: "a",
		},
		{
			Identifier: "b",
		},
		{
			Identifier: "c",
		},
		{
			Identifier: "a",
		},
		{
			Identifier: "e",
		},
		{
			Identifier: ")",
		},
		{
			Identifier: "a",
		},
	}

	fmt.Println(splitTokensAtFirstString(toks, ")"))

	fmt.Println(splitAt(toks, ")"))

	fmt.Println(splitAtClosingBrackets(toks))

}

//only to see the difference between the split funcitons
func TestParser_StripOfBrackets(t *testing.T) {
	toks := []Token{
		{
			Identifier: "(",
		},
		{
			Identifier: "b",
		},
		{
			Identifier: "c",
		},
		{
			Identifier: "a",
		},
		{
			Identifier: "e",
		},
		{
			Identifier: "g",
		},
		{
			Identifier: ")",
		},
	}
	fmt.Println(toks)
	fmt.Println(stripOfBrackets(toks))
}
