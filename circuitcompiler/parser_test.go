package circuitcompiler

import (
	"fmt"
	"math/big"
	"testing"
)

//just a big prime
var Order,_ = new(big.Int).SetString("21888242871839275222246405745257275088548364400416034343698204186575808495617",10)

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

	Parse(code, Order)
}

//only to see the difference between the split funcitons
func TestParser_SplitAt(t *testing.T) {
	toks := []Token{
		{
			Value: "a",
		},
		{
			Value: "b",
		},
		{
			Value: "c",
		},
		{
			Value: "a",
		},
		{
			Value: "e",
		},
		{
			Value: ")",
		},
		{
			Value: "a",
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
			Value: "(",
		},
		{
			Value: "b",
		},
		{
			Value: "c",
		},
		{
			Value: "a",
		},
		{
			Value: "e",
		},
		{
			Value: "g",
		},
		{
			Value: ")",
		},
	}
	fmt.Println(toks)
	fmt.Println(stripOfBrackets(toks))
}
