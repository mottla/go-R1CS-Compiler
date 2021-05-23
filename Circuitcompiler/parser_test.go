package Circuitcompiler

import (
	"fmt"
	"testing"
)

func TestBuild(t *testing.T) {
	c := []string{}
	ArrayStringBuild([]int64{2, 3, 4}, "", &c)
	fmt.Println(c)
}

func TestParser_PrepareFunctionHeader(t *testing.T) {
	code := `
func test(a field, b func(a field))(field,field) {

}
`
	parser := newParser(code, false)
	toks := parser.stackAllTokens()
	functionInput, rest := splitAtFirstHighestStringType(toks[3:], "{")
	fmt.Println(rest)
	fk := NewCircuit("", nil)
	parser.PrepareFunctionSignature(fk, functionInput)
	fmt.Println(fk)
}

func TestArray(t *testing.T) {
	code := `a[4][3]`
	parser := newParser(code, false)
	toks := parser.stackAllTokens()
	toks = toks[:len(toks)-1]
	ct := Constraint{}
	parser.parseExpression(&function{}, toks, &ct)
	fmt.Println(ct)
}
func TestNewParse(t *testing.T) {
	code := `
	func main(x bool[4][5],y bool)(field) {
		bool[4][3] in = [[4,1,1],[1,1,1],[1,1,1],[1,1,1]]
		return x[1][1]*in[0][0] 
	}
`
	p := NewParse(code)
	container := p.Execute()

	gates := container.OrderedGates()
	fmt.Println("\n generating R1CS")
	r1cs := p.GatesToR1CS(gates)
	fmt.Printf("number of gates %v, witness length %v \n ", r1cs.NumberOfGates, r1cs.WitnessLength)
	//
	fmt.Println(r1cs.L)
	fmt.Println(r1cs.R)
	fmt.Println(r1cs.O)
}

func TestNewParse2(t *testing.T) {
	code := `
	func mul(x bool,y bool)(bool){
		return x*y
	}

	func mul2(x bool,y bool)(bool,bool){
		return x*x,y*y
	}
	
	func mul3(x bool,y func(x bool,y bool)(bool) )(field){
		return y(x,x)
	}

	func main(x bool,y field)(field) {
		#func m5(x bool)(bool){return mul(5,x)}
		x,x = mul2(x,x)	
		return mul3(x,mul)
	}

`
	p := NewParse(code)
	container := p.Execute()

	gates := container.OrderedGates()
	fmt.Println("\n generating R1CS")
	r1cs := p.GatesToR1CS(gates)
	fmt.Printf("number of gates %v, witness length %v \n ", r1cs.NumberOfGates, r1cs.WitnessLength)
	//
	fmt.Println(r1cs.L)
	fmt.Println(r1cs.R)
	fmt.Println(r1cs.O)
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
