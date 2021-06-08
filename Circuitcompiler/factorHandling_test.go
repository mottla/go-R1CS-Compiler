package Circuitcompiler

import (
	"fmt"
	"math/big"
	"testing"
)

//Tokens are essential to identify, if a specific Gate has been computed already
//eg. if we can extract a factor from a Gate that is independent of commutativity, multiplicativitz we will do much better, in finding and reusing old outputs do
//minimize the multiplication Gate number
// for example the Gate a*b == Gate b*a hence, we only need to compute one of both.

func TestNewFactors(t *testing.T) {
	tks := Tokens{Token{
		Type:       FIELD,
		Identifier: "",
		value:      bigOne,

		isArgument: false,

		readInLine: 0,
	}}
	tks.Add(tks[0])

	tks.Add(Token{
		Type:       FIELD,
		Identifier: "a",
		value:      bigOne,

		isArgument: true,
		readInLine: 0,
	})
	fmt.Println(tks[0].String())
	tks.Add(Token{
		Type:       FIELD,
		Identifier: "a",
		value:      bigOne,
		isArgument: true,
		readInLine: 0,
	})
	fmt.Println(tks[0].String())
	tks.AddFactors(tks)
	fmt.Println(tks[0].String())
	fmt.Println(big.NewInt(0))
}

func TestToken_CopyAndSetMultiplicative(t *testing.T) {
	var a = 42
	var b = func() int { return a }
	a = 12
	fmt.Println(b())

	var c = []int{a, a}
	a = 3
	fmt.Println(c[1])
}
