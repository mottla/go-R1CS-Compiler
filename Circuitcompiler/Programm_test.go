package Circuitcompiler

import (
	"fmt"
	"testing"
)

func iterate(fromX, toX int, call func(int)) {

	if fromX == toX {

		return
	}
	call(fromX)
	iterate(fromX+1, toX, call)

	return
}
func TestFunction_SPLIT(t *testing.T) {

	a, b := uint8(34), uint8(210)
	fmt.Println(a * b)

}
func TestCombineInputs(t *testing.T) {
	ctr := 0
	var ff = func(k int) {
		var EQ = func(i int) {
			ctr++
		}
		iterate(0, 9, EQ)
	}
	iterate(0, 9, ff)
	fmt.Print(ctr)
	return
}

//func TestCorrectness(t *testing.T) {
//
//	for _, test := range testPrograms.TestPrograms {
//		if test.Skip {
//			continue
//		}
//
//		fmt.Println("\n unreduced")
//		fmt.Println(test.Code)
//		program := Parse(test.Code, true)
//
//		container := program.Execute()
//		gates := container.OrderedGates()
//		fmt.Println("\n generating R1CS")
//		r1cs := program.GatesToR1CS(gates)
//		fmt.Printf("number of gates %v, witness length %v \n ", r1cs.NumberOfGates, r1cs.WitnessLength)
//		//
//		//fmt.Println(r1cs.L)
//		//fmt.Println(r1cs.R)
//		//fmt.Println(r1cs.O)
//
//		for _, io := range test.IO {
//			inputs := CombineInputs(program.GetMainCircuit().InputIdentifiers, io.Inputs)
//
//			fmt.Println("input")
//			fmt.Println(inputs)
//
//			w, err := CalculateTrace(r1cs, inputs)
//			assert.NoError(t, err)
//			fmt.Printf("witness len %v \n ", len(w))
//			fmt.Println(w)
//
//		}
//	}
//}
//
////seems to have trouble when the field size is to small
//func TestFixedBaseExponentiation(t *testing.T) {
//
//	var codeGen = func(exponent string) string {
//		return fmt.Sprintf(`
//	func main( x) {
//	public{
//		x
//	}
//		return x**%v
//	}
//
//`, exponent)
//	}
//
//	for i := 0; i < 10; i++ {
//		exponent, _ := utils.Field.ArithmeticField.Rand()
//		code := codeGen(exponent.String())
//
//		program := Parse(code, true)
//
//		container := program.Execute()
//		gates := container.OrderedGates()
//		r1cs := program.GatesToR1CS(gates)
//		for j := 0; j < 10; j++ {
//			base, _ := utils.Field.ArithmeticField.Rand()
//			expected := utils.Field.ArithmeticField.Exp(base, exponent)
//			inputs := CombineInputs(program.GetMainCircuit().InputIdentifiers, []*big.Int{base})
//			w, err := CalculateTrace(r1cs, inputs)
//			assert.NoError(t, err)
//			assert.Equal(t, w[len(w)-1], expected)
//		}
//	}
//
//}
