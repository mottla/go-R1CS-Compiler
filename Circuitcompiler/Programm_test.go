package Circuitcompiler

import (
	"fmt"
	"github.com/stretchr/testify/assert"
	"math/big"
	"testing"
)

func TestStaticIfProgram(t *testing.T) {
	code := `
	func main(x bool,y field)(field) {
		if 2>3 {
			return x*x
		}
		if 2>3 {
			return y*y
		}else if 5!=5{
			return x*x*x
		}else if 5==5{
			return y*y*y
		}
		return x*x*x
	}

`
	p := Parse(code)
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
func TestFunction_SPLIT(t *testing.T) {

	switch a := FIELD; a {
	case DecimalNumberToken:
		fmt.Println("d")
	case FIELD | U64:
		fmt.Println("s")
	}
}

func TestForProgram(t *testing.T) {
	code := `
	func a()(func(c bool)(bool)) {	
		return func(a bool)(bool){return true}
	}
	func main(x field,y func(x bool)(bool))(func(c bool)(bool)) {	
		var b = a()
		x = x*x*45 * 345
		return b
	}

`

	program := Parse(code)
	container := program.Execute()

	gates := container.OrderedGates()
	fmt.Println("\n generating R1CS")
	r1cs := program.GatesToR1CS(gates)
	fmt.Printf("number of gates %v, witness length %v \n ", r1cs.NumberOfGates, r1cs.WitnessLength)
	//
	//fmt.Println(r1cs.L)
	//fmt.Println(r1cs.R)
	//fmt.Println(r1cs.O)
	inputs := CombineInputs(program.GetMainCircuit().InputIdentifiers, []*big.Int{big.NewInt(int64(27)), big.NewInt(int64(3))})

	fmt.Println("input")
	fmt.Println(inputs)

	w, err := CalculateTrace(r1cs, inputs)
	assert.NoError(t, err)
	fmt.Printf("witness len %v \n ", len(w))
	fmt.Println(w)
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
//			inputs := CombineInputs(program.GetMainCircuit().InputIdentifiers, io.InputTypes)
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

//seems to have trouble when the field size is to small
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
