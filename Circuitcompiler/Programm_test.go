package Circuitcompiler

import (
	"fmt"
	"github.com/mottla/go-R1CS-Compiler/testPrograms"
	"github.com/mottla/go-R1CS-Compiler/utils"
	"github.com/stretchr/testify/assert"
	"math/big"

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
	n1 := new(big.Int).SetInt64(-1)
	//n2 := new(big.Int).SetInt64(23426)
	fmt.Println(utils.Field.ArithmeticField.Affine(n1))
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

func TestCorrectness(t *testing.T) {

	for _, test := range testPrograms.TestPrograms {
		if test.Skip {
			continue
		}

		fmt.Println("\n unreduced")
		fmt.Println(test.Code)
		program := Parse(test.Code, true)

		container := program.Execute()
		gates := container.OrderedGates()
		fmt.Println("\n generating R1CS")
		r1cs := program.GatesToR1CS(gates)
		fmt.Printf("number of gates %v, witness length %v \n ", r1cs.NumberOfGates, r1cs.WitnessLength)

		fmt.Println(r1cs.L)
		fmt.Println(r1cs.R)
		fmt.Println(r1cs.O)

		for _, io := range test.IO {
			inputs := CombineInputs(program.GetMainCircuit().Inputs, io.Inputs)

			fmt.Println("input")
			fmt.Println(inputs)

			w, err := CalculateTrace(r1cs, inputs)
			assert.NoError(t, err)
			fmt.Printf("witness len %v \n ", len(w))
			fmt.Println(w)

		}
	}
}
