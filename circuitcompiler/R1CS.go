package circuitcompiler

import (
	"errors"
	"fmt"
	"github.com/mottla/go-AlgebraicProgram-SNARK/circuitcompiler/fields"
	"math/big"
)

type R1CS struct {
	L [][]*big.Int
	R [][]*big.Int
	O [][]*big.Int
}
type R1CSTransposed struct {
	L [][]*big.Int
	R [][]*big.Int
	O [][]*big.Int
}

func (er1cs *R1CS) Transpose() (transposed R1CSTransposed) {

	transposed.L = fields.Transpose(er1cs.L)
	transposed.R = fields.Transpose(er1cs.R)

	transposed.O = fields.Transpose(er1cs.O)
	return
}

// R1CSToQAP converts the R1CS* values to the EAP values
//it uses Lagrange interpolation to to fit a polynomial through each slice. The x coordinate
//is simply a linear increment starting at 1
//within this process, the polynomial is evaluated at position 0
//so an alpha/beta/gamma value is the polynomial evaluated at 0
// the domain polynomial therefor is (-1+x)(-2+x)...(-n+x)
func (er1cs *R1CSTransposed) ER1CSToEAP(pf fields.Fields) (lPoly, rPoly, ePoly, oPoly [][]*big.Int) {

	lT := er1cs.L
	rT := er1cs.R

	oT := er1cs.O
	for i := 0; i < len(lT); i++ {
		lPoly = append(lPoly, pf.PolynomialField.LagrangeInterpolation(lT[i]))

		rPoly = append(rPoly, pf.PolynomialField.LagrangeInterpolation(rT[i]))

		oPoly = append(oPoly, pf.PolynomialField.LagrangeInterpolation(oT[i]))
	}

	return
}

//Calculates the witness (program trace) given some input
//asserts that R1CS has been computed and is stored in the program p memory calling this function
func (r1cs *R1CS) CalculateWitness(input []*big.Int, f fields.Fields) (witness []*big.Int, err error) {

	witness = fields.ArrayOfBigZeros(len(r1cs.L[0]))
	set := make([]bool, len(witness))
	witness[0] = big.NewInt(int64(1))
	set[0] = true

	for i := range input {
		witness[i+1] = input[i]
		set[i+1] = true
	}

	zero := big.NewInt(int64(0))

	getKnownsAndUnknowns := func(array []*big.Int) (knowns []*big.Int, unknownsAtIndices []int) {
		knowns = fields.ArrayOfBigZeros(len(array))
		for j, val := range array {
			if val.Cmp(zero) != 0 {
				if !set[j] {
					unknownsAtIndices = append(unknownsAtIndices, j)
				} else {
					knowns[j] = val
				}
			}
		}
		return
	}

	sum := func(array []*big.Int) *big.Int {
		return f.ArithmeticField.ScalarProduct(array, witness)
	}

	for i := 0; i < len(r1cs.L); i++ {
		gatesLeftInputs := r1cs.L[i]
		gatesRightInputs := r1cs.R[i]

		gatesOutputs := r1cs.O[i]

		leftKnowns, leftUnknowns := getKnownsAndUnknowns(gatesLeftInputs)
		rightKnowns, rightUnknowns := getKnownsAndUnknowns(gatesRightInputs)
		outKnowns, outUnknowns := getKnownsAndUnknowns(gatesOutputs)

		//equality gate
		if len(leftUnknowns)+len(rightUnknowns)+len(outUnknowns) == 0 {
			result := f.ArithmeticField.Mul(sum(rightKnowns), sum(leftKnowns))
			if result.Cmp(sum(outKnowns)) != 0 {
				return nil, errors.New(fmt.Sprintf("at equality gate %v there is unequality. we cannot process", i))
			}

		}

		if len(leftUnknowns)+len(rightUnknowns)+len(outUnknowns) > 1 {
			return nil, errors.New(fmt.Sprintf("at gate %v:computing more then one unknown in Gate assignment is not possible", i))
		}

		// (a*x + b + c..) (d+e+..) = (f+g+..)   we solve for x
		if len(leftUnknowns) == 1 {
			sumright := sum(rightKnowns)
			if sumright.Cmp(zero) == 0 {
				fmt.Println(r1cs.L[i])
				fmt.Println(r1cs.R[i])
				fmt.Println(r1cs.O[i])

				return nil, errors.New(fmt.Sprintf("at gate %v:the summation of R inputs cannot be 0 if the unknown is in Lexer", i))
			}

			result := f.ArithmeticField.Div(sum(outKnowns), sumright)
			result = f.ArithmeticField.Sub(result, sum(leftKnowns))
			result = f.ArithmeticField.Div(result, gatesLeftInputs[leftUnknowns[0]]) //divide by a
			set[leftUnknowns[0]] = true
			witness[leftUnknowns[0]] = result
			continue
		}
		// (a + b + c..) (d+e*x+..)  = (f+g+..)   we solve for x
		if len(rightUnknowns) == 1 {
			sumleft := sum(leftKnowns)
			if sumleft.Cmp(zero) == 0 {
				return nil, errors.New(fmt.Sprintf("at gate %v:the summation of Lexer inputs cannot be 0 if the unknown is in R", i))
			}
			result := f.ArithmeticField.Div(sum(outKnowns), sumleft)
			result = f.ArithmeticField.Sub(result, sum(rightKnowns))
			result = f.ArithmeticField.Div(result, gatesRightInputs[rightUnknowns[0]]) //divide by e
			set[rightUnknowns[0]] = true
			witness[rightUnknowns[0]] = result
			continue
		}

		// (a + b + c..) (d+e+..) = (f+x*g+..)   we solve for x
		if len(outUnknowns) == 1 {

			result := f.ArithmeticField.Mul(sum(rightKnowns), sum(leftKnowns))
			result = f.ArithmeticField.Sub(result, sum(outKnowns))
			result = f.ArithmeticField.Div(result, gatesOutputs[outUnknowns[0]]) //divide by g
			set[outUnknowns[0]] = true
			witness[outUnknowns[0]] = result
			continue
		}

	}

	return
}
