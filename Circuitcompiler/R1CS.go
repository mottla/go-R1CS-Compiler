package Circuitcompiler

import (
	"errors"
	"fmt"
	"github.com/mottla/go-R1CS-Compiler/utils"
	"math/big"
)

type R1CS struct {
	//indexMap maps each variable to its position in the witness trace
	indexMap                     map[string]int
	WitnessLength, NumberOfGates int
	//splitMap maps each variable (which is split into its bit represants at some point in the code) onto the positions
	//of the its bits in the indexMap
	splitmap map[string][]int
	L        []utils.Poly
	R        []utils.Poly
	O        []utils.Poly
	triggers []func(witness *[]*big.Int, set *[]bool, indexMap map[string]int) bool
}
type R1CSSparse struct {
	indexMap                     map[string]int
	splitmap                     map[string][]int
	WitnessLength, NumberOfGates int
	L                            []*utils.AvlTree
	R                            []*utils.AvlTree
	O                            []*utils.AvlTree
}
type R1CSsPARSETransposed struct {
	indexMap                     map[string]int
	WitnessLength, NumberOfGates int
	L                            []*utils.AvlTree
	R                            []*utils.AvlTree
	O                            []*utils.AvlTree
}
type R1CSTransposed struct {
	indexMap                     map[string]int
	WitnessLength, NumberOfGates int
	L                            []utils.Poly
	R                            []utils.Poly
	O                            []utils.Poly
}

func (er1cs *R1CSSparse) TransposeSparse() (transposed *R1CSsPARSETransposed) {
	transposed = &R1CSsPARSETransposed{}
	transposed.indexMap = er1cs.indexMap
	transposed.NumberOfGates = er1cs.NumberOfGates
	transposed.WitnessLength = er1cs.WitnessLength

	transposed.L = utils.TransposeSparse(er1cs.L, er1cs.WitnessLength)
	transposed.R = utils.TransposeSparse(er1cs.R, er1cs.WitnessLength)
	transposed.O = utils.TransposeSparse(er1cs.O, er1cs.WitnessLength)

	return
}

func (er1cs *R1CS) Transpose() (transposed *R1CSTransposed) {
	transposed = &R1CSTransposed{}
	transposed.indexMap = er1cs.indexMap
	transposed.NumberOfGates = er1cs.NumberOfGates
	transposed.WitnessLength = er1cs.WitnessLength
	transposed.L = utils.Transpose(er1cs.L)
	transposed.R = utils.Transpose(er1cs.R)
	transposed.O = utils.Transpose(er1cs.O)
	return
}

func (er1cs *R1CSTransposed) R1CSToEAP_FFT(fft *utils.FFT_PrecomputedParas, pf *utils.PolynomialField, tau *big.Int) (Ai_Tau, Ri_Tau, Oi_Tau []*big.Int) {

	lT := er1cs.L
	rT := er1cs.R
	oT := er1cs.O

	gates := fft.Size
	lagreangeBasesAtTau := make(utils.Poly, er1cs.NumberOfGates)
	zeta := pf.EvalPoly(fft.Domain, tau)

	lambda := pf.F.Div(zeta, new(big.Int).SetInt64(int64(gates)))
	rho := fft.RootOfUnitys[0]

	lagreangeBasesAtTau[0] = pf.F.Div(lambda, pf.F.Sub(tau, rho))

	for i := 1; i < er1cs.NumberOfGates; i++ {
		lambda = pf.F.Mul(lambda, fft.RootOfUnity)
		index := ((gates >> 1) + i) % gates
		//inv,_ := pf.Div(utils.Poly{bigOne},utils.Poly{fft.RootOfUnitys[ index], bigOne})
		lagreangeBasesAtTau[i] = pf.F.Div(lambda, pf.F.Add(fft.RootOfUnitys[index], tau))
	}
	Ai_Tau, Ri_Tau, Oi_Tau = make([]*big.Int, er1cs.WitnessLength), make([]*big.Int, er1cs.WitnessLength), make([]*big.Int, er1cs.WitnessLength)

	for i := 0; i < er1cs.WitnessLength; i++ {
		Ai_Tau[i] = pf.F.ScalarProduct(lT[i], lagreangeBasesAtTau)

		Ri_Tau[i] = pf.F.ScalarProduct(rT[i], lagreangeBasesAtTau)

		Oi_Tau[i] = pf.F.ScalarProduct(oT[i], lagreangeBasesAtTau)
	}
	return
}

////note that invDFFT and DFFT increase the size of the input array to the next power of two
//func (er1cs *R1CSTransposed) R1CSToEAP_FFT(fft *utils.FFT_PrecomputedParas) (lPoly, rPoly, oPoly []utils.Poly) {
//
//	pf := utils.Field.PolynomialField
//
//	lT := er1cs.L
//	rT := er1cs.R
//	oT := er1cs.O
//	gates := fft.Size
//	lagreangeBases := make([]utils.Poly, gates)
//	invGateNumber := pf.F.Inverse(new(big.Int).SetInt64(int64(gates)))
//	lambda := pf.MulScalar(fft.Domain, invGateNumber)
//	rho := fft.RootOfUnitys[gates>>1]
//	var rest utils.Poly
//	lagreangeBases[0], rest = pf.Div(lambda, utils.Poly{rho, bigOne})
//	if !utils.IsZeroArray(rest) {
//		panic("no rest")
//	}
//
//	for i := 1; i < gates; i++ {
//		lambda = pf.MulScalar(lambda, fft.RootOfUnity)
//		index := ((gates >> 1) + i) % gates
//		//inv,_ := pf.Div(utils.Poly{bigOne},utils.Poly{fft.RootOfUnitys[ index], bigOne})
//		lagreangeBases[i], _ = pf.Div(lambda, utils.Poly{fft.RootOfUnitys[index], bigOne})
//	}
//
//	for i := 0; i < er1cs.WitnessLength; i++ {
//
//		lPoly = append(lPoly, pf.AddPolynomials(pf.LinearCombine(lagreangeBases, lT[i])))
//
//		rPoly = append(rPoly, pf.AddPolynomials(pf.LinearCombine(lagreangeBases, rT[i])))
//
//		oPoly = append(oPoly, pf.AddPolynomials(pf.LinearCombine(lagreangeBases, oT[i])))
//	}
//	return
//}

func CalculateTrace(r1cs *R1CS, input []InputArgument) (witness []*big.Int, err error) {

	witness = utils.ArrayOfBigZeros(len(r1cs.indexMap))
	set := make([]bool, len(witness))

	invIndexMap := make(map[int]string)
	for k, v := range r1cs.indexMap {
		invIndexMap[v] = k
	}

	var setWitness = func(index int, value *big.Int) {
		witness[index] = utils.Field.ArithmeticField.Affine(value)
		set[index] = true

		//go over the list of self triggering funktions
		var remain []func(witness *[]*big.Int, set *[]bool, indexMap map[string]int) bool
		for i := 0; i < len(r1cs.triggers); i++ {
			//we evaluate the trigger function. if it detects, that all values are there it
			//needs to compute some values, it does so, and returns true.
			//from then on, we dont need it anymore and throw it away
			if !(r1cs.triggers[i])(&witness, &set, r1cs.indexMap) {
				remain = append(remain, r1cs.triggers[i])
			}
		}
		r1cs.triggers = remain

	}
	setWitness(0, big.NewInt(int64(1)))

	for _, v := range input {
		setWitness(r1cs.indexMap[v.identifier], v.value)
	}

	zero := big.NewInt(int64(0))

	getKnownsAndUnknowns := func(array []*big.Int) (knowns []*big.Int, unknownsAtIndices []int) {

		knowns = utils.ArrayOfBigZeros(len(array))
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
		return utils.Field.ArithmeticField.ScalarProduct(array, witness)
	}

	for i := 0; i < len(r1cs.L); i++ {
		gatesLeftInputs := r1cs.L[i]
		gatesRightInputs := r1cs.R[i]
		gatesOutputs := r1cs.O[i]

		leftKnowns, leftUnknowns := getKnownsAndUnknowns(gatesLeftInputs)
		rightKnowns, rightUnknowns := getKnownsAndUnknowns(gatesRightInputs)

		outKnowns, outUnknowns := getKnownsAndUnknowns(gatesOutputs)

		if len(leftUnknowns)+len(rightUnknowns)+len(outUnknowns) > 1 {
			return nil, errors.New(fmt.Sprintf("at gate %v:computing more then one unknown in Gate assignment is not possible", i))
		}

		// (a*x + b + c..) (d+e+..) = (F+g+..)   we solve for x
		if len(leftUnknowns) == 1 {
			sumright := sum(rightKnowns)
			sumOut := sum(outKnowns)
			if sumright.Cmp(zero) == 0 && sumOut.Cmp(zero) == 0 {
				return nil, errors.New(fmt.Sprintf("at gate %v: the equation  x*x = 0 is does not allow to determine x", i))
			}
			//result := utils.Field.ArithmeticField.Sub(sum(outKnowns), new(bn256.G1).ScalarBaseMult(sum(exponentKnowns)).X())
			result := utils.Field.ArithmeticField.Div(sumOut, sumright)
			result = utils.Field.ArithmeticField.Sub(result, sum(leftKnowns))
			result = utils.Field.ArithmeticField.Div(result, gatesLeftInputs[leftUnknowns[0]]) //divide by a
			setWitness(leftUnknowns[0], result)
			continue
		}
		// (a + b + c..) (d+e*x+..)  = (F+g+..)   we solve for x
		if len(rightUnknowns) == 1 {
			sumleft := sum(leftKnowns)
			sounOut := sum(outKnowns)
			if sumleft.Cmp(zero) == 0 && sounOut.Cmp(zero) == 0 {
				// 0 * a = 0
				// a cannot be determined
				return nil, errors.New(fmt.Sprintf("at gate %v: the equation 0 * x = 0 is does not allow to determine x", i))
			}
			//if sumleft.Cmp(zero) == 0 && sounOut.Cmp(zero) != 0 {
			//	// 0 * a = 0
			//	// a cannot be determined
			//	return nil, errors.New(fmt.Sprintf("at gate %v:the summation of Lexer inputs cannot be 0 if the unknown is in R", i))
			//}
			result := utils.Field.ArithmeticField.Div(sounOut, sumleft)
			result = utils.Field.ArithmeticField.Sub(result, sum(rightKnowns))
			result = utils.Field.ArithmeticField.Div(result, gatesRightInputs[rightUnknowns[0]]) //divide by a
			setWitness(rightUnknowns[0], result)
			continue
		}

		// (a + b + c..) (d+e+..) = (F+x*g+..)   we solve for x
		if len(outUnknowns) == 1 {

			result := utils.Field.ArithmeticField.Mul(sum(rightKnowns), sum(leftKnowns))
			result = utils.Field.ArithmeticField.Sub(result, sum(outKnowns))
			result = utils.Field.ArithmeticField.Div(result, gatesOutputs[outUnknowns[0]]) //divide by a
			setWitness(outUnknowns[0], result)
			continue
		}
		//we computed the unkown and now check if the ER1C is satisfied
		leftKnowns, leftUnknowns = getKnownsAndUnknowns(gatesLeftInputs)
		rightKnowns, rightUnknowns = getKnownsAndUnknowns(gatesRightInputs)
		outKnowns, outUnknowns = getKnownsAndUnknowns(gatesOutputs)

		if len(leftUnknowns)+len(rightUnknowns)+len(outUnknowns) != 0 {
			return nil, errors.New(fmt.Sprintf("at gate %v some unknowns remain", i))

		}
		//now check if the gate is satisfiable
		result := utils.Field.ArithmeticField.Mul(sum(rightKnowns), sum(leftKnowns))
		if result.Cmp(sum(outKnowns)) != 0 {
			return nil, errors.New(fmt.Sprintf("at equality gate %v there is unequality. %v != %v .We cannot process", i, result.String(), sum(outKnowns).String()))
		}

	}

	return
}
