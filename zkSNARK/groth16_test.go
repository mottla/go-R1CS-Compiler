package zkSNARK

import (
	"fmt"
	"github.com/mottla/go-R1CS-Compiler/circuitcompiler"
	"github.com/mottla/go-R1CS-Compiler/testPrograms"
	"github.com/mottla/go-R1CS-Compiler/utils"
	"github.com/stretchr/testify/assert"
	"math/big"
	"testing"
	"time"
)

func TestGenerateAndVerifyProof_OldArray(t *testing.T) {

	for _, test := range testPrograms.TestPrograms {
		if test.Skip {
			continue
		}

		program := circuitcompiler.Parse(test.Code, true)

		fmt.Println("Code>>")
		fmt.Println(test.Code)

		before := time.Now()
		fmt.Println("Generating CRS...")
		gates := program.Execute()

		fmt.Println("\n generating R1CS")
		r1cs := program.GatesToR1CS(gates)

		trasposedR1Cs := r1cs.Transpose()
		fmt.Println(r1cs.L)
		fmt.Println(r1cs.R)
		fmt.Println(r1cs.O)

		setup, err := GenerateTrustedSetup(program.GlobalInputCount(), trasposedR1Cs)
		fmt.Println("CRS generation time elapsed:", time.Since(before))
		assert.NoError(t, err)

		for _, io := range test.IO {
			inputs := circuitcompiler.CombineInputs(program.GetMainCircuit().Inputs, io.Inputs)
			trace, err := circuitcompiler.CalculateTrace(r1cs, inputs)

			assert.NoError(t, err)
			fmt.Println("input")
			fmt.Println(inputs)
			fmt.Println("trace")
			fmt.Println(trace)

			px := CombinePolynomials2(trace, trasposedR1Cs)

			hx, rx := utils.Field.PolynomialField.Div(px, setup.Pk.Domain)

			if !utils.IsZeroArray(rx) {
				t.Error("Px/Dx has a rest")
			}

			var bigZero = big.NewInt(int64(0))

			//Test if P(x) is indeed 0 at each gate index
			for i := 0; i < len(gates); i++ {
				if bigZero.Cmp(utils.Field.ArithmeticField.EvalPoly(px, new(big.Int).SetInt64(int64(i)))) != 0 {
					t.Error("Px must be zero ate each gate")
				}
			}
			before := time.Now()
			proof, err := GenerateProofs(program.GlobalInputCount(), &setup.Pk, trace, hx)
			fmt.Println("proof generation time elapsed:", time.Since(before))
			assert.Nil(t, err)
			before = time.Now()
			assert.True(t, VerifyProof(&setup.Pk, proof, trace[:program.GlobalInputCount()]))
			fmt.Println("verify proof time elapsed:", time.Since(before))
			fmt.Println("Proof Elements: ", proof)
		}
	}
}

func TestGenerateAndVerifyProof_Sparse(t *testing.T) {

	for _, test := range testPrograms.TestPrograms {
		if test.Skip {
			continue
		}

		program := circuitcompiler.Parse(test.Code, true)

		fmt.Println("Code>>")
		fmt.Println(test.Code)

		before := time.Now()
		fmt.Println("Generating CRS...")
		gates := program.Execute()

		fmt.Println("\n generating R1CS")
		r1cs := program.GatesToSparseR1CS(gates)

		trasposedR1Cs := r1cs.TransposeSparse()
		fmt.Println(r1cs.L)
		fmt.Println(r1cs.R)
		fmt.Println(r1cs.O)

		setup, err := GenerateTrustedSetup_sparse(program.GlobalInputCount(), trasposedR1Cs)
		fmt.Println("CRS generation time elapsed:", time.Since(before))
		assert.NoError(t, err)

		for _, io := range test.IO {
			inputs := circuitcompiler.CombineInputs(program.GetMainCircuit().Inputs, io.Inputs)
			trace, err := circuitcompiler.CalculateTrace_sparse(r1cs, inputs)

			assert.NoError(t, err)
			fmt.Println("input")
			fmt.Println(inputs)
			fmt.Println("trace")
			fmt.Println(trace)

			px := CombineSparsePolynomials(trace, trasposedR1Cs)

			hx, rx := utils.Field.ArithmeticField.DivideSparse(px, utils.NewSparseArrayFromArray(setup.Pk.Domain))

			if rx.Size() > 0 {
				t.Error("Px/Dx has a rest")
			}

			var bigZero = big.NewInt(int64(0))

			//Test if P(x) is indeed 0 at each gate index
			for i := 0; i < len(gates); i++ {
				if bigZero.Cmp(utils.Field.ArithmeticField.EvalSparsePoly(px, new(big.Int).SetInt64(int64(i)))) != 0 {
					t.Error("Px must be zero ate each gate")
				}
			}
			before := time.Now()
			proof, err := GenerateProofs_sparse(program.GlobalInputCount(), &setup.Pk, trace, hx)
			fmt.Println("proof generation time elapsed:", time.Since(before))
			assert.Nil(t, err)
			before = time.Now()
			assert.True(t, VerifyProof(&setup.Pk, proof, trace[:program.GlobalInputCount()]))
			fmt.Println("verify proof time elapsed:", time.Since(before))
			fmt.Println("Proof Elements: ", proof)
		}
	}
}

func TestGenerateAndVerifyProof_FFT(t *testing.T) {

	for _, test := range testPrograms.TestPrograms {
		if test.Skip {
			continue
		}

		program := circuitcompiler.Parse(test.Code, true)

		fmt.Println("Code>>")
		fmt.Println(test.Code)

		before := time.Now()
		fmt.Println("Generating CRS...")
		gates := program.Execute()

		fmt.Println("\n generating R1CS")
		r1cs := program.GatesToR1CS(gates)

		trasposedR1Cs := r1cs.Transpose()
		fmt.Println(r1cs.L)
		fmt.Println(r1cs.R)
		fmt.Println(r1cs.O)

		setup, err := GenerateTrustedSetup(program.GlobalInputCount(), trasposedR1Cs)
		fmt.Println("CRS generation time elapsed:", time.Since(before))
		assert.NoError(t, err)

		for _, io := range test.IO {
			inputs := circuitcompiler.CombineInputs(program.GetMainCircuit().Inputs, io.Inputs)
			trace, err := circuitcompiler.CalculateTrace(r1cs, inputs)

			assert.NoError(t, err)
			fmt.Println("input")
			fmt.Println(inputs)
			fmt.Println("trace")
			fmt.Println(trace)

			px := CombinePolynomials2(trace, trasposedR1Cs)

			hx, rx := utils.Field.PolynomialField.Div(px, setup.Pk.Domain)

			if !utils.IsZeroArray(rx) {
				t.Error("Px/Dx has a rest")
			}

			var bigZero = big.NewInt(int64(0))

			//Test if P(x) is indeed 0 at each gate index
			for i := 0; i < len(gates); i++ {
				if bigZero.Cmp(utils.Field.ArithmeticField.EvalPoly(px, new(big.Int).SetInt64(int64(i)))) != 0 {
					t.Error("Px must be zero ate each gate")
				}
			}
			before := time.Now()
			proof, err := GenerateProofs(program.GlobalInputCount(), &setup.Pk, trace, hx)
			fmt.Println("proof generation time elapsed:", time.Since(before))
			assert.Nil(t, err)
			before = time.Now()
			assert.True(t, VerifyProof(&setup.Pk, proof, trace[:program.GlobalInputCount()]))
			fmt.Println("verify proof time elapsed:", time.Since(before))
			fmt.Println("Proof Elements: ", proof)
		}
	}
}

//func TestGenerateAndVerifyProof_sparse(t *testing.T) {
//
//	for _, test := range testPrograms.TestPrograms {
//		if test.Skip {
//			continue
//		}
//
//		program := circuitcompiler.Parse(test.Code, true)
//
//		fmt.Println("Code>>")
//		fmt.Println(test.Code)
//
//		before := time.Now()
//		fmt.Println("Generating CRS...")
//		gates := program.Execute()
//
//		fmt.Println("\n generating R1CS")
//		r1cs := program.GatesToSparseR1CS(gates, true)
//
//		//r1csSparse := program.GatesToSparseR1CS(gates, true)
//		//transposedR1csSparse := r1csSparse.TransposeSparse()
//		trasposedR1Cs := r1cs.TransposeSparse()
//		fmt.Println(r1cs.L)
//		fmt.Println(r1cs.R)
//		fmt.Println(r1cs.O)
//
//		//fmt.Println(l)
//		//fmt.Println(r)
//		//fmt.Println(e)
//		//fmt.Println(o)
//
//		setup, err := GenerateTrustedSetupSparse(program.GlobalInputCount(), trasposedR1Cs)
//		fmt.Println("CRS generation time elapsed:", time.Since(before))
//		assert.NoError(t, err)
//
//		for _, io := range test.IO {
//			inputs := circuitcompiler.CombineInputs(program.PublicInputs, io.Inputs)
//			w, err := circuitcompiler.CalculateTrace_sparse(r1cs, inputs)
//
//			assert.NoError(t, err)
//			//wsparse, werr := circuitcompiler.CalculateTrace_sparse(r1csSparse, inputs)
//			//assert.NoError(t, werr)
//
//			fmt.Println("input")
//			fmt.Println(inputs)
//			fmt.Println("witness")
//			fmt.Println(w)
//			//fmt.Println(wsparse)
//			//assert.Equal(t, io.result, w[len(w)-1])
//			// CombineSparsePolynomials(program.Fields, w, transposedR1csSparse)
//			px := CombineSparsePolynomials(w, trasposedR1Cs)
//			//mf3,px3 := CombinePolynomials3(program.Fields,w,trasposedR1Cs)
//			//mSparse,pSparse := CombineSparsePolynomials(program.Fields,wSparse,r1csSparse)
//
//			//assert.Equal(t, px, px3)
//			//assert.Equal(t, mf2, mf3)
//			var bigZero = big.NewInt(int64(0))
//
//			//Test if P(x) is indeed 0 at each gate index
//			for i := 0; i < len(gates); i++ {
//				if bigZero.Cmp(utils.Field.ArithmeticField.EvalSparsePoly(px, new(big.Int).SetInt64(int64(i)))) != 0 {
//					t.Error(fmt.Sprintf("Px must be zero ae gate %v", i))
//				}
//			}
//
//			before := time.Now()
//			proof, err := GenerateProof_Sparse(program.GlobalInputCount(), &setup.Pk, w, px)
//
//			fmt.Println("proof generation time elapsed:", time.Since(before))
//			assert.Nil(t, err)
//			before = time.Now()
//			assert.True(t, VerifyProof(&setup.Pk, proof, w[:program.GlobalInputCount()], true))
//			fmt.Println("verify proof time elapsed:", time.Since(before))
//			fmt.Println("Proof Elements: ", proof)
//		}
//
//	}
//
//}

//func TestGenerateAndVerifyProof_both(t *testing.T) {
//
//	for _, test := range testPrograms.TestPrograms {
//		if test.Skip {
//			continue
//		}
//
//		program := circuitcompiler.Parse(test.Code, true)
//
//		fmt.Println("Code>>")
//		fmt.Println(test.Code)
//
//		fmt.Println("Translating Program...")
//		gates := program.Execute()
//		r1cs := program.GatesToR1CS(gates, false)
//		fmt.Printf("number of gates %v, witness length %v \n ", r1cs.NumberOfGates, r1cs.WitnessLength)
//		//fmt.Println(r1cs.L)
//		//fmt.Println(r1cs.R)
//		//fmt.Println(r1cs.E)
//		//fmt.Println(r1cs.O)
//		trasposedR1Cs := r1cs.Transpose()
//
//		before := time.Now()
//		setup, err := GenerateTrustedSetup(program.GlobalInputCount(), trasposedR1Cs)
//		fmt.Println("classic CRS generation time elapsed:", time.Since(before))
//		assert.NoError(t, err)
//
//		r1csSparse := program.GatesToSparseR1CS(gates, false)
//		transposedR1csSparse := r1csSparse.TransposeSparse()
//
//		before = time.Now()
//		setupSparse, erro := GenerateTrustedSetupSparse(program.GlobalInputCount(), transposedR1csSparse)
//		fmt.Println("sparse CRS generation time elapsed:", time.Since(before))
//		assert.NoError(t, erro)
//
//		for _, io := range test.IO {
//			inputs := circuitcompiler.CombineInputs(program.PublicInputs, io.Inputs)
//			w, err := circuitcompiler.CalculateTrace(r1cs, inputs)
//			assert.NoError(t, err)
//			wsparse, werr := circuitcompiler.CalculateTrace_sparse(r1csSparse, inputs)
//			assert.NoError(t, werr)
//
//			fmt.Println("input")
//			fmt.Println(inputs)
//			fmt.Println("witness")
//			fmt.Println(w)
//			//fmt.Println(wsparse)
//			assert.Equal(t, wsparse[:len(wsparse)-1], w[:len(wsparse)-1])
//			// CombineSparsePolynomials(program.Fields, w, transposedR1csSparse)
//			px := CombinePolynomials2(w, trasposedR1Cs)
//
//			//mf3,px3 := CombinePolynomials3(program.Fields,w,trasposedR1Cs)
//			pSparse := CombineSparsePolynomials(wsparse, transposedR1csSparse)
//			fmt.Println("PX es")
//			assert.Equal(t, px, pSparse.ToArray(len(px)))
//
//			var bigZero = big.NewInt(int64(0))
//
//			//Test if P(x) is indeed 0 at each gate index
//			for i := 0; i < r1cs.NumberOfGates; i++ {
//				if bigZero.Cmp(utils.Field.ArithmeticField.EvalPoly(px, new(big.Int).SetInt64(int64(i)))) != 0 {
//					t.Error("Px must be zero ate each gate")
//				}
//				if bigZero.Cmp(utils.Field.ArithmeticField.EvalSparsePoly(pSparse, new(big.Int).SetInt64(int64(i)))) != 0 {
//					t.Error(fmt.Sprintf("Px must be zero ae gate %v", i))
//				}
//				at, _ := utils.Field.ArithmeticField.Rand()
//				if utils.Field.ArithmeticField.EvalPoly(px, at).Cmp(utils.Field.ArithmeticField.EvalSparsePoly(pSparse, at)) != 0 {
//					t.Error("unequal ")
//				}
//			}
//
//			before := time.Now()
//			proof, err := GenerateProofs(program.GlobalInputCount(), &setup.Pk, w, px)
//			fmt.Println("proof classic generation time elapsed:", time.Since(before))
//			assert.Nil(t, err)
//
//			before = time.Now()
//			proofSpares, err := GenerateProof_Sparse(program.GlobalInputCount(), &setupSparse.Pk, wsparse, pSparse)
//			fmt.Println("proof spare generation time elapsed:", time.Since(before))
//			assert.Nil(t, err)
//
//			before = time.Now()
//			assert.True(t, VerifyProof(&setup.Pk, proof, w[:program.GlobalInputCount()], true))
//			fmt.Println("verify classic proof time elapsed:", time.Since(before))
//			fmt.Println("Proof Elements: ", proof)
//
//			before = time.Now()
//			assert.True(t, VerifyProof(&setupSparse.Pk, proofSpares, wsparse[:program.GlobalInputCount()], true))
//			fmt.Println("verify sparse proof time elapsed:", time.Since(before))
//			fmt.Println("Proof Elements: ", proofSpares)
//		}
//
//	}
//
//}
