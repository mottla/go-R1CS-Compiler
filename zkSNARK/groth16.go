package zkSNARK

import (
	"bytes"
	"fmt"
	"github.com/mottla/go-R1CS-Compiler/Circuitcompiler"
	bn256 "github.com/mottla/go-R1CS-Compiler/pairing"
	"github.com/mottla/go-R1CS-Compiler/utils"
	"math/big"
	//"math/bits"
	"time"
)

type Pk struct {
	// Proving Key

	Domain       []*big.Int
	eGHalphaBeta *bn256.GT

	G1 struct {
		RLO_DivDelta         []*bn256.G1 // {( αRi(x)+βLi(x)+Oi(x) ) / δ } from l+1 to m
		Lix                  []*bn256.G1
		Rix                  []*bn256.G1
		Alpha                *bn256.G1
		Beta                 *bn256.G1
		Delta                *bn256.G1
		RLO_DivGamma         []*bn256.G1 // {( αRi(x)+βLi(x)+Oi(x) ) / γ } from 0 to l
		PowersX              []*bn256.G1
		PowersX_Domain_Delta []*bn256.G1 // { G1^( (x^i D(x)) / delta} from 0 to n-1

	}
	G2 struct {
		Beta    *bn256.G2
		Gamma   *bn256.G2
		Delta   *bn256.G2
		PowersX []*bn256.G2
		Rix     []*bn256.G2
	}
}

//type Vk struct {
//	IC [][3]*big.Int
//	G1 struct {
//		Alpha [3]*big.Int
//	}
//	G2 struct {
//		Beta  [3][2]*big.Int
//		Gamma [3][2]*big.Int
//		Delta [3][2]*big.Int
//	}
//}

// Setup is the data structure holding the Trusted Setup data. The Setup.Toxic sub struct must be destroyed after the GenerateTrustedSetup function is completed
type Setup struct {
	Toxic struct {
		x      *big.Int // trusted setup secret
		Kalpha *big.Int
		Kbeta  *big.Int
		Kgamma *big.Int
		Kdelta *big.Int
	}
	fftParas *utils.FFT_PrecomputedParas
	// public
	Pk Pk
	//Vk Vk
}

func NewSetup(witnessLength, publicInputs int) *Setup {
	s := new(Setup)
	s.Pk.G2.Rix = make([]*bn256.G2, witnessLength)
	s.Pk.G1.Rix = make([]*bn256.G1, witnessLength)
	s.Pk.G1.Lix = make([]*bn256.G1, witnessLength)
	s.Pk.G1.RLO_DivGamma = make([]*bn256.G1, publicInputs)
	s.Pk.G1.RLO_DivDelta = make([]*bn256.G1, witnessLength-publicInputs)
	return s
}

// Proof contains the parameters to proof the zkSNARK
type Proof struct {
	PiA *bn256.G1
	PiB *bn256.G2
	PiC *bn256.G1
}

// CombinePolynomials combine the given polynomials arrays into one, also returns the P(x)
func CombinePolynomials_Efficient(ft *utils.FFT_PrecomputedParas, witness []*big.Int, TransposedR1cs *Circuitcompiler.R1CSTransposed) (h []*big.Int) {

	pf := utils.Field.PolynomialField

	L := pf.AddPolynomials(pf.LinearCombine(TransposedR1cs.L, witness))
	R := pf.AddPolynomials(pf.LinearCombine(TransposedR1cs.R, witness))
	O := pf.AddPolynomials(pf.LinearCombine(TransposedR1cs.O, witness))

	//compute the Ng-1 coefficients
	L = pf.InvDFFT(L, nil)
	R = pf.InvDFFT(R, nil)
	O = pf.InvDFFT(O, nil)

	//evalueate the polynomials at the coset of roots

	baseShift := new(big.Int).SetInt64(3)
	L = pf.DFFT(L, baseShift)
	R = pf.DFFT(R, baseShift)
	O = pf.DFFT(O, baseShift)

	h = pf.PointwiseMultiplication(L, R)
	h = pf.PointwiseSub(h, O)

	Zs := pf.F.Inverse(pf.F.EvalPoly(ft.Domain, baseShift)) // domain poly at shifted roots of unity is everywhere the same
	h = pf.MulScalar(h, Zs)

	return pf.InvDFFT(h, baseShift)
}

func g1ScalarBaseMultiply(in *big.Int) *bn256.G1 {
	return new(bn256.G1).ScalarBaseMult(in)
}
func g2ScalarBaseMultiply(in *big.Int) *bn256.G2 {
	return new(bn256.G2).ScalarBaseMult(in)
}

// GenerateTrustedSetup generates the Trusted Setup from a compiled function. The Setup.Toxic sub data structure must be destroyed
func GenerateTrustedSetup_FFT(publicinputs int, r1cs *Circuitcompiler.R1CSTransposed) (setup *Setup, e error) {
	fields := utils.Field
	gates, witnessLength := utils.NextPowerOfTwo(r1cs.NumberOfGates), r1cs.WitnessLength
	setup = NewSetup(witnessLength, publicinputs)
	var err error

	FFT_Paras := fields.PolynomialField.PrepareFFT(gates)
	setup.fftParas = FFT_Paras
	fmt.Println("start setup. Performing interpolation...")
	before := time.Now()
	// generate trapdoor
	setup.Toxic.x, err = fields.ArithmeticField.Rand()
	if err != nil {
		panic("random failed")
	}
	Li, Ri, Oi := r1cs.R1CSToEAP_FFT(FFT_Paras, &fields.PolynomialField, setup.Toxic.x)

	fmt.Println("lagrange interpolation over the roots of unity done in ", time.Since(before))
	if len(Li) != len(Ri) || len(Ri) != len(Oi) {
		panic("amount of polynimials  missmatch")
	}
	if publicinputs >= len(Li) {
		panic("to moany public parameters")
	}

	fmt.Println("Performing Scalar Point Multiplications...")
	before = time.Now()

	// generate trapdoor
	setup.Toxic.Kalpha, err = fields.ArithmeticField.Rand()
	if err != nil {
		panic("random failed")
	}
	setup.Toxic.Kbeta, err = fields.ArithmeticField.Rand()
	if err != nil {
		panic("random failed")
	}
	setup.Toxic.Kgamma, err = fields.ArithmeticField.Rand()
	if err != nil {
		panic("random failed")
	}
	setup.Toxic.Kdelta, err = fields.ArithmeticField.Rand()
	if err != nil {
		panic("random failed")
	}

	//the domain poly over the roots of unity is realy simple
	Domain := FFT_Paras.Domain

	setup.Pk.Domain = FFT_Paras.Domain

	Dx := fields.ArithmeticField.EvalPoly(Domain, setup.Toxic.x)
	invDelta := fields.ArithmeticField.Inverse(setup.Toxic.Kdelta)
	invgamma := fields.ArithmeticField.Inverse(setup.Toxic.Kgamma)
	Dx_div_delta := fields.ArithmeticField.Mul(invDelta, Dx)

	// encrypt x values with curve generators
	// x^i times D(x) divided by delta
	var powersXDomaindivDelta = []*bn256.G1{g1ScalarBaseMultiply(Dx_div_delta)}
	var powersX_onG = []*bn256.G1{g1ScalarBaseMultiply(big.NewInt(1))}
	var powersX_onH = []*bn256.G2{g2ScalarBaseMultiply(big.NewInt(1))}

	//G^{x^i}
	xi := new(big.Int).Set(setup.Toxic.x)
	for i := 1; i < gates; i++ {

		if i < gates-1 {
			powersXDomaindivDelta = append(powersXDomaindivDelta, g1ScalarBaseMultiply(fields.ArithmeticField.Mul(xi, Dx_div_delta)))
		}

		powersX_onG = append(powersX_onG, g1ScalarBaseMultiply(xi))
		powersX_onH = append(powersX_onH, g2ScalarBaseMultiply(xi))
		// x^i -> x^{i+1}
		xi = fields.ArithmeticField.Mul(xi, setup.Toxic.x)
	}

	setup.Pk.G1.PowersX = powersX_onG
	setup.Pk.G2.PowersX = powersX_onH
	setup.Pk.G1.PowersX_Domain_Delta = powersXDomaindivDelta

	setup.Pk.G1.Alpha = g1ScalarBaseMultiply(setup.Toxic.Kalpha)
	setup.Pk.G1.Beta = g1ScalarBaseMultiply(setup.Toxic.Kbeta)
	setup.Pk.G1.Delta = g1ScalarBaseMultiply(setup.Toxic.Kdelta)

	setup.Pk.G2.Beta = g2ScalarBaseMultiply(setup.Toxic.Kbeta)
	setup.Pk.G2.Gamma = g2ScalarBaseMultiply(setup.Toxic.Kgamma)
	setup.Pk.G2.Delta = g2ScalarBaseMultiply(setup.Toxic.Kdelta)

	var wk = func(start, end int) {
		for i := start; i < end; i++ {
			// Li(x)

			// g^{Li(x)}
			setup.Pk.G1.Lix[i] = g1ScalarBaseMultiply(Li[i])

			// Ri(x)

			// h^{Ri(x)}
			setup.Pk.G2.Rix[i] = g2ScalarBaseMultiply(Ri[i])
			// g^{Ri(x)}
			setup.Pk.G1.Rix[i] = g1ScalarBaseMultiply(Ri[i])

			// Oi(x)

			//{alpha * Ri(x) + beta * Li(x) + Oi(x) }
			ter := fields.ArithmeticField.Mul(setup.Toxic.Kalpha, Ri[i])
			ter = fields.ArithmeticField.Add(ter, fields.ArithmeticField.Mul(setup.Toxic.Kbeta, Li[i]))
			ter = fields.ArithmeticField.Add(ter, Oi[i])

			if i < publicinputs {
				ter = fields.ArithmeticField.Mul(invgamma, ter)
				//g^ {alpha * Ri(x) + beta * Li(x) + Oi(x) }/ gamma
				setup.Pk.G1.RLO_DivGamma[i] = g1ScalarBaseMultiply(ter)
			} else {
				ter = fields.ArithmeticField.Mul(invDelta, ter)
				//g^ {alpha * Ri(x) + beta * Li(x) + Oi(x) }/ delta
				setup.Pk.G1.RLO_DivDelta[i-publicinputs] = g1ScalarBaseMultiply(ter)
			}
		}
	}
	utils.Parallelize(witnessLength, wk)

	//precompute e(g^alpha,h^beta)
	setup.Pk.eGHalphaBeta = bn256.Pair(setup.Pk.G1.Alpha, setup.Pk.G2.Beta)
	fmt.Println("Scalar Point Multiplications done in ", time.Since(before))
	return setup, nil
}

// GenerateProofs generates all the parameters to proof the zkSNARK from the function, Setup and the Witness
func GenerateProofs(publicInputs int, provingKey *Pk, witnessTrace []*big.Int, hx []*big.Int) (*Proof, error) {

	var proof = new(Proof)
	//load the field divened over the curve bn256 order
	fields := utils.Field

	//generate random r,s
	r, err := fields.ArithmeticField.Rand()
	if err != nil {
		panic("random failed")
	}
	s, err := fields.ArithmeticField.Rand()
	if err != nil {
		panic("random failed")
	}

	//first element of A, g^L0(x)*a0

	proof.PiA = new(bn256.G1).ScalarMult(provingKey.G1.Lix[0], witnessTrace[0])

	//first element of B, h^L0(x)*a0
	proof.PiB = new(bn256.G2).ScalarMult(provingKey.G2.Rix[0], witnessTrace[0])

	//element of C, g^R0(x)*a0
	tmpR := new(bn256.G1).ScalarMult(provingKey.G1.Rix[0], witnessTrace[0])

	proof.PiC = new(bn256.G1).ScalarMult(provingKey.G1.RLO_DivDelta[0], witnessTrace[publicInputs])

	var QxDx_div_delta = new(bn256.G1).ScalarMult(provingKey.G1.PowersX_Domain_Delta[0], hx[0])

	for i := 1; i < len(provingKey.G1.PowersX_Domain_Delta); i++ {
		tmp := new(bn256.G1).ScalarMult(provingKey.G1.PowersX_Domain_Delta[i], hx[i])
		QxDx_div_delta.Add(QxDx_div_delta, tmp)
	}

	for i := 1; i < len(witnessTrace); i++ {
		//proof element A
		proof.PiA.Add(proof.PiA, new(bn256.G1).ScalarMult(provingKey.G1.Lix[i], witnessTrace[i]))

		//proof element C right part
		tmpR.Add(tmpR, new(bn256.G1).ScalarMult(provingKey.G1.Rix[i], witnessTrace[i]))

		//proof element B
		proof.PiB.Add(proof.PiB, new(bn256.G2).ScalarMult(provingKey.G2.Rix[i], witnessTrace[i]))

		if i > publicInputs {
			//proof element C
			proof.PiC.Add(proof.PiC, new(bn256.G1).ScalarMult(provingKey.G1.RLO_DivDelta[i-publicInputs], witnessTrace[i]))
		}

	}
	//add the alpha and r*delta therm to proof element A
	proof.PiA.Add(proof.PiA, provingKey.G1.Alpha)
	c1 := proof.PiA.Copy()
	proof.PiA.Add(proof.PiA, new(bn256.G1).ScalarMult(provingKey.G1.Delta, r))

	//add the beta and s*delta therm to proof element A
	proof.PiB.Add(proof.PiB, provingKey.G2.Beta)
	proof.PiB.Add(proof.PiB, new(bn256.G2).ScalarMult(provingKey.G2.Delta, s))

	//add the Q(x)D(x)/delta therm to the proof element C
	proof.PiC.Add(proof.PiC, QxDx_div_delta)

	proof.PiC.Add(proof.PiC, new(bn256.G1).ScalarMult(c1, s))
	proof.PiC.Add(proof.PiC, new(bn256.G1).ScalarMult(new(bn256.G1).Add(tmpR, provingKey.G1.Beta), r))

	proof.PiC.Add(proof.PiC, new(bn256.G1).ScalarMult(provingKey.G1.Delta, fields.ArithmeticField.Mul(r, s)))

	return proof, nil
}

// VerifyProof verifies over the BN256 the Pairings of the Proof
func VerifyProof(pk *Pk, proof *Proof, publicSignals []*big.Int) bool {
	//note that the trivial cases should be rejected to

	if len(publicSignals) != len(pk.G1.RLO_DivGamma) {
		fmt.Println("❌ groth16 verification not passed. Signal length wrong")
		return false
	}

	icPubl := new(bn256.G1).ScalarMult(pk.G1.RLO_DivGamma[0], publicSignals[0])
	for i := 1; i < len(publicSignals); i++ {
		icPubl.Add(icPubl, new(bn256.G1).ScalarMult(pk.G1.RLO_DivGamma[i], publicSignals[i]))
	}

	a := bn256.Pair(proof.PiA, proof.PiB)

	b := pk.eGHalphaBeta
	c := bn256.Pair(icPubl, pk.G2.Gamma)
	d := bn256.Pair(proof.PiC, pk.G2.Delta)

	bc := new(bn256.GT).Add(b, c)
	bcd := new(bn256.GT).Add(bc, d)

	if bytes.Equal(a.Marshal(), bcd.Marshal()) {
		fmt.Println("✓ groth16 verification passed")
		return true
	}
	fmt.Println("❌ groth16 verification not passed.")
	return false

}
