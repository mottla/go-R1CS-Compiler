package utils

import (
	"bytes"
	"fmt"
	bn256 "github.com/mottla/go-R1CS-Compiler/pairing"
	"github.com/stretchr/testify/assert"
	"math/big"
	"math/bits"
	"testing"
	"time"
)

func TestTranspose(t *testing.T) {
	fmt.Print(bits.Len(uint(8)))
	b0 := big.NewInt(int64(0))
	b1 := big.NewInt(int64(1))
	bFive := big.NewInt(int64(5))
	a := []Poly{
		{b0, b1, b0, b0, b0, b0},
		{b0, b0, b0, b1, b0, b0},
		{b0, b1, b0, b0, b1, b0},
		{bFive, b0, b0, b0, b0, b1},
	}
	aT := Transpose(a)
	assert.Equal(t, aT, []Poly{
		{b0, b0, b0, bFive},
		{b1, b0, b1, b0},
		{b0, b0, b0, b0},
		{b0, b1, b0, b0},
		{b0, b0, b1, b0},
		{b0, b0, b0, b1},
	})
}

func neg(a *big.Int) *big.Int {
	return new(big.Int).Neg(a)
}

func TestPol(t *testing.T) {
	b0 := big.NewInt(int64(0))
	b1 := big.NewInt(int64(1))
	b2 := big.NewInt(int64(2))
	b3 := big.NewInt(int64(3))
	b4 := big.NewInt(int64(4))
	b5 := big.NewInt(int64(5))
	b6 := big.NewInt(int64(6))
	b16 := big.NewInt(int64(16))

	a := Poly{b1, b0, b5}
	b := Poly{b3, b0, b1}

	// new Finite Field
	r, ok := new(big.Int).SetString("21888242871839275222246405745257275088548364400416034343698204186575808495617", 10)
	assert.True(nil, ok)
	f := NewFiniteField(r)

	// new Polynomial Field
	pf := NewPolynomialField(f)

	// polynomial multiplication
	o := pf.MulNaive(a, b)
	assert.Equal(t, o, Poly{b3, b0, b16, b0, b5})

	// polynomial division
	quo, rem := pf.Div(a, b)
	assert.Equal(t, quo[0].Int64(), int64(5))
	assert.Equal(t, new(big.Int).Sub(rem[0], r).Int64(), int64(-14)) // check the rem result without modulo

	c := Poly{neg(b4), b0, neg(b2), b1}
	d := Poly{neg(b3), b1}
	quo2, rem2 := pf.Div(c, d)
	assert.Equal(t, quo2, Poly{b3, b1, b1})
	assert.Equal(t, rem2[0].Int64(), int64(5))

	// polynomial addition
	o = pf.Add(a, b)
	assert.Equal(t, o, Poly{b4, b0, b6})

	// polynomial subtraction
	o1 := pf.Sub(a, b)
	o2 := pf.Sub(b, a)
	o = pf.Add(o1, o2)
	assert.True(t, bytes.Equal(b0.Bytes(), o[0].Bytes()))
	assert.True(t, bytes.Equal(b0.Bytes(), o[1].Bytes()))
	assert.True(t, bytes.Equal(b0.Bytes(), o[2].Bytes()))

	c = Poly{b5, b6, b1}
	d = Poly{b1, b3}
	o = pf.Sub(c, d)
	assert.Equal(t, o, Poly{b4, b3, b1})

	// NewPolZeroAt
	o = pf.NewPolZeroAt(3, 4, b4)
	assert.Equal(t, f.EvalPoly(o, big.NewInt(3)), b4)
	o = pf.NewPolZeroAt(2, 4, b3)
	assert.Equal(t, f.EvalPoly(o, big.NewInt(2)), b3)
}

func TestLagrangeInterpolation(t *testing.T) {
	// new Finite Field
	var Npoints = int64(100)
	r := new(big.Int).Set(bn256.P)
	f := NewFiniteField(r)

	// new Polynomial Field
	pf := NewPolynomialField(f)
	pf.PrecomputeLagrange(int(Npoints))
	var err error

	Xpoints := make([]*big.Int, Npoints)
	for i := int64(0); i < Npoints; i++ {
		Xpoints[i] = new(big.Int).SetInt64(i)

	}

	for runs := 0; runs < 7; runs++ {
		Ypoints := make([]*big.Int, Npoints)

		for i := int64(0); i < Npoints; i++ {
			Ypoints[i], err = f.Rand()
			assert.Nil(t, err)
		}
		tree := NewSparseArrayFromArray(Ypoints)

		alpha := pf.LagrangeInterpolation(Ypoints)
		interpolatedSparse := pf.InterpolateSparseArray(tree, int(Npoints))
		for i := int64(0); i < Npoints; i++ {
			if f.EvalPoly(alpha, Xpoints[i]).Cmp(Ypoints[i]) != 0 {
				t.Fail()
				fmt.Println("fail")
			}
			if v := f.EvalSparsePoly(interpolatedSparse, Xpoints[i]); v.Cmp(Ypoints[i]) != 0 {
				t.Fail()
				fmt.Printf("fail sparse at %v", Xpoints[i])
			}
		}
	}
}

func TestPolynomialField_DFFT(t *testing.T) {
	var Npoints = int64(1 << 6)

	r := new(big.Int).Set(bn256.Order)
	f := NewFiniteField(r)

	// new Polynomial Field
	pf := NewPolynomialField(f)

	Xpoints := make([]*big.Int, Npoints)
	for i := int64(0); i < Npoints; i++ {
		Xpoints[i], _ = f.Rand()
		//Xpoints[i] = new(big.Int).SetInt64(i)
	}

	offset := new(big.Int).SetInt64(3)
	//offset, _ := f.Rand()

	fmt.Println("x points")
	//for i := int64(0); i < Npoints; i++ {
	//	fmt.Println(Xpoints[i])
	//}

	coefficientsAtRootOfUnity := pf.InvDFFT(Xpoints, offset)
	para, _ := pf.fftPras[NextPowerOfTwo(len(Xpoints))]
	fmt.Println("x points to coefficients")
	for i := int64(0); i < Npoints; i++ {
		omega := pf.F.ExpInt(para.RootOfUnity, i)
		eval := pf.F.EvalPoly(coefficientsAtRootOfUnity, pf.F.Mul(omega, offset))
		if Xpoints[i].Cmp(eval) != 0 {
			t.Error(fmt.Sprintf("value at root of unity %v", i))
		}
	}

	//res2 := pf.DFFT(offset,Xpoints)
	resOffset2 := pf.DFFT(coefficientsAtRootOfUnity, offset)
	fmt.Println("coefficients to evaluation")
	//for i := int64(0); i < Npoints; i++ {
	//	fmt.Println(resOffset2[i])
	//}

	for i := int64(0); i < Npoints; i++ {
		if Xpoints[i].Cmp(resOffset2[i]) != 0 {
			t.Error("FFT -> invFFT -> FFT -> invFFT failed")
		}
	}

}
func TestPolynomialField_FFT_Multiplication(t *testing.T) {
	var Npoints = int64(1 << 3)

	r := new(big.Int).Set(bn256.Order)
	f := NewFiniteField(r)

	// new Polynomial Field
	pf := NewPolynomialField(f)

	Xpoints := make([]*big.Int, Npoints)
	Ypoints := make([]*big.Int, Npoints)

	for i := int64(0); i < Npoints; i++ {
		Xpoints[i], _ = f.Rand()
		Ypoints[i], _ = f.Rand()
		//Xpoints[i] = new(big.Int).SetInt64(i)
	}
	Zpoints := pf.PointwiseMultiplication(Xpoints, Ypoints)

	offset := new(big.Int).SetInt64(2)
	//offset, _ := f.Rand()

	fmt.Println("x points")
	//for i := int64(0); i < Npoints; i++ {
	//	fmt.Println(Xpoints[i])
	//}

	coeffL := pf.InvDFFT(Xpoints, nil)
	coeffR := pf.InvDFFT(Ypoints, nil)
	coeffO := pf.InvDFFT(Zpoints, nil)
	para, _ := pf.fftPras[NextPowerOfTwo(len(Xpoints))]
	LLRR := pf.MulNaive(coeffL, coeffR)
	for i := uint(0); i < uint(Npoints); i++ {
		if pf.F.evalSparsePoly(LLRR, para.RootOfUnitys[i]).Cmp(pf.F.EvalPoly(coeffO, para.RootOfUnitys[i])) != 0 {
			t.Error(fmt.Sprintf("LR(w) != O(w) at w^, i= %v", i))
		}
	}
	fmt.Printf("\n degree of L %v", coeffL.Degree())
	valuesAtRootsL := pf.DFFT(coeffL, offset)
	valuesAtRootsR := pf.DFFT(coeffR, offset)
	valuesAtRootsO := pf.DFFT(coeffO, offset)

	Zs := pf.F.Inverse(pf.F.EvalPoly(para.Domain, offset))

	Hpoints := make([]*big.Int, Npoints)
	Hpoints = pf.PointwiseSub(pf.PointwiseMultiplication(valuesAtRootsL, valuesAtRootsR), valuesAtRootsO)
	Hpoints = pf.MulScalar(Hpoints, Zs)
	//Hpoints = Hpoints[:len(Hpoints)-3]
	Hcoef := pf.InvDFFT(Hpoints, offset)

	fmt.Printf("\n degree of H %v", Hcoef.Degree())
	fmt.Printf("\n degree of Domain %v", para.Domain.Degree())

	HD := pf.MulNaive(Hcoef, para.Domain)
	fmt.Printf("\n degree of H*D %v", HD.Degree())
	lr := pf.MulNaive(coeffL, coeffR)
	fmt.Printf("\n degree of LR %v", lr.Degree())
	R := pf.Sub(lr, coeffO)
	HH, H := pf.Div(R, para.Domain)
	if !IsZeroArray(H) {
		t.Error("no rest allowed")
	}
	fmt.Printf("\n degree of LR-O/ D %v", HH.Degree())
	fmt.Printf("\n degree of LR-O %v", R.Degree())
	if !IsZeroArray(pf.Sub(HD, R)) {
		t.Error("not zero")
	}

	for i := uint(0); i < uint(Npoints); i++ {

		if pf.F.EvalPoly(HH, para.RootOfUnitys[i]).Cmp(pf.F.EvalPoly(Hcoef, para.RootOfUnitys[i])) != 0 {
			t.Error(fmt.Sprintf("H*D not zero at root of unity %v", i))
		}

	}

}
func TestCombine(t *testing.T) {
	// new Finite Field
	var Npoints = int64(100)
	r := new(big.Int).Set(bn256.Order)
	f := NewFiniteField(r)

	// new Polynomial Field
	pf := NewPolynomialField(f)
	pf.PrecomputeLagrange(int(Npoints))
	pf.PrecomputeLagrange((int(Npoints) * 2) - 1)
	var err error

	Xpoints := make([]*big.Int, Npoints)
	for i := int64(0); i < Npoints; i++ {
		Xpoints[i] = new(big.Int).SetInt64(i)

	}

	for runs := 0; runs < 3; runs++ {
		Ypoints := make([]*big.Int, Npoints)
		Zpoints := make([]*big.Int, Npoints)
		for i := int64(0); i < Npoints; i++ {
			Ypoints[i], err = f.Rand()
			assert.Nil(t, err)
			Zpoints[i], err = f.Rand()
			assert.Nil(t, err)
		}
		l1 := pf.LagrangeInterpolation(Ypoints)
		l2 := pf.LagrangeInterpolation(Zpoints)
		sum := pf.Add(l1, l2)
		sum2 := pf.Add(Ypoints, Zpoints)
		sum2In := pf.LagrangeInterpolation(sum2)
		if !BigArraysEqual(sum, sum2In) {
			t.Error("adding and then interpolating must yield the same results as interpolating then adding")
		}

		mulNaive := pf.MulNaive(l1, l2)
		mul2 := pf.MulNaive(Ypoints, Zpoints)

		mul2Iterp := pf.LagrangeInterpolation(mul2)
		if BigArraysEqual(mulNaive, mul2Iterp) {
			t.Error("should be unequal")
		}
		for i := int64(0); i < Npoints; i++ {
			if f.EvalPoly(sum, Xpoints[i]).Cmp(f.Add(Ypoints[i], Zpoints[i])) != 0 {
				t.Fail()
				fmt.Println("fail")
			}
			if f.EvalPoly(mulNaive, Xpoints[i]).Cmp(f.Mul(Ypoints[i], Zpoints[i])) != 0 {
				t.Fail()
				fmt.Println("fail")
			}

		}
	}
}

func TestPolynomialField_CompareLangrangeBaseComputations(t *testing.T) {
	r := new(big.Int).Set(bn256.Order)
	//r := new(big.Int).SetInt64(929)
	f := NewFiniteField(r)
	// new Polynomial Field
	pf := NewPolynomialField(f)

	points := 1 << 6

	before := time.Now()
	pf.PrecomputeLagrangeFFT(points)
	fmt.Println("time elapsed:", time.Since(before))
	para, _ := pf.fftPras[points]
	assert.True(t, pf.testLagrange(pf.basesClassic, para.RootOfUnitys))

	// new Polynomial Field
	pf2 := NewPolynomialField(f)

	before = time.Now()
	pf2.PrecomputeLagrangeFFT_2(points)
	fmt.Println("CRS generation time elapsed:", time.Since(before))

	for i := 0; i < para.Size; i++ {
		r1, _ := pf.basesClassic[baseLengthPair{
			baseIndex: i,
			Length:    para.Size,
		}]
		r2, _ := pf2.basesClassic[baseLengthPair{
			baseIndex: i,
			Length:    para.Size,
		}]

		if !BigArraysEqual(r1, r2) {
			t.Error(fmt.Printf("uneq %v %v %v", i, r1, r2))
		}
	}
	// new Polynomial Field
	pf3 := NewPolynomialField(f)

	before = time.Now()
	pf3.PrecomputeLagrangeFFT_3(points)
	fmt.Println("CRS generation time elapsed:", time.Since(before))

	for i := 0; i < para.Size; i++ {
		r1, _ := pf2.basesClassic[baseLengthPair{
			baseIndex: i,
			Length:    para.Size,
		}]
		r2, _ := pf3.basesClassic[baseLengthPair{
			baseIndex: i,
			Length:    para.Size,
		}]

		if !BigArraysEqual(r1, r2) {
			t.Error(fmt.Printf("uneq %v %v %v", i, r1, r2))
		}
	}

}

func TestPolynomialField_(t *testing.T) {
	r := new(big.Int).Set(bn256.Order)
	//r := new(big.Int).SetInt64(929)
	f := NewFiniteField(r)
	// new Polynomial Field
	pf := NewPolynomialField(f)

	points := 1 << 13
	fmt.Println("Generate Lagrange Bases...")
	before := time.Now()
	pf.PrecomputeLagrangeFFT_3(points)
	fmt.Println("CRS generation time elapsed:", time.Since(before))
}

func TestNewPolynomialField(t *testing.T) {
	r := new(big.Int).Set(bn256.Order)
	//r := new(big.Int).SetInt64(929)
	f := NewFiniteField(r)
	// new Polynomial Field
	pf := NewPolynomialField(f)
	leng := 1 << 4

	fmt.Println("len ", leng)
	before := time.Now()
	pf.PrecomputeLagrangeFFT(leng)
	fmt.Println("Lagrange precumputation:", time.Since(before))

	//assert.True(t, pf.testLagrange(pf.basesClassic, para.RootOfUnitys))

	for i := 0; i < leng; i++ {
		r1, _ := pf.basesClassic[baseLengthPair{
			baseIndex: i,
			Length:    NextPowerOfTwo(leng),
		}]
		m := make(map[string]bool)
		for _, v := range r1 {
			//if _,ex:= m[v];!ex{
			//	m[v]=true
			//}
			m[v.String()] = true
		}
		//fmt.Printf("%v  - repet %v", r1, len(m))
		fmt.Println(r1)
		fmt.Print(",")

	}

	var permute = func(f int, in Poly) (out Poly) {
		out = make(Poly, len(in))
		out[0] = in[0]
		for i := 1; i < len(in); i++ {
			out[i] = in[(i*f)%len(in)]
		}
		return out
	}
	//permute(1,nil)
	var split = func(f int, in Poly) (out Poly) {
		outt := make(Poly, len(in)/2)
		outt[0] = in[0]
		for i := 1; i < len(in)/2; i++ {
			outt[i] = in[(i*f)%len(in)]
		}
		return append(outt, outt...)
	}
	var perDiff = func(a, b Poly) int {
		st := a[1]
		r := 0
		for b[1].Cmp(st) != 0 {
			r += 1
			st = a[(r+1)%len(a)]
		}
		return r
	}

	//base, _ := pf.basesClassic[baseLengthPair{
	//	baseIndex: 1,
	//	Length:    para.Size,
	//}]

	for i := 1; i < leng/2; i *= 2 {
		source, _ := pf.basesClassic[baseLengthPair{
			baseIndex: i,
			Length:    NextPowerOfTwo(leng),
		}]
		target, _ := pf.basesClassic[baseLengthPair{
			baseIndex: i * 2,
			Length:    NextPowerOfTwo(leng),
		}]

		if !BigArraysEqual(split(2, source), target) {
			panic("split-permute error")
		}

		//v := 0
		fmt.Printf("row %v : ", i)
		for j := i; j < leng; j += 2 * i {
			//source, _ := pf.basesClassic[baseLengthPair{
			//	baseIndex: j,
			//	Length:    para.Size,
			//}]
			target, _ := pf.basesClassic[baseLengthPair{
				baseIndex: (j + (2 * i)) % leng,
				Length:    NextPowerOfTwo(leng),
			}]
			//xx:= perDiff(source,target)
			xx := perDiff(source, target)
			fmt.Print((xx + 1) % leng)
			//v = xx
			fmt.Print(",")
			//fmt.Println(source)
			//fmt.Println(target)
			//fmt.Println(permute( xx+1 , source))

			if !BigArraysEqual(permute((xx+1)%leng, source), target) {
				//if !BigArraysEqual(permute( (xx+1)%leng , source), target) {
				panic("permute error")
			}

			//
		}
		fmt.Println()
	}

	bases := make([]Poly, leng)
	start, _ := pf.basesClassic[baseLengthPair{
		baseIndex: 1,
		Length:    NextPowerOfTwo(leng),
	}]
	bases[1] = start

	for i := 1; i < leng; i *= 2 {
		bases[(i*2)%leng] = split(2, start)
		ctr := 1
		compare, _ := pf.basesClassic[baseLengthPair{
			baseIndex: (i * 2) % leng,
			Length:    NextPowerOfTwo(leng),
		}]
		if !BigArraysEqual(bases[(i*2)%leng], compare) {
			panic("split-permute error")
		}
		for j := i; j < leng; j += 2 * i {
			ind := (j + (2 * i)) % leng
			compare, _ := pf.basesClassic[baseLengthPair{
				baseIndex: ind,
				Length:    NextPowerOfTwo(leng),
			}]
			vv := 1 + (2 * ctr)
			next := permute(vv, start)

			if !BigArraysEqual(compare, next) {
				panic("permute error")
			}

			bases[ind] = next
			ctr += 1
		}
		start = bases[(i*2)%leng]
	}
	for k, v := range pf.basesClassic {
		if !BigArraysEqual(v, bases[k.baseIndex]) {
			print("shit")
		}
	}
	//fmt.Println(split(2,s1))
	//fmt.Println(permute(7,r1))
}
