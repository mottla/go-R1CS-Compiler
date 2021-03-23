package utils

import (
	"fmt"
	bn256 "github.com/mottla/go-R1CS-Compiler/pairing"
	"github.com/stretchr/testify/assert"
	"math/big"
	"math/rand"
	"testing"
	"time"
)

func TestEval(t *testing.T) {
	// new Finite Field

	f := NewFiniteField(bn256.Order)
	at, _ := f.Rand()
	//at := big.NewInt(0)
	order := 150000
	//1 is 100% of the coefficients are 0
	sparsityPercent := 0.9
	a := ArrayOfBigZeros(order)

	for i := 0; i < order; i += 1 + rand.Intn(int(float64(order)*sparsityPercent)) {
		a[i], _ = f.Rand()
	}

	before := time.Now()

	sparseA := NewAvlTree()

	for _, v := range rand.Perm(len(a)) {
		//fmt.Println(v,a[v])
		sparseA.Insert(uint(v), a[v])
		//fmt.Println(sparseA.String())
	}

	before = time.Now()
	sparseAt := f.EvalSparsePoly(sparseA, at)
	fmt.Println("evaluate sparse took", time.Since(before))

	before = time.Now()
	classic := f.EvalPoly(a, at)
	fmt.Println("evaluate classic took", time.Since(before))

	//fmt.Println(f.EvalSparsePoly(sparseC,b16).String())
	if sparseAt.Cmp(classic) != 0 {
		t.Error(fmt.Sprintf("classic poly %v and sparse poly %v evaluation differ. At leas one of both must be wrong", sparseAt.String(), classic.String()))
	}

	for i := 0; i < 10; i++ {
		at, _ = f.Rand()
		sparseAt = f.EvalSparsePoly(sparseA, at)
		classic = f.EvalPoly(a, at)
		if sparseAt.Cmp(classic) != 0 {
			t.Error(fmt.Sprintf("classic poly %v and sparse poly %v evaluation differ. At leas one of both must be wrong", sparseAt.String(), classic.String()))
		}

	}
}

func TestMultiply(t *testing.T) {
	// new Finite Field

	f := NewFiniteField(bn256.Order)
	at, _ := f.Rand()

	order := 3000
	//1 is 100% of the coefficients are 0
	sparsityPercent := 0.1
	a := ArrayOfBigZeros(order)
	b := ArrayOfBigZeros(order)
	for i := 0; i < order; i += 1 + rand.Intn(int(float64(order)*sparsityPercent)) {
		a[i], _ = f.Rand()
		//a[i]=big.NewInt(int64(1))
	}
	for i := 0; i < order; i += 1 + rand.Intn(int(float64(order)*sparsityPercent)) {
		//b[i]=big.NewInt(int64(1))
		b[i], _ = f.Rand()
	}
	// new Polynomial Field
	pf := NewPolynomialField(f)

	before := time.Now()
	c := pf.MulNaive(a, b)
	fmt.Println("multiply with horner took", time.Since(before))
	sparseA := NewAvlTree()
	sparseB := NewAvlTree()

	for _, v := range rand.Perm(len(a)) {
		sparseA.Insert(uint(v), a[v])
	}
	for _, v := range rand.Perm(len(b)) {
		sparseB.Insert(uint(v), b[v])
	}
	before = time.Now()
	sparseC := f.MulSparse(sparseA, sparseB)
	fmt.Println("multiply sparse took", time.Since(before))
	before = time.Now()
	sparseAt := f.EvalSparsePoly(sparseC, at)
	fmt.Println("evaluate sparse took", time.Since(before))

	before = time.Now()
	classic := f.EvalPoly(c, at)
	fmt.Println("evaluate classic took", time.Since(before))

	//fmt.Println(f.EvalSparsePoly(sparseC,b16).String())
	if sparseAt.Cmp(classic) != 0 {
		t.Error(fmt.Sprintf("classic poly %v and sparse poly %v evaluation differ. At leas one of both must be wrong", sparseAt.String(), classic.String()))
	}

}

func TestAdd(t *testing.T) {
	// new Finite Field

	f := NewFiniteField(bn256.Order)
	at, _ := f.Rand()

	order := 100000
	//1 is 100% of the coefficients are 0
	sparsityPercent := 0.1
	a := ArrayOfBigZeros(order)
	b := ArrayOfBigZeros(order)
	for i := 0; i < order; i += 1 + rand.Intn(int(float64(order)*sparsityPercent)) {
		a[i], _ = f.Rand()
	}
	for i := 0; i < order; i += 1 + rand.Intn(int(float64(order)*sparsityPercent)) {
		b[i], _ = f.Rand()
	}
	// new Polynomial Field
	pf := NewPolynomialField(f)

	before := time.Now()
	c := pf.Add(a, b)
	fmt.Println("add classic took", time.Since(before))

	sparseA := NewAvlTree()
	sparseB := NewAvlTree()

	for _, v := range rand.Perm(len(a)) {
		sparseA.Insert(uint(v), a[v])
		//sparseA.Insert(uint(v), big.NewInt(int64(v)))
	}

	for _, v := range rand.Perm(len(b)) {
		sparseB.Insert(uint(v), b[v])
		//sparseB.Insert(uint(v), big.NewInt(int64(v)))
	}
	before = time.Now()
	sparseC := f.AddToSparse(sparseA, sparseB)
	fmt.Println("add sparse took", time.Since(before))

	before = time.Now()
	sparseAt := f.EvalSparsePoly(sparseC, at)
	fmt.Println("evaluate sparse took", time.Since(before))

	before = time.Now()
	classic := f.EvalPoly(c, at)
	fmt.Println("evaluate classic took", time.Since(before))

	//fmt.Println(f.EvalSparsePoly(sparseC,b16).String())
	if sparseAt.Cmp(classic) != 0 {
		t.Error(fmt.Sprintf("classic poly %v and sparse poly %v evaluation differ. At leas one of both must be wrong", sparseAt.String(), classic.String()))
	}

}

func TestSub(t *testing.T) {
	// new Finite Field

	f := NewFiniteField(bn256.Order)
	at, _ := f.Rand()

	order := 10000
	//1 is 100% of the coefficients are 0
	sparsityPercent := 0.1
	a := ArrayOfBigZeros(order)
	b := ArrayOfBigZeros(order)
	for i := 0; i < order; i += 1 + rand.Intn(int(float64(order)*sparsityPercent)) {
		a[i], _ = f.Rand()
	}
	for i := 0; i < order; i += 1 + rand.Intn(int(float64(order)*sparsityPercent)) {
		b[i], _ = f.Rand()
	}
	// new Polynomial Field
	pf := NewPolynomialField(f)

	before := time.Now()
	c := pf.Sub(a, b)
	fmt.Println("sub classic took", time.Since(before))

	sparseA := NewAvlTree()
	sparseB := NewAvlTree()
	for _, v := range rand.Perm(len(a)) {
		sparseA.Insert(uint(v), a[v])
	}

	for _, v := range rand.Perm(len(b)) {
		sparseB.Insert(uint(v), b[v])
	}
	before = time.Now()
	sparseC := f.SubToSparse(sparseA, sparseB)
	fmt.Println("sub sparse took", time.Since(before))
	before = time.Now()
	sparseAt := f.EvalSparsePoly(sparseC, at)
	fmt.Println("evaluate sparse took", time.Since(before))

	before = time.Now()
	classic := f.EvalPoly(c, at)
	fmt.Println("evaluate classic took", time.Since(before))

	//fmt.Println(f.EvalSparsePoly(sparseC,b16).String())
	if sparseAt.Cmp(classic) != 0 {
		t.Error(fmt.Sprintf("classic poly %v and sparse poly %v evaluation differ. At leas one of both must be wrong", sparseAt.String(), classic.String()))
	}

}
func TestSub2(t *testing.T) {
	// new Finite Field

	f := NewFiniteField(bn256.Order)
	at, _ := f.Rand()

	order := 2000
	sparsityPercent := 0.4
	a := ArrayOfBigZeros(order * 2)
	b := ArrayOfBigZeros(order)
	for i := 0; i < order*2; i += 1 + rand.Intn(int(float64(order)*sparsityPercent)) {
		a[i], _ = f.Rand()
		//a[i]=big.NewInt(int64(i))
	}
	for i := 0; i < order; i += 1 + rand.Intn(int(float64(order)*sparsityPercent)) {
		b[i], _ = f.Rand()
		//b[i]=big.NewInt(int64(i))
	}
	//a[0]=big.NewInt(-1)
	//a[1]=big.NewInt(0)
	//a[2]=big.NewInt(2)
	//b[0]=big.NewInt(0)
	//b[1]=big.NewInt(1)
	before := time.Now()
	sparseA := NewAvlTree()
	sparseB := NewAvlTree()
	for _, v := range rand.Perm(len(a)) {
		sparseA.Insert(uint(v), a[v])
	}

	for _, v := range rand.Perm(len(b)) {
		sparseB.Insert(uint(v), b[v])
	}

	before = time.Now()
	classic2 := f.EvalSparsePoly(sparseA, at)
	cDivSparse := f.SubToSparse(sparseA, sparseB)
	fmt.Println("sub sparse took", time.Since(before))

	//sparseA - sparseB= cDivSparse
	cd := f.AddToSparse(cDivSparse, sparseB)

	classic1 := f.EvalSparsePoly(cd, at)

	//fmt.Println(f.EvalSparsePoly(sparseC,b16).String())
	if classic1.Cmp(classic2) != 0 {
		t.Error(fmt.Sprintf("classic poly %v and sparse poly %v evaluation differ. At leas one of both must be wrong", classic1.String(), classic2.String()))
	}

}

//note that something weird happens with common division if sparsity increases.
//could not find out whats the issue
func TestDivide2(t *testing.T) {
	// new Finite Field

	f := NewFiniteField(bn256.Order)
	at, _ := f.Rand()

	polyField := &PolynomialField{F: f}
	order := 300
	sparsityPercent := 0.01
	a := ArrayOfBigZeros(order * 2)
	b := ArrayOfBigZeros(order)
	for i := 0; i < order*2; i += 1 + rand.Intn(int(float64(order)*sparsityPercent)) {
		a[i], _ = f.Rand()
		//a[i]=big.NewInt(int64(i))
	}
	for i := 0; i < order; i += 1 + rand.Intn(int(float64(order)*sparsityPercent)) {
		b[i], _ = f.Rand()
		//b[i]=big.NewInt(int64(i))
	}
	before := time.Now()
	sparseA := NewAvlTree()
	sparseB := NewAvlTree()
	for _, v := range rand.Perm(len(a)) {
		sparseA.Insert(uint(v), a[v])
	}

	for _, v := range rand.Perm(len(b)) {
		sparseB.Insert(uint(v), b[v])
	}

	before = time.Now()
	cDivSparse, rem2 := f.DivideSparse(sparseA, sparseB)
	fmt.Println("sparse division took", time.Since(before))

	before = time.Now()
	cdiv, crem := polyField.Div(a, b)
	fmt.Println("classic division took", time.Since(before))

	//sparseA=CdivSparece*sparseB +rem2
	mul := f.MulSparse(cDivSparse, sparseB)
	cd := f.AddToSparse(mul, rem2)

	reconstructed := f.EvalSparsePoly(cd, at)
	sparseEvaluated := f.EvalSparsePoly(sparseA, at)

	re := polyField.MulNaive(cdiv, b)
	re = polyField.Add(re, crem)
	reEval := f.EvalPoly(re, at)
	//fmt.Println(f.EvalSparsePoly(sparseC,b16).String())
	if reconstructed.Cmp(sparseEvaluated) != 0 || reEval.Cmp(sparseEvaluated) != 0 {
		t.Error(fmt.Sprintf("classic poly %v and sparse poly %v evaluation  and %v classic division differ.  At leas one of both must be wrong", reconstructed.String(), sparseEvaluated.String(), reEval.String()))
	}

}

func TestDivide(t *testing.T) {
	// new Finite Field

	f := NewFiniteField(bn256.Order)
	at, _ := f.Rand()

	order := 2000
	sparsityPercent := 0.1
	a := ArrayOfBigZeros(order * 2)
	b := ArrayOfBigZeros(order)
	for i := 0; i < order*2; i += 1 + rand.Intn(1+int(float64(order)*sparsityPercent)) {
		a[i], _ = f.Rand()
		//a[i]=big.NewInt(int64(i))
	}
	for i := 0; i < order; i += 1 + rand.Intn(1+int(float64(order)*sparsityPercent)) {
		b[i], _ = f.Rand()
		//b[i]=big.NewInt(int64(i))
	}

	before := time.Now()
	sparseA := NewAvlTree()
	sparseB := NewAvlTree()
	for _, v := range rand.Perm(len(a)) {
		sparseA.Insert(uint(v), a[v])
	}

	for _, v := range rand.Perm(len(b)) {
		sparseB.Insert(uint(v), b[v])
	}

	before = time.Now()
	cDivSparse, rem2 := f.DivideSparse(sparseA, sparseB)
	fmt.Println("sparse division took", time.Since(before))

	//sparseA:sparseB=CdivSparece +rem2
	cd := f.AddToSparse(f.MulSparse(cDivSparse, sparseB), rem2)
	classic1 := f.EvalSparsePoly(cd, at)
	classic2 := f.EvalSparsePoly(sparseA, at)

	//fmt.Println(f.EvalSparsePoly(sparseC,b16).String())
	if classic1.Cmp(classic2) != 0 {
		t.Error(fmt.Sprintf("classic poly %v and sparse poly %v evaluation differ. At leas one of both must be wrong", classic1.String(), classic2.String()))
	}
}

func TestSparseLagrangeInterpolation(t *testing.T) {
	// new Finite Field
	var Npoints = 250
	sparsityPercent := 0.8
	f := NewFiniteField(bn256.Order)
	// new Polynomial Field
	pf := NewPolynomialField(f)

	var err error

	Xpoints := make([]*big.Int, Npoints)
	for i := 0; i < Npoints; i++ {
		Xpoints[i] = new(big.Int).SetInt64(int64(i))
	}

	Ypoints := ArrayOfBigZeros(Npoints)

	for i := 0; i < Npoints; i += 1 + rand.Intn(1+int(float64(Npoints)*sparsityPercent)) {
		Ypoints[i], err = f.Rand()
		assert.Nil(t, err)
	}

	sparse := NewSparseArrayFromArray(Ypoints)
	sparse = pf.InterpolateSparseArray(sparse, Npoints)
	alpha := pf.LagrangeInterpolation(Ypoints)
	for i := Npoints - 1; i >= 0; i-- {
		if f.EvalPoly(alpha, Xpoints[i]).Cmp(Ypoints[i]) != 0 {
			t.Error("fail")
		}
		val := f.EvalSparsePoly(sparse, Xpoints[i])
		//fmt.Println(sparse)
		if val.Cmp(Ypoints[i]) != 0 {
			t.Error(fmt.Sprintf("fail sparse %v. Got %v", i, val.String()))
		}

	}

}
