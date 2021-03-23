package utils

import (
	"fmt"
	bn256 "github.com/mottla/go-R1CS-Compiler/pairing"
	"github.com/stretchr/testify/assert"
	"math/big"
	"testing"
	"time"
)

func TestPolynomialField_Mul(t *testing.T) {
	// new Finite Field
	var Npoints = 1 << 10
	r := new(big.Int).Set(bn256.Order)
	f := NewFiniteField(r)
	// new Polynomial Field
	pf := NewPolynomialField(f)

	var err error

	for runs := 0; runs < 1; runs++ {
		Ypoints := make([]*big.Int, Npoints)
		Zpoints := make([]*big.Int, Npoints)
		for i := 0; i < Npoints; i++ {
			Ypoints[i], err = f.Rand()
			assert.Nil(t, err)
			Zpoints[i], err = f.Rand()
			assert.Nil(t, err)
		}
		tt := time.Now()
		fmt.Println("Lagrange Interpolation...")
		l1 := pf.LagrangeInterpolation_RootOfUnity(ExtendArrayWithZeros(Ypoints, NextPowerOfTwo(Npoints)*2))
		l2 := pf.LagrangeInterpolation_RootOfUnity(ExtendArrayWithZeros(Zpoints, NextPowerOfTwo(Npoints)*2))
		fmt.Println("Lagrange Interpolation took ", time.Since(tt))

		tt = time.Now()
		fmt.Println("FFT Interpolation...")
		f1 := pf.InvDFFT(ExtendArrayWithZeros(Ypoints, NextPowerOfTwo(Npoints)*2), nil)
		f2 := pf.InvDFFT(ExtendArrayWithZeros(Zpoints, NextPowerOfTwo(Npoints)*2), nil)
		fmt.Println("FFT Interpolation took ", time.Since(tt))

		tt = time.Now()
		fmt.Println("FFT Multiplication...")
		mulFFT := pf.MulFFT(l1, l2)
		fmt.Println("Multiplication took ", time.Since(tt))
		mulFFTF := pf.MulFFT(f1, f2)
		fftPara := pf.fftPras[NextPowerOfTwo(Npoints)*2]
		assert.True(t, BigArraysEqual(mulFFT, mulFFTF))
		for i := 0; i < Npoints; i++ {
			if f.EvalPoly(mulFFT, fftPara.RootOfUnitys[i]).Cmp(f.Mul(Ypoints[i], Zpoints[i])) != 0 {
				t.Fail()
				fmt.Println("fail")
			}
		}
	}
}
