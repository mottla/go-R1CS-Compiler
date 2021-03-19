package utils

import (
	bn256 "github.com/mottla/go-R1CS-Compiler/pairing"
	"math/big"
)

var bigZero = big.NewInt(int64(0))
var bigOne = big.NewInt(int64(1))

//Todo I dont like this pubic accessible thing here
var Field = PrepareFields(bn256.Order)

// Transpose transposes the *big.Int matrix
func Transpose(matrix [][]*big.Int) [][]*big.Int {
	r := make([][]*big.Int, len(matrix[0]))
	for x, _ := range r {
		r[x] = make([]*big.Int, len(matrix))
	}
	for y, s := range matrix {
		for x, e := range s {
			r[x][y] = e
		}
	}
	return r
}

// ArrayOfBigZeros creates a *big.Int array with n elements to zero
func ArrayOfBigZeros(num int) []*big.Int {

	var r = make([]*big.Int, num, num)
	for i := 0; i < num; i++ {
		r[i] = bigZero
	}
	return r
}
func BigArraysEqual(a, b []*big.Int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := 0; i < len(a); i++ {
		if a[i].Cmp(b[i]) != 0 {
			return false
		}
	}
	return true
}

func IsZeroArray(a []*big.Int) bool {
	for i := 0; i < len(a); i++ {
		if a[i].Cmp(bigZero) != 0 {
			return false
		}
	}
	return true
}
func BigIsOdd(n *big.Int) bool {
	return n.Bit(0) == 1
}

func maxInt(a, b int) int {
	if a > b {
		return a
	}
	return b
}

//checks if a integer is a power of 2
//From Henry Warrens Hackers Delight
func IsPowerTwo(in uint64) bool {
	if in == 0 {
		return false
	}
	n := in & (in - 1)
	if n == 0 {
		return true
	}
	return false
}

func Equal(a, b int) int {
	if a == b {
		return 1
	}
	return 0
}

func NextPowerOfTwo(n int) int {
	p := int(1)
	if (n & (n - 1)) == 0 {
		return n
	}
	for p < n {
		p <<= 1
	}
	return p
}

//euclid returns q,r s.t. a=bq+r
func euclid(a, b uint) (q, r uint) {
	return a / b, a % b
}
