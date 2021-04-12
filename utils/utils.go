package utils

import (
	//bn256 "github.com/mottla/go-R1CS-Compiler/pairing"
	"math"
	"math/big"
)

var bigZero = big.NewInt(int64(0))
var bigOne = big.NewInt(int64(1))

//Todo I dont like this pubic accessible thing here
//var Field = PrepareFields(bn256.Order)

var Field = PrepareFields(new(big.Int).SetInt64(1009))

// Transpose transposes the *big.Int matrix
func Transpose(matrix []Poly) []Poly {
	r := make([]Poly, len(matrix[0]))
	for x, _ := range r {
		r[x] = make(Poly, len(matrix))
	}
	for y, s := range matrix {
		for x, e := range s {
			r[x][y] = e
		}
	}
	return r
}

// ArrayOfBigZeros creates a *big.Int array with n elements to zero
func ArrayOfBigZeros(num int) Poly {

	var r = make(Poly, num, num)
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
func Addicity(n int) int {
	p := int(1)
	ad := 0
	for p&n == 0 {
		n = n >> 1
		ad += 1
	}
	return ad
}

//euclid returns q,r s.t. a=bq+r
func euclid(a, b uint) (q, r uint) {
	return a / b, a % b
}
func maximum(a ...int) int {
	if len(a) == 0 {
		return math.MinInt64
	}
	return max(a[0], maximum(a[1:]...))

}
func max(a, b int) int {

	if a > b {
		return a
	}
	return b
}
func AbsInt(i int) int {
	if i < 0 {
		return -i
	}
	return i
}
func ExtendArrayWithZeros(in []*big.Int, desiredLength int) []*big.Int {
	if len(in) < desiredLength {
		rest := desiredLength - len(in)
		//fmt.Printf("\npolysize %v, filled up to next power of two 2^%v. Add %v dummy values", len(polynomial), bit, rest)
		in = append(in, ArrayOfBigZeros(rest)...)
	}
	return in
}

//returns the absolute value of a signed int and a flag telling if the input was positive or not
//this implementation is awesome and fast (see Henry S Warren, Hackers's Delight)
func Abs(n int) (val int, positive bool) {
	y := n >> 63
	return (n ^ y) - y, y == 0
}

type FastBool struct {
	val *big.Int
}

func NewFastBool() FastBool {
	return FastBool{val: new(big.Int)}
}

func (fb FastBool) Set(pos int) {
	fb.val.SetBit(fb.val, pos, 1)
}
func (fb FastBool) IsSet(pos int) bool {
	if fb.val.Bit(pos) == 1 {
		return true
	}
	return false
}

//the go a%b return negative representants too.. that sucks so much
func Mod(a, b int) int {
	r := a % b
	if r < 0 {
		r += b
	}
	return r
}
