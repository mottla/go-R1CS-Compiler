package fields

import (
	"math/big"
)

var bigZero = big.NewInt(int64(0))

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

// PolynomialField is the Polynomial over a Finite Field where the polynomial operations are performed
type PolynomialField struct {
	F     Fq
	bases [][]*big.Int
}

//InitBases precomputes the basis polynomials so later lagrangian interpolation
//can be done by simply computing the linear product
func (pf *PolynomialField) InitBases(totalPoints int) {
	v := make([][]*big.Int, totalPoints)

	//(xj-x0)(xj-x1)..(xj-x_j-1)(xj-x_j+1)..(x_j-x_k)
	for pointPos := 0; pointPos < totalPoints; pointPos++ {
		var iterator = new(big.Int)
		facBig := big.NewInt(1)

		for i := 0; i < pointPos; i++ {
			iterator.SetInt64(int64(pointPos - i))
			facBig = pf.F.Mul(facBig, iterator)
		}
		for i := pointPos + 1; i < totalPoints; i++ {
			iterator.SetInt64(int64(pointPos - i))
			facBig = pf.F.Mul(facBig, iterator)
		}
		hf := pf.F.Inverse(facBig)
		r := []*big.Int{hf}
		for i := 0; i < totalPoints; i++ {
			if i != pointPos {
				ineg := big.NewInt(int64(-i))
				b1 := big.NewInt(int64(1))
				r = pf.Mul(r, []*big.Int{ineg, b1})
			}
		}
		v[pointPos] = r
	}

	pf.bases = v
}

// NewPolynomialField creates a new PolynomialField with the given FiniteField
//and precomputed lagriang bases
func NewPolynomialFieldPrecomputedLagriangian(f Fq, NumBases int) (pf *PolynomialField) {
	polyField := &PolynomialField{F: f}
	polyField.InitBases(NumBases)
	return polyField
}

// Mul multiplies two polinomials over the Finite Field
func (pf PolynomialField) Mul(a, b []*big.Int) []*big.Int {
	r := ArrayOfBigZeros(len(a) + len(b) - 1)
	for i := 0; i < len(a); i++ {
		for j := 0; j < len(b); j++ {
			r[i+j] = pf.F.Add(
				r[i+j],
				pf.F.Mul(a[i], b[j]))
		}
	}
	return r
}

// Mul multiplies two polinomials over the Finite Field
func (pf PolynomialField) MulSimple(a, b []*big.Int) []*big.Int {
	r := ArrayOfBigZeros(max(len(a), len(b)))
	for i := 0; i < len(a); i++ {
		r[i] = pf.F.Add(r[i], a[i])
	}
	for i := 0; i < len(b); i++ {
		r[i] = pf.F.Mul(r[i], b[i])
	}
	return r
}

// Div divides two polinomials over the Finite Field, returning the result and the remainder
func (pf PolynomialField) Div(a, b []*big.Int) ([]*big.Int, []*big.Int) {
	// https://en.wikipedia.org/wiki/Division_algorithm
	r := ArrayOfBigZeros(len(a) - len(b) + 1)
	rem := a
	for len(rem) >= len(b) {
		l := pf.F.Div(rem[len(rem)-1], b[len(b)-1])
		pos := len(rem) - len(b)
		r[pos] = l
		aux := ArrayOfBigZeros(pos)
		aux1 := append(aux, l)
		aux2 := pf.Sub(rem, pf.Mul(b, aux1))
		rem = aux2[:len(aux2)-1]
	}
	return r, rem
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// Add adds two polinomials over the Finite Field
func (pf PolynomialField) Add(a, b []*big.Int) []*big.Int {
	r := ArrayOfBigZeros(max(len(a), len(b)))
	for i := 0; i < len(a); i++ {
		r[i] = pf.F.Add(r[i], a[i])
	}
	for i := 0; i < len(b); i++ {
		r[i] = pf.F.Add(r[i], b[i])
	}
	return r
}

// Sub subtracts two polinomials over the Finite Field
func (pf PolynomialField) Sub(a, b []*big.Int) []*big.Int {
	r := ArrayOfBigZeros(max(len(a), len(b)))
	for i := 0; i < len(a); i++ {
		r[i] = pf.F.Add(r[i], a[i])
	}
	for i := 0; i < len(b); i++ {
		r[i] = pf.F.Sub(r[i], b[i])
	}
	return r
}

//// Eval evaluates the polinomial over the Finite Field at the given value x
//func (pf PolynomialField) Eval(v []*big.Int, x *big.Int) *big.Int {
//	r := big.NewInt(int64(0))
//	for i := 0; i < len(v); i++ {
//		xi := pf.F.Exp(x, big.NewInt(int64(i)))
//		elem := pf.F.Mul(v[i], xi)
//		r = pf.F.Add(r, elem)
//	}
//	return r
//}

// NewPolZeroAt generates a new polynomial that has value zero at the given value
func (pf PolynomialField) NewPolZeroAt(pointPos, totalPoints int, height *big.Int) []*big.Int {

	facBig := big.NewInt(1)
	var iterator = new(big.Int)
	//(xj-x0)(xj-x1)..(xj-x_j-1)(xj-x_j+1)..(x_j-x_k)
	for i := 0; i < pointPos; i++ {
		iterator.SetInt64(int64(pointPos - i))
		facBig = pf.F.Mul(facBig, iterator)
	}
	for i := pointPos + 1; i < totalPoints; i++ {
		iterator.SetInt64(int64(pointPos - i))
		facBig = pf.F.Mul(facBig, iterator)
	}

	hf := pf.F.Div(height, facBig)
	r := []*big.Int{hf}
	for i := 0; i < totalPoints; i++ {
		if i != pointPos {
			ineg := big.NewInt(int64(-i))
			b1 := big.NewInt(int64(1))
			r = pf.Mul(r, []*big.Int{ineg, b1})
		}
	}
	return r
}

// LagrangeInterpolation performs the Lagrange Interpolation / Lagrange Polynomials operation
func (pf PolynomialField) LagrangeInterpolation(v []*big.Int) []*big.Int {
	// https://en.wikipedia.org/wiki/Lagrange_polynomial
	var r []*big.Int
	for i := 0; i < len(v); i++ {
		//NOTE this comparison gives a huge performance boost
		if v[i].Cmp(bigZero) != 0 {
			r = pf.Add(r, pf.Mul(pf.bases[i], []*big.Int{v[i]}))
		} else {
			r = pf.Add(r, ArrayOfBigZeros(len(v)))
		}

		//r = pf.Mul(v[i], pf.NewPolZeroAt(i+1, len(v), v[i]))
	}
	//
	return r
}

func (pf PolynomialField) LinearCombine(polynomials [][]*big.Int, w []*big.Int) (scaledPolynomials [][]*big.Int) {
	scaledPolynomials = make([][]*big.Int, len(w))
	for i := 0; i < len(w); i++ {
		scaledPolynomials[i] = pf.Mul([]*big.Int{w[i]}, polynomials[i])

	}
	return
}
func (pf PolynomialField) AddPolynomials(polynomials [][]*big.Int) (sumPoly []*big.Int) {
	sumPoly = []*big.Int{}
	for i := 0; i < len(polynomials); i++ {
		sumPoly = pf.Add(sumPoly, polynomials[i])
	}
	return
}
