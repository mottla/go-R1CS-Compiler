package utils

import (
	"math/big"
)

// PolynomialField is the Polynomial over a Finite Field where the polynomial operations are performed
type PolynomialField struct {
	F Fq
}

// NewPolynomialField creates a new PolynomialField with the given FiniteField

func NewPolynomialField(f Fq) (pf *PolynomialField) {
	polyField := &PolynomialField{F: f}
	return polyField
}

// Mul multiplies two polinomials over the Finite Field
func (pf PolynomialField) Mul(a, b []*big.Int) []*big.Int {
	r := ArrayOfBigZeros(len(a) + len(b) - 1)
	for i := 0; i < len(a); i++ {
		for j := 0; j < len(b); j++ {
			if a[i].Cmp(bigZero) == 0 || b[j].Cmp(bigZero) == 0 {
				r[i+j] = new(big.Int).Set(r[i+j])
				continue
			}
			r[i+j] = pf.F.Add(
				r[i+j],
				pf.F.Mul(a[i], b[j]))
		}
	}
	return r
}

// Mul multiplies two polinomials over the Finite Field
func (pf PolynomialField) MulSimple(a, b []*big.Int) []*big.Int {
	r := ArrayOfBigZeros(maxInt(len(a), len(b)))
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

// Add adds two polinomials over the Finite Field
func (pf PolynomialField) Add(a, b []*big.Int) []*big.Int {
	r := ArrayOfBigZeros(maxInt(len(a), len(b)))
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
	r := ArrayOfBigZeros(maxInt(len(a), len(b)))
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

//returns (x)(x-1)..(x-(len-1))
func (pf PolynomialField) DomainPolynomial_FFT(Ng int) []*big.Int {
	Domain := ArrayOfBigZeros(Ng + 1)
	Domain[0] = new(big.Int).SetInt64(-1)
	Domain[Ng] = new(big.Int).SetInt64(1)
	return Domain
}

//returns (x)(x-1)..(x-(len-1))
func (pf PolynomialField) DomainPolynomial(len int) []*big.Int {
	Domain := []*big.Int{big.NewInt(int64(0)), big.NewInt(int64(1))}

	for i := 1; i < len; i++ {
		Domain = pf.Mul(
			Domain,
			[]*big.Int{
				pf.F.Neg(big.NewInt(int64(i))), big.NewInt(int64(1)),
			})
	}
	return Domain
}

func (pf PolynomialField) degree(p []*big.Int) int {
	for i := len(p) - 1; i > 0; i-- {
		if p[i].Cmp(bigZero) != 0 {
			return i
		}
	}
	return 0

}
func (pf PolynomialField) shift(shift *big.Int, invert bool, inputValues []*big.Int) (shiftedValues []*big.Int) {
	if shift == nil || shift.Cmp(bigZero) == 0 {
		return inputValues
	}
	shiftedValues = make([]*big.Int, len(inputValues))

	in := new(big.Int).Set(shift)
	if invert {
		in = pf.F.Inverse(in)
	}
	res := new(big.Int).SetInt64(1)
	for k, v := range inputValues {
		shiftedValues[k] = pf.F.Mul(res, v)
		res = pf.F.Mul(res, in)
	}

	return shiftedValues
}

// LagrangeInterpolation performs the Lagrange Interpolation / Lagrange Polynomials operation
func (pf PolynomialField) LagrangeInterpolation(datapoints []*big.Int) (polynom []*big.Int) {
	// https://en.wikipedia.org/wiki/Lagrange_polynomial

	var base = func(pointPos, totalPoints int) (r []*big.Int) {

		if v, ex := pf.F.basesClassic[baseLengthPair{baseIndex: pointPos, Length: totalPoints}]; ex {
			return v
		}
		facBig := big.NewInt(1)

		for i := 0; i < pointPos; i++ {
			facBig = pf.F.Mul(facBig, big.NewInt(int64(pointPos-i)))
		}
		for i := pointPos + 1; i < totalPoints; i++ {
			facBig = pf.F.Mul(facBig, big.NewInt(int64(pointPos-i)))
		}
		hf := pf.F.Inverse(facBig)

		r = []*big.Int{new(big.Int).SetInt64(1)}
		for i := 0; i < totalPoints; i++ {
			if i != pointPos {
				r = pf.Mul(r, []*big.Int{big.NewInt(int64(-i)), big.NewInt(int64(1))})
			}
		}
		r = pf.MulScalar(r, hf)
		pf.F.basesClassic[baseLengthPair{baseIndex: pointPos, Length: totalPoints}] = r
		return r
	}
	//if IsZeroArray(datapoints){
	//	//at position -1 we store the all zero polynomial
	//	if v,ex:=pf.F.basesClassic[baseLengthPair{baseIndex: -1,Length: len(datapoints)}];ex{
	//		return v
	//	}
	//	DomainPoly := pf.DomainPolynomial(len(datapoints))
	//	pf.F.basesClassic[baseLengthPair{baseIndex: -1,Length: len(datapoints)}]= DomainPoly
	//	return DomainPoly
	//}
	polynom = ArrayOfBigZeros(len(datapoints))
	for i, v := range datapoints {
		if v.Cmp(bigZero) == 0 {
			continue
		}
		prod := pf.MulScalar(base(i, len(datapoints)), v)
		polynom = pf.Add(polynom, prod)
	}

	return polynom
}

// LagrangeInterpolation performs the Lagrange Interpolation / Lagrange Polynomials operation
func (pf *PolynomialField) PrecomputeLagrangeFFT(fft *FFT_PrecomputedParas) {
	// https://en.wikipedia.org/wiki/Lagrange_polynomial

	var base = func(pointPos int) {
		var lagrangeBase_i []*big.Int
		facBig := big.NewInt(1)

		for i := 0; i < pointPos; i++ {
			facBig = pf.F.Mul(facBig, pf.F.Sub(fft.RootOfUnitys[pointPos], fft.RootOfUnitys[i]))
		}
		for i := pointPos + 1; i < fft.Size; i++ {
			facBig = pf.F.Mul(facBig, pf.F.Sub(fft.RootOfUnitys[pointPos], fft.RootOfUnitys[i]))
		}
		hf := pf.F.Inverse(facBig)

		lagrangeBase_i = []*big.Int{new(big.Int).SetInt64(1)}
		for i := 0; i < fft.Size; i++ {
			if i != pointPos {
				lagrangeBase_i = pf.Mul(lagrangeBase_i, []*big.Int{fft.RootOfUnitys[(i+(fft.Size>>1))%fft.Size], big.NewInt(int64(1))})
			}
		}
		lagrangeBase_i = pf.MulScalar(lagrangeBase_i, hf)
		pf.F.basesClassic[baseLengthPair{baseIndex: pointPos, Length: fft.Size}] = lagrangeBase_i
	}

	var hardwork = func(start, end int) {
		for i := start; i < end; i++ {
			base(i)
		}
	}
	Parallelize(fft.Size, hardwork)

}

// LagrangeInterpolation performs the Lagrange Interpolation / Lagrange Polynomials operation
func (pf *PolynomialField) PrecomputeLagrange(totalPoints int) {
	// https://en.wikipedia.org/wiki/Lagrange_polynomial

	var base = func(pointPos int) {
		var r []*big.Int
		facBig := big.NewInt(1)

		for i := 0; i < pointPos; i++ {
			facBig = pf.F.Mul(facBig, big.NewInt(int64(pointPos-i)))
		}
		for i := pointPos + 1; i < totalPoints; i++ {
			facBig = pf.F.Mul(facBig, big.NewInt(int64(pointPos-i)))
		}
		hf := pf.F.Inverse(facBig)

		r = []*big.Int{new(big.Int).SetInt64(1)}
		for i := 0; i < totalPoints; i++ {
			if i != pointPos {
				r = pf.Mul(r, []*big.Int{big.NewInt(int64(-i)), big.NewInt(int64(1))})
			}
		}
		r = pf.MulScalar(r, hf)
		pf.F.basesClassic[baseLengthPair{baseIndex: pointPos, Length: totalPoints}] = r
	}

	var hardwork = func(start, end int) {
		for i := start; i < end; i++ {
			base(i)
		}
	}
	Parallelize(totalPoints, hardwork)

}

// LagrangeInterpolation performs the Lagrange Interpolation / Lagrange Polynomials operation
func (pf *PolynomialField) LagrangeInterpolation_RootOfUnity(fft *FFT_PrecomputedParas, datapoints []*big.Int) (polynom []*big.Int) {
	// https://en.wikipedia.org/wiki/Lagrange_polynomial
	if len(datapoints) != fft.Size {
		panic("not allowed")
	}

	var base = func(pointPos, totalPoints int) (r []*big.Int) {

		if v, ex := pf.F.basesClassic[baseLengthPair{baseIndex: pointPos, Length: totalPoints}]; ex {
			return v
		}
		panic("asdf")

		return r
	}
	//if IsZeroArray(datapoints){
	//	//at position -1 we store the all zero polynomial
	//	if v,ex:=pf.F.basesClassic[baseLengthPair{baseIndex: -1,Length: len(datapoints)}];ex{
	//		return v
	//	}
	//	DomainPoly := pf.DomainPolynomial(len(datapoints))
	//	pf.F.basesClassic[baseLengthPair{baseIndex: -1,Length: len(datapoints)}]= DomainPoly
	//	return DomainPoly
	//}
	polynom = ArrayOfBigZeros(len(datapoints))

	//var hardwork =func(start,end int){
	//	for i, v := range datapoints[start:end] {
	//		if v.Cmp(bigZero) == 0 {
	//			continue
	//		}
	//		prod := pf.MulScalar(base(i, len(datapoints)), v)
	//		polynom = pf.Add(polynom, prod)
	//	}
	//}
	for i, v := range datapoints {
		if v.Cmp(bigZero) == 0 {
			continue
		}
		prod := pf.MulScalar(base(i, len(datapoints)), v)
		polynom = pf.Add(polynom, prod)
	}
	//Parallelize(len(datapoints),hardwork)
	return polynom
}

func (pf PolynomialField) MulScalar(polynomial []*big.Int, w *big.Int) (scaledPolynomial []*big.Int) {
	scaledPolynomial = make([]*big.Int, len(polynomial))
	for i := 0; i < len(polynomial); i++ {
		if polynomial[i].Cmp(bigZero) == 0 {
			scaledPolynomial[i] = new(big.Int).SetInt64(0)
			continue
		}
		scaledPolynomial[i] = pf.F.Mul(w, polynomial[i])
	}
	return
}

func (pf PolynomialField) PointwiseMultiplication(a []*big.Int, b []*big.Int) (ab []*big.Int) {
	ab = make([]*big.Int, len(a))
	for i := 0; i < len(a); i++ {
		ab[i] = pf.F.Mul(a[i], b[i])
	}
	return ab
}
func (pf PolynomialField) PointwiseAdd(a []*big.Int, b []*big.Int) (ab []*big.Int) {
	ab = make([]*big.Int, len(a))
	for i := 0; i < len(a); i++ {
		ab[i] = pf.F.Add(a[i], b[i])
	}
	return ab
}
func (pf PolynomialField) PointwiseSub(a []*big.Int, b []*big.Int) (ab []*big.Int) {
	ab = make([]*big.Int, len(a))
	for i := 0; i < len(a); i++ {
		ab[i] = pf.F.Sub(a[i], b[i])
	}
	return ab
}

func (pf PolynomialField) LinearCombine(polynomials [][]*big.Int, w []*big.Int) (scaledPolynomials [][]*big.Int) {
	scaledPolynomials = make([][]*big.Int, len(w))
	for i := 0; i < len(w); i++ {
		scaledPolynomials[i] = pf.MulScalar(polynomials[i], w[i])
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

//EvalPoly Evaluates a polynomial v at position x, using the Horners Rule
func (pf PolynomialField) EvalPoly(v []*big.Int, x *big.Int) *big.Int {
	return pf.F.EvalPoly(v, x)
}
