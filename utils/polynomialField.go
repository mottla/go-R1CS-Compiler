package utils

import (
	"math/big"
	"sync"
)

// PolynomialField is the Polynomial over a Finite Field where the polynomial operations are performed
type PolynomialField struct {
	F Fq
	//artifacts.. we used to precompute the lagrange basis polys. it turned out to be uneccessary however
	bases        map[baseLengthPair]*AvlTree
	basesClassic map[baseLengthPair]Poly

	mutex   *sync.Mutex
	fftPras map[int]*FFT_PrecomputedParas
}

type Poly []*big.Int

//func (p Poly) String() string {
//	str := "["
//	for _, v := range p {
//
//		str += v.String()[0:7]
//		str += ","
//	}
//	str += "]"
//	return str
//}

// NewPolynomialField creates a new PolynomialField with the given FiniteField

func NewPolynomialField(f Fq) (pf *PolynomialField) {
	polyField := &PolynomialField{
		F:            f,
		bases:        make(map[baseLengthPair]*AvlTree),
		basesClassic: make(map[baseLengthPair]Poly),
		mutex:        &sync.Mutex{},
		fftPras:      map[int]*FFT_PrecomputedParas{},
	}
	return polyField
}

func (pf PolynomialField) testLagrange(bases map[baseLengthPair]Poly, roots Poly) bool {
	var at *big.Int
	for k, v := range bases {
		for kk, root := range roots {
			if pf.EvalPoly(v, root).Cmp(new(big.Int).SetInt64(int64(Equal(k.baseIndex, kk)))) != 0 {
				return false
			}
			at, _ = pf.F.Rand()
			if pf.EvalPoly(v, at).Cmp(bigZero) == 0 {
				return false
			}
		}

	}

	return true
}

// MulNaive multiplies two polinomials over the Finite Field
func (pf *PolynomialField) MulNaive(a, b Poly) Poly {
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

// MulFFT multiplies two polynomials over the Finite Field
func (pf *PolynomialField) MulFFT(a, b Poly) Poly {
	//aExtd := ExtendArrayWithZeros(a, NextPowerOfTwo(len(a))*2)
	//bExtd := ExtendArrayWithZeros(b, NextPowerOfTwo(len(a))*2)
	aPoints := pf.DFFT(a, nil)
	bPoints := pf.DFFT(b, nil)
	cPoints := pf.PointwiseMultiplication(aPoints, bPoints)
	return pf.InvDFFT(cPoints, nil)
}

// Div divides two polinomials over the Finite Field, returning the result and the remainder
func (pf PolynomialField) Div(a, b Poly) (Poly, Poly) {
	// https://en.wikipedia.org/wiki/Division_algorithm
	r := ArrayOfBigZeros(len(a) - len(b) + 1)
	rem := a
	for len(rem) >= len(b) {
		l := pf.F.Div(rem[len(rem)-1], b[len(b)-1])
		pos := len(rem) - len(b)
		r[pos] = l
		aux := ArrayOfBigZeros(pos)
		aux1 := append(aux, l)
		aux2 := pf.Sub(rem, pf.MulNaive(b, aux1))
		rem = aux2[:len(aux2)-1]
	}
	return r, rem
}

// TODO
func (pf PolynomialField) DivFFT(a, b Poly) (Poly, Poly) {
	// https://en.wikipedia.org/wiki/Division_algorithm
	r := ArrayOfBigZeros(len(a) - len(b) + 1)
	rem := a
	for len(rem) >= len(b) {
		l := pf.F.Div(rem[len(rem)-1], b[len(b)-1])
		pos := len(rem) - len(b)
		r[pos] = l
		aux := ArrayOfBigZeros(pos)
		aux1 := append(aux, l)
		aux2 := pf.Sub(rem, pf.MulFFT(b, aux1))
		rem = aux2[:len(aux2)-1]
	}
	return r, rem
}

// Add adds two polinomials over the Finite Field
func (pf PolynomialField) Add(a, b Poly) Poly {
	r := ArrayOfBigZeros(MaxInt(len(a), len(b)))
	for i := 0; i < len(a); i++ {
		r[i] = new(big.Int).Set(a[i])
	}
	for i := 0; i < len(b); i++ {
		r[i] = pf.F.Add(r[i], b[i])
	}
	return r
}

// Sub subtracts two polinomials over the Finite Field
func (pf PolynomialField) Sub(a, b Poly) Poly {
	r := ArrayOfBigZeros(MaxInt(len(a), len(b)))
	for i := 0; i < len(a); i++ {
		r[i] = new(big.Int).Set(a[i])
	}
	for i := 0; i < len(b); i++ {
		r[i] = pf.F.Sub(r[i], b[i])
	}
	return r
}

//// Eval evaluates the polinomial over the Finite Field at the given value x
//func (pf PolynomialField) Eval(v Poly, x *big.Int) *big.Int {
//	r := big.NewInt(int64(0))
//	for i := 0; i < len(v); i++ {
//		xi := pf.F.Exp(x, big.NewInt(int64(i)))
//		elem := pf.F.MulNaive(v[i], xi)
//		r = pf.F.Add(r, elem)
//	}
//	return r
//}

// NewPolZeroAt generates a new polynomial that has value zero at the given value
func (pf PolynomialField) NewPolZeroAt(pointPos, totalPoints int, height *big.Int) Poly {

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
			r = pf.MulNaive(r, Poly{ineg, b1})
		}
	}
	return r
}

//returns (x)(x-1)..(x-(len-1))
func (pf PolynomialField) DomainPolynomial(len int) Poly {
	Domain := Poly{big.NewInt(int64(0)), big.NewInt(int64(1))}

	for i := 1; i < len; i++ {
		Domain = pf.MulNaive(
			Domain,
			Poly{
				pf.F.Neg(big.NewInt(int64(i))), big.NewInt(int64(1)),
			})
	}
	return Domain
}

func (p Poly) Degree() int {
	for i := len(p) - 1; i > 0; i-- {
		if p[i].Cmp(bigZero) != 0 {
			return i
		}
	}
	return 0
}

//if less then 20% of the entries are unequal to 0, we consider the polynomial to be sparse
func (p Poly) IsSparse() bool {
	v := 0
	for i := len(p) - 1; i > 0; i-- {
		if p[i].Cmp(bigZero) != 0 {
			v += 1
		}
	}

	if v*5 < len(p) {
		return true
	}
	return false

}

func (pf PolynomialField) shift(shift *big.Int, invert bool, inputValues Poly) (shiftedValues Poly) {
	if shift == nil || shift.Cmp(bigZero) == 0 {
		return inputValues
	}
	shiftedValues = make(Poly, len(inputValues))

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
func (pf PolynomialField) LagrangeInterpolation(datapoints Poly) (polynom Poly) {
	// https://en.wikipedia.org/wiki/Lagrange_polynomial

	var base = func(pointPos, totalPoints int) (r Poly) {

		if v, ex := pf.basesClassic[baseLengthPair{baseIndex: pointPos, Length: totalPoints}]; ex {
			return v
		}
		panic("asdf")
		return r
	}

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
func (pf *PolynomialField) PrecomputeLagrangeFFT(points int) {
	// https://en.wikipedia.org/wiki/Lagrange_polynomial
	fft := pf.PrepareFFT(points)

	var base = func(pointPos int) {
		var lagrangeBase_i Poly
		facBig := big.NewInt(1)

		for i := 0; i < pointPos; i++ {
			facBig = pf.F.Mul(facBig, pf.F.Sub(fft.RootOfUnitys[pointPos], fft.RootOfUnitys[i]))
		}
		for i := pointPos + 1; i < fft.Size; i++ {
			facBig = pf.F.Mul(facBig, pf.F.Sub(fft.RootOfUnitys[pointPos], fft.RootOfUnitys[i]))
		}
		hf := pf.F.Inverse(facBig)

		lagrangeBase_i = Poly{new(big.Int).SetInt64(1)}
		for i := 0; i < fft.Size; i++ {
			if i != pointPos {
				lagrangeBase_i = pf.MulNaive(lagrangeBase_i, Poly{fft.RootOfUnitys[(i+(fft.Size>>1))%fft.Size], big.NewInt(int64(1))})
			}
		}
		lagrangeBase_i = pf.MulScalar(lagrangeBase_i, hf)
		pf.mutex.Lock()
		pf.basesClassic[baseLengthPair{baseIndex: pointPos, Length: fft.Size}] = lagrangeBase_i
		pf.mutex.Unlock()
	}

	var hardwork = func(start, end int) {
		for i := start; i < end; i++ {
			base(i)
		}
	}
	Parallelize(fft.Size, hardwork)

}

func (pf *PolynomialField) PrecomputeLagrangeFFT_2(points int) {
	// https://en.wikipedia.org/wiki/Lagrange_polynomial
	fft := pf.PrepareFFT(points)
	gates := fft.Size
	lagreangeBases := make([]Poly, gates)
	invGateNumber := pf.F.Inverse(new(big.Int).SetInt64(int64(gates)))
	lambda := pf.MulScalar(fft.Domain, invGateNumber)
	rho := fft.RootOfUnitys[gates>>1]
	var rest Poly
	lagreangeBases[0], rest = pf.Div(lambda, Poly{rho, bigOne})
	if !IsZeroArray(rest) {
		panic("no rest")
	}
	pf.basesClassic[baseLengthPair{baseIndex: 0, Length: fft.Size}] = lagreangeBases[0]

	for i := 1; i < gates; i++ {
		lambda = pf.MulScalar(lambda, fft.RootOfUnity)
		index := ((gates >> 1) + i) % gates
		//inv,_ := pf.Div(utils.Poly{bigOne},utils.Poly{fft.RootOfUnitys[ index], bigOne})
		lagreangeBases[i], _ = pf.Div(lambda, Poly{fft.RootOfUnitys[index], bigOne})

		pf.basesClassic[baseLengthPair{baseIndex: i, Length: fft.Size}] = lagreangeBases[i]
	}

}

// Author Mathias Wolf 2020
func (pf *PolynomialField) PrecomputeLagrangeFFT_3(points int) {

	fft := pf.PrepareFFT(points)

	if _, ex := pf.basesClassic[baseLengthPair{baseIndex: 0, Length: fft.Size}]; ex {
		return
	}

	gates := fft.Size
	lagreangeBases := make([]Poly, gates)
	invGateNumber := pf.F.Inverse(new(big.Int).SetInt64(int64(gates)))
	lambda := pf.MulScalar(fft.Domain, invGateNumber)
	lambda = pf.MulScalar(lambda, fft.RootOfUnity)
	index := ((gates >> 1) + 1) % gates
	lagreangeBases[1], _ = pf.Div(lambda, Poly{fft.RootOfUnitys[index], bigOne})

	var permute = func(f int, in Poly) (out Poly) {
		out = make(Poly, len(in))
		out[0] = in[0]
		for i := 1; i < len(in); i++ {
			out[i] = in[(i*f)%len(in)]
		}
		return out
	}

	var split = func(f int, in Poly) (out Poly) {
		outt := make(Poly, len(in)/2)
		outt[0] = in[0]
		for i := 1; i < len(in)/2; i++ {
			outt[i] = in[(i*f)%len(in)]
		}
		return append(outt, outt...)
	}
	start := lagreangeBases[1]
	for i := 1; i < gates; i *= 2 {
		lagreangeBases[(i*2)%gates] = split(2, start)
		ctr := 1
		for j := i; j < gates; j += 2 * i {
			ind := (j + (2 * i)) % gates
			vv := 1 + (2 * ctr)
			next := permute(vv, start)
			lagreangeBases[ind] = next
			ctr += 1
		}
		start = lagreangeBases[(i*2)%gates]
	}
	for k, v := range lagreangeBases {
		pf.basesClassic[baseLengthPair{baseIndex: k, Length: gates}] = v
	}

}

// LagrangeInterpolation performs the Lagrange Interpolation / Lagrange Polynomials operation
func (pf *PolynomialField) PrecomputeLagrange(totalPoints int) {
	// https://en.wikipedia.org/wiki/Lagrange_polynomial

	var base = func(pointPos int) {
		var r Poly
		facBig := big.NewInt(1)

		for i := 0; i < pointPos; i++ {
			facBig = pf.F.Mul(facBig, big.NewInt(int64(pointPos-i)))
		}
		for i := pointPos + 1; i < totalPoints; i++ {
			facBig = pf.F.Mul(facBig, big.NewInt(int64(pointPos-i)))
		}
		hf := pf.F.Inverse(facBig)

		r = Poly{new(big.Int).SetInt64(1)}
		for i := 0; i < totalPoints; i++ {
			if i != pointPos {
				r = pf.MulNaive(r, Poly{big.NewInt(int64(-i)), big.NewInt(int64(1))})
			}
		}
		r = pf.MulScalar(r, hf)
		pf.mutex.Lock()
		pf.basesClassic[baseLengthPair{baseIndex: pointPos, Length: totalPoints}] = r
		pf.mutex.Unlock()
	}

	var hardwork = func(start, end int) {
		for i := start; i < end; i++ {
			base(i)
		}
	}
	Parallelize(totalPoints, hardwork)

}

// LagrangeInterpolation performs the Lagrange Interpolation / Lagrange Polynomials operation
func (pf *PolynomialField) LagrangeInterpolation_RootOfUnity(datapoints Poly) (polynom Poly) {

	pf.PrecomputeLagrangeFFT_3(len(datapoints))

	var base = func(pointPos, totalPoints int) (r Poly) {

		if v, ex := pf.basesClassic[baseLengthPair{baseIndex: pointPos, Length: totalPoints}]; ex {
			return v
		}
		panic("asdf")
		return r
	}

	polynom = ArrayOfBigZeros(len(datapoints))

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

func (pf PolynomialField) MulScalar(polynomial Poly, w *big.Int) (scaledPolynomial Poly) {
	scaledPolynomial = make(Poly, len(polynomial))

	var wk = func(start, end int) {
		for i := start; i < end; i++ {
			if polynomial[i].Cmp(bigZero) == 0 {
				scaledPolynomial[i] = new(big.Int).SetInt64(0)
				continue
			}
			scaledPolynomial[i] = pf.F.Mul(w, polynomial[i])
		}
	}
	Parallelize(len(polynomial), wk)

	return
}

func (pf PolynomialField) LinearCombine(polynomials []Poly, w Poly) (scaledPolynomials []Poly) {
	scaledPolynomials = make([]Poly, len(w))
	for i := 0; i < len(w); i++ {
		scaledPolynomials[i] = pf.MulScalar(polynomials[i], w[i])
	}
	return
}

func (pf PolynomialField) PointwiseMultiplication(a Poly, b Poly) (ab Poly) {
	ab = make(Poly, len(a))
	var wk = func(start, end int) {
		for i := start; i < end; i++ {
			ab[i] = pf.F.Mul(a[i], b[i])
		}
	}
	Parallelize(len(a), wk)
	return ab
}
func (pf PolynomialField) PointwiseAdd(a Poly, b Poly) (ab Poly) {
	ab = make(Poly, len(a))
	var wk = func(start, end int) {
		for i := start; i < end; i++ {
			ab[i] = pf.F.Add(a[i], b[i])
		}
	}
	Parallelize(len(a), wk)

	return ab
}
func (pf PolynomialField) PointwiseSub(a Poly, b Poly) (ab Poly) {
	ab = make(Poly, len(a))
	var wk = func(start, end int) {
		for i := start; i < end; i++ {
			ab[i] = pf.F.Sub(a[i], b[i])
		}
	}
	Parallelize(len(a), wk)
	return ab
}

func (pf PolynomialField) AddPolynomials(polynomials []Poly) (sumPoly Poly) {
	sumPoly = Poly{}
	for i := 0; i < len(polynomials); i++ {
		sumPoly = pf.Add(sumPoly, polynomials[i])
	}
	return
}

//EvalPoly Evaluates a polynomial v at position x, using the Horners Rule
func (pf PolynomialField) EvalPoly(v Poly, x *big.Int) *big.Int {
	return pf.F.EvalPoly(v, x)
}
