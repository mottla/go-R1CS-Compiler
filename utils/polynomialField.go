package utils

import (
	"math/big"
	"math/bits"
)

var bigZero = big.NewInt(int64(0))
var bigOne = big.NewInt(int64(1))

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

func maxInt(a, b int) int {
	if a > b {
		return a
	}
	return b
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

//returns (x-omega^0)(x-omega^1)..(x-omega^(len-1))
func (pf PolynomialField) DomainPolynomial_OverUnityRoos(roots []*big.Int) []*big.Int {
	Domain := []*big.Int{pf.F.Neg(roots[0]), big.NewInt(int64(1))}

	for i := 1; i < len(roots); i++ {
		Domain = pf.Mul(
			Domain,
			[]*big.Int{
				pf.F.Neg(roots[i]), big.NewInt(int64(1)),
			})
	}
	return Domain
}

//todo be clone or overwrite?
func (pf PolynomialField) shift(shift *big.Int, invert bool, inputValues []*big.Int) (shiftedValues []*big.Int) {
	if shift == nil {
		return inputValues
	}
	in := new(big.Int).Set(shift)
	if invert {
		in = pf.F.Inverse(in)
	}
	res := new(big.Int).Set(in)
	for k, v := range inputValues {
		inputValues[k] = pf.F.Mul(res, v)
		res = pf.F.Mul(res, in)
	}

	return inputValues
}

type FFT_PrecomputedParas struct {
	RootOfUnity                  *big.Int
	RootOfUnitys_SquareSteps     []*big.Int // w,w^2,w^4,w^8..
	RootOfUnity_inv              *big.Int
	RootOfUnity_invs_SquareSteps []*big.Int // w^-1,w^-2,w^-4,w^-8..
	RootOfUnitys                 []*big.Int // 1,w,w^2,w^3,w^4..,
	RootOfUnity_invs             []*big.Int // 1,w^-1,w^-2,w^-3,w^-4..
	Size                         int
	pf                           *PolynomialField
}

func (pf *PolynomialField) PrepareFFT(length uint) *FFT_PrecomputedParas {
	paras := new(FFT_PrecomputedParas)
	paras.pf = pf
	adicity, _ := pf.F.Adicity()
	rootOfUnity, _ := new(big.Int).SetString("19103219067921713944291392827692070036145651957329286315305642004821462161904", 10)
	//now find the closest possible power of two, to fill up the datapoints
	bit := bits.Len(nextPowerOfTwo(length))
	// 1000 = 1 << 3, so we remove 1 from bit
	bit = bit - 1
	if adicity < bit {
		panic("field has two low adicity to support fft for that many values")
	}
	paras.Size = bit
	//now determine the root of unity
	// exponent = c*2^(adicity-(bit+1))
	// now for any k in Field, (k^(exponent))^2^(bit+1)==1 mod P
	//fmt.Println(c.String())
	exponent := new(big.Int).Lsh(new(big.Int).SetInt64(1), uint(adicity-bit-1))

	alphaSqrt := pf.F.Exp(rootOfUnity, exponent)
	//fmt.Println(alphaSqrt.String())
	alpha := pf.F.Mul(alphaSqrt, alphaSqrt)
	alphaInv := pf.F.Inverse(alpha)
	if pf.F.Exp(alpha, new(big.Int).Lsh(new(big.Int).SetInt64(1), uint(bit))).Cmp(bigOne) != 0 {
		panic("(k^(exponent))^2^(bit+1) != 1 mod P ")
	}

	bigAlphas_SS := make([]*big.Int, bit)
	bigAlphas_Inv_SS := make([]*big.Int, bit)
	bigAlphas_Inv := make([]*big.Int, 1<<(bit-1))
	bigAlphas := make([]*big.Int, 1<<(bit-1))

	bigAlphas_SS[0] = alpha
	bigAlphas_Inv_SS[0] = alphaInv
	for i := 1; i < bit; i++ {
		bigAlphas_SS[i] = pf.F.Mul(bigAlphas_SS[i-1], bigAlphas_SS[i-1])
		bigAlphas_Inv_SS[i] = pf.F.Mul(bigAlphas_Inv_SS[i-1], bigAlphas_Inv_SS[i-1])
	}

	bigAlphas[0] = new(big.Int).SetInt64(1)
	bigAlphas_Inv[0] = new(big.Int).SetInt64(1)
	for i := 1; i < len(bigAlphas); i++ {
		bigAlphas[i] = pf.F.Mul(bigAlphas[i-1], alpha)
		bigAlphas_Inv[i] = pf.F.Mul(bigAlphas_Inv[i-1], alphaInv)
	}

	paras.RootOfUnity = alpha
	paras.RootOfUnity_inv = alphaInv
	paras.RootOfUnity_invs_SquareSteps = bigAlphas_Inv_SS
	paras.RootOfUnitys_SquareSteps = bigAlphas_SS
	paras.RootOfUnitys = bigAlphas
	paras.RootOfUnity_invs = bigAlphas_Inv

	if pf.F.Exp(bigAlphas_SS[len(bigAlphas_SS)-1], new(big.Int).SetInt64(2)).Cmp(bigOne) != 0 {
		panic("cannot happen")
	}

	return paras
}

func (p *FFT_PrecomputedParas) InvDFFT(ValuesAtRoots []*big.Int, shift *big.Int) (coefficients []*big.Int) {
	c1 := make(chan []*big.Int)
	go p._dfft(p.RootOfUnity_invs_SquareSteps, p.RootOfUnity_invs, p.extendPolyToDomain(ValuesAtRoots), c1)
	coefficients = <-c1
	div := new(big.Int).SetInt64(int64(len(coefficients)))
	for i, v := range coefficients {
		coefficients[i] = p.pf.F.Div(v, div)
	}
	//return p.pf.shift(shift, true, coefficients)
	return coefficients
}

func (p *FFT_PrecomputedParas) DFFT(polynomial []*big.Int, shift *big.Int) (evaluatedAtRoots []*big.Int) {
	c1 := make(chan []*big.Int)
	go p._dfft(p.RootOfUnitys_SquareSteps, p.RootOfUnitys, p.extendPolyToDomain(polynomial), c1)
	return <-c1
	//return p.pf.shift(shift, false, valuesAtRoots)
}

func (p *FFT_PrecomputedParas) extendPolyToDomain(in []*big.Int) []*big.Int {
	if len(in) < 1<<p.Size {
		rest := (1 << p.Size) - len(in)
		//fmt.Printf("\npolysize %v, filled up to next power of two 2^%v. Add %v dummy values", len(polynomial), bit, rest)
		in = append(in, ArrayOfBigZeros(rest)...)
	}
	return in
}

func (p *FFT_PrecomputedParas) _dfft(bigAlphas_SS, bigAlphas, data []*big.Int, in chan []*big.Int) {
	if len(data) == 1 {
		in <- data
		return
	}
	even := []*big.Int{}
	odd := []*big.Int{}
	for k, _ := range data {
		if k%2 == 1 {
			odd = append(odd, data[k])
		} else {
			even = append(even, data[k])
		}
	}

	removeUneven := make([]*big.Int, 0, len(bigAlphas)/2)
	for k, v := range bigAlphas {
		if k%2 == 0 {
			removeUneven = append(removeUneven, v)
		}
	}
	c1, c2 := make(chan []*big.Int, 1), make(chan []*big.Int, 1)

	go p._dfft(bigAlphas_SS[1:], removeUneven, even, c1)
	go p._dfft(bigAlphas_SS[1:], removeUneven, odd, c2)
	y, y2 := <-c1, <-c2

	res := make([]*big.Int, len(data))
	for k := 0; k < len(data)/2; k++ {
		wy := p.pf.F.Mul(bigAlphas[k], y2[k])
		res[k] = p.pf.F.Add(y[k], wy)
		res[k+(len(data)/2)] = p.pf.F.Sub(y[k], wy)
	}
	in <- res
	return
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

func (pf PolynomialField) MulScalar(polynomial []*big.Int, w *big.Int) (scaledPolynomial []*big.Int) {
	scaledPolynomial = make([]*big.Int, len(polynomial))
	for i := 0; i < len(polynomial); i++ {
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

func nextPowerOfTwo(n uint) uint {
	p := uint(1)
	if (n & (n - 1)) == 0 {
		return n
	}
	for p < n {
		p <<= 1
	}
	return p
}
