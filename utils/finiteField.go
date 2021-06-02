package utils

import (
	"bytes"
	"crypto/rand"
	"fmt"
	"math/big"
)

type Fields struct {
	ArithmeticField Fq
	PolynomialField PolynomialField
}

//var Field = PrepareFields(big.NewInt(601), big.NewInt(601))

//PrepareFields For prime r, in order to prove statements about F_r-arithmetic circuit
//satisfiability, one instantiates (G, P, V ) using an elliptic curve E defined over some finite field F_q , where the
//group E(F_q) of F_q-rational points has order r = #E(F q ) (or, more generally, r divides #E(F q )).
func PrepareFields(r *big.Int) Fields {
	// new Finite Field
	fqR := NewFiniteField(r)

	return Fields{
		ArithmeticField: fqR,
		PolynomialField: *NewPolynomialField(fqR),
	}
}

type baseLengthPair struct {
	baseIndex, Length int
}

// Fq is the Z field over modulus Q
type Fq struct {
	Q *big.Int // Q

}

// NewFiniteField generates a new Fq
func NewFiniteField(order *big.Int) Fq {
	if !order.ProbablyPrime(20) {
		panic(fmt.Sprint(order, "is not prime"))
	}
	return Fq{
		order,
	}
}

// Zero returns a Zero value on the Fq
func (fq Fq) Zero() *big.Int {
	return big.NewInt(int64(0))
}

// One returns a One value on the Fq
func (fq Fq) One() *big.Int {
	return big.NewInt(int64(1))
}

func (fq Fq) StringToFieldElement(a string) (v *big.Int, s bool) {
	v, s = new(big.Int).SetString(a, 10)

	return v.Mod(v, fq.Q), s
}

func (fq Fq) ScalarProduct(l, r []*big.Int) (sum *big.Int) {
	if len(l) != len(r) {
		panic("vector lengths missmatch")
	}
	return fq.scalarProduct(l, r)
}
func (fq Fq) scalarProduct(l, r []*big.Int) (sum *big.Int) {
	if len(l) == 0 {
		return bigZero
	}
	return fq.Add(new(big.Int).Mul(l[0], r[0]), fq.ScalarProduct(l[1:], r[1:]))
}

// Add performs an addition on the Fq
func (fq Fq) Add(a, b *big.Int) *big.Int {
	r := new(big.Int).Add(a, b)
	return new(big.Int).Mod(r, fq.Q)
	// return r
}

// Double performs a doubling on the Fq
func (fq Fq) Double(a *big.Int) *big.Int {
	r := new(big.Int).Add(a, a)
	return new(big.Int).Mod(r, fq.Q)
	// return r
}

// Sub performs a subtraction on the Fq
func (fq Fq) Sub(a, b *big.Int) *big.Int {
	r := new(big.Int).Sub(a, b)
	return new(big.Int).Mod(r, fq.Q)
	// return r
}

// Neg returns the additive inverse -a over Fq
func (fq Fq) Neg(a *big.Int) *big.Int {
	m := new(big.Int).Neg(a)
	return new(big.Int).Mod(m, fq.Q)
	// return m
}

// Mul performs a multiplication on the Fq
func (fq Fq) Mul(a, b *big.Int) *big.Int {
	m := new(big.Int).Mul(a, b)
	return new(big.Int).Mod(m, fq.Q)
	// return m
}

// Inverse returns the inverse on the Fq
func (fq Fq) Inverse(a *big.Int) *big.Int {
	if a.Cmp(bigZero) == 0 {
		return a
	}
	return new(big.Int).ModInverse(a, fq.Q)

}

// Div performs the division over the finite field
func (fq Fq) Div(a, b *big.Int) *big.Int {
	d := fq.Mul(a, fq.Inverse(b))
	return new(big.Int).Mod(d, fq.Q)
}

// Square performs a square operation on the Fq
func (fq Fq) Square(a *big.Int) *big.Int {
	m := new(big.Int).Mul(a, a)
	return new(big.Int).Mod(m, fq.Q)
}

// Exp performs the exponential over Fq
//unsafe when e is negative
func (fq Fq) Exp(base *big.Int, e *big.Int) *big.Int {
	return new(big.Int).Exp(base, e, fq.Q)
}

// Exp performs the exponential over Fq
func (fq Fq) ExpInt(base *big.Int, e int64) *big.Int {
	return fq.Exp(base, new(big.Int).SetInt64(e))
}

//EvalPoly Evaluates a polynomial v at position x, using the Horners Rule
func (fq Fq) EvalPoly(v Poly, x *big.Int) *big.Int {

	if x.Cmp(bigZero) == 0 {
		return new(big.Int).Set(v[0])
	}
	if !v.IsSparse() {
		return fq.evalPoly_horner(v, x)
	}
	//for i := int64(1); i < int64(len(v)); i++ {
	//	if v[i].Cmp(bigZero) != 0 {
	//		//note since we expect the polynomials to be sparse, we compute the x^i straight away.. maybe incremental would still be more efficient
	//		r = fq.Add(r, fq.Mul(v[i], fq.Exp(x, big.NewInt(i))))
	//	}
	//}
	return fq.evalSparsePoly(v, x)
}

//EvalPoly Evaluates a sparse polynomial
func (fq Fq) evalSparsePoly(poly Poly, at *big.Int) (result *big.Int) {

	//tree that stores intermediate results of exponent ladder. Key is the exponent. value is at^key
	alredyComputedExponents := NewAvlTree()
	alredyComputedExponents.InsertNoOverwriteAllowed(1, at)
	result = new(big.Int).SetInt64(0)
	for deg, coefficient := range poly {
		if coefficient.Cmp(bigZero) == 0 {
			continue
		}
		rem := uint(deg)
		q := uint(0)
		nextPower := new(big.Int).SetInt64(1)
		//apply a greedy algorithm to tackle the knapsack problem we face her. we want to create the
		//next power by reusing already computed powers, starting from the biggest.
		//example: we want x^15, we have x^8,x^3,x
		//we get x^15 = (x^8)^1 * (x^3)^2 * (x)^1
		for _, highestAlreadyComputedExponent := range alredyComputedExponents.DecendingNodes() {
			q, rem = euclid(rem, highestAlreadyComputedExponent.Key)
			vv := fq.ExpInt(highestAlreadyComputedExponent.Value, int64(q))
			alredyComputedExponents.Insert(q*highestAlreadyComputedExponent.Key, vv)
			nextPower = fq.Mul(nextPower, vv)
			if rem == 0 {
				break
			}
		}
		result = fq.Add(result, fq.Mul(coefficient, nextPower))
	}
	return result
}

//EvalPoly Evaluates a polynomial v at position x, using the Horners Rule
func (fq Fq) evalPoly_horner(v []*big.Int, x *big.Int) *big.Int {
	if len(v) == 1 {
		return v[0]
	}
	return fq.Add(fq.Mul(fq.EvalPoly(v[1:], x), x), v[0])

}

//AdicityBig returns the biggest power of 2, that divides the input i.e. 2^n | in
func AdicityBig(input *big.Int) (twoadicity int) {
	bits := fmt.Sprintf("%b", input)
	ad := 0
	for len(bits) != 0 && string(bits[len(bits)-1]) == "0" {
		bits = bits[:len(bits)-1]
		ad += 1
	}
	return ad
}

//this is the same function as above, but imperative style. the speed difference turned out to be marginally better this way, however FAR LESS ELEGANT
//WE KEEP THIS AS REMINDER HOW SHIT IMPERATIVE PROGRAMMING IS
//func (fq Fq) EvalPoly3(v []*big.Int, x *big.Int) *big.Int {
//	r := new(big.Int).Set(v[len(v)-1])
//	for i := len(v)-2; i >= 0; i-- {
//		if v[i].Cmp(bigZero) != 0 {
//			//note since we expect the polynomials to be sparse, we compute the x^i straight away.. maybe incremental would still be more efficient
//			r = fq.Add(fq.MulNaive(r, x),v[i])
//		}
//
//	}
//	return r
//}

func (fq Fq) Rand() (*big.Int, error) {

	// twoexp := new(big.Int).Exp(big.NewInt(2), big.NewInt(int64(maxbits)), nil)
	// MaxInt := new(big.Int).Sub(twoexp, big.NewInt(1))
	//rand
	maxbits := fq.Q.BitLen()
	b := make([]byte, (maxbits/8)-1)
	// b := make([]byte, 3)
	// b := make([]byte, 3)
	_, err := rand.Read(b)
	if err != nil {
		return nil, err
	}
	r := new(big.Int).SetBytes(b)
	rq := new(big.Int).Mod(r, fq.Q)

	// return r over q, nil
	return rq, nil
}

func (fq Fq) IsZero(a *big.Int) bool {
	return bytes.Equal(a.Bytes(), fq.Zero().Bytes())
}

func (fq Fq) Copy(a *big.Int) *big.Int {
	return new(big.Int).Mod(a, fq.Q)
}

func (fq Fq) Affine(a *big.Int) *big.Int {
	nq := fq.Neg(fq.Q)

	aux := a
	if aux.Cmp(big.NewInt(int64(0))) == -1 { // negative value
		if aux.Cmp(nq) != 1 { // aux less or equal nq
			aux = new(big.Int).Mod(aux, fq.Q)
		}
		if aux.Cmp(big.NewInt(int64(0))) == -1 { // negative value
			aux = new(big.Int).Add(aux, fq.Q)
		}
	} else {
		if aux.Cmp(fq.Q) != -1 { // aux greater or equal nq
			aux = new(big.Int).Mod(aux, fq.Q)
		}
	}
	return aux
}

func (fq Fq) Equal(a, b *big.Int) bool {
	aAff := fq.Affine(a)
	bAff := fq.Affine(b)
	return bytes.Equal(aAff.Bytes(), bAff.Bytes())
}
