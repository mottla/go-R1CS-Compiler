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
	Q            *big.Int // Q
	bases        map[baseLengthPair]*AvlTree
	basesClassic map[baseLengthPair][]*big.Int
}

// NewFiniteField generates a new Fq
func NewFiniteField(order *big.Int) Fq {
	if !order.ProbablyPrime(20) {
		panic(fmt.Sprint(order, "is not prime"))
	}
	return Fq{
		order,
		make(map[baseLengthPair]*AvlTree),
		make(map[baseLengthPair][]*big.Int),
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
func (fq Fq) MulScalar(base, e *big.Int) *big.Int {
	return fq.Mul(base, e)
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
	res := fq.One()
	rem := fq.Copy(e)
	exp := base

	for !bytes.Equal(rem.Bytes(), bigZero.Bytes()) {
		if BigIsOdd(rem) {
			res = fq.Mul(res, exp)
		}
		exp = fq.Square(exp)
		rem.Rsh(rem, 1)
	}
	return res
}

// Exp performs the exponential over Fq
func (fq Fq) ExpInt(base *big.Int, e uint) *big.Int {
	res := fq.One()
	exp := base

	for e > 0 {
		if e&1 == 1 {
			res = fq.Mul(res, exp)
		}
		exp = fq.Square(exp)
		e >>= 1
	}
	return res
}

//EvalPoly Evaluates a polynomial v at position x, using the Horners Rule
func (fq Fq) EvalPoly(v []*big.Int, x *big.Int) *big.Int {
	r := new(big.Int).Set(v[0])
	for i := int64(1); i < int64(len(v)); i++ {
		if v[i].Cmp(bigZero) != 0 {
			//note since we expect the polynomials to be sparse, we compute the x^i straight away.. maybe incremental would still be more efficient
			r = fq.Add(r, fq.Mul(v[i], fq.Exp(x, big.NewInt(i))))
		}

	}
	return r
}

//returns (twoadicity,remainingFactor) s.t. : remainingFactor * 2^twoadicity == p -1, where p is the field characteristic
func (fq Fq) Adicity() (twoadicity int, remainingFactor *big.Int) {
	adicity := 0
	var b = new(big.Int).SetInt64(1)
	b.Lsh(b, uint(fq.Q.BitLen())) // 1<<bitlen*2 == 2^2*bitlen
	remainingFactor = new(big.Int)
	for i := fq.Q.BitLen(); i >= 0; i-- {
		if new(big.Int).Mod(fq.Q, b).Cmp(bigOne) == 0 {
			adicity = i
			remainingFactor.Div(new(big.Int).Sub(fq.Q, bigOne), b)
			break
		}
		b.Rsh(b, 1)
	}
	if new(big.Int).Mul(remainingFactor, b).Cmp(new(big.Int).Sub(fq.Q, bigOne)) != 0 {
		panic("c * 2^adicity != p -1 ")
	}
	if adicity == 0 {
		fmt.Print("warning. adicity is 0")
	}
	return adicity, remainingFactor
}

//this is the same function as above, but imperative style. the speed difference turned out to be marginally better this way, however FAR LESS ELEGANT
//WE KEEP THIS AS REMINDER HOW SHIT IMPERATIVE PROGRAMMING IS
//func (fq Fq) EvalPoly3(v []*big.Int, x *big.Int) *big.Int {
//	r := new(big.Int).Set(v[len(v)-1])
//	for i := len(v)-2; i >= 0; i-- {
//		if v[i].Cmp(bigZero) != 0 {
//			//note since we expect the polynomials to be sparse, we compute the x^i straight away.. maybe incremental would still be more efficient
//			r = fq.Add(fq.Mul(r, x),v[i])
//		}
//
//	}
//	return r
//}

func (fq Fq) Rand() (*big.Int, error) {

	// twoexp := new(big.Int).Exp(big.NewInt(2), big.NewInt(int64(maxbits)), nil)
	// maxInt := new(big.Int).Sub(twoexp, big.NewInt(1))
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
