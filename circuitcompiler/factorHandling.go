package circuitcompiler

import (
	"crypto/sha256"
	"fmt"
	"github.com/mottla/go-AlgebraicProgram-SNARK/circuitcompiler/fields"
	bn256 "github.com/mottla/go-AlgebraicProgram-SNARK/circuitcompiler/pairing"
	"math/big"
	"sort"
	"strings"
)

var field = fields.NewFq(bn256.Order)
var bigZero = big.NewInt(0)
var bigOne = big.NewInt(1)

type factors []*factor

type factor struct {
	typ            Token
	invert         bool
	multiplicative *big.Int
}

func (f factors) Len() int {
	return len(f)
}

func (f factors) Swap(i, j int) {
	f[i], f[j] = f[j], f[i]
}

func (f factors) Less(i, j int) bool {
	if strings.Compare(f[i].String(), f[j].String()) < 0 {
		return false
	}
	return true
}

func (f factor) String() string {

	str := f.typ.Value
	if f.invert {
		str += "^-1"
	}
	return fmt.Sprintf("(\"%s\"  fac: %v)", str, f.multiplicative)
}

func (f factors) clone() (res factors) {
	res = make(factors, len(f))
	for k, v := range f {
		res[k] = &factor{multiplicative: new(big.Int).Set(v.multiplicative), typ: v.typ, invert: v.invert}
	}
	return
}

func extractGCD(f factors) (factors, *big.Int) {

	gcd := f[0].multiplicative
	for i := 1; i < len(f); i++ {
		gcd = new(big.Int).GCD(nil, nil, f[i].multiplicative, gcd)
	}
	for i := 0; i < len(f); i++ {
		f[i].multiplicative = field.Div(f[i].multiplicative, gcd)

	}
	return f, gcd

}

func hashToBig(f factors) *big.Int {
	sha := sha256.New()
	for _, fac := range f {
		sha.Write([]byte(fac.String()))
	}
	return new(big.Int).SetBytes(sha.Sum(nil))
}

func extractConstant(leftFactors, rightFactors factors) (sig MultiplicationGateSignature, extractedLeftFactors, extractedRightFactors factors) {

	mulL, facL := factorSignature(leftFactors)
	mulR, facR := factorSignature(rightFactors)

	//we did all this, because multiplication is commutativ, and we want the signature of a
	//mulitplication Gate   factorsSignature(a,b) == factorsSignature(b,a)
	//since we use a cryptographic hash, addition is save enough e.g. collisions are very unlikely
	mul := append(facL, facR...)
	sort.Sort(mul)
	hash := hashToBig(mul)

	res := field.Mul(mulL, mulR)

	return MultiplicationGateSignature{identifier: Token{Value: hash.String()[:16]}, commonExtracted: res}, facL, facR
}

func factorSignature(rightFactors factors) (gcd *big.Int, extractedRightFactors factors) {
	rightFactors = rightFactors.clone()
	rightFactors, gcd = extractGCD(rightFactors)
	sort.Sort(rightFactors)
	return gcd, rightFactors
}

//multiplies factor elements and returns the result
//in case the factors do not hold any constants and all inputs are distinct, the output will be the concatenation of left+right
func mulFactors(leftFactors, rightFactors factors) (result factors) {

	if len(leftFactors) < len(rightFactors) {
		tmp := leftFactors
		leftFactors = rightFactors
		rightFactors = tmp
	}

	for i, left := range leftFactors {

		for _, right := range rightFactors {

			if left.typ.Type == NumberToken && right.typ.Type&IN != 0 {
				leftFactors[i] = &factor{typ: right.typ, invert: right.invert, multiplicative: field.Mul(right.multiplicative, left.multiplicative)}
				continue
			}

			if left.typ.Type&IN != 0 && right.typ.Type == NumberToken {
				leftFactors[i] = &factor{typ: left.typ, invert: left.invert, multiplicative: field.Mul(right.multiplicative, left.multiplicative)}
				continue
			}

			if right.typ.Type&left.typ.Type == NumberToken {
				leftFactors[i] = &factor{typ: left.typ, multiplicative: field.Mul(right.multiplicative, left.multiplicative)}
				continue

			}
			//tricky part here
			//this one should only be reached, after a true multiplicationGate had its left and right braches computed. here we
			//a factor can appear at most in quadratic form. we reduce terms a*a^-1 here.
			if right.typ.Type&left.typ.Type&IN != 0 {
				if left.typ.Value == right.typ.Value {
					if right.invert != left.invert {
						leftFactors[i] = &factor{typ: Token{Type: NumberToken}, multiplicative: field.Mul(right.multiplicative, left.multiplicative)}
						continue
					}
				}

				//rightFactors[i] = factor{typ: CONST, negate: Xor(facRight.negate, facLeft.negate), multiplicative: mul2DVector(facRight.multiplicative, facLeft.multiplicative)}
				//continue

			}
			fmt.Println("")
			panic("unexpected. If this errror is thrown, its probably brcause a true multiplication Gate has been skipped and treated as on with constant multiplication or addition ")

		}

	}

	return leftFactors
}

//returns the absolute value of a signed int and a flag telling if the input was positive or not
//this implementation is awesome and fast (see Henry S Warren, Hackers's Delight)
func abs(n int) (val int, positive bool) {
	y := n >> 63
	return (n ^ y) - y, y == 0
}

//adds two factors to one iff they are both are constants or of the same variable
func addFactor(facLeft, facRight *factor) (couldAdd bool, sum *factor) {
	if facLeft.typ.Type&facRight.typ.Type == NumberToken {
		return true, &factor{typ: Token{
			Type: NumberToken,
		}, multiplicative: field.Add(facLeft.multiplicative, facRight.multiplicative)}

	}
	if facLeft.typ.Type&facRight.typ.Type&IN != 0 && facLeft.typ.Value == facRight.typ.Value {
		return true, &factor{typ: facRight.typ, multiplicative: field.Add(facLeft.multiplicative, facRight.multiplicative)}

	}
	//panic("unexpected")
	return false, &factor{}

}

//returns the reduced sum of two input factor arrays
//if no reduction was done, it returns the concatenation of the input arrays
func addFactors(leftFactors, rightFactors factors) factors {
	var found bool
	res := make(factors, 0, len(leftFactors)+len(rightFactors))
	for _, facLeft := range leftFactors {

		found = false
		for i, facRight := range rightFactors {

			var sum *factor
			found, sum = addFactor(facLeft, facRight)

			if found {
				rightFactors[i] = sum
				break
			}

		}
		if !found {
			res = append(res, facLeft)
		}
	}

	//not that we should keep at leost one 0 factor, in case all are 0
	for _, val := range rightFactors {
		if val.multiplicative.Cmp(bigZero) != 0 {
			res = append(res, val)
		}
	}

	return res
}
func negateFactors(leftFactors factors) factors {
	for i := range leftFactors {
		leftFactors[i].multiplicative = field.Neg(leftFactors[i].multiplicative)
	}
	return leftFactors
}
func invertFactors(leftFactors factors) factors {
	for i := range leftFactors {
		leftFactors[i].multiplicative = field.Inverse(leftFactors[i].multiplicative)
		leftFactors[i].invert = !leftFactors[i].invert
	}
	return leftFactors
}
