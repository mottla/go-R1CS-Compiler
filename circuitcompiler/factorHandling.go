package circuitcompiler

import (
	"crypto/sha256"
	"fmt"
	bn256 "github.com/mottla/go-R1CS-Compiler/pairing"
	"github.com/mottla/go-R1CS-Compiler/utils"
	"math/big"
	"sort"
	"strings"
)

var field = utils.NewFiniteField(bn256.Order)
var bigZero = big.NewInt(0)
var bigOne = big.NewInt(1)

type factors []factor

type factor struct {
	typ            Token
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

	str := f.typ.Identifier

	return fmt.Sprintf("(\"%s\"  fac: %v)", str, f.multiplicative)
}

func (f factors) clone() (res factors) {
	res = make(factors, len(f))
	for k, v := range f {
		res[k] = factor{multiplicative: new(big.Int).Set(v.multiplicative), typ: v.typ}
	}
	return
}
func (f factors) isSingleNumber() bool {
	return len(f) == 1 && f[0].typ.Type == NumberToken

}

func extractGCD(f factors) (factors, *big.Int) {

	gcd := f[0].multiplicative
	for i := 1; i < len(f); i++ {
		gcd = new(big.Int).GCD(nil, nil, f[i].multiplicative, gcd)
	}
	for i := 0; i < len(f); i++ {
		f[i].multiplicative = new(big.Int).Div(f[i].multiplicative, gcd)

	}
	return f, gcd

}

func hashFactorsToBig(f factors) *big.Int {
	sha := sha256.New()
	for _, fac := range f {
		sha.Write([]byte(fac.String()))
	}
	return new(big.Int).SetBytes(sha.Sum(nil))
}

func (fac factors) factorSignature() string {
	h := hashFactorsToBig(fac)
	return h.String()[:16]
}

func extractGCD_andSignature(leftFactors factors) (sig MultiplicationGateSignature, extractedLeftFactors factors) {
	mulL, facL := factorSignature(leftFactors)
	return MultiplicationGateSignature{identifier: Token{Type: ARGUMENT, Identifier: facL.factorSignature()}, commonExtracted: mulL}, facL
}

func extractConstant(leftFactors, rightFactors factors) (gcd *big.Int, extractedLeftFactors, extractedRightFactors factors) {

	mulL, facL := factorSignature(leftFactors)
	mulR, facR := factorSignature(rightFactors)

	res := field.Mul(mulL, mulR)

	return res, facL, facR
}

func factorSignature(facs factors) (gcd *big.Int, extractedRightFactors factors) {
	facs = facs.clone()
	facs, gcd = extractGCD(facs)
	sort.Sort(facs)
	return gcd, facs
}

func (fact factors) scalarMultiplyFactors(x *big.Int) (result factors) {
	for _, left := range fact {
		left.multiplicative = field.Mul(left.multiplicative, x)
	}
	return fact
}

//multiplies factor elements and returns the result
//in case the factors do not hold any constants and all inputs are distinct, the extractedConstants will be the concatenation of left+right
func mulFactors(leftFactors, rightFactors factors) (result factors) {

	if len(leftFactors) < len(rightFactors) {
		tmp := leftFactors
		leftFactors = rightFactors
		rightFactors = tmp
	}

	for i, left := range leftFactors {

		for _, right := range rightFactors {

			if left.typ.Type == NumberToken && right.typ.Type&IN != 0 {
				leftFactors[i] = factor{typ: right.typ, multiplicative: field.Mul(right.multiplicative, left.multiplicative)}
				//leftFactors[i] = &factor{typ: right.typ, multiplicative: field.Mul(right.multiplicative, left.multiplicative)}
				continue
			}

			if left.typ.Type&IN != 0 && right.typ.Type == NumberToken {
				leftFactors[i] = factor{typ: left.typ, multiplicative: field.Mul(right.multiplicative, left.multiplicative)}
				//leftFactors[i] = &factor{typ: left.typ, multiplicative: field.Mul(right.multiplicative, left.multiplicative)}
				continue
			}

			if right.typ.Type&left.typ.Type == NumberToken {
				res := field.Mul(right.multiplicative, left.multiplicative)
				leftFactors[i] = factor{typ: Token{Type: NumberToken, Identifier: res.String()}, multiplicative: res}
				//res := field.Mul(right.multiplicative, left.multiplicative)
				//leftFactors[i] = &factor{typ: Token{Type: NumberToken, Identifier: res.String()}, multiplicative: res}
				continue

			}
			//tricky part here
			//this one should only be reached, after a true multiplicationGate had its left and right braches computed. here we
			//a factor can appear at most in quadratic form. we reduce terms a*a^-1 here.
			//if right.typ.Type&left.typ.Type&IN != 0 {
			//	if left.typ.Identifier == right.typ.Identifier {
			//		if right.invert != left.invert {
			//			leftFactors[i] = &factor{typ: Token{Type: NumberToken}, multiplicative: field.Mul(right.multiplicative, left.multiplicative)}
			//			continue
			//		}
			//	}
			//
			//	//rightFactors[i] = factor{typ: CONST, negate: Xor(facRight.negate, facLeft.negate), multiplicative: mul2DVector(facRight.multiplicative, facLeft.multiplicative)}
			//	//continue
			//
			//}
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
func addFactor(facLeft, facRight factor) (couldAdd bool, sum factor) {
	if facLeft.typ.Type&facRight.typ.Type == NumberToken {
		//res := field.Add(facLeft.multiplicative, facRight.multiplicative)
		res := field.Add(facLeft.multiplicative, facRight.multiplicative)
		return true, factor{typ: Token{
			Type:       NumberToken,
			Identifier: res.String(),
		}, multiplicative: res}

	}

	if facLeft.typ.Type == facRight.typ.Type && facLeft.typ.Identifier == facRight.typ.Identifier {
		return true, factor{typ: facRight.typ, multiplicative: field.Add(facLeft.multiplicative, facRight.multiplicative)}

	}
	//panic("unexpected")
	return false, factor{}

}

//returns the reduced sum of two input factor arrays
//if no reduction was done, it returns the concatenation of the input arrays
func addFactors(leftFactors, rightFactors factors) factors {
	var found bool
	res := make(factors, 0, len(leftFactors)+len(rightFactors))
	for _, facLeft := range leftFactors {

		found = false
		for i, facRight := range rightFactors {

			var sum factor
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

	if len(res) == 0 {
		res = []factor{factor{
			typ: Token{
				Type:       NumberToken,
				Identifier: "0",
			},
			multiplicative: bigZero,
		}}
	}
	return res
}
func negateFactors(leftFactors factors) factors {
	for i := range leftFactors {
		//leftFactors[i].multiplicative = field.Neg(leftFactors[i].multiplicative)
		leftFactors[i].multiplicative = leftFactors[i].multiplicative.Neg(leftFactors[i].multiplicative)
		if leftFactors[i].typ.Type == NumberToken {
			leftFactors[i].typ.Identifier = leftFactors[i].multiplicative.String()
		}
	}
	return leftFactors
}
func (fac factors) bitComplexity() (res int) {
	for _, i := range fac {
		res += i.multiplicative.BitLen()
	}
	return res
}

func invertFactors(leftFactors factors) factors {
	leftFactors = leftFactors.clone()
	for i := range leftFactors {
		leftFactors[i].multiplicative = field.Inverse(leftFactors[i].multiplicative)
		if leftFactors[i].typ.Type == NumberToken {
			leftFactors[i].typ.Identifier = leftFactors[i].multiplicative.String()
		}
	}
	return leftFactors
}

func (from factors) primitiveReturnfunction() (gives *function) {
	if len(from) == 0 {
		return &function{}
	}
	if len(from) == 1 {
		return from[0].primitiveReturnfunction()
	}
	return combineFunctions("+", from[0].primitiveReturnfunction(), from[1:].primitiveReturnfunction())
}

func (from factor) primitiveReturnfunction() (gives *function) {

	if from.typ.Type == NumberToken {
		c := from.typ.primitiveReturnfunction()
		c.isNumber = true
		c.value = from.multiplicative
		return c
	}
	if from.multiplicative.Cmp(bigOne) == 0 {
		return from.typ.primitiveReturnfunction()
	}
	rmp := newCircuit(from.typ.Identifier, nil)
	rmp.taskStack.add(&Constraint{
		Output: Token{
			Type: RETURN,
			//Identifier: fmt.Sprintf("%v*%v",from.multiplicative.String(),from.typ.Identifier),
			Identifier: "",
		},
		Inputs: []*Constraint{
			&Constraint{
				Output: Token{
					Type:       ArithmeticOperatorToken,
					Identifier: "*",
				},
			}, &Constraint{
				Output: Token{
					Type:       NumberToken,
					Identifier: from.multiplicative.String(),
				},
			}, &Constraint{
				Output: from.typ,
			},
		},
	})
	return rmp
}

//TODO add assertions
func combineFunctions(operation string, a, b *function) *function {
	if a.isNumber && b.isNumber {
		switch operation {
		case "*":
			f := factor{
				typ: Token{
					Type:       NumberToken,
					Identifier: field.Mul(a.value, b.value).String(),
				},
				multiplicative: field.Mul(a.value, b.value),
			}
			return f.primitiveReturnfunction()
		case "/":
			f := factor{
				typ: Token{
					Type:       NumberToken,
					Identifier: field.Div(a.value, b.value).String(),
				},
				multiplicative: field.Div(a.value, b.value),
			}
			return f.primitiveReturnfunction()
		case "-":
			f := factor{
				typ: Token{
					Type:       NumberToken,
					Identifier: field.Sub(a.value, b.value).String(),
				},
				multiplicative: field.Sub(a.value, b.value),
			}
			return f.primitiveReturnfunction()
		case "+":
			f := factor{
				typ: Token{
					Type:       NumberToken,
					Identifier: field.Add(a.value, b.value).String(),
				},
				multiplicative: field.Add(a.value, b.value),
			}
			return f.primitiveReturnfunction()
		default:

		}
	}

	rmp := newCircuit("", nil)
	rmp.functions[a.Name] = (a)
	rmp.functions[b.Name] = (b)
	rmp.taskStack.add(&Constraint{
		Output: Token{
			Type:       RETURN,
			Identifier: "", //fmt.Sprintf("%v%v%v",a.Name,operation,b.Name),
		},
		Inputs: []*Constraint{
			&Constraint{
				Output: Token{
					Type:       ArithmeticOperatorToken,
					Identifier: operation,
				},
			}, &Constraint{
				Output: Token{
					Type:       FUNCTION_CALL,
					Identifier: a.Name,
				},
			}, &Constraint{
				Output: Token{
					Type:       FUNCTION_CALL,
					Identifier: b.Name,
				},
			}}})
	return rmp
}
