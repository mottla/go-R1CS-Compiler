package Circuitcompiler

import (
	"crypto/sha256"
	"fmt"
	"github.com/mottla/go-R1CS-Compiler/utils"
	"math/big"
	"sort"
	"strings"
)

var bigZero = big.NewInt(0)
var bigOne = big.NewInt(1)

type factors []factor

type factor struct {
	Typ            Token
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
func (f factor) CopyAndSetMultiplicative(v *big.Int) (n factor) {
	return factor{
		Typ:            f.Typ,
		multiplicative: new(big.Int).Set(v),
	}
}

func (f factors) containsArgument() bool {
	if len(f) == 0 {
		return false
	}
	for _, v := range f {
		if v.Typ.isArgument {
			return true
		}
	}
	return false
}

func (f factor) Negate() (new factor) {
	new = factor{
		Typ:            f.Typ,
		multiplicative: utils.Field.ArithmeticField.Neg(f.multiplicative),
	}
	return new
}
func (f factors) Negate() (new factors) {
	new = make(factors, len(f))
	for i, v := range f {
		new[i] = v.Negate()
	}
	return
}

func (t factor) toFactors() factors {
	return factors{t}
}
func (f factor) String() string {

	str := f.Typ.Identifier

	return fmt.Sprintf("(\"%s\"  fac: %v)", str, f.multiplicative)
}

func (f factor) clone() (res factor) {
	return factor{multiplicative: new(big.Int).Set(f.multiplicative), Typ: f.Typ}

}

func (f factors) clone() (res factors) {
	res = make(factors, len(f))
	for k, v := range f {
		res[k] = v.clone()
	}
	return
}
func (f factors) isSingleNumber() bool {
	return len(f) == 1 && f[0].Typ.Type == DecimalNumberToken

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

func extractConstant(leftFactors, rightFactors factors) (gcd *big.Int, extractedLeftFactors, extractedRightFactors factors) {

	mulL, facL := factorSignature(leftFactors)
	mulR, facR := factorSignature(rightFactors)

	res := utils.Field.ArithmeticField.Mul(mulL, mulR)

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
		left.multiplicative = utils.Field.ArithmeticField.Mul(left.multiplicative, x)
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

			if left.Typ.Type == DecimalNumberToken && right.Typ.Type&IN != 0 {
				leftFactors[i] = factor{Typ: right.Typ, multiplicative: utils.Field.ArithmeticField.Mul(right.multiplicative, left.multiplicative)}
				//leftFactors[i] = &factor{Typ: right.Typ, multiplicative: utils.Field.ArithmeticField.MulNaive(right.multiplicative, left.multiplicative)}
				continue
			}

			if left.Typ.Type&IN != 0 && right.Typ.Type == DecimalNumberToken {
				leftFactors[i] = factor{Typ: left.Typ, multiplicative: utils.Field.ArithmeticField.Mul(right.multiplicative, left.multiplicative)}
				//leftFactors[i] = &factor{Typ: left.Typ, multiplicative: utils.Field.ArithmeticField.MulNaive(right.multiplicative, left.multiplicative)}
				continue
			}

			if right.Typ.Type&left.Typ.Type == DecimalNumberToken {
				res := utils.Field.ArithmeticField.Mul(right.multiplicative, left.multiplicative)
				leftFactors[i] = factor{Typ: Token{Type: DecimalNumberToken, Identifier: res.String()}, multiplicative: res}
				//res := utils.Field.ArithmeticField.MulNaive(right.multiplicative, left.multiplicative)
				//leftFactors[i] = &factor{Typ: Token{Type: DecimalNumberToken, identifier: res.String()}, multiplicative: res}
				continue

			}
			//tricky part here
			//this one should only be reached, after a true multiplicationGate had its left and right braches computed. here we
			//a factor can appear at most in quadratic form. we reduce terms a*a^-1 here.
			//if right.Typ.Type&left.Typ.Type&IN != 0 {
			//	if left.Typ.identifier == right.Typ.identifier {
			//		if right.invert != left.invert {
			//			leftFactors[i] = &factor{Typ: Token{Type: DecimalNumberToken}, multiplicative: utils.Field.ArithmeticField.MulNaive(right.multiplicative, left.multiplicative)}
			//			continue
			//		}
			//	}
			//
			//	//rightFactors[i] = factor{Typ: CONST, negate: Xor(facRight.negate, facLeft.negate), multiplicative: mul2DVector(facRight.multiplicative, facLeft.multiplicative)}
			//	//continue
			//
			//}
			panic("unexpected. If this errror is thrown, its probably brcause a true multiplication Gate has been skipped and treated as on with constant multiplication or addition ")
		}
	}
	return leftFactors
}

//adds two factors to one iff they are both are constants or of the same variable
func addFactor(facLeft, facRight factor) (couldAdd bool, sum factor) {
	if facLeft.Typ.Type&facRight.Typ.Type == DecimalNumberToken {
		//res := utils.Field.ArithmeticField.Add(facLeft.multiplicative, facRight.multiplicative)
		res := utils.Field.ArithmeticField.Add(facLeft.multiplicative, facRight.multiplicative)
		return true, factor{Typ: Token{
			Type:       DecimalNumberToken,
			Identifier: res.String(),
		}, multiplicative: res}

	}

	if facLeft.Typ.Type == facRight.Typ.Type && facLeft.Typ.Identifier == facRight.Typ.Identifier {
		return true, factor{Typ: facRight.Typ, multiplicative: utils.Field.ArithmeticField.Add(facLeft.multiplicative, facRight.multiplicative)}

	}

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
		res = []factor{{
			Typ: Token{
				Type:       DecimalNumberToken,
				Identifier: "0",
			},
			multiplicative: bigZero,
		}}
	}
	return res
}
func negateFactors(in factors) (r factors) {
	r = make(factors, len(in))
	for i := range in {
		//leftFactors[i].multiplicative = utils.Field.ArithmeticField.Neg(leftFactors[i].multiplicative)
		r[i].multiplicative = utils.Field.ArithmeticField.Neg(in[i].multiplicative)
		r[i].Typ = Token{
			Type:       in[i].Typ.Type,
			Identifier: in[i].Typ.Identifier}
		if in[i].Typ.Type == DecimalNumberToken {
			r[i].Typ.Identifier = r[i].multiplicative.String()
		}
	}
	return r
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
		leftFactors[i].multiplicative = utils.Field.ArithmeticField.Inverse(leftFactors[i].multiplicative)
		if leftFactors[i].Typ.Type == DecimalNumberToken {
			leftFactors[i].Typ.Identifier = leftFactors[i].multiplicative.String()
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

	if from.Typ.Type == DecimalNumberToken {
		c := from.Typ.primitiveReturnfunction()
		return c
	}
	if from.multiplicative == nil || from.multiplicative.Cmp(bigOne) == 0 {
		return from.Typ.primitiveReturnfunction()
	}
	rmp := NewCircuit(from.Typ.Identifier, nil)
	rmp.taskStack.add(&Constraint{
		Output: Token{
			Type: RETURN,
			//identifier: fmt.Sprintf("%v*%v",from.multiplicative.String(),from.Typ.identifier),
			Identifier: "",
		},
		Inputs: []*Constraint{
			{
				Output: Token{
					Type:       ArithmeticOperatorToken,
					Identifier: "*",
				},
			}, {
				Output: Token{
					Type:       DecimalNumberToken,
					Identifier: from.multiplicative.String(),
				},
			}, {
				Output: from.Typ,
			},
		},
	})
	return rmp
}

//TODO add assertions
func combineFunctions(operation string, a, b *function) *function {
	//if a.isNumber && b.isNumber {
	//	switch operation {
	//	case "*":
	//		f := factor{
	//			Typ: Token{
	//				Type:       DecimalNumberToken,
	//				Identifier: utils.Field.ArithmeticField.Mul(a.value, b.value).String(),
	//			},
	//			multiplicative: utils.Field.ArithmeticField.Mul(a.value, b.value),
	//		}
	//		return f.primitiveReturnfunction()
	//	case "/":
	//		f := factor{
	//			Typ: Token{
	//				Type:       DecimalNumberToken,
	//				Identifier: utils.Field.ArithmeticField.Div(a.value, b.value).String(),
	//			},
	//			multiplicative: utils.Field.ArithmeticField.Div(a.value, b.value),
	//		}
	//		return f.primitiveReturnfunction()
	//	case "-":
	//		f := factor{
	//			Typ: Token{
	//				Type:       DecimalNumberToken,
	//				Identifier: utils.Field.ArithmeticField.Sub(a.value, b.value).String(),
	//			},
	//			multiplicative: utils.Field.ArithmeticField.Sub(a.value, b.value),
	//		}
	//		return f.primitiveReturnfunction()
	//	case "+":
	//		f := factor{
	//			Typ: Token{
	//				Type:       DecimalNumberToken,
	//				Identifier: utils.Field.ArithmeticField.Add(a.value, b.value).String(),
	//			},
	//			multiplicative: utils.Field.ArithmeticField.Add(a.value, b.value),
	//		}
	//		return f.primitiveReturnfunction()
	//	default:
	//
	//	}
	//}

	rmp := NewCircuit("", nil)
	rmp.functions[a.Name] = a
	rmp.functions[b.Name] = b
	rmp.taskStack.add(&Constraint{
		Output: Token{
			Type:       RETURN,
			Identifier: "", //fmt.Sprintf("%v%v%v",a.Name,operation,b.Name),
		},
		Inputs: []*Constraint{
			{
				Output: Token{
					Type:       UNASIGNEDVAR,
					Identifier: operation,
				},
				Inputs: []*Constraint{
					{
						Output: Token{
							Type:       ArithmeticOperatorToken,
							Identifier: operation,
						},
					}, {
						Output: Token{
							Type:       FUNCTION_CALL,
							Identifier: a.Name,
						},
					}, {
						Output: Token{
							Type:       FUNCTION_CALL,
							Identifier: b.Name,
						},
					}},
			},
		}})
	return rmp
}
func checkRangeValidity(in *big.Int, tokenType TokenType) bool {
	switch tokenType {
	case BOOL:
		if v := in.Uint64(); v > 1 {
			panic(fmt.Sprintf("boolean expected, got %v", v))
		}
	case U8:
		if v := in.Uint64(); v > 1<<8 {
			panic(fmt.Sprintf("Uint8 expected, got %v", v))
		}

	case U16:
		if v := in.Uint64(); v > 1<<16 {
			panic(fmt.Sprintf("Uint16 expected, got %v", v))
		}

	case U32:
		if v := in.Uint64(); v > 1<<32 {
			panic(fmt.Sprintf("uint32 expected, got %v", v))
		}

	case U64: //cannot be reached. since uint64 conversion would fail anyway
		if v := in.Uint64(); v > ^uint64(1) {
			panic(fmt.Sprintf("uint64 expected, got %v", v))
		}
	case FIELD:

	}
	return true
}
