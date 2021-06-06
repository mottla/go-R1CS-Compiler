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

func (f Tokens) Len() int {
	return len(f)
}

func (f Tokens) Swap(i, j int) {
	f[i], f[j] = f[j], f[i]
}

func (f Tokens) Less(i, j int) bool {
	if strings.Compare(f[i].String(), f[j].String()) < 0 {
		return false
	}
	return true
}
func (f Token) CopyAndSetMultiplicative(v *big.Int) (n Token) {
	n = f.copy()
	n.value = v
	return
}

func (f Tokens) containsArgument() bool {
	if len(f) == 0 {
		return false
	}
	for _, v := range f {
		if v.isArgument {
			return true
		}
	}
	return false
}

func (f Token) Negate() (n Token) {
	return f.CopyAndSetMultiplicative(new(big.Int).Neg(f.value))
}
func (f Tokens) Negate() (n Tokens) {
	n = make(Tokens, len(f))
	for i, v := range f {
		n[i] = v.Negate()
	}
	return
}
func (t Token) toFactors() Tokens {
	if t.value == nil {
		t.value = bigOne
	}
	return Tokens{t}
}

func (f Tokens) clone() (res Tokens) {
	res = make(Tokens, len(f))
	for k, v := range f {
		res[k] = v.copy()
	}
	return
}

func extractGCD(f Tokens) (Tokens, *big.Int) {

	gcd := f[0].value
	for i := 1; i < len(f); i++ {
		gcd = new(big.Int).GCD(nil, nil, f[i].value, gcd)
	}
	for i := 0; i < len(f); i++ {
		f[i].value = new(big.Int).Div(f[i].value, gcd)

	}
	return f, gcd

}

func hashFactorsToBig(f Tokens) *big.Int {
	sha := sha256.New()
	for _, fac := range f {
		sha.Write([]byte(fac.String()))
	}
	return new(big.Int).SetBytes(sha.Sum(nil))
}

func (fac Tokens) factorSignature() string {
	h := hashFactorsToBig(fac)
	return h.String()[:16]
}

func extractConstant(leftFactors, rightFactors Tokens) (gcd *big.Int, extractedLeftFactors, extractedRightFactors Tokens) {

	mulL, facL := factorSignature(leftFactors)
	mulR, facR := factorSignature(rightFactors)

	res := utils.Field.ArithmeticField.Mul(mulL, mulR)

	return res, facL, facR
}

func factorSignature(facs Tokens) (gcd *big.Int, extractedRightFactors Tokens) {
	facs = facs.clone()
	facs, gcd = extractGCD(facs)
	sort.Sort(facs)
	return gcd, facs
}

//multiplies factor elements and returns the result
//in case the Tokens do not hold any constants and all inputs are distinct, the extractedConstants will be the concatenation of left+right
func mulFactors(leftFactors, rightFactors Tokens) (result Tokens) {

	if len(leftFactors) < len(rightFactors) {
		tmp := leftFactors
		leftFactors = rightFactors
		rightFactors = tmp
	}

	for i, left := range leftFactors {

		for _, right := range rightFactors {

			leftFactors[i] = leftFactors[i].CopyAndSetMultiplicative(mulType(right.value, left.value, right.Type))

		}
	}
	return leftFactors
}
func divideFactors(leftFactors Tokens, rightFactor Token) (result Tokens) {

	for i, left := range leftFactors {

		leftFactors[i] = leftFactors[i].CopyAndSetMultiplicative(divType(rightFactor.value, left.value, rightFactor.Type))

	}
	return leftFactors
}

//adds two Tokens to one iff they are both are constants or of the same variable
func (in *Tokens) Add(a Token) {

	for i, v := range *in {
		if v.equalDescription(a) {
			(*in)[i].value = addType(v.value, a.value, a.Type)
			return
		}
	}
	*in = append(*in, a)

	return

}

//returns the reduced sum of two input factor arrays
//if no reduction was done, it returns the concatenation of the input arrays
func (this Tokens) AddFactors(with Tokens) Tokens {

	for _, facRight := range with {
		this.Add(facRight)
	}

	return this
}

func (from Tokens) primitiveReturnfunction() (gives *function) {
	if len(from) == 0 {
		return &function{}
	}
	if len(from) == 1 {
		return from[0].primitiveReturnfunction()
	}
	panic("")
	//return combineFunctions("+", from[0].primitiveReturnfunction(), from[1:].primitiveReturnfunction())

}

func combineConstraints(operation string, lc, rc *Constraint) *Constraint {
	c := &Constraint{
		Output: Token{
			Type:       ArithmeticOperatorToken,
			Identifier: operation,
		},
		Inputs: []*Constraint{lc, rc}}
	return c
}

func combineFunctions(operation Token, l, r, context *function) *function {

	rmp := NewCircuit("", context)
	if len(l.OutputTypes) != 1 {
		panic("")
	}
	if eq, err := l.hasEqualDescription(r); !eq {
		panic(err)
	}
	if operation.Type == BinaryComperatorToken || operation.Type == BooleanOperatorToken {
		rmp.OutputTypes = []returnTypes{{
			typ: Token{
				Type: BOOL,
			},
		}}
	} else {
		rmp.OutputTypes = l.OutputTypes
	}

	idL, idR := "l", "r"
	rmp.functions[idL] = l
	rmp.functions[idR] = r
	rmp.taskStack.add(&Constraint{
		Output: operation,
		Inputs: []*Constraint{
			{
				Output: Token{
					Type:       FUNCTION_CALL,
					Identifier: idL,
				},
			}, {
				Output: Token{
					Type:       FUNCTION_CALL,
					Identifier: idR,
				},
			}},
	})
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
func addType(l, r *big.Int, tokenType TokenType) *big.Int {
	checkRangeValidity(l, tokenType)
	checkRangeValidity(r, tokenType)
	switch tokenType {
	case BOOL:
		return new(big.Int).Xor(l, r)
	case U8:
		ll, rr := l.Uint64(), r.Uint64()

		return new(big.Int).SetUint64(uint64(uint8(ll) + uint8(rr)))

	case U16:
		ll, rr := l.Uint64(), r.Uint64()

		return new(big.Int).SetUint64(uint64(uint16(ll) + uint16(rr)))

	case U32:
		ll, rr := l.Uint64(), r.Uint64()

		return new(big.Int).SetUint64(uint64(uint32(ll) + uint32(rr)))

	case U64: //cannot be reached. since uint64 conversion would fail anyway
		ll, rr := l.Uint64(), r.Uint64()

		return new(big.Int).SetUint64(ll + rr)

	case FIELD:
		return utils.Field.ArithmeticField.Add(l, r)

	case DecimalNumberToken:

		return utils.Field.ArithmeticField.Add(l, r)
	}
	panic("")
}

func mulType(l, r *big.Int, tokenType TokenType) *big.Int {
	checkRangeValidity(l, tokenType)
	checkRangeValidity(r, tokenType)
	switch tokenType {
	case BOOL:
		return utils.Field.ArithmeticField.Mul(l, r)
	case U8:
		ll, rr := l.Uint64(), r.Uint64()

		return new(big.Int).SetUint64(uint64(uint8(ll) * uint8(rr)))

	case U16:
		ll, rr := l.Uint64(), r.Uint64()

		return new(big.Int).SetUint64(uint64(uint16(ll) * uint16(rr)))

	case U32:
		ll, rr := l.Uint64(), r.Uint64()

		return new(big.Int).SetUint64(uint64(uint32(ll) * uint32(rr)))

	case U64: //cannot be reached. since uint64 conversion would fail anyway
		ll, rr := l.Uint64(), r.Uint64()

		return new(big.Int).SetUint64(ll * rr)

	case FIELD:
		return utils.Field.ArithmeticField.Mul(l, r)
	case DecimalNumberToken:
		return utils.Field.ArithmeticField.Mul(l, r)
	}
	panic("")
}
func divType(l, r *big.Int, tokenType TokenType) *big.Int {
	checkRangeValidity(l, tokenType)
	checkRangeValidity(r, tokenType)
	switch tokenType {
	case BOOL:
		return utils.Field.ArithmeticField.Div(l, r)
	case U8:
		ll, rr := l.Uint64(), r.Uint64()

		return new(big.Int).SetUint64(uint64(uint8(ll) / uint8(rr)))

	case U16:
		ll, rr := l.Uint64(), r.Uint64()

		return new(big.Int).SetUint64(uint64(uint16(ll) / uint16(rr)))

	case U32:
		ll, rr := l.Uint64(), r.Uint64()

		return new(big.Int).SetUint64(uint64(uint32(ll) / uint32(rr)))

	case U64: //cannot be reached. since uint64 conversion would fail anyway
		ll, rr := l.Uint64(), r.Uint64()

		return new(big.Int).SetUint64(ll / rr)

	case FIELD:
		return utils.Field.ArithmeticField.Div(l, r)
	case DecimalNumberToken:

	}
	panic("")
}
