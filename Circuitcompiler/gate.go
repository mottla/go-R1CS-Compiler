package Circuitcompiler

import (
	"fmt"
	"github.com/mottla/go-R1CS-Compiler/utils"
	"math/big"
	"sort"
)

type Gate struct {
	identifier string
	leftIns    Tokens //leftIns and RightIns after addition gates have been reduced. only multiplication gates remain
	rightIns   Tokens
	outIns     Tokens
	//extractedConstants *big.Int
	noNewOutput bool

	computeYourselfe func(witness *[]*big.Int, set utils.FastBool, indexMap map[string]int) bool
}

func (g Gate) String() string {
	return fmt.Sprintf("Gate: %v  with left %v right  %v out %v", g.identifier, g.leftIns, g.rightIns, g.outIns)
}

func (gate *Gate) ID() (id string) {
	//TODO should we include the multiplicative in the hash?
	if gate.identifier == "" {
		return gate.setAndGetID()
	}
	return gate.identifier
}

func (gate *Gate) setAndGetID() (id string) {
	//TODO rethink
	//a*b = c
	//b*a = c
	//b= a^-1 * c
	//a*b *c^-1 = 1
	sort.Sort(gate.leftIns)
	sort.Sort(gate.rightIns)
	l := hashFactorsToBig(gate.leftIns)
	r := hashFactorsToBig(gate.rightIns)
	//we Add the hashes of the multiplication part. a cheap way to consider the commutativity in the id creation to avoid duplicates such as a*b and b*a
	lr := new(big.Int).Add(l, r)
	sort.Sort(gate.outIns)
	o := hashFactorsToBig(gate.outIns)
	lr = new(big.Int).Mul(lr, o)
	gate.identifier = lr.String()[:16]
	return gate.identifier
}

// id * id^-1 = 1
func inverseGate(id Tokens) (g *Gate) {
	g = &Gate{
		leftIns: id,
		outIns: Token{
			Type: DecimalNumberToken,
		}.toFactors(),
	}
	g.rightIns = Token{Identifier: g.ID()}.toFactors()
	return
}

func multiplicationGate(left, right Tokens) (g *Gate) {
	g = &Gate{
		leftIns:  left,
		rightIns: right,
	}
	g.outIns = Token{Identifier: g.ID()}.toFactors()
	return
}

//ensures that either left*right=0
func zeroConstraintGate(left, right Tokens) (g *Gate) {
	g = &Gate{
		leftIns:     left,
		rightIns:    right,
		noNewOutput: true,
	}
	return
}

// a/b = c -> a = b*c
func divisionGate(a, b Tokens) (g *Gate) {
	g = &Gate{
		leftIns: b,
		outIns:  a,
	}
	g.rightIns = Token{Identifier: g.ID()}.toFactors()
	return
}

// (1-id)* id = 0
func zeroOrOneGate(id string) (g *Gate) {
	one := Token{
		Type:  ARGUMENT,
		value: bigOne,
	}

	g = &Gate{
		leftIns: Tokens{one,
			Token{
				Identifier: id,
			}.Negate(),
		},
		rightIns: Token{
			Identifier: id,
		}.toFactors(),
		identifier: id,
	}
	return
}

// a xor b = c as arithmetic circuit (asserting that a,b \in {0,1}
// 2a*b =  a + b - c
func xorGate(a, b Token) (g *Gate) {

	var mGate = new(Gate)
	//some dangerous stuff is happening here.. check later dude
	mGate.leftIns = Tokens{a.CopyAndSetMultiplicative(new(big.Int).Mul(a.value, big.NewInt(2)))}
	mGate.rightIns = Tokens{b}
	mGate.outIns = Tokens{a, b}

	xor := Token{Identifier: mGate.ID(), value: bigOne}

	mGate.outIns = append(mGate.outIns, xor.Negate())

	return mGate
}

// a or b = c as arithmetic circuit (asserting that a,b \in {0,1}
// a*b =  a + b - c
func orGate(a, b Token) (g *Gate) {

	var mGate = new(Gate)

	mGate.leftIns = Tokens{a}
	mGate.rightIns = Tokens{b}
	mGate.outIns = Tokens{a, b}

	xor := Token{Identifier: mGate.ID(), value: bigOne}
	mGate.outIns = append(mGate.outIns, xor.Negate())

	return mGate
}

//left * 1 = right
func equalityGate(left, right Tokens) (g *Gate) {
	one := Token{
		Type: DecimalNumberToken,
	}.toFactors()
	g = &Gate{
		leftIns:     left,
		rightIns:    one,
		outIns:      right,
		noNewOutput: true,
	}
	return
}

// (in1+in2+... )*1 = newOut
func summationGate(in Tokens) (g *Gate) {
	one := Token{
		Type: DecimalNumberToken,
	}.toFactors()
	g = &Gate{
		leftIns:  in,
		rightIns: one,
	}
	g.outIns = Token{Identifier: g.ID()}.toFactors()
	return
}

func (gate *Gate) minimizeR1CSDescriptiveComplexity() {

	//g^(e1+e2+..) + (r1+r2+..)*(l1+l2+..) = (c1+c2+..)
	//if g^e is 0, we can try if the constraint
	//l * 1/c = 1/r  or
	//r * 1/c = 1/l  or
	//r * 1/c = 1/l  or
	//1/r*1/l=1/c
	//is the better represenant regarding bit complexity

}
