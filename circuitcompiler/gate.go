package circuitcompiler

import (
	"fmt"
	"math/big"
	"sort"
)

type gateType uint8

const (
	multiplicationGate gateType = iota
	returnMultiplicationGate

	scalarBaseMultiplyGate
	equalityGate
	additionGate
	sumCheckGate
	zeroOrOneGate
)

type Gate struct {
	gateType   gateType
	identifier string
	leftIns    factors //leftIns and RightIns after addition gates have been reduced. only multiplication gates remain
	rightIns   factors
	expoIns    factors
	outIns     factors
	//extractedConstants *big.Int
	noNewOutput bool
	//only for yero or one gates. they carry the information
	arithmeticRepresentatnt Token
}

func (g Gate) String() string {
	return fmt.Sprintf("Gate: %v  with left %v right %v expo %v out %v", g.identifier, g.leftIns, g.rightIns, g.expoIns, g.outIns)
}

func (gate *Gate) ID() (id string) {
	if gate.identifier == "" {
		return gate.setAndGetID()
	}
	return gate.identifier
}

func (gate *Gate) setAndGetID() (id string) {

	sort.Sort(gate.leftIns)
	sort.Sort(gate.rightIns)
	l := hashFactorsToBig(gate.leftIns)
	r := hashFactorsToBig(gate.rightIns)
	//we add the hashes of the multiplication part. a cheap way to consider the commutativity in the id creation to avoid duplicates such as a*b and b*a
	lr := new(big.Int).Add(l, r)
	sort.Sort(gate.expoIns)
	sort.Sort(gate.outIns)
	eo := hashFactorsToBig(append(gate.expoIns, gate.outIns...))
	gate.identifier = new(big.Int).Xor(lr, eo).String()[:16]
	return gate.identifier
}
func (gate *Gate) minimizeR1CSDescriptiveComplexity() {

	//g^(e1+e2+..) + (r1+r2+..)*(l1+l2+..) = (c1+c2+..)
	//if g^e is 0, we can try if the constraint
	//l * 1/c = 1/r  or
	//r * 1/c = 1/l  or
	//r * 1/c = 1/l  or
	//1/r*1/l=1/c
	//is the better represenant regarding bit complexity
	if gate.expoIns == nil {
		//rInv, lInv, cInv := invertFactors(gate.leftIns), invertFactors(gate.rightIns), invertFactors(gate.outIns)

	}
}
