package circuitcompiler

import (
	"fmt"
	"math/big"
)

type gateType uint8

const (
	multiplicationGate gateType = iota
	equalityGate
)

type Gate struct {
	gateType gateType
	index    int
	value    MultiplicationGateSignature
	leftIns  factors //leftIns and RightIns after addition gates have been reduced. only multiplication gates remain
	rightIns factors
	output   *big.Int
}

func (g Gate) String() string {
	return fmt.Sprintf("Gate %v : %v  with left %v right %v", g.index, g.value, g.leftIns, g.rightIns)
}
