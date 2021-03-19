package Circuitcompiler

type gateContainer struct {
	orderedmGates    []*Gate
	computedFactors  map[string]bool
	splittedElements map[string][]string
}

func newGateContainer() *gateContainer {
	return &gateContainer{
		orderedmGates:    []*Gate{},
		computedFactors:  make(map[string]bool),
		splittedElements: map[string][]string{}, //the array stores the indices of the zeroOrOne check gates
	}
}
func (g *gateContainer) OrderedGates() []*Gate {
	return g.orderedmGates
}

func (g *gateContainer) completeFunction(f factors) {

	//if len f is 1, we can simpl
	if f == nil || len(f) == 0 || f.isSingleNumber() {
		return
	}
	//if the function {..return x*1} , we dont introduce a new gate, as knowledge proof of a multiplication with 1 is trivial and not necessary
	if len(f) == 1 && f[0].multiplicative.Cmp(bigOne) == 0 {
		return
	}
	//the function returned but had a extracted constant
	// example
	//main (x,y){
	//var x = y*y*3
	//return 4*x }
	// x will be set as y*y, and the 3 will be stored aside. each time we access x, we include the 3
	// if we now return, we get the factor 12. we expect the prover to perform the computation x*12
	// Note that some optimization still could be done here. if a gate is not used by others, we could multiply the factor into it
	// insteead of creating a new addition gate
	rootGate := &Gate{
		gateType:   additionGate,
		Identifier: f.factorSignature(),
		leftIns:    f,
		outIns: factors{factor{
			Typ: Token{
				Type:       ARGUMENT,
				Identifier: f.factorSignature(),
			},
			multiplicative: bigOne,
		}},
	}
	g.Add(rootGate)

}

func (g *gateContainer) contains(tok string) bool {
	_, ex := g.computedFactors[tok]
	return ex
}

func (g *gateContainer) Add(gate *Gate) {

	if !g.contains(gate.ID()) {
		g.computedFactors[gate.ID()] = true
		g.orderedmGates = append(g.orderedmGates, gate)
	} else {
		//fmt.Println("saved reuse of "+gate.String())
	}

	return
}
