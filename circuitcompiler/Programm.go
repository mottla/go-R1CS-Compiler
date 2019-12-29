package circuitcompiler

import (
	"fmt"
	"github.com/mottla/go-AlgebraicProgram-SNARK/circuitcompiler/fields"
	"math/big"
	"sort"
)

type MultiplicationGateSignature struct {
	identifier      Token
	commonExtracted *big.Int //if the multiplicationGate had a extractable factor, it will be stored here
}

func (m MultiplicationGateSignature) String() string {
	return fmt.Sprintf("%s extracted %v", m.identifier.String(), m.commonExtracted)
}

type Program struct {
	functions    map[string]*Circuit
	globalInputs []Token
	Fields       fields.Fields //find a better name
	//to reduce the number of multiplication gates, we store each factor signature, and the variable name,
	//so each time a variable is computed, that happens to have the very same factors, we reuse the former
	//it boost setup and proof time
	computedFactors map[Token]MultiplicationGateSignature
}

func newProgram(CurveOrder, FieldOrder *big.Int) (program *Program) {

	program = &Program{
		//functions:    map[string]*Circuit{"scalarBaseMultiply": G, "equal": E},
		functions:    map[string]*Circuit{},
		globalInputs: []Token{},
		Fields:       fields.PrepareFields(CurveOrder, FieldOrder),
	}

	return
}

//returns the cardinality of all main inputs + 1 for the "one" signal
func (p *Program) GlobalInputCount() int {
	return len(p.globalInputs)
}

func (p *Program) CompileToGates() (orderedmGates []*Gate) {

	orderedmGates = []*Gate{}
	p.globalInputs = append(p.globalInputs, Token{
		Type:  NumberToken,
		Value: "1",
	})

	for _, rootC := range p.getMainCircuit().Inputs {
		p.globalInputs = append(p.globalInputs, rootC.Output)
	}

	p.computedFactors = make(map[Token]MultiplicationGateSignature)

	mainCircuit := p.getMainCircuit()

	for i := 0; i < mainCircuit.rootConstraints.len(); i++ {
		f, _ := p.build(mainCircuit, mainCircuit.rootConstraints.data[i], &orderedmGates)
		fmt.Println(f)
		for _, fac := range f {
			for k := range orderedmGates {
				if orderedmGates[k].value.identifier.Value == fac.typ.Value {
					orderedmGates[k].output = p.Fields.ArithmeticField.Inverse(fac.multiplicative)
				}
			}
		}
	}
	return orderedmGates
}

func (p *Program) getMainCircuit() *Circuit {
	return p.functions["main"]
}

func (p *Program) rereferenceFunctionInputs(currentCircuit *Circuit, functionName string, newInputs []*Constraint) (oldInputs []*Constraint, nextContext *Circuit) {

	//first we check if the function is defined internally
	if newCircut, v := currentCircuit.functions[functionName]; v {
		if len(newCircut.Inputs) != len(newInputs) {
			panic("argument size missmatch")
		}
		oldInputss := make([]*Constraint, len(newCircut.Inputs))
		for i, _ := range newCircut.Inputs {
			oldInputss[i] = newCircut.Inputs[i].clone()
			*newCircut.Inputs[i] = *newInputs[i]
		}
		return oldInputss, newCircut

	}

	//now we check if its defined externally. maybe we remove this when we make the program a circuit too.
	if newCircut, v := p.functions[functionName]; v {

		if len(newCircut.Inputs) != len(newInputs) {
			panic("argument size missmatch")
		}
		oldInputss := make([]*Constraint, len(newCircut.Inputs))
		for i, _ := range newCircut.Inputs {
			oldInputss[i] = newCircut.Inputs[i].clone()
			*newCircut.Inputs[i] = *newInputs[i]

		}

		return oldInputss, newCircut
	}
	panic("undeclared function call. check your source")
	return nil, nil
}

//recursively walks through the parse tree to create a list of all
//multiplication gates needed for the QAP construction
//Takes into account, that multiplication with constants and addition (= substraction) can be reduced, and does so
func (p *Program) build(currentCircuit *Circuit, currentConstraint *Constraint, orderedmGates *[]*Gate) (facs factors, variableEnd bool) {

	if len(currentConstraint.Inputs) == 0 {
		switch currentConstraint.Output.Type {
		case NumberToken:

			value, success := p.Fields.ArithmeticField.StringToFieldElement(currentConstraint.Output.Value)
			if !success {
				panic("not a constant")
			}
			return factors{{typ: Token{Type: NumberToken}, multiplicative: value}}, false
		case IdentToken:
			if con, ex := currentCircuit.constraintMap[currentConstraint.Output.Value]; ex {
				return p.build(currentCircuit, con, orderedmGates)
			}
			panic("asdf")
		case UNASIGNEDVAR:
			if con, ex := currentCircuit.constraintMap[currentConstraint.Output.Value]; ex {
				return p.build(currentCircuit, con, orderedmGates)
			}
			panic("asdf")
		case RETURN:
			//panic("empty return not implemented yet")
			fac := &factor{typ: Token{
				Type: NumberToken,
			}, multiplicative: big.NewInt(1)}
			return factors{fac}, false
		case ARGUMENT:
			fac := &factor{typ: Token{
				Type:  ARGUMENT,
				Value: currentConstraint.Output.Value, //+string(hashTraceBuildup),
			}, multiplicative: big.NewInt(1)}
			return factors{fac}, true
		default:
			panic("")
		}
	}

	if currentConstraint.Output.Type == FUNCTION_CALL {
		switch currentConstraint.Output.Value {

		case "equal":
			if len(currentConstraint.Inputs) != 2 {
				panic("equality constraint requires 2 arguments")
			}
			leftClone := currentConstraint.Inputs[0].clone()
			rightClone := currentConstraint.Inputs[1].clone()
			l, _ := p.build(currentCircuit, leftClone, orderedmGates)
			r, _ := p.build(currentCircuit, rightClone, orderedmGates)
			sort.Sort(l)
			sort.Sort(r)
			hl := hashToBig(l)
			hr := hashToBig(r)
			//this way equal(a,b) = equal(b,a). collision is unlikely but possible
			sig := new(big.Int).Add(hl, hr).String()[:16]

			nTok := Token{
				Type:  FUNCTION_CALL,
				Value: sig,
			}
			if _, ex := p.computedFactors[nTok]; !ex {
				rootGate := &Gate{
					gateType: equalityGate,
					index:    len(*orderedmGates),
					value:    MultiplicationGateSignature{identifier: nTok, commonExtracted: bigOne},
					leftIns:  l,
					rightIns: r,
					output:   bigOne,
				}
				p.computedFactors[nTok] = rootGate.value
				*orderedmGates = append(*orderedmGates, rootGate)
			}

			return factors{&factor{
				typ:            nTok,
				multiplicative: bigOne,
			}}, true
			return
		default:
			oldInputss, nextCircuit := p.rereferenceFunctionInputs(currentCircuit, currentConstraint.Output.Value, currentConstraint.Inputs)

			for i := 0; i < nextCircuit.rootConstraints.len()-1; i++ {
				f, _ := p.build(nextCircuit, nextCircuit.rootConstraints.data[i], orderedmGates)
				for _, fac := range f {
					for k := range *orderedmGates {
						if (*orderedmGates)[k].value.identifier.Value == fac.typ.Value {
							(*orderedmGates)[k].output = p.Fields.ArithmeticField.Inverse(fac.multiplicative)
						}
					}
				}
			}

			facss, varEnd := p.build(nextCircuit, nextCircuit.rootConstraints.data[nextCircuit.rootConstraints.len()-1], orderedmGates)
			p.rereferenceFunctionInputs(currentCircuit, currentConstraint.Output.Value, oldInputss)
			return facss, varEnd
		}

	}
	if currentConstraint.Output.Type == ARRAY_CALL {

		if len(currentConstraint.Inputs) != 1 {
			panic("scalarBaseMultiply argument missmatch")
		}
		indexFactors, variable := p.build(currentCircuit, currentConstraint.Inputs[0], orderedmGates)
		if variable {
			panic("cannot access array dynamically in an arithmetic circuit currently")
		}
		if len(facs) > 1 {
			panic("unexpected")
		}
		elementName := fmt.Sprintf("%s[%v]", currentConstraint.Output.Value, indexFactors[0].multiplicative.String())
		if con, ex := currentCircuit.constraintMap[elementName]; ex {
			return p.build(currentCircuit, con, orderedmGates)
		}
		panic(fmt.Sprintf("entry %v not found", elementName))
	}

	if len(currentConstraint.Inputs) == 1 {
		switch currentConstraint.Output.Type {
		case VARIABLE_DECLARE:
			return p.build(currentCircuit, currentConstraint.Inputs[0], orderedmGates)
		case RETURN:
			return p.build(currentCircuit, currentConstraint.Inputs[0], orderedmGates)
		case UNASIGNEDVAR:
			return p.build(currentCircuit, currentConstraint.Inputs[0], orderedmGates)
		case IdentToken:
			return p.build(currentCircuit, currentConstraint.Inputs[0], orderedmGates)
		case VARIABLE_OVERLOAD:
			return p.build(currentCircuit, currentConstraint.Inputs[0], orderedmGates)
		default:
			panic("")
		}
	}

	if len(currentConstraint.Inputs) == 3 {

		left := currentConstraint.Inputs[1]
		right := currentConstraint.Inputs[2]
		operation := currentConstraint.Inputs[0].Output
		var leftFactors, rightFactors factors
		var variableAtLeftEnd, variableAtRightEnd bool

		switch operation.Type {
		case BinaryComperatorToken:
			break
		case BitOperatorToken:
			break
		case BooleanOperatorToken:
			break
		case ArithmeticOperatorToken:
			switch operation.Value {
			case "*":
				leftFactors, variableAtLeftEnd = p.build(currentCircuit, left, orderedmGates)
				rightFactors, variableAtRightEnd = p.build(currentCircuit, right, orderedmGates)
				break
			case "+":
				leftFactors, variableAtLeftEnd = p.build(currentCircuit, left, orderedmGates)
				rightFactors, variableAtRightEnd = p.build(currentCircuit, right, orderedmGates)
				return addFactors(leftFactors, rightFactors), variableAtLeftEnd || variableAtRightEnd
			case "/":
				leftFactors, variableAtLeftEnd = p.build(currentCircuit, left, orderedmGates)
				rightFactors, variableAtRightEnd = p.build(currentCircuit, right, orderedmGates)
				rightFactors = invertFactors(rightFactors)
				break
			case "-":
				leftFactors, variableAtLeftEnd = p.build(currentCircuit, left, orderedmGates)
				rightFactors, variableAtRightEnd = p.build(currentCircuit, right, orderedmGates)
				rightFactors = negateFactors(rightFactors)
				return addFactors(leftFactors, rightFactors), variableAtLeftEnd || variableAtRightEnd
			}
			break
		case AssignmentOperatorToken:
			break
		default:
			panic("unsupported operation")
		}

		if !(variableAtLeftEnd && variableAtRightEnd) {
			return mulFactors(leftFactors, rightFactors), variableAtLeftEnd || variableAtRightEnd
		}
		sig, newLef, newRigh := extractConstant(leftFactors, rightFactors)
		if out, ex := p.computedFactors[sig.identifier]; ex {
			return factors{{typ: out.identifier, multiplicative: sig.commonExtracted}}, true
		}
		//currentConstraint.Output.Value += "@"
		//currentConstraint.Output.Value += sig.identifier.Value
		nTok := Token{
			Type: currentConstraint.Output.Type,
			//Value: currentConstraint.Output.Value + "@" + sig.identifier.Value,
			Value: sig.identifier.Value,
		}
		rootGate := &Gate{
			gateType: multiplicationGate,
			index:    len(*orderedmGates),
			value:    MultiplicationGateSignature{identifier: nTok, commonExtracted: sig.commonExtracted},
			leftIns:  newLef,
			rightIns: newRigh,
			output:   big.NewInt(int64(1)),
		}

		p.computedFactors[sig.identifier] = MultiplicationGateSignature{identifier: nTok, commonExtracted: sig.commonExtracted}
		*orderedmGates = append(*orderedmGates, rootGate)

		return factors{{typ: nTok, multiplicative: sig.commonExtracted}}, true
	}

	panic(currentConstraint)
}

// GenerateR1CS generates the R1CS polynomials from the Circuit
func (p *Program) GatesToR1CS(mGates []*Gate) (r1CS R1CS) {
	// from flat code to R1CS

	offset := len(p.globalInputs)
	//  one + in1 +in2+... + gate1 + gate2 .. + out
	size := offset + len(mGates)
	indexMap := make(map[Token]int)

	for i, v := range p.globalInputs {
		indexMap[v] = i
	}

	for _, v := range mGates {
		if v.gateType == equalityGate {
			size = size - 1
			continue
		}
		if _, ex := indexMap[v.value.identifier]; !ex {
			indexMap[v.value.identifier] = len(indexMap)
		} else {
			panic(fmt.Sprintf("rewriting %v ", v.value))
		}

	}

	insertValue := func(val *factor, arr []*big.Int) {
		if val.typ.Type != NumberToken {
			if _, ex := indexMap[val.typ]; !ex {
				panic(fmt.Sprintf("%v index not found!!!", val))
			}
		}
		value := val.multiplicative
		//not that index is 0 if its a constant, since 0 is the map default if no entry was found
		arr[indexMap[val.typ]] = value
	}

	for _, g := range mGates {

		switch g.gateType {
		case multiplicationGate:
			aConstraint := fields.ArrayOfBigZeros(size)
			bConstraint := fields.ArrayOfBigZeros(size)
			cConstraint := fields.ArrayOfBigZeros(size)

			for _, val := range g.leftIns {
				insertValue(val, aConstraint)
			}

			for _, val := range g.rightIns {
				insertValue(val, bConstraint)
			}
			cConstraint[indexMap[g.value.identifier]] = g.output

			//cConstraint[indexMap[g.value.identifier]] = big.NewInt(int64(1))

			if g.rightIns[0].invert {
				tmp := aConstraint
				aConstraint = cConstraint
				cConstraint = tmp
			}
			r1CS.L = append(r1CS.L, aConstraint)
			r1CS.R = append(r1CS.R, bConstraint)
			r1CS.O = append(r1CS.O, cConstraint)
			break

		case equalityGate:
			aConstraint := fields.ArrayOfBigZeros(size)
			bConstraint := fields.ArrayOfBigZeros(size)
			cConstraint := fields.ArrayOfBigZeros(size)

			for _, val := range g.leftIns {
				insertValue(val, aConstraint)
			}

			for _, val := range g.rightIns {
				insertValue(val, cConstraint)
			}

			bConstraint[0] = big.NewInt(int64(1))

			r1CS.L = append(r1CS.L, aConstraint)
			r1CS.R = append(r1CS.R, bConstraint)
			r1CS.O = append(r1CS.O, cConstraint)
			break
		default:
			panic("no supported gate type")

		}

	}

	return
}
