package circuitcompiler

import (
	"fmt"
	"math/big"
)

var variableIndicationSign = "@"

// function is the data structure of the compiled circuit
type function struct {
	Name     string
	isNumber bool
	value    *big.Int

	Inputs []string //the inputs of a circuit are circuits. Tis way we can pass functions as arguments

	Outputs []string

	//parent function. this function inherits all functions wich are accessible from his anchestors. Recent overload Late
	Context *function

	//this are the functions, that are defined within this function
	functions map[string]*function

	//this will stay. the concept of a taskstack is relevant
	taskStack *watchstack
	//lets turn this into something we only need for the semantic check
	constraintMap map[string]*Constraint
}

type watchstack struct {
	data []*Constraint
}

func newCircuit(name string, context *function) *function {

	c := &function{Context: context, Name: name, Inputs: []string{}, constraintMap: make(map[string]*Constraint), taskStack: newWatchstack(), functions: make(map[string]*function)}

	return c
}

func RegisterFunctionFromConstraint(constraint *Constraint, context *function) (c *function) {

	name := constraint.Output.Identifier
	c = newCircuit(name, context)

	for _, arg := range constraint.Inputs {
		c.Inputs = append(c.Inputs, arg.Output.Identifier)

		cl := arg.clone()
		cl.Output.Type = FUNCTION_CALL
		c.constraintMap[arg.Output.Identifier] = cl
		//if i add them as functions, then they later can be replaced by functions.
		rmp := newCircuit(arg.Output.Identifier, nil)
		rmp.taskStack.add(&Constraint{
			Output: Token{
				Type:       RETURN,
				Identifier: "",
			},
			Inputs: []*Constraint{arg},
		})
		if _, ex := c.functions[rmp.Name]; ex {
			panic(fmt.Sprintf("argument: %v , is not unique ", rmp.Name))
		}
		c.functions[rmp.Name] = rmp
	}

	return
}

func (currentCircuit *function) preCompile(constraintStack []*Constraint) {
	if len(constraintStack) == 0 {
		return
	}
	currentConstraint := constraintStack[0]

	switch currentConstraint.Output.Type {
	case PUBLIC:
		if currentCircuit.Name != "main" {
			panic("public zkSNARK parameters need to be declared in main function")
		}
	case IF:
		entireIfElse, outsideIfElse := splitAtIfEnd(constraintStack)

		identifier := fmt.Sprintf("%vif", currentCircuit.taskStack.len())

		newFunc := newCircuit(identifier, currentCircuit)

		currentCircuit.functions[identifier] = newFunc

		currentCircuit.taskStack.add(&Constraint{
			Output: Token{
				Type:       IF_FUNCTION_CALL,
				Identifier: identifier,
			},
		})

		firstIf, elseAndRest, succ2 := splitAtNestedEnd(entireIfElse)
		if !succ2 {
			panic("unexpected, should be detected at parsing")
		}

		identifier2 := fmt.Sprintf("%vif", newFunc.taskStack.len())
		ifcircuit := newCircuit("if", currentCircuit)

		newFunc.functions[identifier2] = ifcircuit

		newFunc.taskStack.add(&Constraint{
			Output: Token{
				Type:       IF,
				Identifier: identifier2,
			},
			Inputs: firstIf[0].Inputs,
		})
		currentCircuit.preCompile(outsideIfElse)
		ifcircuit.preCompile(firstIf[1:])
		newFunc.preCompile(elseAndRest)

		return
	case ELSE:

		firstIf, elseAndRest, succ2 := splitAtNestedEnd(constraintStack)
		if !succ2 {
			panic("unexpected, should be detected at parsing")
		}

		identifier2 := fmt.Sprintf("%vif", currentCircuit.taskStack.len())

		ifcircuit := newCircuit("else", currentCircuit)

		currentCircuit.functions[identifier2] = ifcircuit

		currentCircuit.taskStack.add(&Constraint{
			Output: Token{
				Type:       IF,
				Identifier: identifier2,
			},
			Inputs: firstIf[0].Inputs,
		})
		currentCircuit.preCompile(elseAndRest)
		ifcircuit.preCompile(firstIf[1:])

		return
	case IF_ELSE_CHAIN_END:
		break
	case ARRAY_DECLARE:
		if _, ex := currentCircuit.constraintMap[currentConstraint.Output.Identifier]; ex {
			panic(fmt.Sprintf("array %s already declared", currentConstraint.Output.Identifier))
		}

		currentCircuit.constraintMap[currentConstraint.Output.Identifier] = currentConstraint

		for i := 0; i < len(currentConstraint.Inputs); i++ {
			element := fmt.Sprintf("%s[%v]", currentConstraint.Output.Identifier, i)
			constr := &Constraint{
				Output: Token{
					Type:       VARIABLE_DECLARE,
					Identifier: element,
				},
				Inputs: []*Constraint{currentConstraint.Inputs[i]},
			}
			currentCircuit.preCompile([]*Constraint{constr})

			//circ.taskUpdate(constr)
		}
	case VARIABLE_DECLARE:
		if _, ex := currentCircuit.functions[currentConstraint.Output.Identifier]; ex {
			panic(fmt.Sprintf("variable %s already declared", currentConstraint.Output.Identifier))
		}
		rmp := newCircuit(currentConstraint.Output.Identifier, currentCircuit)
		rmp.taskStack.add(&Constraint{
			Output: Token{
				Type:       RETURN,
				Identifier: "",
			},
			Inputs: currentConstraint.Inputs,
		},
		)
		currentCircuit.functions[currentConstraint.Output.Identifier] = rmp
		//is this still necessary..
		currentCircuit.constraintMap[currentConstraint.Output.Identifier] = &Constraint{
			Output: Token{
				Type:       FUNCTION_CALL,
				Identifier: currentConstraint.Output.Identifier,
			},
		}
		//declarations exist only for static semantic check. from now on we treat it like overload
		currentCircuit.taskStack.add(&Constraint{
			Output: Token{
				Type: VARIABLE_OVERLOAD,
			},
			Inputs: []*Constraint{{
				Output: currentConstraint.Output,
			},
				currentConstraint.Inputs[0],
			},
		})
	case FUNCTION_DEFINE:
		insideFunc, outsideFunc, succ := splitAtNestedEnd(constraintStack)
		if !succ {
			panic("unexpected, should be detected at parsing")
		}
		if _, ex := currentCircuit.functions[currentConstraint.Output.Identifier]; ex {
			panic(fmt.Sprintf("function %s already declared", currentConstraint.Output.Identifier))
		}
		if _, ex := currentCircuit.constraintMap[currentConstraint.Output.Identifier]; ex {
			panic(fmt.Sprintf("function %s overloads variable with same name", currentConstraint.Output.Identifier))
		}
		newFunc := RegisterFunctionFromConstraint(currentConstraint, currentCircuit)
		currentCircuit.functions[currentConstraint.Output.Identifier] = newFunc
		//currentCircuit.constraintMap[currentConstraint.Output.Identifier] = &Constraint{
		//	Output: Token{
		//		Type:       FUNCTION_CALL, //if someone accesses the function, he does not necessarily want to call it
		//		Identifier: currentConstraint.Output.Identifier,
		//	},
		//	//Inputs: currentConstraint.Inputs,
		//}
		currentCircuit.preCompile(outsideFunc)
		newFunc.preCompile(insideFunc[1:])
		return
	case FOR:
		//gather stuff, then evaluate
		insideFor, outsideFor, succ := splitAtNestedEnd(constraintStack)
		if !succ {
			panic("unexpected, should be detected at parsing")
		}

		identifier2 := fmt.Sprintf("%vfor", currentCircuit.taskStack.len())

		forCircuit := newCircuit("for", currentCircuit)

		currentCircuit.functions[identifier2] = forCircuit

		currentCircuit.taskStack.add(&Constraint{
			Output: Token{
				Type:       FOR,
				Identifier: identifier2,
			},
			Inputs: currentConstraint.Inputs,
		})
		currentCircuit.preCompile(outsideFor)
		forCircuit.preCompile(insideFor[1:])
		return
	case NESTED_STATEMENT_END:
		//skippp over
		break
	case VARIABLE_OVERLOAD:
		//TODO Is overloading a task?
		currentCircuit.taskStack.add(currentConstraint)

	case FUNCTION_CALL:
		//since function does not need to be defined, we do nothing
		currentCircuit.taskStack.add(currentConstraint)
	case RETURN:
		currentCircuit.taskStack.add(currentConstraint)
	default:

	}
	currentCircuit.contextCheckInputs(currentConstraint)
	currentCircuit.preCompile(constraintStack[1:])
}

func (circ *function) contextCheckInputs(constraint *Constraint) {
	for i := 0; i < len(constraint.Inputs); i++ {
		circ.contextCheck(constraint.Inputs[i])
	}
}
func (circ *function) contextCheck(constraint *Constraint) {

	for i := 0; i < len(constraint.Inputs); i++ {
		circ.contextCheck(constraint.Inputs[i])
	}

	if constraint.Output.Type&(NumberToken|Operator) != 0 {
		return
	}

	switch constraint.Output.Type {
	case ARGUMENT:
		if _, ex := circ.constraintMap[constraint.Output.Identifier]; !ex {
			panic(fmt.Sprintf("variable %s not found", constraint.Output.Identifier))
		}
	case VARIABLE_OVERLOAD:
		panic("unexpected reach")
	case FUNCTION_CALL:
		//if _, ex := circ.findFunctionInBloodline(constraint.Output.Identifier); !ex {
		//	panic(fmt.Sprintf("function %s used but not declared", constraint.Output.Identifier))
		//}
	case VARIABLE_DECLARE:
		panic("unexpected reach")
	case ARRAY_DECLARE:
		panic("unexpected reach")
	case IDENTIFIER_VARIABLE:
		if _, ex := circ.findConstraintInBloodline(constraint.Output.Identifier); !ex {
			if _, ex := circ.findFunctionInBloodline(constraint.Output.Identifier); !ex {
				panic(fmt.Sprintf("variable %s used but not declared", constraint.Output.Identifier))
			}
		}
	case ARRAY_CALL:
		//we handy arra acces completely during execution now. some precompilation checks could be implemented however
		//TODO rethink
		//if _, ex := circ.findConstraintInBloodline(constraint.Output.Identifier); !ex {
		//	panic(fmt.Sprintf("array %s not declared", constraint.Output.Identifier))
		//}
	default:

	}

	return
}

func (currentCircuit *function) findFunctionInBloodline(identifier string) (*function, bool) {
	if currentCircuit == nil {
		return nil, false
	}
	if con, ex := currentCircuit.functions[identifier]; ex {
		return con, true
	}
	return currentCircuit.Context.findFunctionInBloodline(identifier)

}

//TODO maybe I add context to every constraint as its done with the functions. in the ende everything is a function anyways
func (currentCircuit *function) findConstraintInBloodline(identifier string) (*Constraint, bool) {
	if currentCircuit == nil {
		return nil, false
	}
	if con, ex := currentCircuit.constraintMap[identifier]; ex {
		return con, true
	}
	return currentCircuit.Context.findConstraintInBloodline(identifier)

}

func (currentCircuit *function) getCircuitContainingConstraintInBloodline(identifier string) (*function, bool) {
	if currentCircuit == nil {
		return nil, false
	}
	if _, ex := currentCircuit.constraintMap[identifier]; ex {
		return currentCircuit, true
	}
	return currentCircuit.Context.getCircuitContainingConstraintInBloodline(identifier)

}
func (currentCircuit *function) getCircuitContainingFunctionInBloodline(identifier string) (*function, bool) {
	if currentCircuit == nil {
		return nil, false
	}
	if _, ex := currentCircuit.functions[identifier]; ex {
		return currentCircuit, true
	}
	return currentCircuit.Context.getCircuitContainingFunctionInBloodline(identifier)

}

func (circ *function) flatCopy() (clone *function) {
	if circ == nil {
		panic("")
	}
	clone = newCircuit(circ.Name, circ.Context)
	inputs := make([]string, len(circ.Inputs))
	copy(inputs, circ.Inputs)
	clone.Inputs = inputs
	//clone.functions = circ.functions
	//clone.constraintMap = circ.constraintMap
	for k, v := range circ.functions {
		f := v.flatCopy()
		f.Context = clone
		clone.functions[k] = f
	}
	for k, v := range circ.constraintMap {
		clone.constraintMap[k] = v.clone()
	}
	clone.taskStack = circ.taskStack.clone()
	return
}

func (circ *function) snapshot() (keys []string) {
	for k, _ := range circ.constraintMap {
		keys = append(keys, k)
	}
	return keys
}
func (circ *function) restore(keys []string) {
	tmp := make(map[string]*Constraint)
	for _, k := range keys {
		tmp[k] = circ.constraintMap[k]
	}
	circ.constraintMap = tmp
}

func (currentCircuit *function) checkStaticCondition(c *Constraint) (isSatisfied bool) {
	//unelegant...

	var factorsA, factorsB factors
	var A, B *big.Int

	factorsA, _, _ = currentCircuit.compile(c.Inputs[1], newGateContainer())
	factorsB, _, _ = currentCircuit.compile(c.Inputs[2], newGateContainer())

	A = factorsA[0].multiplicative
	B = factorsB[0].multiplicative

	if !factorsA.isSingleNumber() || !factorsB.isSingleNumber() {
		panic("no dynamic looping supported")
	}
	switch c.Inputs[0].Output.Identifier {
	case "==":
		if A.Cmp(B) != 0 {
			return false
		}
		break
	case "!=":
		if A.Cmp(B) == 0 {
			return false
		}
		break
	case ">":
		if A.Cmp(B) != 1 {
			return false
		}
		break
	case ">=":
		if A.Cmp(B) == -1 {
			return false
		}
		break
	case "<":
		if A.Cmp(B) != -1 {
			return false
		}
		break
	case "<=":
		if A.Cmp(B) == 1 {
			return false
		}
		break
	default:
		panic(c.Inputs[0].Output.String())

	}
	return true
}

func newWatchstack() *watchstack {

	return &watchstack{
		data: []*Constraint{},
	}

}

func (w *watchstack) clone() (clone *watchstack) {
	clone = newWatchstack()
	for _, v := range w.data {
		clone.data = append(clone.data, v.clone())
	}
	return
}

func (w *watchstack) len() int {
	return len(w.data)
}

func (w *watchstack) add(c *Constraint) {
	w.data = append(w.data, c)

}
func (w *watchstack) addPrimitiveReturn(tok Token) {
	w.add(&Constraint{
		Output: Token{
			Type:       RETURN,
			Identifier: "",
		},
		Inputs: []*Constraint{
			{
				Output: tok,
			}},
	})
}

func (from Token) primitiveReturnfunction() (gives *function) {
	rmp := newCircuit(from.Identifier, nil)
	rmp.taskStack.addPrimitiveReturn(from)
	return rmp
}

func splitAtIfEnd(cs []*Constraint) (inside, outside []*Constraint) {

	for i, v := range cs {
		if v.Output.Type == IF_ELSE_CHAIN_END {
			return cs[:i], cs[i:]
		}
	}
	panic("unexpected reach")
	return inside, outside
}

func splitAtNestedEnd(cs []*Constraint) (insideNested, outsideNested []*Constraint, success bool) {

	ctr := 0

	for i, c := range cs {
		if c.Output.Type == ELSE || c.Output.Type == FOR || c.Output.Type == FUNCTION_DEFINE || c.Output.Type == IF {
			ctr++
		}
		if c.Output.Type == NESTED_STATEMENT_END {
			ctr--
		}
		if ctr == 0 {
			if i == len(cs)-1 {
				return cs[0:i], outsideNested, true
			}
			return cs[0:i], cs[i+1:], true
		}
	}
	return
}
