package circuitcompiler

import (
	"fmt"
	"math/big"
	"strings"
)

var variableIndicationSign = "@"

// Circuit is the data structure of the compiled circuit
type Circuit struct {
	Inputs          []*Constraint
	Name            string
	rootConstraints *watchstack
	//after reducing
	//constraintMap map[string]*Constraint
	constraintMap map[string]*Constraint

	functions map[string]*Circuit
}

func newWatchstack() *watchstack {

	return &watchstack{
		data:     []*Constraint{},
		watchmap: make(map[string]bool),
	}

}

type watchstack struct {
	data     []*Constraint
	watchmap map[string]bool
}

func (w *watchstack) len() int {
	return len(w.data)
}

func (w *watchstack) remove(c *Constraint) {
	if ex := w.watchmap[c.MD5Signature()]; ex {
		delete(w.watchmap, c.MD5Signature())
		for index, k := range w.data {
			if k.MD5Signature() == c.MD5Signature() {
				if index == len(w.data)-1 {
					w.data = w.data[:index]
					return
				}
				w.data = append(w.data[:index], w.data[index+1:]...)
			}
		}

	}
}

func (w *watchstack) add(c *Constraint) {
	if _, ex := w.watchmap[c.MD5Signature()]; !ex {
		w.watchmap[c.MD5Signature()] = true
		w.data = append(w.data, c)
	}

}

func newCircuit(name string) *Circuit {
	c := &Circuit{Name: name, constraintMap: make(map[string]*Constraint), rootConstraints: newWatchstack(), functions: make(map[string]*Circuit)}
	//c.specialBuild = func(currentCircuit *Circuit, currentConstraint *Constraint, orderedmGates *[]*Gate, negate bool, invert bool,i func(currentCircuit *Circuit, currentConstraint *Constraint, orderedmGates *[]*Gate, negate bool, invert bool) (facs factors, variableEnd bool)) (facs factors, variableEnd bool) {
	//	return i(currentCircuit, currentConstraint, orderedmGates, negate, invert)
	//}
	return c
}

//only identtokens can be arguments
func (c *Circuit) isArgument(in Token) (isArg bool, arg *Constraint) {
	if in.Type == IdentToken {
		for _, v := range c.Inputs {
			if v.Output.Value == in.Value {
				return true, v
			}
		}
	}
	return false, nil
}

func (circ *Circuit) snapshot() (keys []string) {
	for k, _ := range circ.constraintMap {
		keys = append(keys, k)
	}
	return keys
}
func (circ *Circuit) restore(keys []string) {
	tmp := make(map[string]*Constraint)
	for _, k := range keys {
		tmp[k] = circ.constraintMap[k]
	}
	circ.constraintMap = tmp
}

func (circ *Circuit) updateRootsMap(constraint *Constraint) {

	circ._updateRootsMap(constraint)
	circ.rootConstraints.add(constraint)
}

func (circ *Circuit) _updateRootsMap(constraint *Constraint) {

	for _, v := range constraint.Inputs {
		circ._updateRootsMap(v)
		circ.rootConstraints.remove(v)
	}

}

func (circ *Circuit) semanticCheck_RootMapUpdate(constraint *Constraint) *Constraint {

	if v, ex := circ.constraintMap[constraint.Output.Value]; !ex {
		if v == constraint {
			return constraint
		}
	}
	if constraint.Output.Type&(ARGUMENT|NumberToken|binOp|ARRAY_CALL) != 0 {
		return constraint
	}
	for i := 0; i < len(constraint.Inputs); i++ {
		//circ.semanticCheck_RootMapUpdate(constraint.Inputs[i])
		//if v, ex := circ.constraintMap[constraint.Inputs[i].Output.Value]; ex {
		//	*constraint.Inputs[i] = *v
		//	continue
		//}

		constraint.Inputs[i] = circ.semanticCheck_RootMapUpdate(constraint.Inputs[i])
	}

	switch constraint.Output.Type {
	case IF:
		break
	case VARIABLE_OVERLOAD:
		if _, ex := circ.constraintMap[constraint.Output.Value]; !ex {
			panic(fmt.Sprintf("variable %s not declared", constraint.Output.Value))
		}
		circ.constraintMap[constraint.Output.Value] = constraint
		break
	case FUNCTION_CALL:
		//constraint.Output.Value = composeNewFunctionName(constraint)
		break
	case VARIABLE_DECLARE:
		if _, ex := circ.constraintMap[constraint.Output.Value]; ex {
			panic(fmt.Sprintf("variable %s already declared", constraint.Output.Value))
		}
		(constraint.Output.Type) = UNASIGNEDVAR

		circ.constraintMap[constraint.Output.Value] = constraint
		break
	case ARRAY_Define:

		for i := 0; i < len(constraint.Inputs); i++ {
			element := fmt.Sprintf("%s[%v]", constraint.Output.Value, i)
			circ.constraintMap[element] = constraint.Inputs[i]
		}
		return constraint
	case RETURN:
		//constraint.Output.Value= fmt.Sprintf("%s%v",circ.Name,len(constraint.Output.Value))
		constraint.Output.Value = circ.Name

		break
	case UNASIGNEDVAR:
		//TODO break or return
		break
	case IdentToken:
		if v, ex := circ.constraintMap[constraint.Output.Value]; ex {
			return v
			//constraint.Output = v.Output
			//constraint.Inputs = v.Inputs
			break
		}
		panic(fmt.Sprintf("variable %s used but not declared", constraint.Output.Value))
		//circ.constraintMap[constraint.Output.Value] = constraint
		break
	default:
		panic(fmt.Sprintf("not implemented %v", constraint))

	}
	circ.updateRootsMap(constraint)
	return constraint
}

func RegisterFunctionFromConstraint(constraint *Constraint) (c *Circuit) {

	name := constraint.Output.Value
	c = newCircuit(name)

	duplicateMap := make(map[string]bool)
	for _, arg := range constraint.Inputs {

		if _, ex := duplicateMap[arg.Output.Value]; ex {
			panic("argument must be unique ")
		}
		duplicateMap[arg.Output.Value] = true
		c.constraintMap[arg.Output.Value] = arg
	}
	c.Inputs = constraint.Inputs
	return
}

func splitAtNestedEnd(cs []*Constraint) (insideNested, outsideNested []*Constraint, success bool) {

	ctr := 1
	for i, c := range cs {
		if c.Output.Type == FOR || c.Output.Type == FUNCTION_DEFINE_Internal {
			ctr++
		}
		if c.Output.Type == NESTED_STATEMENT_END {
			ctr--
		}
		if ctr == 0 {

			return cs[:i], cs[i:], true
		}
	}
	return
}

func (p *Program) internal(currentCircuit *Circuit, constraintStack []*Constraint) {
	if len(constraintStack) == 0 {
		return
	}
	currentConstraint := constraintStack[0]
	switch currentConstraint.Output.Type {
	case FUNCTION_DEFINE:
		if _, ex := p.functions[currentConstraint.Output.Value]; ex {
			panic(fmt.Sprintf("function %s already declared", currentConstraint.Output.Value))
		}
		currentCircuit = RegisterFunctionFromConstraint(currentConstraint)
		p.functions[currentConstraint.Output.Value] = currentCircuit
		break
	case FUNCTION_DEFINE_Internal:
		insideFunc, outsideFunc, succ := splitAtNestedEnd(constraintStack[1:])
		if !succ {
			panic("unexpected, should be detected at parsing")
		}
		if _, ex := currentCircuit.functions[currentConstraint.Output.Value]; ex {
			panic(fmt.Sprintf("function %s already declared", currentConstraint.Output.Value))
		}
		newFunc := RegisterFunctionFromConstraint(currentConstraint)
		currentCircuit.functions[currentConstraint.Output.Value] = newFunc

		for k, v := range currentCircuit.constraintMap {
			//we keep the arguments this way
			if _, ex := newFunc.constraintMap[k]; !ex {
				newFunc.constraintMap[k] = v
			}
		}

		p.internal(newFunc, insideFunc)

		p.internal(currentCircuit, outsideFunc)
		return
	case FOR:
		//gather stuff, then evaluate
		insideFor, outsideFor, succ := splitAtNestedEnd(constraintStack[1:])
		if !succ {
			panic("unexpected, should be detected at parsing")
		}
		var factorsA, factorsB factors
		var varEndA, varEndB bool
		var A, B *big.Int
	out:
		for {
			factorsA, varEndA = p.build(currentCircuit, currentConstraint.Inputs[0].Inputs[1], &[]*Gate{})
			factorsB, varEndB = p.build(currentCircuit, currentConstraint.Inputs[0].Inputs[2], &[]*Gate{})

			A = factorsA[0].multiplicative
			B = factorsB[0].multiplicative
			if varEndA || varEndB {
				panic("no dynamic looping supported")
			}
			switch currentConstraint.Inputs[0].Inputs[0].Output.Value {
			case "==":
				if A.Cmp(B) != 0 {
					break out
				}
				break
			case "!=":
				if A.Cmp(B) == 0 {
					break out
				}
				break
			case ">":
				if A.Cmp(B) != 1 {
					break out
				}
				break
			case ">=":
				if A.Cmp(B) == -1 {
					break out
				}
				break
			case "<":
				if A.Cmp(B) != -1 {
					break out
				}
				break
			case "<=":
				if A.Cmp(B) == 1 {
					break out
				}
				break
			default:
				panic("unsupported loop condition ")

			}
			snap2 := currentCircuit.snapshot()
			//n := make([]*Constraint,len(insideFor))
			//for i,v:=range insideFor{
			//	n[i]=v.clone()
			//}
			//p.internal(currentCircuit, n)
			p.internal(currentCircuit, insideFor)
			currentCircuit.restore(snap2)
			//a = a+1
			currentCircuit.semanticCheck_RootMapUpdate(currentConstraint.Inputs[1].clone())
			//currentCircuit.restore(snap2)
		}
		//cut of the part within the for loop
		constraintStack = outsideFor

		//currentCircuit.restore(snap1)
		//p.internal(currentCircuit, constraintStack)
		break
	case NESTED_STATEMENT_END:
		//skippp over
		break
	default:
		currentCircuit.semanticCheck_RootMapUpdate(constraintStack[0].clone())

	}
	p.internal(currentCircuit, constraintStack[1:])
}

func (circ *Circuit) currentOutputName() string {

	return composeNewFunction(circ.Name, circ.currentOutputs())
}

//clone returns a deep copy of c
func (c *Constraint) clone() *Constraint {
	in := make([]*Constraint, len(c.Inputs))
	for i, cc := range c.Inputs {
		in[i] = cc.clone()
	}
	return &Constraint{
		Output: c.Output,
		Inputs: in,
	}
}

//currentOutputs returns the composite name of the function/circuit with the recently assigned inputs
func (circ *Circuit) currentOutputs() []string {

	renamedInputs := make([]string, len(circ.Inputs))
	for i, in := range circ.Inputs {
		renamedInputs[i] = in.Output.Value
	}

	return renamedInputs

}

func composeNewFunction(fname string, inputs []string) string {
	builder := strings.Builder{}
	builder.WriteString(fname)
	builder.WriteRune('(')
	for i := 0; i < len(inputs); i++ {
		builder.WriteString(inputs[i])
		if i < len(inputs)-1 {
			builder.WriteRune(',')
		}
	}
	builder.WriteRune(')')
	return builder.String()
}
