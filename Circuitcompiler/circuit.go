package Circuitcompiler

import (
	"fmt"
	"math/big"
	"strconv"
)

var variableIndicationSign = "@"

// function is the data structure of the compiled circuit
type function struct {
	Name string

	value *big.Int

	ArgumentIdentifiers    []string //the inputs of a circuit are circuits. Tis way we can pass functions as arguments
	SNARK_Public_Statement []string
	Outputs                []returnTypes

	//parent function. this function inherits all functions wich are accessible from his anchestors. Recent overload Late
	Context *function

	//this are the functions, that are defined within this function
	functions map[string]*function

	//this will stay. the concept of a taskstack is relevant
	taskStack *watchstack
	//lets turn this into something we only need for the semantic check
	constraintMap map[string]*Constraint
}

type returnTypes struct {
	functionReturn bool
	fkt            *function
	typ            Token
}

func newCircuit(name string, context *function) *function {

	c := &function{Context: context, Name: name, ArgumentIdentifiers: []string{}, constraintMap: make(map[string]*Constraint), taskStack: newWatchstack(), functions: make(map[string]*function)}

	return c
}
func NewCircuit(name string, context *function) *function {
	return &function{
		Name:                name,
		ArgumentIdentifiers: []string{},
		Context:             context,
		functions:           make(map[string]*function),
		taskStack:           newWatchstack(),
		constraintMap:       make(map[string]*Constraint),
	}

}

func (this *function) hasEqualReturnTypeThen(that *function) (answer bool) {
	if len(this.Outputs) != len(that.Outputs) {
		return false
	}
	if len(this.Outputs) == 0 {
		return true
	}
	for i := 0; i < len(this.Outputs); i++ {
		l, r := this.Outputs[i], that.Outputs[i]
		if !l.functionReturn && !r.functionReturn {
			if l.typ.Type != r.typ.Type {
				return false
			}
			continue
		}
		if l.functionReturn && r.functionReturn {
			if !l.fkt.hasEqualReturnTypeThen(r.fkt) {
				return false
			}
			continue
		}
		return false
	}
	return true
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

	if constraint.Output.Type&(DecimalNumberToken|Operator) != 0 {
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
		//if _, ex := circ.findFunctionInBloodline(constraint.Output.identifier); !ex {
		//	panic(fmt.Sprintf("function %s used but not declared", constraint.Output.identifier))
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
		//if _, ex := circ.findConstraintInBloodline(constraint.Output.identifier); !ex {
		//	panic(fmt.Sprintf("array %s not declared", constraint.Output.identifier))
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
	inputs := make([]string, len(circ.ArgumentIdentifiers))
	copy(inputs, circ.ArgumentIdentifiers)
	clone.ArgumentIdentifiers = inputs
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

func (currentCircuit *function) checkStaticCondition(c *Constraint) (isStatic, isSatisfied bool) {
	//else condition
	if len(c.Inputs) == 0 {
		return true, true
	}

	var factorsA, factorsB bundle
	var A, B *big.Int

	factorsA, _ = currentCircuit.compile(c.Inputs[1], newGateContainer())
	if factorsA.fac().containsArgument() {
		return false, false

	}
	factorsB, _ = currentCircuit.compile(c.Inputs[2], newGateContainer())
	if factorsB.fac().containsArgument() {
		return false, false
	}
	A = factorsA.fac()[0].multiplicative
	B = factorsB.fac()[0].multiplicative

	switch c.Inputs[0].Output.Identifier {
	case "==":
		if A.Cmp(B) != 0 {
			return true, false
		}
		break
	case "!=":
		if A.Cmp(B) == 0 {
			return true, false
		}
		break
	case ">":
		if A.Cmp(B) != 1 {
			return true, false
		}
		break
	case ">=":
		if A.Cmp(B) == -1 {
			return true, false
		}
		break
	case "<":
		if A.Cmp(B) != -1 {
			return true, false
		}
		break
	case "<=":
		if A.Cmp(B) == 1 {
			return true, false
		}
		break
	default:
		panic(c.Inputs[0].Output.String())

	}
	return true, true
}

type watchstack struct {
	data []*Constraint
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
func (w *watchstack) PopFirst() (bool, *Constraint) {
	if len(w.data) == 1 {
		f := w.data[0]
		w.data = nil
		return true, f
	}
	if w.data == nil || len(w.data) == 0 {
		return false, nil
	}
	f := w.data[0]
	w.data = w.data[1:]
	return true, f
}
func (w *watchstack) PeekLast() (bool, *Constraint) {
	if len(w.data) == 0 {
		return false, nil
	}
	return true, w.data[len(w.data)-1]
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
	rmp.Outputs = []returnTypes{{
		functionReturn: false,
		typ:            from,
	}}
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

func (currentCircuit *function) getFunctionInputs() (oldInputs []*function) {
	for _, name := range currentCircuit.ArgumentIdentifiers {
		oldInputs = append(oldInputs, currentCircuit.functions[name])
	}
	return

}
func (currentCircuit *function) setFunctionInputs(inputs []string, fktInputs []*function) {
	currentCircuit.ArgumentIdentifiers = inputs
	for i, name := range inputs {
		currentCircuit.functions[name] = fktInputs[i]
	}
	return

}

func (currentCircuit *function) getsLoadedWith(newInputs []*function) (allArgumentsLoaded bool) {
	allArgumentsLoaded = len(currentCircuit.ArgumentIdentifiers) == len(newInputs)
	if len(currentCircuit.ArgumentIdentifiers) < len(newInputs) {
		panic(fmt.Sprintf("%v takes %v arguments, got %v", currentCircuit.Name, len(currentCircuit.ArgumentIdentifiers), len(newInputs)))
	}
	for i := 0; i < len(newInputs); i++ {
		currentCircuit.functions[currentCircuit.ArgumentIdentifiers[i]] = newInputs[i]
	}
	currentCircuit.ArgumentIdentifiers = currentCircuit.ArgumentIdentifiers[len(newInputs):]
	return
}

//pani
func (currentCircuit *function) resolveArrayName(id string, inputs []*Constraint) (composedName string) {

	var arrayIdentifier = id
	//if len(c.ArgumentIdentifiers) < 1 {
	//	panic("accessing array index failed")
	//}
	for _, in := range inputs {
		indexFactors, _ := currentCircuit.compile(in, newGateContainer())
		if !indexFactors.fac().isSingleNumber() {
			panic("cannot access array dynamically in an arithmetic circuit currently")
		}
		if len(indexFactors) > 1 {
			panic("unexpected")
		}
		tmp, err := strconv.ParseInt(indexFactors.fac()[0].Typ.Identifier, 10, 64)
		if err != nil || tmp < 0 {
			panic(err.Error())
		}
		arrayIdentifier += fmt.Sprintf("[%v]", tmp)
	}
	return arrayIdentifier
}

func (currentCircuit *function) execute(gateCollector *gateContainer) (bundle, bool) {

	for _, task := range currentCircuit.taskStack.data {
		bundl, rett := currentCircuit.compile(task, gateCollector)
		if rett {
			return bundl, rett
		}
		//gateCollector.completeFunction(f)
	}
	return emptyRets()
}

func (from *Constraint) primitiveReturnfunction() (gives *function) {
	rmp := newCircuit(from.Output.Identifier, nil)
	rmp.taskStack.add(&Constraint{
		Output: Token{
			Type:       RETURN,
			Identifier: "",
		},
		Inputs: []*Constraint{from}})
	return rmp
}
