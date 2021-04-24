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

	ArgumentIdentifiers    []returnTypes //the inputs of a circuit are circuits. Tis way we can pass functions as arguments
	SNARK_Public_Statement []string
	Inputs                 []returnTypes
	Outputs                []returnTypes

	//parent function. this function inherits all functions wich are accessible from his anchestors. Recent overload Late
	Context *function

	//this are the functions, that are defined within this function
	functions map[string]*function

	//this will stay. the concept of a taskstack is relevant
	taskStack *watchstack
}

type returnTypes struct {
	functionReturn bool
	fkt            *function
	typ            Token
}

func newCircuit(name string, context *function) *function {

	c := &function{Context: context, Name: name, ArgumentIdentifiers: []string{}, taskStack: newWatchstack(), functions: make(map[string]*function)}

	return c
}
func NewCircuit(name string, context *function) *function {
	return &function{
		Name:                name,
		ArgumentIdentifiers: []string{},
		Outputs:             []returnTypes{},
		Inputs:              []returnTypes{},
		Context:             context,
		functions:           make(map[string]*function),
		taskStack:           newWatchstack(),
	}

}

func (this *function) description() string {
	res := ""
	for i := 0; i < len(this.ArgumentIdentifiers); i++ {
		v, ex := this.functions[this.ArgumentIdentifiers[i]]
		if !ex {
			panic("cannot happen")
		}
		res += v.

		if !v.hasEqualDescription(v2) {
			return false
		}

	}

	for i := 0; i < len(this.Outputs); i++ {
		l, r := this.Outputs[i], thenThat.Outputs[i]
		if !l.functionReturn && !r.functionReturn {
			if l.typ.Type != r.typ.Type {
				return false
			}
			continue
		}
		if l.functionReturn && r.functionReturn {
			if !l.fkt.hasEqualDescription(r.fkt) {
				return false
			}
			continue
		}
		return false
	}
}

func (this *function) hasEqualDescription(thenThat *function) (answer bool) {
	if len(this.ArgumentIdentifiers) != len(thenThat.ArgumentIdentifiers) {
		return false
	}
	if len(this.Outputs) != len(thenThat.Outputs) {
		return false
	}

	for i := 0; i < len(this.ArgumentIdentifiers); i++ {
		v, ex := this.functions[this.ArgumentIdentifiers[i]]
		if !ex {
			panic("cannot happen")
		}
		v2, ex2 := thenThat.functions[thenThat.ArgumentIdentifiers[i]]
		if !ex2 {
			panic("cannot happen")
		}

		if !v.hasEqualDescription(v2) {
			return false
		}

	}

	for i := 0; i < len(this.Outputs); i++ {
		l, r := this.Outputs[i], thenThat.Outputs[i]
		if !l.functionReturn && !r.functionReturn {
			if l.typ.Type != r.typ.Type {
				return false
			}
			continue
		}
		if l.functionReturn && r.functionReturn {
			if !l.fkt.hasEqualDescription(r.fkt) {
				return false
			}
			continue
		}
		return false
	}
	return true
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
		return nil
	}
	clone = newCircuit(circ.Name, circ.Context)
	argumentIdentifiers := make([]string, len(circ.ArgumentIdentifiers))

	copy(argumentIdentifiers, circ.ArgumentIdentifiers)
	clone.ArgumentIdentifiers = argumentIdentifiers

	outputs := make([]returnTypes, len(circ.Outputs))
	clone.Outputs = outputs

	for k, v := range circ.Outputs {
		outputs[k] = returnTypes{
			functionReturn: v.functionReturn,
			fkt:            v.fkt.flatCopy(),
			typ:            v.typ,
		}
	}
	inputs := make([]returnTypes, len(circ.Inputs))
	clone.Inputs = inputs

	for k, v := range circ.Inputs {
		inputs[k] = returnTypes{
			functionReturn: v.functionReturn,
			fkt:            v.fkt.flatCopy(),
			typ:            v.typ,
		}
	}
	for k, v := range circ.functions {
		f := v.flatCopy()
		f.Context = clone
		clone.functions[k] = f
	}

	clone.taskStack = circ.taskStack.clone()
	return
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

func (currentCircuit *function) getFunctionInputs() (oldInputs []*function) {
	for _, name := range currentCircuit.ArgumentIdentifiers {
		oldInputs = append(oldInputs, currentCircuit.functions[name])
	}
	return

}

func (currentCircuit *function) getsLoadedWith(newInputs []*function) (allArgumentsLoaded bool) {
	allArgumentsLoaded = len(currentCircuit.ArgumentIdentifiers) == len(newInputs)
	if len(currentCircuit.ArgumentIdentifiers) < len(newInputs) {
		panic(fmt.Sprintf("%v takes %v arguments, got %v", currentCircuit.Name, len(currentCircuit.ArgumentIdentifiers), len(newInputs)))
	}
	for i := 0; i < len(newInputs); i++ {
		if v, ex := currentCircuit.functions[currentCircuit.ArgumentIdentifiers[i]]; ex {
			if !v.hasEqualDescription(newInputs[i]) {
				panic("assignment missmatch")
			}
			currentCircuit.functions[currentCircuit.ArgumentIdentifiers[i]] = newInputs[i]
			continue
		}
		panic("cannot happen")

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
