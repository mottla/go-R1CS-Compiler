package Circuitcompiler

import (
	"fmt"
	"math/big"
	"strings"
)

var variableIndicationSign = "@"

// function is the data structure of the compiled circuit
type function struct {
	Name string

	InputIdentifiers       []string //the inputs of a circuit are circuits. Tis way we can pass functions as arguments
	SNARK_Public_Statement []string
	InputTypes             []returnTypes
	OutputTypes            []returnTypes

	//parent function. this function inherits all functions wich are accessible from his anchestors. Recent overload Late
	Context *function
	//
	Dimension            []int64
	skipLoadVerification bool //predeclared functions
	//this are the functions (signatures only!), that are defined within this function
	functions map[string]*function

	//this will stay. the concept of a taskstack is relevant
	taskStack *watchstack
}

type returnTypes struct {
	functionReturn bool
	fkt            *function
	typ            Token
}

func (r returnTypes) String() string {
	if r.functionReturn {
		return fmt.Sprintf("%v", r.fkt.description())
	}
	return fmt.Sprintf("%v", r.typ.getType())
}

func NewCircuit(name string, context *function) *function {
	return &function{
		Name:             name,
		InputIdentifiers: []string{},
		OutputTypes:      []returnTypes{},
		InputTypes:       []returnTypes{},
		Context:          context,
		functions:        make(map[string]*function),
		taskStack:        newWatchstack(),
	}

}

func (this *function) description() string {
	if this == nil {
		return ""
	}
	res := "("
	for _, v := range this.InputTypes {
		res += v.String()
		res += ","
	}
	res += ")->("
	for _, v := range this.OutputTypes {
		res += v.String()
		res += ","
	}
	res += ")"
	return res
}

func (a returnTypes) compare(in returnTypes) (bool, string) {
	l := a.String()
	r := in.String()
	if strings.EqualFold(l, r) {
		return true, ""
	}

	return false, fmt.Sprintf("expected %v, got %v", l, r)
}
func (v *function) outputs() []returnTypes {
	//the function has not been fully loaded. it therefor returns itself
	if len(v.InputIdentifiers) != 0 {
		return []returnTypes{{
			functionReturn: true,
			fkt:            v}}
	}
	return v.OutputTypes
}

func (f *function) HasBooleanOutput() (answer bool, error string) {

	if len(f.OutputTypes) != 1 {
		return false, "bool-output type function  expected"
	}

	return f.OutputTypes[0].compare(returnTypes{
		functionReturn: false,
		fkt:            nil,
		typ: Token{
			Type: BOOL,
		},
	})

}
func (f *function) HasIntegerOutput() (answer bool, error string) {

	if len(f.OutputTypes) != 1 {
		return false, "integer-output type function  expected"
	}

	return f.OutputTypes[0].compare(returnTypes{
		functionReturn: false,
		fkt:            nil,
		typ: Token{
			Type: FIELD,
		},
	})

}

func (f *function) checkIfReturnsMatchHeader(thenThese []*function) (answer bool, error string) {
	nrReturns := 0

	collecteOutputs := []returnTypes{}
	for _, v := range thenThese {
		//todo we need to find a solution to the function without arguments problem..
		out := v.outputs()
		nrReturns += len(out)
		collecteOutputs = append(collecteOutputs, out...)
	}

	if len(f.OutputTypes) != nrReturns {
		return false, fmt.Sprintf("expected %v return values, got %v", len(f.OutputTypes), len(thenThese))
	}

	for i, v := range f.OutputTypes {
		if b, err := v.compare(collecteOutputs[i]); !b {
			return false, err
		}
	}

	return true, ""
}

func (this *function) hasEqualDescription2(thenThat returnTypes) (answer bool, error string) {
	l := this.description()
	r := ""
	if !thenThat.functionReturn {
		r = thenThat.typ.primitiveReturnfunction().description()
	} else {
		r = thenThat.fkt.description()
	}

	if strings.EqualFold(l, r) {
		return true, ""
	}

	return false, fmt.Sprintf("Type missmatch:  %v vs. %v", l, r)
}
func (this *function) hasEqualDescription(thenThat *function) (answer bool, error string) {
	l := this.description()
	r := thenThat.description()
	if strings.EqualFold(l, r) {
		return true, ""
	}

	return false, fmt.Sprintf("%v expects %v, got %v", this.Name, l, r)
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
	clone = NewCircuit(circ.Name, circ.Context)
	argumentIdentifiers := make([]string, len(circ.InputIdentifiers))
	clone.skipLoadVerification = circ.skipLoadVerification
	copy(argumentIdentifiers, circ.InputIdentifiers)
	clone.Dimension = make([]int64, len(circ.Dimension))
	copy(clone.Dimension, circ.Dimension)

	clone.InputIdentifiers = argumentIdentifiers

	outputs := make([]returnTypes, len(circ.OutputTypes))
	clone.OutputTypes = outputs

	for k, v := range circ.OutputTypes {
		outputs[k] = v

	}
	inputs := make([]returnTypes, len(circ.InputTypes))
	clone.InputTypes = inputs

	for k, v := range circ.InputTypes {
		inputs[k] = v
	}
	for k, v := range circ.functions {
		clone.functions[k] = v
	}

	clone.taskStack = circ.taskStack
	return
}

func (currentCircuit *function) checkStaticCondition(c *Constraint) (isStatic, isSatisfied bool) {

	var factorsA, factorsB bundle
	var A, B *big.Int

	factorsA, _ = currentCircuit.compile(c.Inputs[0], newGateContainer())
	if factorsA.fac().containsArgument() {
		return false, false

	}
	factorsB, _ = currentCircuit.compile(c.Inputs[1], newGateContainer())
	if factorsB.fac().containsArgument() {
		return false, false
	}
	A = factorsA.fac()[0].value
	B = factorsB.fac()[0].value

	switch c.Output.Identifier {
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
		panic(fmt.Sprintf("unknown operation %v", c.Inputs[0].Output.Identifier))

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
	if c.Output.Type == 0 {
		return
	}
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
	rmp := NewCircuit(from.Identifier, nil)
	//here do smthn..
	rmp.OutputTypes = []returnTypes{{
		functionReturn: false,
		typ:            from,
	}}
	rmp.taskStack.addPrimitiveReturn(from)
	return rmp
}

func (currentCircuit *function) getFunctionInputs() (oldInputs []*function) {
	for _, name := range currentCircuit.InputIdentifiers {
		oldInputs = append(oldInputs, currentCircuit.functions[name])
	}
	return

}

func (currentCircuit *function) getsLoadedWith(newInputs []*function) {

	if !currentCircuit.skipLoadVerification {
		if len(currentCircuit.InputTypes) < len(newInputs) {
			panic(fmt.Sprintf("%v takes %v arguments, got %v", currentCircuit.Name, len(currentCircuit.InputIdentifiers), len(newInputs)))
		}
		for i := 0; i < len(newInputs); i++ {
			out := newInputs[i].outputs()
			if len(out) != 1 {
				//a()-> x,y
				//so we dont allos foo( a()), even when foo takes two arguments. should we stay with go syntax?
				panic("")
			}
			b, err := currentCircuit.InputTypes[i].compare(out[0])
			if b {
				//do we need this map assignment?
				currentCircuit.functions[currentCircuit.InputIdentifiers[0]] = newInputs[0]
				currentCircuit.InputTypes = currentCircuit.InputTypes[1:]
				currentCircuit.InputIdentifiers = currentCircuit.InputIdentifiers[1:]
				continue
			}

			panic(err)

		}
	}
	return
}

//pani
func (currentCircuit *function) resolveArrayName(id string, inputs []*Constraint) (composedName string) {

	var arrayIdentifier = id
	//if len(c.InputIdentifiers) < 1 {
	//	panic("accessing array index failed")
	//}
	for _, in := range inputs {
		indexFactors, _ := currentCircuit.compile(in, newGateContainer())
		if indexFactors.fac().containsArgument() {
			panic("cannot access array dynamically in an arithmetic circuit currently")
		}
		if len(indexFactors) > 1 {
			panic("unexpected")
		}
		tmp := indexFactors.fac()[0].value
		arrayIdentifier += fmt.Sprintf("[%v]", tmp)
	}
	return arrayIdentifier
}

func (currentCircuit *function) execute(gateCollector *gateContainer) (bundle, bool) {
	if len(currentCircuit.InputTypes) != 0 {
		return bundle{returnTyped{
			facs:              nil,
			preloadedFunction: currentCircuit,
		}}, false
	}

	for _, task := range currentCircuit.taskStack.data {
		bundl, rett := currentCircuit.compile(task, gateCollector)
		if rett {
			return bundl, rett
		}
		//gateCollector.completeFunction(f)
	}
	return emptyRets()
}

func (from *Constraint) primitiveReturnfunction(typ Token) (gives *function) {
	rmp := NewCircuit(from.Output.Identifier, nil)
	rmp.taskStack.add(&Constraint{
		Output: Token{
			Type:       RETURN,
			Identifier: "",
		},
		Inputs: []*Constraint{from}})
	rmp.OutputTypes = []returnTypes{{
		functionReturn: false,
		fkt:            nil,
		typ:            typ,
	}}
	return rmp
}
