package Circuitcompiler

import (
	"fmt"
	"github.com/mottla/go-R1CS-Compiler/utils"
	"math/big"
)

//recursively walks through the parse tree to create a list of all
//multiplication gates needed for the QAP construction
//Takes into account, that multiplication with constants and addition (= substraction) can be reduced, and does so
//
func (currentCircuit *function) compile(currentConstraint *Constraint, gateCollector *gateContainer) (facs factors, reachedReturn bool, preloadedFunction function) {

	switch currentConstraint.Output.Type {
	case DecimalNumberToken:
		value, success := utils.Field.ArithmeticField.StringToFieldElement(currentConstraint.Output.Identifier)
		if !success {
			panic("not a constant")
		}
		f := factor{Typ: Token{Type: DecimalNumberToken, Identifier: currentConstraint.Output.Identifier}, multiplicative: value}
		return factors{f}, false, function{}
	case IDENTIFIER_VARIABLE:
		// an Identifier is always a lone indentifier. If such one is reached. we are at a leaf and either can resolve him as argument or declared function/variable

		if f, ex := currentCircuit.findFunctionInBloodline(currentConstraint.Output.Identifier); ex {
			if f.isNumber {
				fac := factor{Typ: Token{Type: DecimalNumberToken, Identifier: f.Name}, multiplicative: f.value}
				return factors{fac}, false, function{}
			}
			if len(f.Inputs) == 0 {

				if con, ex := currentCircuit.findConstraintInBloodline(currentConstraint.Output.Identifier); ex {
					return currentCircuit.compile(con, gateCollector)
				}
				panic(fmt.Sprintf("variable %s not declared", currentConstraint.Output.Identifier))
			}
			return nil, false, *f

			//return f.execute(gateCollector)

		}

		panic(fmt.Sprintf("variable %s not declared", currentConstraint.Output.Identifier))

	case FOR:
		//we check the condition each time we rerun the loop
		for isStatic, isSat := currentCircuit.checkStaticCondition(currentConstraint.Inputs[0]); isSat && isStatic; {
			content := &Constraint{
				Output: Token{
					Type:       FUNCTION_CALL,
					Identifier: currentConstraint.Output.Identifier,
				},
			}
			f, retu, fkt := currentCircuit.compile(content, gateCollector)
			if retu {
				return f, true, fkt
			}
			currentCircuit.compile(currentConstraint.Inputs[1], gateCollector)
		}
		return nil, false, function{}
	case UNASIGNEDVAR:
		switch len(currentConstraint.Inputs) {
		case 0:
			if con, ex := currentCircuit.constraintMap[currentConstraint.Output.Identifier]; ex {
				return currentCircuit.compile(con, gateCollector)
			}
		case 1:
			return currentCircuit.compile(currentConstraint.Inputs[0], gateCollector)
		case 3:
		default:
			panic(currentConstraint)
		}
	case RETURN:
		switch len(currentConstraint.Inputs) {
		case 0:
			fac := factor{Typ: Token{
				Type: DecimalNumberToken,
			}, multiplicative: big.NewInt(1)}
			return factors{fac}, true, function{}
		case 1:
			f, _, fkt := currentCircuit.compile(currentConstraint.Inputs[0], gateCollector)
			return f, true, fkt
		default:

		}
	case ARGUMENT:
		fac := factor{Typ: Token{
			Type:       ARGUMENT,
			Identifier: currentConstraint.Output.Identifier,
		}, multiplicative: big.NewInt(1)}
		return factors{fac}, false, function{}
	case VARIABLE_OVERLOAD:

		if len(currentConstraint.Inputs) != 2 {
			panic("unexpected reach")
		}

		var toOverloadIdentifier = currentConstraint.Inputs[0].Output.Identifier

		if currentConstraint.Inputs[0].Output.Type == ARRAY_CALL {
			// myArray[7*3] = expression
			toOverloadIdentifier = currentCircuit.resolveArrayName(currentConstraint.Inputs[0])
			//myArray[21] = expression
		}

		//resolve the expression
		facs, _, fkt := currentCircuit.compile(currentConstraint.Inputs[1], gateCollector)
		context, ex := currentCircuit.getCircuitContainingFunctionInBloodline(toOverloadIdentifier)
		if !ex {
			panic("does not exist")
		}
		if facs == nil {
			context.functions[toOverloadIdentifier] = &fkt
			return nil, false, function{}
		}
		//overwrite

		context.functions[toOverloadIdentifier] = facs.primitiveReturnfunction()

		return facs, false, function{}

	case ARRAY_CALL:
		resolvedName := currentCircuit.resolveArrayName(currentConstraint)
		if con, ex := currentCircuit.findConstraintInBloodline(resolvedName); ex {
			a, _, fkt := currentCircuit.compile(con, gateCollector)
			return a, false, fkt
		} else {
			panic(fmt.Sprintf("array %s not declared", resolvedName))
		}
	//case IF:
	//	//else
	//	if len(currentConstraint.Inputs) == 0 {
	//		return currentCircuit.compile(&Constraint{
	//			Output: Token{
	//				Type:       IF_FUNCTION_CALL,
	//				Identifier: currentConstraint.Output.Identifier,
	//			},
	//		}, gateCollector)
	//	}
	//
	//	if currentCircuit.checkStaticCondition(currentConstraint.Inputs[0]) {
	//		return currentCircuit.compile(&Constraint{
	//			Output: Token{
	//				Type:       IF_FUNCTION_CALL,
	//				Identifier: currentConstraint.Output.Identifier,
	//			},
	//		}, gateCollector)
	//	} else {
	//		return currentCircuit.compile(currentConstraint.Inputs[0], gateCollector)
	//	}
	case IF_FUNCTION_CALL:
		var ifElseCircuits *function
		var ex bool
		if ifElseCircuits, ex = currentCircuit.findFunctionInBloodline(currentConstraint.Output.Identifier); !ex {
			panic(fmt.Sprintf("function %s not declared", currentConstraint.Output.Identifier))
		}

		negatedConditions := []*function{}
		for _, task := range ifElseCircuits.taskStack.data {
			//check if the condition is static. if that is the case, and it is true, we execute
			//the statement and return. the remaining if-else conditions are ignored
			//else condition
			if task.Inputs == nil || len(task.Inputs) == 0 {
				return ifElseCircuits.compile(&Constraint{
					Output: Token{
						Type:       FUNCTION_CALL,
						Identifier: task.Output.Identifier,
					},
				}, gateCollector)
			}
			if isStat, isSat := currentCircuit.checkStaticCondition(task.Inputs[0]); isSat && isStat {
				return ifElseCircuits.compile(&Constraint{
					Output: Token{
						Type:       FUNCTION_CALL,
						Identifier: task.Output.Identifier,
					},
				}, gateCollector)
			} else if !isStat {

				//the condition
				_, ret, condition := ifElseCircuits.compile(task.Inputs[0], gateCollector)
				if ret {
					panic("cannot return")
				}

				if statement, ex := ifElseCircuits.functions[task.Output.Identifier]; ex {
					f, r, _ := statement.execute(gateCollector)
					if r {
						composed := combineFunctions("*", &condition, f.primitiveReturnfunction())
						for _, negatedCondition := range negatedConditions {
							composed = combineFunctions("*", composed, negatedCondition)
						}
						composed.execute(gateCollector)
					}

				} else {
					panic(" wtf")
				}

				//everything the statement returnes, must be multiplied with the condition
				//but what about overwrites inside the statement of variables outside the scope? problem for future
				//mathias, or I give up the concept of overloading variables

				one := factors{factor{
					Typ: Token{
						Type:       DecimalNumberToken,
						Identifier: "",
					},
					multiplicative: bigOne,
				}}
				negateFkt := combineFunctions("-", one.primitiveReturnfunction(), &condition)
				negatedConditions = append(negatedConditions, negateFkt)

			}

		}
	case FUNCTION_CALL:
		switch currentConstraint.Output.Identifier {
		case "BREAK":
			// DEBUG function. Set a break point somewhere and read all arguments that were passed to BREAK(args...)
			for _, v := range currentConstraint.Inputs {
				in, _, _ := currentCircuit.compile(v.clone(), gateCollector)

				st := fmt.Sprintf("%v , %v", v.String(), in)
				fmt.Println(st)
			}
			//break point
			return nil, false, function{}
		case "SPLIT":
			if len(currentConstraint.Inputs) != 1 {
				panic("split")
			}
			//prepare input number Z
			arg := currentConstraint.Inputs[0].clone()
			in, _, _ := currentCircuit.compile(arg, gateCollector)
			if len(in) != 1 {
				panic("arguments in split must be pure")
			}
			split(gateCollector, arg.Output)
			//hmm what is this actually good for
			//sig := hashFactorsToBig(in).String()[:16]

			//get the number N of bits needed to represent an arbitrary element of the finite field

			return nil, false, function{}
		case "AND":
			if len(currentConstraint.Inputs) != 2 {
				panic("addition constraint requires 2 arguments")
			}
			//leftClone := currentConstraint.Inputs[0].clone()
			//rightClone := currentConstraint.Inputs[1].clone()
			//leftFactors, _, _ := currentCircuit.compile(leftClone, gateCollector)
			//rightFactors, _, _ := currentCircuit.compile(rightClone, gateCollector)
		case "NAND":
		case "OR":
		case "NOT":
		case "addGateConstraint":
			if len(currentConstraint.Inputs) != 2 {
				panic("addition constraint requires 2 arguments")
			}
			leftClone := currentConstraint.Inputs[0].clone()
			rightClone := currentConstraint.Inputs[1].clone()
			leftFactors, _, _ := currentCircuit.compile(leftClone, gateCollector)
			rightFactors, _, _ := currentCircuit.compile(rightClone, gateCollector)
			commonExtracted, extractedLeft, extractedRight := extractConstant(leftFactors, rightFactors)

			rootGate := &Gate{
				gateType: additionGate,
				leftIns:  addFactors(extractedLeft, extractedRight),
			}

			nTok := Token{
				Type:       ARGUMENT,
				Identifier: rootGate.setAndGetID(),
			}
			gateCollector.Add(rootGate)
			rootGate.outIns = factors{factor{
				Typ:            nTok,
				multiplicative: bigOne,
			}}
			fres := factor{
				Typ:            nTok,
				multiplicative: commonExtracted,
			}
			return factors{fres}, false, function{}

		case "equal":
			if len(currentConstraint.Inputs) != 2 {
				panic("equality constraint requires 2 arguments")
			}
			leftClone := currentConstraint.Inputs[0].clone()
			rightClone := currentConstraint.Inputs[1].clone()
			leftFactors, _, _ := currentCircuit.compile(leftClone, gateCollector)
			rightFactors, _, _ := currentCircuit.compile(rightClone, gateCollector)

			rootGate := &Gate{
				gateType:    equalityGate,
				leftIns:     leftFactors,
				rightIns:    rightFactors,
				noNewOutput: true,
			}
			gateCollector.Add(rootGate)
			return nil, false, function{}
		default:
			var nextCircuit *function
			var ex bool

			if nextCircuit, ex = currentCircuit.findFunctionInBloodline(currentConstraint.Output.Identifier); !ex {
				panic(fmt.Sprintf("function %s not declared", currentConstraint.Output.Identifier))
			}
			var nxt *function
			nxt = nextCircuit.flatCopy()

			inputs := make([]*function, len(currentConstraint.Inputs))
			//if the argument is a function call, we need to call it and give the result as argument i thinl
			//if the argument is a function, but not a call, we pass it on
			for i, v := range currentConstraint.Inputs {

				fkts, _, retFkt := currentCircuit.compile(v, gateCollector)
				if fkts == nil {
					inputs[i] = &retFkt
					continue
				}
				inputs[i] = fkts.primitiveReturnfunction()
			}

			//old := nxt.getFunctionInputs()
			//old2 := nxt.Inputs
			//defer nxt.setFunctionInputs(old2,old)
			isReadyToEvaluate := nxt.getsLoadedWith(inputs)
			if !isReadyToEvaluate {
				return nil, false, *nxt
			}
			facs, _, fkt := nxt.execute(gateCollector)
			return facs, false, fkt
		}
	default:
	}

	if len(currentConstraint.Inputs) == 3 {

		left := currentConstraint.Inputs[1]
		right := currentConstraint.Inputs[2]
		operation := currentConstraint.Inputs[0].Output

		var leftFactors, rightFactors factors

		switch operation.Type {
		case BinaryComperatorToken:
			leftFactors, _, _ = currentCircuit.compile(left, gateCollector)
			rightFactors, _, _ = currentCircuit.compile(right, gateCollector)

			switch operation.Identifier {
			case "==":
				bitsLeft := split(gateCollector, leftFactors[0].Typ)
				bitsRight := split(gateCollector, rightFactors[0].Typ)

				xorIDs := make([]string, len(bitsRight))

				for i := 0; i < len(bitsLeft); i++ {
					// a xor b = c as arithmetic circuit (asserting that a,b \in {0,1}
					// 2a*b = c - a - b - 1
					one := factor{
						Typ: Token{
							Type: DecimalNumberToken,
						},
						multiplicative: new(big.Int).SetInt64(1),
					}

					mGate := &Gate{
						gateType: multiplicationGate,
						leftIns:  factors{bitsLeft[i].SetMultiplicative(new(big.Int).SetInt64(2))},
						rightIns: factors{bitsRight[i]},
						outIns:   factors{one.Negate(), bitsLeft[i].Negate(), bitsRight[i].Negate()},
					}
					xor := factor{
						Typ: Token{Identifier: mGate.ID()},
					}
					mGate.outIns = append(mGate.outIns, xor)
					xorIDs[i] = gateCollector.Add(mGate)
				}
				first := xorIDs[0]
				for i := 1; i < len(xorIDs); i++ {
					mGate := &Gate{
						gateType: multiplicationGate,
						leftIns: factors{factor{
							Typ: Token{Identifier: first},
						}},
						rightIns: factors{factor{
							Typ: Token{Identifier: xorIDs[i]},
						}},
					}
					first = mGate.ID()
					tok := Token{
						Type:       ARGUMENT,
						Identifier: first}

					mGate.outIns = factors{factor{
						Typ: tok,
					}}
					gateCollector.Add(mGate)
					return mGate.outIns, false, *tok.primitiveReturnfunction()
				}

			case "!=":

				break
			case ">":

				break
			case ">=":

				break
			case "<":

				break
			case "<=":

				break
			default:

			}
			panic("not supported")
			break
		case BitOperatorToken:
			break
		case BooleanOperatorToken:
			break
		case ArithmeticOperatorToken:
			switch operation.Identifier {
			case "*":
				leftFactors, _, _ = currentCircuit.compile(left, gateCollector)
				rightFactors, _, _ = currentCircuit.compile(right, gateCollector)

				if leftFactors.isSingleNumber() || rightFactors.isSingleNumber() {
					return mulFactors(leftFactors, rightFactors), currentConstraint.Output.Type == RETURN, function{}
				}
				commonFactor, newLeft, newRight := extractConstant(leftFactors, rightFactors)
				mGate := &Gate{
					gateType: multiplicationGate,
					leftIns:  newLeft,
					rightIns: newRight,
				}
				id := gateCollector.Add(mGate)
				//TODO this is bad. why manipulate the gate after insert
				nTok := Token{Type: ARGUMENT, Identifier: id}
				mGate.outIns = factors{factor{
					Typ:            nTok,
					multiplicative: bigOne,
				}}
				f := factor{Typ: nTok, multiplicative: commonFactor}
				return factors{f}, currentConstraint.Output.Type == RETURN, function{}
			case "/":
				leftFactors, _, _ = currentCircuit.compile(left, gateCollector)
				rightFactors, _, _ = currentCircuit.compile(right, gateCollector)

				if rightFactors.isSingleNumber() { // (x1+x2..)/6
					return mulFactors(leftFactors, invertFactors(rightFactors)), currentConstraint.Output.Type == RETURN, function{}
				}

				gcdl, facL := factorSignature(leftFactors)
				gcdR, facR := factorSignature(rightFactors)
				commonF := field.Div(gcdl, gcdR)
				mGate := &Gate{
					gateType: multiplicationGate,
					rightIns: facR,
					outIns:   facL,
				}
				gateCollector.Add(mGate)
				nTok := Token{Type: ARGUMENT, Identifier: mGate.ID()}
				mGate.leftIns = factors{factor{
					Typ:            nTok,
					multiplicative: bigOne,
				}}
				f := factor{Typ: nTok, multiplicative: commonF}
				return factors{f}, currentConstraint.Output.Type == RETURN, function{}
			case "+":
				leftFactors, _, _ = currentCircuit.compile(left, gateCollector)
				rightFactors, _, _ = currentCircuit.compile(right, gateCollector)
				addedFactors := addFactors(leftFactors, rightFactors)
				return addedFactors, currentConstraint.Output.Type == RETURN, function{}

			case "-":
				leftFactors, _, _ = currentCircuit.compile(left, gateCollector)
				rightFactors, _, _ = currentCircuit.compile(right, gateCollector)
				rightFactors = negateFactors(rightFactors)
				addedFactors := addFactors(leftFactors, rightFactors)
				return addedFactors, currentConstraint.Output.Type == RETURN, function{}
			}
			break
		case AssignmentOperatorToken:
			break
		default:
			panic("unsupported operation")
		}

	}
	panic(currentConstraint)
}

//constants, which have been excluded get added to the constraints at the end
//for example: given a expression x*z*23, will only create a constraint where x*z is multiplied
//this increases the chance, of beiing abe to reuse the constrain if for example x*z*24 is called, we use the ouput of x*z and thereby safe a multiplication gate

type MultiplicationGateSignature struct {
	identifier      Token
	commonExtracted *big.Int //if the multiplicationGate had a extractable factor, it will be stored here
}

func (m MultiplicationGateSignature) String() string {
	return fmt.Sprintf("%s extracted %v", m.identifier.String(), m.commonExtracted)
}

type Program struct {
	globalFunction *function
	PublicInputs   []string
}

func newProgram() (program *Program) {
	program = &Program{
		globalFunction: newCircuit("global", nil),
		PublicInputs:   []string{"1"},
	}
	return
}

type InputArgument struct {
	identifier string
	value      *big.Int
}

func (in InputArgument) String() string {
	return fmt.Sprintf("(%v,%v)", in.identifier, in.value.String())
}

func CombineInputs(abstract []string, concrete []*big.Int) (res []InputArgument) {
	//we add the neutral element here

	if len(abstract) != len(concrete) {
		panic(fmt.Sprintf("argument missmatch, programm requires %v inputs, you provided %v", len(abstract), len(concrete)))
	}

	res = make([]InputArgument, len(abstract))

	for k, v := range abstract {
		res[k] = InputArgument{identifier: v, value: concrete[k]}
	}

	return res
}

//returns the cardinality of all public inputs (+ 1 for the "one" signal)
func (p *Program) GlobalInputCount() int {
	return len(p.PublicInputs)
}
func (p *Program) GetMainCircuit() *function {
	return p.globalFunction.functions["main"]
}

//Execute runs on a program and returns a precursor for the final R1CS description
func (p *Program) Execute() (orderedmGates *gateContainer) {

	container := newGateContainer()
	mainCircuit := p.GetMainCircuit()

	for i := 0; i < mainCircuit.taskStack.len(); i++ {
		f, returns, _ := mainCircuit.compile(mainCircuit.taskStack.data[i], container)
		container.completeFunction(f)
		if returns {
			break
		}
	}
	return container
}

// GenerateR1CS generates the R1CS Language from an array of gates
func (p *Program) GatesToR1CS(mGates []*Gate) (r1CS *R1CS) {
	// from flat code to R1CS
	r1CS = &R1CS{}
	r1CS.splitmap = make(map[string][]int)

	//offset := len(p.PublicInputs) + 2
	//  one + in1 +in2+... + gate1 + gate2 .. + randIn + randOut
	//size := offset + len(mGates)
	indexMap := make(map[string]int)
	r1CS.indexMap = indexMap

	//first we add the public inputs
	for _, v := range p.PublicInputs {
		indexMap[v] = len(indexMap)
	}
	for _, v := range p.GetMainCircuit().Inputs {
		if _, ex := indexMap[v]; ex {
			continue
		}
		indexMap[v] = len(indexMap)
	}

	for _, v := range mGates {
		//there are gates, which do not increase the size of the trace such as equality check constraint, sum check constraint after binary split
		if v.noNewOutput {
			//size = size - 1
			continue
		}
		if _, ex := indexMap[v.Identifier]; !ex {
			indexMap[v.Identifier] = len(indexMap)

		} else {
			panic(fmt.Sprintf("rewriting %v ", v.Identifier))
		}
		//we store where a variables bit representatives are
		if v.gateType == zeroOrOneGate {
			if _, ex := r1CS.splitmap[v.arithmeticRepresentatnt.Identifier]; !ex {
				r1CS.splitmap[v.arithmeticRepresentatnt.Identifier] = []int{indexMap[v.Identifier]}
			} else {
				r1CS.splitmap[v.arithmeticRepresentatnt.Identifier] = append(r1CS.splitmap[v.arithmeticRepresentatnt.Identifier], indexMap[v.Identifier])
			}
		}
	}

	insertValue := func(val factor, arr []*big.Int) {
		if val.Typ.Type == DecimalNumberToken {
			arr[0] = val.multiplicative
			return
		}
		index, ex := indexMap[val.Typ.Identifier]
		if !ex {
			panic(fmt.Sprintf("%v index not found!!!", val))
		}
		if val.multiplicative != nil {
			arr[index] = val.multiplicative
			return
		}
		arr[index] = bigOne

	}
	size := len(indexMap)
	for _, g := range mGates {
		//we want to reduce the number of exponentiations in the slower group G2
		//since a*b = b*a, we swap in case
		if len(g.rightIns) > len(g.leftIns) {
			tmp := g.rightIns
			g.rightIns = g.leftIns
			g.leftIns = tmp
		}

		switch g.gateType {
		case multiplicationGate:
			aConstraint := utils.ArrayOfBigZeros(size)
			bConstraint := utils.ArrayOfBigZeros(size)

			cConstraint := utils.ArrayOfBigZeros(size)

			for _, val := range g.leftIns {
				insertValue(val, aConstraint)
			}

			for _, val := range g.rightIns {
				insertValue(val, bConstraint)
			}

			for _, val := range g.outIns {
				insertValue(val, cConstraint)
			}

			//cConstraint[indexMap[g.Identifier]] = g.extractedConstants

			//cConstraint[indexMap[g.value.Identifier]] = big.NewInt(int64(1))

			r1CS.L = append(r1CS.L, aConstraint)
			r1CS.R = append(r1CS.R, bConstraint)
			r1CS.O = append(r1CS.O, cConstraint)
			break

		case equalityGate:
			aConstraint := utils.ArrayOfBigZeros(size)
			bConstraint := utils.ArrayOfBigZeros(size)

			cConstraint := utils.ArrayOfBigZeros(size)

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
		case zeroOrOneGate:
			// (n-1)n = 0 to ensure n == 1 or 0
			aConstraint := utils.ArrayOfBigZeros(size)
			bConstraint := utils.ArrayOfBigZeros(size)

			cConstraint := utils.ArrayOfBigZeros(size)

			aConstraint[0] = big.NewInt(int64(-1))
			aConstraint[indexMap[g.Identifier]] = big.NewInt(int64(1))
			bConstraint[indexMap[g.Identifier]] = big.NewInt(int64(1))
			r1CS.L = append(r1CS.L, aConstraint)
			r1CS.R = append(r1CS.R, bConstraint)
			r1CS.O = append(r1CS.O, cConstraint)
			break
		case additionGate:
			aConstraint := utils.ArrayOfBigZeros(size)
			bConstraint := utils.ArrayOfBigZeros(size)
			cConstraint := utils.ArrayOfBigZeros(size)

			for _, val := range g.leftIns {
				insertValue(val, aConstraint)
			}
			bConstraint[0] = big.NewInt(int64(1))
			for _, val := range g.outIns {
				insertValue(val, cConstraint)
			}
			r1CS.L = append(r1CS.L, aConstraint)
			r1CS.R = append(r1CS.R, bConstraint)
			r1CS.O = append(r1CS.O, cConstraint)
			break
		case sumCheckGate:
			aConstraint := utils.ArrayOfBigZeros(size)
			bConstraint := utils.ArrayOfBigZeros(size)
			cConstraint := utils.ArrayOfBigZeros(size)

			for _, val := range g.leftIns {
				insertValue(val, aConstraint)
			}
			bConstraint[0] = big.NewInt(int64(1))
			cConstraint[indexMap[g.arithmeticRepresentatnt.Identifier]] = big.NewInt(int64(1))
			r1CS.L = append(r1CS.L, aConstraint)
			r1CS.R = append(r1CS.R, bConstraint)
			r1CS.O = append(r1CS.O, cConstraint)
			break
		default:
			panic("no supported gate type")
		}
	}

	r1CS.NumberOfGates = len(r1CS.L)
	r1CS.WitnessLength = len(indexMap)
	return
}

func split(gateCollector *gateContainer, arg Token) (bits []factor) {
	N := utils.Field.ArithmeticField.Q.BitLen()
	//we create N new R1CS elements Z_i. 			//
	//each represents a bit of Z
	//each Z_i is introduced by a constraint  (Z_i - 1 ) * Z_i = 0, to ensure its either 0 or 1
	// Z_0 is the lsb

	bits = make([]factor, N)

	for i := 0; i < N; i++ {
		nTok := Token{
			Type: ARGUMENT,
			//wirst the number, then the variable, to avoid collisions
			Identifier: fmt.Sprintf("%v%v", i, arg.Identifier),
		}
		zeroOrOneGate := &Gate{
			gateType:                zeroOrOneGate,
			Identifier:              nTok.Identifier,
			arithmeticRepresentatnt: arg,
		}
		gateCollector.Add(zeroOrOneGate)

		bits[i] = factor{
			Typ:            nTok,
			multiplicative: new(big.Int).Lsh(bigOne, uint(i)),
		}
		// we need to add the bits during precompilations so we can access them like from an array
		//	currentCircuit.constraintMap
	}
	//add the bits constraint \bits Z_i 2^i = Z to ensure that the Zi are the bit representation of Z

	sumgate := &Gate{
		gateType:                sumCheckGate,
		arithmeticRepresentatnt: arg,
		leftIns:                 bits,
		noNewOutput:             true,
	}
	//cConstraint[indexMap[g.value.Identifier.Identifier]] = g.extractedConstants
	gateCollector.Add(sumgate)
	return bits
}
