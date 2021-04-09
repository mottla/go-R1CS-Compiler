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
	case ARGUMENT:

		return Token{
			Type:       ARGUMENT,
			Identifier: currentConstraint.Output.Identifier,
		}.toFactors(), false, function{}
	case DecimalNumberToken:
		value, success := utils.Field.ArithmeticField.StringToFieldElement(currentConstraint.Output.Identifier)
		if !success {
			panic("not a constant")
		}
		f := factor{Typ: Token{Type: DecimalNumberToken, Identifier: currentConstraint.Output.Identifier}, multiplicative: value}
		return factors{f}, false, function{}
	case IDENTIFIER_VARIABLE:
		// an identifier is always a lone indentifier. If such one is reached. we are at a leaf and either can resolve him as argument or declared function/variable

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
		isStatic, isSat := currentCircuit.checkStaticCondition(currentConstraint.Inputs[0])
		for isSat && isStatic {
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
			//the increment condition i += 1
			currentCircuit.compile(currentConstraint.Inputs[1], gateCollector)
			isStatic, isSat = currentCircuit.checkStaticCondition(currentConstraint.Inputs[0])
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
			fac := Token{
				Type: DecimalNumberToken,
			}
			return fac.toFactors(), true, function{}
		case 1:
			f, _, fkt := currentCircuit.compile(currentConstraint.Inputs[0], gateCollector)
			return f, true, fkt
		case 3:

		default:
			panic("not possible")
		}

	case VARIABLE_OVERLOAD:

		if len(currentConstraint.Inputs) != 2 {
			panic("unexpected reach")
		}

		var toOverloadIdentifier = currentConstraint.Inputs[0].Output.Identifier

		if currentConstraint.Inputs[0].Output.Type == ARRAY_CALL {
			var id string
			if f, ex := currentCircuit.findFunctionInBloodline(toOverloadIdentifier); !ex {
				id = currentConstraint.Inputs[0].Output.Identifier
			} else {
				id = f.Name
			}
			// myArray[7*3] = expression
			toOverloadIdentifier = currentCircuit.resolveArrayName(id, currentConstraint.Inputs[0].Inputs)
			//myArray[21] = expression

		}
		//new(big.Int).Xor()
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
		var id string
		if f, ex := currentCircuit.findFunctionInBloodline(currentConstraint.Output.Identifier); !ex {
			id = currentConstraint.Output.Identifier
		} else {
			id = f.Name
		}
		resolvedName := currentCircuit.resolveArrayName(id, currentConstraint.Inputs)
		if f, ex := currentCircuit.findFunctionInBloodline(resolvedName); ex {
			return f.execute(gateCollector)
		}

		if con, ex := currentCircuit.findConstraintInBloodline(resolvedName); ex {
			a, _, fkt := currentCircuit.compile(con, gateCollector)
			return a, false, fkt
		} else {
			panic(fmt.Sprintf("array %s not declared", resolvedName))
		}
	case IF_FUNCTION_CALL:
		var ifElseCircuits *function
		var ex bool
		if ifElseCircuits, ex = currentCircuit.findFunctionInBloodline(currentConstraint.Output.Identifier); !ex {
			panic(fmt.Sprintf("function %s not declared", currentConstraint.Output.Identifier))
		}

		negatedConditions := []*function{}
		var result factors
		for _, task := range ifElseCircuits.taskStack.data {
			//check if the condition is static. if that is the case, and it is true, we execute
			//the statement and return. the remaining if-else conditions are ignored
			//else condition
			if task.Inputs == nil || len(task.Inputs) == 0 {

				fak, _, _ := ifElseCircuits.compile(&Constraint{
					Output: Token{
						Type:       FUNCTION_CALL,
						Identifier: task.Output.Identifier,
					},
				}, gateCollector)
				if result == nil {
					return fak, true, function{}
				}

				var composed = fak.primitiveReturnfunction()

				for _, negatedCondition := range negatedConditions {
					composed = combineFunctions("*", composed, negatedCondition)
				}
				f, _, _ := composed.execute(gateCollector)

				return addFactors(result, f), true, function{}
			}
			if isStat, isSat := currentCircuit.checkStaticCondition(task.Inputs[0]); isSat && isStat {

				fak, _, _ := ifElseCircuits.compile(&Constraint{
					Output: Token{
						Type:       FUNCTION_CALL,
						Identifier: task.Output.Identifier,
					},
				}, gateCollector)
				if result == nil {
					return fak, true, function{}
				}
				panic("not done yet")

				return addFactors(fak, result), true, function{}
			} else if !isStat {

				//the condition
				condition, ret, _ := ifElseCircuits.compile(task.Inputs[0], gateCollector)
				if ret {
					panic("cannot return")
				}

				if statement, ex := ifElseCircuits.functions[task.Output.Identifier]; ex {
					//run whats inside the if { }
					//if there is a return, we append the conditional to it.
					//TODO how to handle overwrites?
					f, r, _ := statement.execute(gateCollector)
					if r {
						composed := combineFunctions("*", condition.primitiveReturnfunction(), f.primitiveReturnfunction())
						for _, negatedCondition := range negatedConditions {
							composed = combineFunctions("*", composed, negatedCondition)
						}
						f, _, _ = composed.execute(gateCollector)
						result = addFactors(result, f)
					}

				} else {
					panic(" wtf")
				}

				//everything the statement returnes, must be multiplied with the condition
				//but what about overwrites inside the statement of variables outside the scope? problem for future
				//mathias, or I give up the concept of overloading variables

				one := Token{
					Type:       DecimalNumberToken,
					Identifier: "1",
				}
				negateFkt := combineFunctions("-", one.primitiveReturnfunction(), condition.primitiveReturnfunction())
				negatedConditions = append(negatedConditions, negateFkt)

			}

		}
		return result, false, function{}
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
			if len(currentConstraint.Inputs) == 0 {
				//nothing to do
				return nil, false, function{}
			}
			//prepare input number Z
			arg := currentConstraint.Inputs[0].clone()
			currentCircuit.SPLIT(true, arg, gateCollector)
			return nil, false, function{}

		case "addGateConstraint":
			if len(currentConstraint.Inputs) != 2 {
				panic("addition constraint requires 2 arguments")
			}
			leftClone := currentConstraint.Inputs[0].clone()
			rightClone := currentConstraint.Inputs[1].clone()
			leftFactors, _, _ := currentCircuit.compile(leftClone, gateCollector)
			rightFactors, _, _ := currentCircuit.compile(rightClone, gateCollector)
			commonExtracted, extractedLeft, extractedRight := extractConstant(leftFactors, rightFactors)

			sGate := summationGate(addFactors(extractedLeft, extractedRight))
			id := gateCollector.Add(sGate)

			fres := factors{factor{
				Typ:            id,
				multiplicative: commonExtracted,
			}}
			return fres, false, *fres.primitiveReturnfunction()
		case "equal":
			if len(currentConstraint.Inputs) != 2 {
				panic("equality constraint requires 2 arguments")
			}
			leftClone := currentConstraint.Inputs[0].clone()
			rightClone := currentConstraint.Inputs[1].clone()
			leftFactors, _, _ := currentCircuit.compile(leftClone, gateCollector)
			rightFactors, _, _ := currentCircuit.compile(rightClone, gateCollector)
			gateCollector.Add(equalityGate(leftFactors, rightFactors))
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

			switch operation.Identifier {
			case "==":
				argLeft, bitsLeft := currentCircuit.SPLIT(false, left, gateCollector)
				argRight, bitsRight := currentCircuit.SPLIT(false, right, gateCollector)

				xorIDs := make(factors, len(bitsRight))

				for i := 0; i < len(bitsLeft); i++ {
					// a xor b = c as arithmetic circuit (asserting that a,b \in {0,1}
					// 2a*b = c + a + b - 1
					xorIDs[i] = gateCollector.Add(xorGate(bitsLeft[i], bitsRight[i])).toFactor()
				}

				// we now want to AND all xors
				// so first we now that our result bit will be one or zero
				// c1 * (1-c1) = 0
				zg := zeroOrOneGate(argLeft.Identifier + "==" + argRight.Identifier)
				zg.computeYourselfe = func(witness *[]*big.Int, set *[]bool, indexMap map[string]int) bool {
					if (*set)[indexMap[argLeft.Identifier]] && (*set)[indexMap[argRight.Identifier]] {
						l := (*witness)[indexMap[argLeft.Identifier]]
						r := (*witness)[indexMap[argRight.Identifier]]
						(*set)[indexMap[zg.ID()]] = true
						result := 1 - int64(utils.AbsInt(l.Cmp(r)))
						(*witness)[indexMap[zg.ID()]] = new(big.Int).SetInt64(result)
						return true
					}
					return false
				}

				c1 := gateCollector.Add(zg)

				//nex we now that (N - Sum_i xor_i ) * b = 0
				// -> if b is 1, then all xors must be 1 as well
				minusN := factor{Typ: Token{
					Type:       DecimalNumberToken,
					Identifier: "",
				},
					multiplicative: new(big.Int).SetInt64(-int64(len(xorIDs))),
				}
				// -N + Sum_i xor_i -> N - Sum_i xor_i
				sumtherm := (append(xorIDs, minusN)).Negate()

				gateCollector.Add(zeroConstraintGate(sumtherm, c1.toFactors()))

				//finally, if b =0 , then the sum over all xors is some number less then N
				// so we need to ensure that (N - Sum_i xor_i ) * (N - Sum_i xor_i )^-1 = 1-b
				//
				inverseSumtherm := gateCollector.Add(inverseGate(sumtherm))
				//
				var c3 = new(Gate)
				c3.leftIns = sumtherm
				c3.noNewOutput = true
				//c3.rightIns = inverseSumtherm.toFactors()
				c3.outIns = factors{Token{Type: DecimalNumberToken}.toFactor(), c1.toFactor().Negate()}
				c3.rightIns = inverseSumtherm.toFactors()
				gateCollector.Add(c3)
				return c1.toFactors(), false, function{}
			case "!=":
				argLeft, bitsLeft := currentCircuit.SPLIT(false, left, gateCollector)
				argRight, bitsRight := currentCircuit.SPLIT(false, right, gateCollector)

				xorIDs := make(factors, len(bitsRight))

				for i := 0; i < len(bitsLeft); i++ {
					// a xor b = c as arithmetic circuit (asserting that a,b \in {0,1}
					// 2a*b = c + a + b - 1
					xorIDs[i] = gateCollector.Add(xorGate(bitsLeft[i], bitsRight[i])).toFactor()
				}

				// we now want to AND all xors
				// so first we now that our result bit will be one or zero
				// c1 * (1-c1) = 0
				zg := zeroOrOneGate(argLeft.Identifier + "!=" + argRight.Identifier)
				zg.computeYourselfe = func(witness *[]*big.Int, set *[]bool, indexMap map[string]int) bool {
					if (*set)[indexMap[argLeft.Identifier]] && (*set)[indexMap[argRight.Identifier]] {
						l := (*witness)[indexMap[argLeft.Identifier]]
						r := (*witness)[indexMap[argRight.Identifier]]
						(*set)[indexMap[zg.ID()]] = true
						result := int64(utils.AbsInt(l.Cmp(r)))
						(*witness)[indexMap[zg.ID()]] = new(big.Int).SetInt64(result)
						return true
					}
					return false
				}

				c1 := gateCollector.Add(zg)

				//nex we now that (N - Sum_i xor_i ) * (1-b) = 0
				// -> if b is 1, then all xors must be 1 as well
				minusN := factor{Typ: Token{
					Type:       DecimalNumberToken,
					Identifier: "",
				},
					multiplicative: new(big.Int).SetInt64(-int64(len(xorIDs))),
				}
				// -N + Sum_i xor_i -> N - Sum_i xor_i
				sumtherm := (append(xorIDs, minusN)).Negate()

				gateCollector.Add(zeroConstraintGate(sumtherm, factors{Token{Type: DecimalNumberToken}.toFactor(), c1.toFactor().Negate()}))

				//finally, if b =0 , then the sum over all xors is some number less then N
				// so we need to ensure that (N - Sum_i xor_i ) * (N - Sum_i xor_i )^-1 = b
				//
				inverseSumtherm := gateCollector.Add(inverseGate(sumtherm))
				//
				var c3 = new(Gate)
				c3.leftIns = sumtherm
				c3.noNewOutput = true
				//c3.rightIns = inverseSumtherm.toFactors()
				c3.outIns = c1.toFactors()
				c3.rightIns = inverseSumtherm.toFactors()
				gateCollector.Add(c3)
				return c1.toFactors(), false, function{}
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
				mGate := multiplicationGate(newLeft, newRight)
				nTok := gateCollector.Add(mGate)

				f := factor{Typ: nTok, multiplicative: commonFactor}
				return factors{f}, currentConstraint.Output.Type == RETURN, function{}
			case "/":
				//a / b
				leftFactors, _, _ = currentCircuit.compile(left, gateCollector)
				rightFactors, _, _ = currentCircuit.compile(right, gateCollector)

				if rightFactors.isSingleNumber() { // (x1+x2..)/6
					return mulFactors(leftFactors, invertFactors(rightFactors)), currentConstraint.Output.Type == RETURN, function{}
				}

				gcdl, facL := factorSignature(leftFactors)
				gcdR, facR := factorSignature(rightFactors)
				//TODO is this a good idea?
				commonF := utils.Field.ArithmeticField.Div(gcdl, gcdR)

				//inverse gate enforces the input to be non zero
				//eg. b*b^-1 = 1
				var inversB = inverseGate(facR)
				var g = divisionGate(facL, facR)

				gateCollector.Add(inversB)
				nTok := gateCollector.Add(g)

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

func (currentCircuit *function) SPLIT(makeTheBitsAvailableInCurrentCircuit bool, toSplit *Constraint, gateCollector *gateContainer) (arg Token, bits []factor) {

	in, _, _ := currentCircuit.compile(toSplit, gateCollector)
	if len(in) > 1 {
		tok := gateCollector.Add(summationGate(in))
		return tok, split(makeTheBitsAvailableInCurrentCircuit, currentCircuit, gateCollector, tok)
	}
	if in.isSingleNumber() {
		fmt.Println("you really wanna split a constant number into its bits? ")
	}
	//if say : Split(5*x), then we need to introduce the constant multiplication gate. even if its stupid..
	if in[0].multiplicative.Cmp(bigOne) != 0 {
		one := Token{
			Type: DecimalNumberToken,
		}.toFactors()
		tok := gateCollector.Add(multiplicationGate(in, one))

		return tok, split(makeTheBitsAvailableInCurrentCircuit, currentCircuit, gateCollector, tok)
	}

	return in[0].Typ, split(makeTheBitsAvailableInCurrentCircuit, currentCircuit, gateCollector, in[0].Typ)
}

func split(makeTheBitsAvailableInCurrentCircuit bool, currentCircuit *function, gateCollector *gateContainer, arg Token) (bits []factor) {
	N := utils.Field.ArithmeticField.Q.BitLen()
	N = N - 1
	//we create N new R1CS elements Z_i. 			//
	//each represents a bit of Z
	//each Z_i is introduced by a constraint  (Z_i - 1 ) * Z_i = 0, to ensure its either 0 or 1
	// Z_0 is the lsb

	bitsScaled := make([]factor, N)
	bits = make([]factor, N)
	for i := 0; i < N; i++ {
		nTok := Token{
			Type: ARGUMENT,
			//wirst the number, then the variable, to avoid collisions
			Identifier: fmt.Sprintf("%v[%v]", arg.Identifier, i),
		}
		zeroOrOneGate := zeroOrOneGate(nTok.Identifier)

		bit := gateCollector.Add(zeroOrOneGate)

		bitsScaled[i] =
			factor{
				Typ:            bit,
				multiplicative: new(big.Int).Lsh(bigOne, uint(i)),
			}

		bits[i] = bit.toFactor()
		//say we split var x, from now on we can call x[i] to get the i'th bit
		if makeTheBitsAvailableInCurrentCircuit {
			currentCircuit.functions[nTok.Identifier] = bit.primitiveReturnfunction()
		}

		// we need to add the bits during precompilations so we can access them like from an array
		//	currentCircuit.constraintMap
	}
	//add the bits constraint \bits Z_i 2^i = Z to ensure that the Zi are the bit representation of Z

	//cConstraint[indexMap[g.value.identifier.identifier]] = g.extractedConstants
	eq := equalityGate(bitsScaled, arg.toFactors())
	eq.computeYourselfe = func(witness *[]*big.Int, set *[]bool, indexMap map[string]int) bool {
		if (*set)[indexMap[arg.Identifier]] {
			for i, bit := range bits {
				(*witness)[indexMap[bit.Typ.Identifier]] = big.NewInt(int64((*witness)[indexMap[arg.Identifier]].Bit(i)))
				(*set)[indexMap[bit.Typ.Identifier]] = true
			}
			return true
		}
		return false
	}
	gateCollector.Add(eq)
	return bits
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

	for _, taks := range mainCircuit.taskStack.data {
		f, returns, _ := mainCircuit.compile(taks, container)
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
		if v.computeYourselfe != nil {
			r1CS.triggers = append(r1CS.triggers, v.computeYourselfe)
		}

		//there are gates, which do not increase the size of the trace such as equality check constraint, sum check constraint after binary split
		if v.noNewOutput {
			//size = size - 1
			continue
		}
		if _, ex := indexMap[v.ID()]; !ex {
			indexMap[v.ID()] = len(indexMap)

		} else {
			panic(fmt.Sprintf("rewriting %v ", v.identifier))
		}
		//we store where a variables bit representatives are
		//TODO suspicious.. we should probablly consider the bit position
		//if v.isABitOf != "" {
		//	if _, ex := r1CS.splitmap[v.isABitOf]; !ex {
		//		r1CS.splitmap[v.isABitOf] = []int{indexMap[v.identifier]}
		//	} else {
		//		r1CS.splitmap[v.isABitOf] = append(r1CS.splitmap[v.isABitOf], indexMap[v.identifier])
		//	}
		//}
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

		r1CS.L = append(r1CS.L, aConstraint)
		r1CS.R = append(r1CS.R, bConstraint)
		r1CS.O = append(r1CS.O, cConstraint)

	}

	r1CS.NumberOfGates = len(r1CS.L)
	r1CS.WitnessLength = len(indexMap)
	return
}
