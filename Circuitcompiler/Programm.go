package Circuitcompiler

import (
	"fmt"
	"github.com/mottla/go-R1CS-Compiler/utils"
	"math/big"
)

type returnTyped struct {
	facs              Tokens
	preloadedFunction *function
}
type bundle []returnTyped

func ret(facs Tokens,
	preloadedFunction *function) returnTyped {
	return returnTyped{
		facs:              facs,
		preloadedFunction: preloadedFunction,
	}
}
func rets(facs Tokens,
	preloadedFunction *function) []returnTyped {
	return []returnTyped{{
		facs:              facs,
		preloadedFunction: preloadedFunction},
	}
}

func (t Token) toBundle() bundle {
	return []returnTyped{{
		facs:              Tokens{t},
		preloadedFunction: nil},
	}
}
func emptyRets() ([]returnTyped, bool) {
	return rets(nil, nil), false
}
func (f bundle) fac() Tokens {
	if len(f) == 0 {
		return nil
	}
	return f[0].facs
}

//recursively walks through the parse tree to create a list of all
//multiplication gates needed for the QAP construction
//Takes into account, that multiplication with constants and addition (= substraction) can be reduced, and does so
//
func (currentCircuit *function) compile(currentConstraint *Constraint, gateCollector *gateContainer) (returnBundle bundle, reachedReturn bool) {
	if currentConstraint.Output.Type&Types != 0 {
		return currentConstraint.Output.toBundle(), false
	}

	switch currentConstraint.Output.Type {
	case IDENTIFIER_VARIABLE:

		if f, ex := currentCircuit.findFunctionInBloodline(currentConstraint.Output.Identifier); ex {
			if len(f.InputIdentifiers) == 0 {
				return f.execute(gateCollector)
			}
			return rets(nil, f), false
		}

		panic(fmt.Sprintf("variable %s not declared", currentConstraint.Output.Identifier))
	case FOR:
		//we check the condition each time we rerun the loop
		//isStatic, isSat := currentCircuit.checkStaticCondition(currentConstraint.InputTypes[0])
		//for isSat && isStatic {
		//	content := &Constraint{
		//		Output: Token{
		//			Type:       FUNCTION_CALL,
		//			Identifier: currentConstraint.Output.Identifier,
		//		},
		//	}
		//	f, retu, fkt := currentCircuit.compile(content, gateCollector)
		//	if retu {
		//		return rets(f, true, fkt)
		//	}
		//	//the increment condition i += 1
		//	currentCircuit.compile(currentConstraint.InputTypes[1], gateCollector)
		//	isStatic, isSat = currentCircuit.checkStaticCondition(currentConstraint.InputTypes[0])
		//}
		//return nil, false, nil
	case UNASIGNEDVAR:
		switch len(currentConstraint.Inputs) {
		case 0:

		case 1:
			return currentCircuit.compile(currentConstraint.Inputs[0], gateCollector)
		case 3:
		default:
			panic(currentConstraint)
		}
	case FUNCTION_DEFINE:
		if len(currentConstraint.FktInputs) != 1 {
			panic("wtf")
		}
		//we got a function define with immediate call func(){}()
		if len(currentConstraint.Inputs) != 0 {
			var nxt *function
			nxt = currentConstraint.FktInputs[0].flatCopy()

			inputs := make([]*function, len(currentConstraint.Inputs))
			//if the argument is a function call, we need to call it and give the result as argument i thinl
			//if the argument is a function, but not a call, we pass it on
			for i, v := range currentConstraint.Inputs {

				re, _ := currentCircuit.compile(v, gateCollector)
				if re[0].facs == nil {
					inputs[i] = re[0].preloadedFunction
					continue
				}
				inputs[i] = re[0].facs.primitiveReturnfunction()
			}

			nxt.getsLoadedWith(inputs)

			rr, _ := nxt.execute(gateCollector)

			return rr, false
		}

		return bundle{returnTyped{
			facs:              nil,
			preloadedFunction: currentConstraint.FktInputs[0],
		}}, false
	case VARIABLE_DECLARE:
		fmt.Println("")
	case RETURN:
		var r = []returnTyped{}

		for _, v := range currentConstraint.Inputs {
			bund, _ := currentCircuit.compile(v, gateCollector)
			r = append(r, bund...)
		}

		//for _, v := range currentConstraint.FktInputs {
		//	bund, _ := v.execute(gateCollector)
		//	r = append(r, bund...)
		//}

		return r, true
	case VARIABLE_OVERLOAD:
		var bund bundle
		for _, c := range currentConstraint.Inputs[1].Inputs {
			re, _ := currentCircuit.compile(c, gateCollector)
			bund = append(bund, re...)
		}

		//range over the
		for i, v := range currentConstraint.Inputs[0].Inputs {
			var toOverloadIdentifier = v.Output.Identifier

			if v.Output.Type == ARRAY_CALL {
				toOverloadIdentifier = currentCircuit.resolveArrayName(toOverloadIdentifier, v.Inputs)
			}

			context, ex := currentCircuit.getCircuitContainingFunctionInBloodline(toOverloadIdentifier)
			if !ex {
				panic("should be cought at parsing already")
			}

			//fkt, _ := context.functions[toOverloadIdentifier]
			var assign *function
			if bund[i].facs == nil {
				assign = bund[i].preloadedFunction
			} else {
				assign = bund[i].facs.primitiveReturnfunction()
			}

			context.functions[toOverloadIdentifier] = assign
		}
		return emptyRets()
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

		panic(fmt.Sprintf("array %s not declared", resolvedName))

	case IF_FUNCTION_CALL:

		ifElseCircuits := currentConstraint.FktInputs[0]

		mulTok := Token{
			Type:       ArithmeticOperatorToken,
			Identifier: "*",
		}
		negatedConditions := []*function{}
		var result Tokens
		for _, task := range ifElseCircuits.taskStack.data {
			statement := task.FktInputs[0]

			//check if the condition is static. if that is the case, and it is true, we execute
			//the statement and return. the remaining if-else conditions are ignored
			//else condition
			if task.Output.Type == ELSE {

				bund, retu := statement.execute(gateCollector)
				if result == nil {
					return bund, retu
				}

				var composed = bund.fac().primitiveReturnfunction()

				for _, negatedCondition := range negatedConditions {
					composed = combineFunctions(mulTok, composed, negatedCondition, currentCircuit)
				}
				bund, _ = composed.execute(gateCollector)

				return rets((result.AddFactors(bund.fac())), nil), retu
			}
			if isStat, isSat := currentCircuit.checkStaticCondition(task.Inputs[0]); isSat && isStat {

				bund, retu := statement.execute(gateCollector)
				if result == nil {
					return bund, retu
				}

				var composed = bund.fac().primitiveReturnfunction()

				for _, negatedCondition := range negatedConditions {
					composed = combineFunctions(mulTok, composed, negatedCondition, currentCircuit)
				}
				bund, _ = composed.execute(gateCollector)

				return rets((result.AddFactors(bund.fac())), nil), retu
			} else if !isStat {

				//the condition
				conditionBund, r := currentCircuit.compile(task.Inputs[0], gateCollector)
				if r || len(conditionBund) != 1 {
					panic("an error during compilation of the if condition appeared")
				}

				//run whats inside the if { }
				//if there is a return, we append the conditional to it.
				//TODO how to handle overwrites?
				rr, r := statement.execute(gateCollector)
				if r {
					composed := combineFunctions(mulTok, conditionBund.fac().primitiveReturnfunction(), rr.fac().primitiveReturnfunction(), currentCircuit)
					for _, negatedCondition := range negatedConditions {
						composed = combineFunctions(mulTok, composed, negatedCondition, currentCircuit)
					}
					f, _ := composed.execute(gateCollector)
					result = (result.AddFactors(f.fac()))
				}

				//everything the statement returnes, must be multiplied with the condition
				//but what about overwrites inside the statement of variables outside the scope? problem for future
				//mathias, or I give up the concept of overloading variables

				one := Token{
					Type:       DecimalNumberToken,
					Identifier: "1",
				}
				negateFkt := combineFunctions(Token{
					Type:       ArithmeticOperatorToken,
					Identifier: "-",
				}, one.primitiveReturnfunction(), conditionBund.fac().primitiveReturnfunction(), currentCircuit)
				negatedConditions = append(negatedConditions, negateFkt)

			}

		}
		return emptyRets()
	case FUNCTION_CALL:
		switch currentConstraint.Output.Identifier {
		case "BREAK":
			// DEBUG function. Set a break point somewhere and read all arguments that were passed to BREAK(args...)
			//for _, v := range currentConstraint.InputTypes {
			//	//in, _, _ := currentCircuit.compile(v.clone(), gateCollector)
			//	//
			//	//st := fmt.Sprintf("%v , %v", v.String(), in)
			//	//fmt.Println(st)
			//}
			//break point
			return emptyRets()
		case "SPLIT":
			if len(currentConstraint.Inputs) == 0 {
				//nothing to do
				return emptyRets()
			}
			//prepare input number Z
			arg := currentConstraint.Inputs[0]
			currentCircuit.SPLIT(true, arg, gateCollector)
			return emptyRets()

		case "add":
			if len(currentConstraint.Inputs) != 2 {
				panic("addition constraint requires 2 arguments")
			}
			leftClone := currentConstraint.Inputs[0]
			rightClone := currentConstraint.Inputs[1]
			leftFactors, _ := currentCircuit.compile(leftClone, gateCollector)
			rightFactors, _ := currentCircuit.compile(rightClone, gateCollector)

			sGate := summationGate(leftFactors.fac().AddFactors(rightFactors.fac()))
			gateCollector.Add(sGate)

			return emptyRets()
		case "equal":
			if len(currentConstraint.Inputs) != 2 {
				panic("equality constraint requires 2 arguments")
			}
			leftClone := currentConstraint.Inputs[0]
			rightClone := currentConstraint.Inputs[1]
			leftFactors, _ := currentCircuit.compile(leftClone, gateCollector)
			rightFactors, _ := currentCircuit.compile(rightClone, gateCollector)
			gateCollector.Add(equalityGate(leftFactors.fac(), rightFactors.fac()))
			return emptyRets()
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

				re, _ := currentCircuit.compile(v, gateCollector)
				if re[0].facs == nil {
					inputs[i] = re[0].preloadedFunction
					continue
				}
				inputs[i] = re[0].facs.primitiveReturnfunction()
			}

			nxt.getsLoadedWith(inputs)

			rr, _ := nxt.execute(gateCollector)

			return rr, false
		}
	default:
	}

	var leftFactors, rightFactors bundle

	switch currentConstraint.Output.Type {

	//case BinaryComperatorToken:
	//
	//	switch currentConstraint.Output.Identifier {
	//	case "==":
	//		return rets(currentCircuit.equalityGate(currentConstraint, gateCollector), nil), false
	//	case "!=":
	//		fk := currentCircuit.equalityGate(currentConstraint, gateCollector)
	//		return rets(AddFactors(Token{
	//			Type: DecimalNumberToken,
	//		}.toFactors(), fk.Negate()), nil), false
	//	case ">":
	//
	//		break
	//	case ">=":
	//
	//		break
	//	case "<":
	//
	//		break
	//	case "<=":
	//
	//		break
	//	default:
	//
	//	}
	//
	//	break
	//case BitOperatorToken:
	//	left := currentConstraint.Inputs[0]
	//	right := currentConstraint.Inputs[1]
	//
	//	var fkt = func(op string) (shift uint64) {
	//		rightFactors, _ = currentCircuit.compile(right, gateCollector)
	//		if !rightFactors.fac().isSingleNumber() {
	//			panic("right side operand of" + op + " must be a compile-time constant")
	//		}
	//
	//		if !rightFactors.fac()[0].multiplicative.IsUint64() {
	//			panic("right side operand of " + op + " must be a Uint64")
	//
	//		}
	//		N := utils.Field.ArithmeticField.Q.BitLen()
	//		shift = rightFactors.fac()[0].multiplicative.Uint64()
	//		if shift > uint64(N) {
	//			panic("right side operand of " + op + " must be smaller then bit size of the underlying field")
	//
	//		}
	//		return shift
	//	}
	//	switch currentConstraint.Output.Identifier {
	//	case "<<":
	//
	//		_, bitsLeft := currentCircuit.SPLIT(false, left, gateCollector)
	//		shift := fkt("<<")
	//		bitsScaled := []factor{}
	//		//bit[0] is the lsb,
	//		//if we left shift, we remove the leading bits, and Add same number of zeros before the lsb
	//		// 100011 << 3  becomes 100011000
	//		for i := 0; i < len(bitsLeft)-int(shift); i++ {
	//			tok := bitsLeft[i].CopyAndSetMultiplicative(new(big.Int).Lsh(bigOne, uint(i)+uint(shift)))
	//			bitsScaled = append(bitsScaled, tok)
	//		}
	//		out := gateCollector.Add(multiplicationGate(bitsScaled, Token{Type: DecimalNumberToken}.toFactors()))
	//		//if we ever want to access the bits of this new derived expression
	//		//we give back the bits we already computed
	//		// var v = x<<3, v[3] = x[0]
	//		for i := int(shift); i < len(bitsLeft)-int(shift); i++ {
	//			currentCircuit.functions[fmt.Sprintf("%v[%v]", out.Identifier, i)] = bitsLeft[i-int(shift)].primitiveReturnfunction()
	//		}
	//		return rets(out.toFactors(), nil), false
	//	case ">>":
	//		_, bitsLeft := currentCircuit.SPLIT(false, left, gateCollector)
	//		shift := fkt(">>")
	//		bitsScaled := []factor{}
	//		//bit[0] is the lsb,
	//		//if we left shift, we remove the leading bits, and Add same number of zeros before the lsb
	//		// 100011 >> 3  becomes 100
	//		for i := int(shift); i < len(bitsLeft); i++ {
	//			tok := bitsLeft[i].CopyAndSetMultiplicative(new(big.Int).Lsh(bigOne, uint(i)-uint(shift)))
	//
	//			bitsScaled = append(bitsScaled, tok)
	//		}
	//		out := gateCollector.Add(multiplicationGate(bitsScaled, Token{Type: DecimalNumberToken}.toFactors()))
	//		//if we ever want to access the bits of this new derived expression
	//		//we give back the bits we already computed
	//		// var v = x>>3, v[0] = x[3]
	//		for i := 0; i < len(bitsLeft)-int(shift); i++ {
	//			currentCircuit.functions[fmt.Sprintf("%v[%v]", out.Identifier, i)] = bitsLeft[i+int(shift)].primitiveReturnfunction()
	//
	//		}
	//		return rets(out.toFactors(), nil), false
	//	case ">>>":
	//		_, bitsLeft := currentCircuit.SPLIT(false, left, gateCollector)
	//		shift := fkt(">>>")
	//		bitsScaled := []factor{}
	//		//bit[0] is the lsb,
	//		//if we left shift, we remove the leading bits, and Add same number of zeros before the lsb
	//		// 100011 >>> 3  becomes 011100
	//		for i := 0; i < len(bitsLeft); i++ {
	//			tok := bitsLeft[i].CopyAndSetMultiplicative(new(big.Int).Lsh(bigOne, uint(utils.Mod(i-int(shift), len(bitsLeft)))))
	//
	//			bitsScaled = append(bitsScaled, tok)
	//		}
	//		out := gateCollector.Add(multiplicationGate(bitsScaled, Token{Type: DecimalNumberToken}.toFactors()))
	//		//if we ever want to access the bits of this new derived expression
	//		//we give back the bits we already computed
	//		// var v = x>>3, v[0] = x[3]
	//		for i := 0; i < len(bitsLeft); i++ {
	//			currentCircuit.functions[fmt.Sprintf("%v[%v]", out.Identifier, i)] = bitsLeft[utils.Mod(i+int(shift), len(bitsLeft))].primitiveReturnfunction()
	//
	//		}
	//		return rets(out.toFactors(), nil), false
	//	case "<<<":
	//		_, bitsLeft := currentCircuit.SPLIT(false, left, gateCollector)
	//		shift := fkt("<<<")
	//		bitsScaled := []factor{}
	//		//bit[0] is the lsb,
	//		//if we left shift, we remove the leading bits, and Add same number of zeros before the lsb
	//		// 100011 <<< 3  becomes 011100
	//		for i := 0; i < len(bitsLeft); i++ {
	//			tok := bitsLeft[i].CopyAndSetMultiplicative(new(big.Int).Lsh(bigOne, uint(utils.Mod(i+int(shift), len(bitsLeft)))))
	//
	//			bitsScaled = append(bitsScaled, tok)
	//		}
	//		out := gateCollector.Add(multiplicationGate(bitsScaled, Token{Type: DecimalNumberToken}.toFactors()))
	//		//if we ever want to access the bits of this new derived expression
	//		//we give back the bits we already computed
	//		for i := 0; i < len(bitsLeft); i++ {
	//			currentCircuit.functions[fmt.Sprintf("%v[%v]", out.Identifier, i)] = bitsLeft[utils.Mod(i-int(shift), len(bitsLeft))].primitiveReturnfunction()
	//
	//		}
	//		return rets(out.toFactors(), nil), false
	//	case "^": //bitwise xor
	//		_, _, xorIDs := currentCircuit.xor(currentConstraint, gateCollector)
	//
	//		bitsScaled := make(Tokens, len(xorIDs))
	//		for i, v := range xorIDs {
	//			bitsScaled[i] = v.CopyAndSetMultiplicative(new(big.Int).Lsh(bigOne, uint(i)))
	//		}
	//		eq := gateCollector.Add(multiplicationGate(bitsScaled, Token{Type: DecimalNumberToken}.toFactors()))
	//
	//		//say we split var x, from now on we can call x[i] to get the i'th bit
	//		for i, v := range xorIDs {
	//			currentCircuit.functions[fmt.Sprintf("%v[%v]", eq.Identifier, i)] = v.primitiveReturnfunction()
	//		}
	//		return rets(eq.toFactors(), nil), false
	//	case "&": //bitwise and
	//		_, _, andIDs := currentCircuit.and(currentConstraint, gateCollector)
	//
	//		bitsScaled := make(Tokens, len(andIDs))
	//		for i, v := range andIDs {
	//			bitsScaled[i] = v.CopyAndSetMultiplicative(new(big.Int).Lsh(bigOne, uint(i)))
	//		}
	//		eq := gateCollector.Add(multiplicationGate(bitsScaled, Token{Type: DecimalNumberToken}.toFactors()))
	//
	//		//say we split var x, from now on we can call x[i] to get the i'th bit
	//		for i, v := range andIDs {
	//			currentCircuit.functions[fmt.Sprintf("%v[%v]", eq.Identifier, i)] = v.primitiveReturnfunction()
	//		}
	//		return rets(eq.toFactors(), nil), false
	//	case "|": //bitwise or
	//		_, _, andIDs := currentCircuit.or(currentConstraint, gateCollector)
	//
	//		bitsScaled := make(Tokens, len(andIDs))
	//		for i, v := range andIDs {
	//			bitsScaled[i] = v.CopyAndSetMultiplicative(new(big.Int).Lsh(bigOne, uint(i)))
	//		}
	//		eq := gateCollector.Add(multiplicationGate(bitsScaled, Token{Type: DecimalNumberToken}.toFactors()))
	//
	//		//say we split var x, from now on we can call x[i] to get the i'th bit
	//		for i, v := range andIDs {
	//			currentCircuit.functions[fmt.Sprintf("%v[%v]", eq.Identifier, i)] = v.primitiveReturnfunction()
	//		}
	//		return rets(eq.toFactors(), nil), false
	//	}
	//	break
	case BooleanOperatorToken:
		break
	case ArithmeticOperatorToken:
		left := currentConstraint.Inputs[0]
		right := currentConstraint.Inputs[1]

		switch currentConstraint.Output.Identifier {
		case "*":
			leftFactors, _ = currentCircuit.compile(left, gateCollector)
			rightFactors, _ = currentCircuit.compile(right, gateCollector)

			if len(leftFactors) != 1 || len(rightFactors) != 1 {
				panic("")
			}
			if !leftFactors[0].facs.containsArgument() || !rightFactors[0].facs.containsArgument() {
				return rets(mulFactors(leftFactors.fac(), rightFactors.fac()), nil), currentConstraint.Output.Type == RETURN
			}
			commonFactor, newLeft, newRight := extractConstant(leftFactors.fac(), rightFactors.fac())
			mGate := multiplicationGate(newLeft, newRight)
			nTok := gateCollector.Add(mGate).CopyAndSetMultiplicative(commonFactor)

			return nTok.toBundle(), currentConstraint.Output.Type == RETURN
		case "/":
			//a / b
			leftFactors, _ = currentCircuit.compile(left, gateCollector)
			rightFactors, _ = currentCircuit.compile(right, gateCollector)

			if len(leftFactors) != 1 || len(rightFactors) != 1 {
				panic("")
			}

			if !rightFactors[0].facs.containsArgument() { // (x1+x2..)/6
				return rets(divideFactors(leftFactors.fac(), (rightFactors.fac()[0])), nil), currentConstraint.Output.Type == RETURN
			}

			gcdl, facL := factorSignature(leftFactors.fac())
			gcdR, facR := factorSignature(rightFactors.fac())
			//TODO is this a good idea?
			commonF := utils.Field.ArithmeticField.Div(gcdl, gcdR)

			//inverse gate enforces the input to be non zero
			//eg. b*b^-1 = 1
			var inversB = inverseGate(facR)
			var g = divisionGate(facL, facR)

			gateCollector.Add(inversB)
			nTok := gateCollector.Add(g)

			return nTok.CopyAndSetMultiplicative(commonF).toBundle(), currentConstraint.Output.Type == RETURN
		//case "**":
		//	//apply a fixed exponent exponentiation using a simple square and multiply method
		//	leftFactors, _ = currentCircuit.compile(left, gateCollector)
		//	rightFactors, _ = currentCircuit.compile(right, gateCollector)
		//	if len(leftFactors) != 1 || len(rightFactors) != 1 {
		//		panic("")
		//	}
		//
		//	if rightFactors[0].facs.containsArgument() { // (x1+x2..)/6
		//		panic("exponent must be a compile time constant")
		//	}
		//	if rightFactors.fac()[0].multiplicative.Sign() == -1 {
		//		rightFactors.fac()[0].multiplicative = utils.Field.ArithmeticField.Affine(rightFactors.fac()[0].multiplicative)
		//	}
		//	processedExponent := new(big.Int).Set(rightFactors.fac()[0].multiplicative)
		//
		//	base := leftFactors.fac().clone()
		//	result := Token{Type: DecimalNumberToken}.toFactors()
		//	//TODO use Yao's method instead.
		//	for ; processedExponent.Cmp(bigOne) == 1; processedExponent.Rsh(processedExponent, 1) {
		//
		//		if processedExponent.Bit(0) == 0 {
		//			square := gateCollector.Add(multiplicationGate(base, base))
		//			base = square.toFactors()
		//		} else {
		//			if result.isSingleNumber() {
		//				result = mulFactors(result, base)
		//			} else {
		//				y := gateCollector.Add(multiplicationGate(result, base))
		//				result = y.toFactors()
		//			}
		//
		//			square := gateCollector.Add(multiplicationGate(base, base))
		//			base = square.toFactors()
		//		}
		//
		//	}
		//	if result.isSingleNumber() {
		//		return rets(mulFactors(result, base), nil), false
		//	}
		//	combine := gateCollector.Add(multiplicationGate(result, base))
		//	result = combine.toFactors()
		//	return rets(result, nil), false

		case "+":
			leftFactors, _ = currentCircuit.compile(left, gateCollector)
			rightFactors, _ = currentCircuit.compile(right, gateCollector)
			addedFactors := leftFactors.fac().AddFactors(rightFactors.fac())
			return rets(addedFactors, nil), currentConstraint.Output.Type == RETURN

		case "-":
			leftFactors, _ = currentCircuit.compile(left, gateCollector)
			rightFactors, _ = currentCircuit.compile(right, gateCollector)
			rf := rightFactors.fac().Negate()
			addedFactors := rf.AddFactors(leftFactors.fac())
			return rets(addedFactors, nil), currentConstraint.Output.Type == RETURN
		}
		break
	case AssignmentOperatorToken:
		break
	default:
		panic("unsupported operation")
	}

	panic(currentConstraint)
}

func (currentCircuit *function) SPLIT(makeTheBitsAvailableInCurrentCircuit bool, toSplit *Constraint, gateCollector *gateContainer) (arg Token, bits Tokens) {

	in, _ := currentCircuit.compile(toSplit, gateCollector)
	if len(in.fac()) > 1 {
		tok := gateCollector.Add(summationGate(in.fac()))
		return tok, split(makeTheBitsAvailableInCurrentCircuit, currentCircuit, gateCollector, tok)
	}
	//if in.fac().isSingleNumber() {
	//	fmt.Println("you really wanna split a constant number into its bits? ")
	//}
	//if say : Split(5*x), then we need to introduce the constant multiplication gate. even if its stupid..
	if in.fac()[0].value.Cmp(bigOne) != 0 {
		one := Token{
			Type: DecimalNumberToken,
		}.toFactors()
		tok := gateCollector.Add(multiplicationGate(in.fac(), one))

		return tok, split(makeTheBitsAvailableInCurrentCircuit, currentCircuit, gateCollector, tok)
	}

	return in.fac()[0], split(makeTheBitsAvailableInCurrentCircuit, currentCircuit, gateCollector, in.fac()[0])
}

func split(makeTheBitsAvailableInCurrentCircuit bool, currentCircuit *function, gateCollector *gateContainer, arg Token) (bits Tokens) {
	N := utils.Field.ArithmeticField.Q.BitLen()

	//we create N new R1CS elements Z_i. 			//
	//each represents a bit of Z
	//each Z_i is introduced by a constraint  (Z_i - 1 ) * Z_i = 0, to ensure its either 0 or 1
	// Z_0 is the lsb

	bitsScaled := make(Tokens, N)
	bits = make(Tokens, N)
	for i := N - 1; i >= 0; i-- {

		nTok := Token{
			Type: ARGUMENT,
			//wirst the number, then the variable, to avoid collisions
			Identifier: fmt.Sprintf("%v[%v]", arg.Identifier, i),
		}

		var bit Token
		if fkt, ex := currentCircuit.functions[nTok.Identifier]; !ex {
			zeroOrOneGate := zeroOrOneGate(nTok.Identifier)
			bit = gateCollector.Add(zeroOrOneGate)
		} else {
			bit = Token{
				Type:       ARGUMENT,
				Identifier: fkt.Name,
			}
		}

		bitsScaled[i] = bit.CopyAndSetMultiplicative(new(big.Int).Lsh(bigOne, uint(i)))

		bits[i] = bit
		//say we split var x, from now on we can call x[i] to get the i'th bit
		if makeTheBitsAvailableInCurrentCircuit {
			currentCircuit.functions[nTok.Identifier] = bit.primitiveReturnfunction()
		}

		// we need to Add the bits during precompilations so we can access them like from an array
		//	currentCircuit.constraintMap
	}
	//Add the bits constraint \bits Z_i 2^i = Z to ensure that the Zi are the bit representation of Z

	//cConstraint[indexMap[g.value.identifier.identifier]] = g.extractedConstants
	eq := equalityGate(bitsScaled, arg.toFactors())
	eq.computeYourselfe = func(witness *[]*big.Int, set utils.FastBool, indexMap map[string]int) bool {
		if set.IsSet(indexMap[arg.Identifier]) {
			for i, bit := range bits {
				b := int64((*witness)[indexMap[arg.Identifier]].Bit(i))
				(*witness)[indexMap[bit.Identifier]] = big.NewInt(b)
				set.Set(indexMap[bit.Identifier])
			}
			return true
		}
		return false
	}
	gateCollector.Add(eq)
	return bits
}

func (currentCircuit *function) xor(currentConstraint *Constraint, gateCollector *gateContainer) (argLeft, argRight Token, xorIDS Tokens) {
	left := currentConstraint.Inputs[1]
	right := currentConstraint.Inputs[2]

	argLeft, bitsLeft := currentCircuit.SPLIT(false, left, gateCollector)
	argRight, bitsRight := currentCircuit.SPLIT(false, right, gateCollector)

	xorIDs := make(Tokens, len(bitsRight))

	for i := len(bitsLeft) - 1; i >= 0; i-- {
		// a xor b = c as arithmetic circuit (asserting that a,b \in {0,1}
		// 2a*b = c + a + b - 1
		xorIDs[i] = gateCollector.Add(xorGate(bitsLeft[i], bitsRight[i]))
	}
	return argLeft, argRight, xorIDs
}

func (currentCircuit *function) and(currentConstraint *Constraint, gateCollector *gateContainer) (argLeft, argRight Token, xorIDS Tokens) {
	left := currentConstraint.Inputs[1]
	right := currentConstraint.Inputs[2]

	argLeft, bitsLeft := currentCircuit.SPLIT(false, left, gateCollector)
	argRight, bitsRight := currentCircuit.SPLIT(false, right, gateCollector)

	andIDs := make(Tokens, len(bitsRight))

	for i := len(bitsLeft) - 1; i >= 0; i-- {
		//a*b = c
		andIDs[i] = gateCollector.Add(multiplicationGate(bitsLeft[i].toFactors(), bitsRight[i].toFactors()))
	}
	return argLeft, argRight, andIDs
}
func (currentCircuit *function) or(currentConstraint *Constraint, gateCollector *gateContainer) (argLeft, argRight Token, xorIDS Tokens) {
	left := currentConstraint.Inputs[1]
	right := currentConstraint.Inputs[2]

	argLeft, bitsLeft := currentCircuit.SPLIT(false, left, gateCollector)
	argRight, bitsRight := currentCircuit.SPLIT(false, right, gateCollector)

	orIds := make(Tokens, len(bitsRight))

	for i := len(bitsLeft) - 1; i >= 0; i-- {
		//ab = -c + a + b
		orIds[i] = gateCollector.Add(orGate(bitsLeft[i], bitsRight[i]))
	}
	return argLeft, argRight, orIds
}

func (currentCircuit *function) equalityGate(currentConstraint *Constraint, gateCollector *gateContainer) (facs Tokens) {

	argLeft, argRight, xorIDs := currentCircuit.xor(currentConstraint, gateCollector)

	// we now want to AND all xors
	// so first we now that our result bit will be one or zero
	// c1 * (1-c1) = 0
	var zg *Gate

	zg = zeroOrOneGate(argLeft.Identifier + "==" + argRight.Identifier)

	zg.computeYourselfe = func(witness *[]*big.Int, set utils.FastBool, indexMap map[string]int) bool {
		if set.IsSet(indexMap[argLeft.Identifier]) && set.IsSet(indexMap[argRight.Identifier]) {
			l := (*witness)[indexMap[argLeft.Identifier]]
			r := (*witness)[indexMap[argRight.Identifier]]
			set.Set(indexMap[zg.ID()])
			var result int64

			result = 1 - int64(utils.AbsInt(l.Cmp(r)))

			(*witness)[indexMap[zg.ID()]] = new(big.Int).SetInt64(result)
			return true
		}
		return false
	}

	// b * (1-b) = 0
	c1 := gateCollector.Add(zg)

	//nex we now that (Sum_i xor_i ) * (b) = 0
	// -> if b is 1, then all xors must be 0 as well
	gateCollector.Add(zeroConstraintGate(xorIDs, c1.toFactors()))

	//finally, if b =0 , then the sum over all xors is some number less then N
	// so we need to ensure that (N - Sum_i xor_i ) * (N - Sum_i xor_i )^-1 = 1-b
	//
	inverseSumtherm := gateCollector.Add(inverseGate(xorIDs))
	//
	var c3 = new(Gate)
	c3.leftIns = xorIDs
	c3.noNewOutput = true

	c3.outIns = Tokens{Token{
		Type:  DecimalNumberToken,
		value: bigOne,
	}, c1.Negate()}

	c3.rightIns = inverseSumtherm.toFactors()
	gateCollector.Add(c3)
	return c1.toFactors()
}

//constants, which have been excluded get added to the constraints at the end
//for example: given a expression x*z*23, will only create a constraint where x*z is multiplied
//this increases the chance to reuse the constrain if for example x*z*24 is called, we use the output of x*z and thereby safe a multiplication gate

type MultiplicationGateSignature struct {
	identifier      Token
	commonExtracted *big.Int //if the multiplicationGate had a extractable factor, it will be stored here
}

type Program struct {
	globalFunction *function
	PublicInputs   []string
}

func newProgram() (program *Program) {
	program = &Program{
		globalFunction: NewCircuit("global", nil),
		PublicInputs:   []string{"1"},
	}
	//handle the fixed functions.. either here or elsewhere
	program.globalFunction.functions = predeclaredFunctionsMap
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
	//we Add the neutral element here

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
		bund, rt := mainCircuit.compile(taks, container)
		container.completeFunction(bund.fac())
		if rt {
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

	//first we Add the public inputs
	for _, v := range p.PublicInputs {
		indexMap[v] = len(indexMap)
	}
	for _, v := range p.GetMainCircuit().InputIdentifiers {
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

	}

	insertValue := func(val Token, arr []*big.Int) {
		//if val.Typ.Type == DecimalNumberToken {
		//	arr[0] = val.multiplicative
		//	return
		//}
		if val.Identifier == "" {
			arr[0] = val.value
			return
		}
		index, ex := indexMap[val.Identifier]
		if !ex {
			panic(fmt.Sprintf("%v index not found!!!", val))
		}
		if val.value != nil {
			arr[index] = val.value
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
