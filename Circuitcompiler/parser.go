package Circuitcompiler

import (
	"errors"
	"fmt"
	"strconv"
)

type Parser struct {
	lexer          *Lexer
	ErrorHandler   func(e string)
	Err            error
	tokenChannel   chan Token
	done           chan struct{}
	log            bool
	constraintChan chan *Task

	//constraintBuilder chan<- Token
}

func (l *Parser) error(e string, a ...interface{}) {
	if l.ErrorHandler != nil {
		l.Err = errors.New(fmt.Sprintf(e, a...))
		l.ErrorHandler(e)
	} else {
		//since parser and lexer run concurrently, we use the lexers error handler, to get the line-number where the program failed
		l.lexer.Error(fmt.Sprintf(e, a...))
		//panic(fmt.Sprintf("%v ", e, ))
	}
}

func (p *Parser) nextToken() *Token {
	tok, done := p.lexer.NextToken()
	if done {
		//close(p.done)
		return &Token{Type: EOF}
	}
	for tok.Type == CommentToken {
		tok = p.nextToken()
	}
	return tok
}

func NewParser(code string, log bool) (p *Parser) {
	lexer := New(code, ProbablyWhitespaceState)
	lexer.Start()
	return &Parser{constraintChan: make(chan *Task), tokenChannel: make(chan Token), done: make(chan struct{}), lexer: lexer, log: log}
}

func Parse(code string) (p *Program) {

	parser := NewParser(code, false)
	toks := parser.stackAllTokens()
	p = newProgram()

	parser.PreCompile(p.globalFunction, toks)

	return p
}

func (c *Task) add(in *Task) *Task {
	c.Inputs = append(c.Inputs, in)
	return c
}
func (c *Task) Set(in Token) *Task {
	c.Description = in
	return c
}

// write some pattern matcher

//arrayCall := arrayCall[Number] | identifier
//arrayDefine := [Number]arrayDefine | identifier
//functionHeaderDefine := func(TypedArg) | func(TypedArg)(out)
// epx := (exp) | exp Operator exp |  identifier | Number | arrayCall | identifier(call) | functionHeaderDefine{stm} | functionHeaderDefine{stm}(call) | arrayDefine
// TypedArg := " " | identifier Type | TypedArg , identifier Type | identifier arrayDefine | identifier functionHeaderDefine
// out := " " | Type | out , Type | arrayDefine | functionHeaderDefine
//call := exp | call, exp

//i think i need to revive the contraint part.. the outFkt should only serve the purpose of type checking
func (p *Parser) parseExpression(stack []Token, context *function, in *Task) (outFktSignature *function) {
	//(exp)->exp
	stack = stripOfBrackets(stack)

	if len(stack) == 0 {

		return NewCircuit("", nil)
	}

	//can only be IN | Number
	if len(stack) == 1 {
		if stack[0].Type == DecimalNumberToken {
			stack[0].Type = FIELD
			in.Description = stack[0]
			return stack[0].primitiveReturnfunction()
		}
		if stack[0].Type == True || stack[0].Type == False {
			stack[0].Type = BOOL
			in.Description = stack[0]
			return stack[0].primitiveReturnfunction()
		}

		if stack[0].Type == IDENTIFIER_VARIABLE {
			v, ex := context.findFunctionInBloodline(stack[0].Identifier)
			if !ex {
				panic("")
			}
			in.Description = stack[0]
			return v.flatCopy()
		}

		p.error("Variable or number expected, got %v ", stack[0])
	}

	//maybe use different split style.. we now read everyting if no operator comes.
	l, binOperation, r := splitAtFirstHighestTokenType(stack, Operator)
	l, r = stripOfBrackets(l), stripOfBrackets(r)

	// exp Operator exp
	if binOperation.Type&Operator != 0 {

		lc, rc := &Task{}, &Task{}
		lfkt := p.parseExpression(l, context, lc)
		rfkt := p.parseExpression(r, context, rc)

		if eq, err := lfkt.hasEqualDescription(rfkt); !eq {
			p.error(err)
		}
		in.Description = Token{
			Type:       ArithmeticOperatorToken,
			Identifier: binOperation.Identifier,
		}
		in.Inputs = []*Task{lc, rc}

		return combineFunctions(binOperation, lfkt, rfkt, &function{})

	} else if binOperation.Type != 0 {
		p.error("unsuported operation %v", binOperation)
	}

	if stack[0].Type == IDENTIFIER_VARIABLE || stack[0].Type == FUNCTION_CALL {
		v, ex := context.findFunctionInBloodline(stack[0].Identifier)
		if !ex {
			panic("")
		}
		outFktSignature = v.flatCopy()
		in.Description.Identifier = stack[0].Identifier
		stack = stack[1:]
	}

FKTCALL:
	if stack[0].Identifier == "(" && outFktSignature != nil {
		in.Description.Type = FUNCTION_CALL
		rest, argFks, argConst := p.parseArguments(context, stack[1:], splitAtClosingBrackets)
		stack = rest
		in.Inputs = append(in.Inputs, argConst...)
		outFktSignature.getsLoadedWith(argFks)
		//whats the taskmap here?
		if len(stack) != 0 {
			if stack[0].Identifier == "(" {
				goto FKTCALL
			}
			if stack[0].Identifier == "[" {
				goto ARYCALL
			}
			panic("")
		}
		return outFktSignature
	}

ARYCALL:
	if stack[0].Identifier == "[" && outFktSignature != nil {
		in.Description.Type = ARRAY_CALL
		if len(outFktSignature.Dimension) == 0 {
			p.error("not an array")
		}

		left, rest, success := splitAtClosingSquareBrackets(stack)
		stack = rest
		if !success {
			p.error("")
		}
		outconst := &Task{}
		outfkt := p.parseExpression(left, context, outconst)
		in.Inputs = append(in.Inputs, outconst)
		//currently only static array access supported
		if ew, err := outfkt.HasIntegerOutput(); !ew {
			p.error(err)
		}

		outFktSignature.Dimension = outFktSignature.Dimension[1:]

		if len(stack) != 0 {
			//TODO we will overload the arraycall... hmm
			if stack[0].Identifier == "(" {
				goto FKTCALL
			}
			if stack[0].Identifier == "[" {
				goto ARYCALL
			}
			panic("")
		}
		return outFktSignature

	}

	if stack[0].Type == FUNCTION_DEFINE {
		in.Description.Type = FUNCTION_DEFINE

		p.Assert("(", stack[1])
		inside, statement := splitAtFirstHighestStringType(stack[1:], "{")
		outFktSignature = NewCircuit("", context)
		p.PrepareFunctionSignature(outFktSignature, inside)

		//dont need this

		in.FktInputs = append(in.FktInputs, outFktSignature)

		var s bool
		statement, stack, s = splitAtClosingSwingBrackets(statement)
		if !s {
			panic("closing brackets missing")
		}
		p.PreCompile(outFktSignature, statement)
		//we add the function to the constraint
		//so we cann later call it if needed

		//check if the returns match the header description
		ex, v := outFktSignature.taskStack.PeekLast()
		fktReturns := ex && v.Description.Type == RETURN
		if len(outFktSignature.OutputTypes) != 0 && !fktReturns {
			p.error("return missing")
		}
		b, err := outFktSignature.checkIfReturnsMatchHeader(v.FktInputs)
		if !b {
			panic(err)
		}

		//now check, if the fresh defined function is being called
		if len(stack) != 0 {
			if stack[0].Identifier == "(" {
				goto FKTCALL
			}
			if stack[0].Identifier == "[" {
				goto ARYCALL
			}
			panic("")
		}

		return
	}

	if stack[0].Identifier == "{" {

		//if something is in brackest, we expect all of
		//them things to be of equal type, and we return the type+ dimension

		inputs, restt, s := splitAtClosingSwingBrackets(stack)
		if !s || len(restt) != 0 {
			panic("closing brackets missing")
		}
		restAfterBracketsClosed, argFkts, argCtrs := p.parseArguments(context, inputs, splitNothing)
		if len(restAfterBracketsClosed) != 0 {
			panic(" ")
		}
		in.Inputs = argCtrs

		//all returned args must be of equal type
		for i := 1; i < len(argFkts); i++ {
			if eq, err := argFkts[i].hasEqualDescription(argFkts[i-1]); !eq {
				p.error(err)
			}
		}
		argFkts[0].Dimension = append([]int64{int64(len(argFkts))}, argFkts[0].Dimension...)

		//since we tested that all types in the brackets are the same, we return one, to test type assertion correctness in the layer above
		return argFkts[0]
	}
	//lol look at this shit.. this is also possible
	//var a = func()[2]int{return [2]int{1,2}}()[1]
	//

	if stack[0].Identifier == "[" {
		in.Description.Type = ARRAY_DECLARE
		//var a = [2]func(a bool)(bool){}

		//we will return an empty constraint. thats fo sure
		//since an array declaration is not a task that has to be exectured?
		var dim []int64
		stack, dim = p.readStaticDimension(stack, []int64{})

		arrayType, arrayDeclare := splitTokensAtFirstString(stack, "{")
		p.Assert("{", arrayDeclare[0])
		_, rest, s := splitAtClosingSwingBrackets(arrayDeclare)
		if !s || len(rest) != 0 {
			p.error("")
		}
		outFktSignature = NewCircuit("", context)
		outFktSignature.Dimension = dim

		p.prepareType(outFktSignature, arrayType)

		args := p.parseExpression(arrayDeclare, context, in)
		//we do type checking of the outmost brakets here
		if len(in.Inputs) != int(dim[0]) {
			p.error("expect %v array inputs, got %v", (dim[0]), len(in.Inputs))
		}
		//we only need to check for one, cuz equivalenz of all other has been asserted in the brackets part
		if eq, err := args.hasEqualDescription(outFktSignature); !eq {
			p.error(err)
		}
		//not so smooth..
		//the problem is, that we need to store the array data somwhere..
		//outFktSignature.taskStack = &watchstack{
		//	data: []*Task{in},
		//}
		in.FktInputs = []*function{outFktSignature}
		return outFktSignature
	}

	panic("unexpected reach")

	return

}

func (p *Parser) parseArguments(context *function, stack []Token, bracketSplitFunction func(in []Token) (cutLeft, cutRight []Token, success bool)) (rest []Token, args []*function, argConstr []*Task) {

	functionInput, rem, success := bracketSplitFunction(stack)

	if !success {
		p.error("closing brackets missing")
	}
	if len(functionInput) == 0 {
		return rem, nil, nil

	}
	//arguments can be expressions, so we need to parse them
	for arguments, remm := splitAtFirstHighestStringType(functionInput, ","); ; arguments, remm = splitAtFirstHighestStringType(remm, ",") {
		arguments = removeLeadingAndTrailingBreaks(arguments)
		inConstraint := &Task{}
		f := p.parseExpression(arguments, context, inConstraint)
		args = append(args, f)
		argConstr = append(argConstr, inConstraint)
		if remm == nil {
			break
		}
	}
	return rem, args, argConstr

}

func (p *Parser) PrepareFunctionSignature(newFunction *function, stack []Token) {

	inputs, outputs, success := splitAtClosingBrackets(stack)
	if !success {
		p.error("closing brackets missing")
	}
	for arguments, remm := splitAtFirstHighestStringType(inputs, ","); ; arguments, remm = splitAtFirstHighestStringType(remm, ",") {
		arguments = removeLeadingAndTrailingBreaks(arguments)
		p.prepareArguments(newFunction, arguments)
		if remm == nil {
			break
		}
	}
	outputs = stripOfBrackets(outputs)
	for arguments, remm := splitAtFirstHighestStringType(outputs, ","); ; arguments, remm = splitAtFirstHighestStringType(remm, ",") {
		arguments = removeLeadingAndTrailingBreaks(arguments)
		p.prepareReturns(newFunction, arguments)
		if remm == nil {
			break
		}
	}

	return
}

func (p *Parser) prepareArguments(current *function, stack []Token) {
	if len(stack) == 0 {
		return
	}
	if len(stack) == 1 {
		p.error("need argument + type")
	}

	if stack[0].Type != IDENTIFIER_VARIABLE {
		p.error("identifier expected")
	}
	if _, ex := current.functions[stack[0].Identifier]; ex {
		p.error(fmt.Sprintf("argument: %v , is not unique ", stack[0].Identifier))
	}
	current.InputIdentifiers = append(current.InputIdentifiers, stack[0].Identifier)

	argument := NewCircuit(stack[0].Identifier, nil)
	p.prepareType(argument, stack[1:])
	if len(argument.InputTypes) == 0 && len(argument.OutputTypes) == 1 {
		current.InputTypes = append(current.InputTypes, argument.OutputTypes[0])
	} else {
		current.InputTypes = append(current.InputTypes, returnTypes{
			functionReturn: true,
			fkt:            argument,
		})
	}

	current.functions[stack[0].Identifier] = argument
	return

}
func (p *Parser) prepareReturns(current *function, stack []Token) {
	if len(stack) == 0 {
		return
	}
	argument := NewCircuit("", nil)
	p.prepareType(argument, stack)
	if len(argument.InputTypes) == 0 && len(argument.OutputTypes) == 1 {
		current.OutputTypes = append(current.OutputTypes, argument.OutputTypes[0])
	} else {
		current.OutputTypes = append(current.OutputTypes, returnTypes{
			functionReturn: true,
			fkt:            argument,
		})
	}
	return

}
func (p *Parser) prepareType(argument *function, stack []Token) {
	if stack[0].Identifier == "[" {
		stack, argument.Dimension = p.readStaticDimension(stack, []int64{})
	}

	if stack[0].Type&Types != 0 {
		//a bool
		tok := Token{
			Type:       stack[0].Type,
			isArgument: true,
			//TODO hmm I dont like this
			value: bigOne,
		}
		retTyp := returnTypes{
			typ: tok,
		}
		argument.OutputTypes = append(argument.OutputTypes, retTyp)
	} else if stack[0].Type == FUNCTION_DEFINE {
		p.PrepareFunctionSignature(argument, stack[1:])
	} else {
		p.error("not defined type %v", stack[1].Identifier)
	}
}

func (p *Parser) PreCompile(currentCircuit *function, tokens []Token) {

	tokens = removeLeadingAndTrailingBreaks(tokens)

	if len(tokens) == 0 {
		return
	}
	if tokens[0].Type == EOF {
		close(p.done)
		return
	}

	switch typ := tokens[0].Type; typ {
	case PUBLIC:

		if tokens[1].Identifier != "{" {
			p.error("define publics via: 'public {a,b,c}'")
		}
		inside, rest, success := splitAtClosingSwingBrackets(tokens[1:])
		if !success {
			panic("closing brackets missing")
		}
		for arguments, remm := splitAtFirstHighestStringType(inside, ","); ; arguments, remm = splitAtFirstHighestStringType(remm, ",") {
			arguments = removeLeadingAndTrailingBreaks(arguments)
			//todo array overload
			p.AssertTypes(arguments, IDENTIFIER_VARIABLE)
			//the first input is the thing to overwrite
			_, ex := currentCircuit.findFunctionInBloodline(arguments[0].Identifier)
			if !ex {
				p.error(fmt.Sprintf("variable %s not declared", arguments[0].Identifier))
			}
			currentCircuit.SNARK_Public_Statement = append(currentCircuit.SNARK_Public_Statement, arguments[0].Identifier)
			if remm == nil {
				break
			}
		}

		p.PreCompile(currentCircuit, rest)
		return
	case IMPORT:
		//if len(tokens) == 1 || tokens[1].Type != IDENTIFIER_VARIABLE {
		//	p.error("import failed")
		//}
		//
		//dat, err := ioutil.ReadFile(tokens[1].Identifier)
		//if err != nil {
		//	panic(err)
		//}
		//tmpParser := NewParser(string(dat), false)
		//t := tmpParser.stackAllTokens()
		////we glue em together, but remove the EOF from t first
		//p.PreCompile(currentCircuit, append(t[:len(t)-1], tokens[2:]...))
		////TODO then?
	case FUNCTION_DEFINE:
		//func maaain(){}
		p.AssertIdentifiedType(FUNCTION_CALL, tokens[1])

		if _, ex := currentCircuit.functions[tokens[1].Identifier]; ex {
			p.error(fmt.Sprintf("function %s already declared", tokens[1].Identifier))
		}
		if _, ex := predeclaredFunctionsMap[tokens[1].Identifier]; ex {
			panic(fmt.Sprintf("cannot redeclare predefined function %s ", tokens[1].Identifier))
		}
		fkt := NewCircuit(tokens[1].Identifier, currentCircuit)
		//we Add the function
		currentCircuit.functions[fkt.Name] = fkt

		p.Assert("(", tokens[2])

		inside, rest := splitAtFirstHighestStringType(tokens[2:], "{")

		p.PrepareFunctionSignature(fkt, inside)

		inside, rest, success := splitAtClosingSwingBrackets(rest)
		if !success {
			panic("closing brackets missing")
		}

		//we first compile whats outside the function. so
		//this enables us, to access not yet defined functions inside
		//the function scope
		p.PreCompile(currentCircuit, rest)

		//now compile the function
		p.PreCompile(fkt, inside)

		//check if the returns match the header description

		ex, v := fkt.taskStack.PeekLast()
		lastTaskIsReturn := ex && v.Description.Type == RETURN

		if len(fkt.OutputTypes) != 0 && !lastTaskIsReturn {
			p.error("return missing")
		}
		b, err := fkt.checkIfReturnsMatchHeader(v.FktInputs)
		if !b {
			panic(err)
		}

		return
	case IF: //if a<b { }   if (a<b) {

		ifFunction := NewCircuit("", currentCircuit)

		ifConstraint := &Task{
			Description: Token{
				Type: IF_FUNCTION_CALL,
			},
			FktInputs: []*function{ifFunction},
		}
		currentCircuit.taskStack.add(ifConstraint)

		var fk = func(t []Token) (condition, statement, rest []Token, success bool) {
			condition, rest = splitAtFirstHighestStringType(t, "{")
			statement, rest, success = splitAtClosingSwingBrackets(rest)
			if !success {
				panic("")
			}
			if removeLeadingBreaks(rest)[0].Type != ELSE {
				return condition, statement, rest, false
			}
			return
		}

		statements := [][]Token{}
		conditions := [][]Token{}
		c, s, afterIFElse, contin := fk(tokens)
		for ; ; c, s, afterIFElse, contin = fk(afterIFElse) {
			statements = append(statements, s)
			conditions = append(conditions, c)
			if !contin {
				break
			}
		}

		p.PreCompile(currentCircuit, afterIFElse)
		for i, statement := range statements {

			tsk := NewCircuit("", currentCircuit)
			ifConstraint := &Task{
				Description: Token{
					Type: IF,
				},
				FktInputs: []*function{tsk},
			}
			if i == 0 {
				p.AssertTypes(conditions[i], IF)
				c := &Task{}
				f := p.parseExpression(conditions[0][1:], currentCircuit, c)
				if b, err := f.HasBooleanOutput(); !b {
					p.error(err)
				}
				ifConstraint.Inputs = append(ifConstraint.Inputs, c)
			} else if i < len(statements)-1 {
				p.AssertTypes(conditions[i], ELSE, IF)
				c := &Task{}
				f := p.parseExpression(conditions[i][2:], currentCircuit, c)
				if b, err := f.HasBooleanOutput(); !b {
					p.error(err)
				}
				ifConstraint.Inputs = append(ifConstraint.Inputs, c)
			} else {
				if len(conditions[i]) > 1 {
					p.AssertTypes(conditions[i], ELSE, IF)
					c := &Task{}
					f := p.parseExpression(conditions[i][2:], currentCircuit, c)
					if b, err := f.HasBooleanOutput(); !b {
						p.error(err)
					}
					ifConstraint.Inputs = append(ifConstraint.Inputs, c)
				} else {
					p.AssertTypes(conditions[i], ELSE)
					ifConstraint.Description.Type = ELSE
					//no condition
				}
			}

			ifFunction.taskStack.add(ifConstraint)

			//todo note that if we return inside a nested if. we get problem with type checking the returns
			p.PreCompile(tsk, statement)

			ex, v := tsk.taskStack.PeekLast()
			lastTaskIsReturn := ex && v.Description.Type == RETURN

			if lastTaskIsReturn {
				b, err := currentCircuit.checkIfReturnsMatchHeader(v.FktInputs)
				if !b {
					panic(err)
				}
			}

		}

		return
	case FOR:
		forFkt := NewCircuit("forLoop", currentCircuit)
		//we Add the function
		//currentCircuit.functions[fkt.Name] = fkt
		currentCircuit.taskStack.add(&Task{
			Description: Token{
				Type: FUNCTION_CALL,
			},
			FktInputs: []*function{forFkt},
		})

		// for a ; b ;c {  rest
		condition, rest := splitAtFirstHighestStringType(tokens[1:], "{")

		re := &Task{
			Description: Token{
				Type: FOR,
			},
			Inputs: make([]*Task, 2),
			//inputs are the condition + optional assignment after the loop
			//fktInput is all inside the for statement {}
		}
		forFkt.taskStack.add(re)

		// a ; b ;c
		var arguments []Tokens
		condition = stripOfBrackets(condition)
		for argument, remm := splitAtFirstHighestStringType(condition, ";"); len(arguments) < 3; argument, remm = splitAtFirstHighestStringType(remm, ";") {
			argument = removeLeadingAndTrailingBreaks(argument)
			arguments = append(arguments, argument)
			if remm == nil {
				break
			}
		}
		switch len(arguments) {
		case 1: // a
			constr := &Task{}
			f := p.parseExpression(arguments[0], forFkt, constr)
			re.Inputs[0] = constr
			if b, err := f.HasBooleanOutput(); !b {
				panic(err)
			}

			// insideFor } rest
			insideFor, r, success := splitAtClosingSwingBrackets(rest)
			rest = r
			if !success {
				panic("closing brackets missing")
			}

			stm := NewCircuit("forStatement", forFkt)
			p.PreCompile(stm, insideFor)
			re.Inputs[1] = &Task{
				Description: Token{
					Type: FUNCTION_CALL,
				},
				FktInputs: []*function{stm},
			}
		case 3: // a ; b ;c

			//a
			p.PreCompile(forFkt, arguments[0])

			if b, t := forFkt.taskStack.PeekLast(); !b || (t.Description.Type != VARIABLE_DECLARE) && (t.Description.Type != VARIABLE_OVERLOAD) {
				panic("for either takes 1 boolean expression, or 3 expressions, where the first is an assignment, second a boolean condition, third an assignment")
			}

			//b
			constr := &Task{}
			f := p.parseExpression(arguments[1], forFkt, constr)
			re.Inputs[0] = constr
			if b, err := f.HasBooleanOutput(); !b {
				panic(err)
			}

			// insideFor } rest
			insideFor, r, success := splitAtClosingSwingBrackets(rest)
			rest = r
			if !success {
				panic("closing brackets missing")
			}

			stm := NewCircuit("forStatement", forFkt)
			p.PreCompile(stm, insideFor)
			re.Inputs[1] = &Task{
				Description: Token{
					Type: FUNCTION_CALL,
				},
				FktInputs: []*function{stm},
			}
			//c
			if arguments[2].Len() != 0 {
				p.PreCompile(stm, arguments[2])
				b, t := forFkt.taskStack.PeekLast()
				if !b || (t.Description.Type != VARIABLE_DECLARE) && (t.Description.Type != VARIABLE_OVERLOAD) {
					panic("for either takes 1 boolean expression, or 3 expressions, where the first is an assignment, second a boolean condition, third an assignment")
				}
			}
		default:
			panic("")
		}
		p.PreCompile(currentCircuit, rest)

		return

	case RETURN:
		re := &Task{
			Description: Token{
				Type: RETURN,
			},
		}

		tokens, re.FktInputs, re.Inputs = p.parseArguments(currentCircuit, tokens[1:], splitNothing)
		currentCircuit.taskStack.add(re)
		if len(tokens) != 0 {
			fmt.Sprintf("unreachable stuff after return..")
		}
		return
	case FUNCTION_CALL:
		//fkt(args...)    equal(a,2) -> creates assertion gates s.t. a=2

		varConst := &Task{
			Description: tokens[0],
		}
		if _, ex := currentCircuit.findFunctionInBloodline(tokens[0].Identifier); !ex {
			if _, ex = predeclaredFunctionsMap[tokens[0].Identifier]; ex {
				panic(fmt.Sprintf("function %s not declared", tokens[0].Identifier))
			}
		}
		//todo.. what if we call a function with a function call as argument, of a function that has not yet been declared
		tokens, _, varConst.Inputs = p.parseArguments(currentCircuit, tokens[1:], splitAtClosingBrackets)
		currentCircuit.taskStack.add(varConst)

		p.PreCompile(currentCircuit, tokens)
		return
	default:

		if typ == IDENTIFIER_VARIABLE || typ == VARIABLE_DECLARE {

			if typ == VARIABLE_DECLARE {
				tokens = tokens[1:]
			}

			overload, expr := splitAtFirstHighestStringType(tokens, "=")
			toOverload := &Task{
				Description: Token{
					Type: UNASIGNEDVAR,
				},
			}
			overloadWith := &Task{
				Description: Token{
					Type: RETURN,
				},
			}
			varConst := &Task{
				Description: Token{
					Type: VARIABLE_OVERLOAD,
				},
				Inputs: []*Task{toOverload, overloadWith},
			}

			for argument, remm := splitAtFirstHighestStringType(overload, ","); ; argument, remm = splitAtFirstHighestStringType(remm, ",") {
				argument = removeLeadingAndTrailingBreaks(argument)
				p.AssertTypes(argument, IDENTIFIER_VARIABLE)
				id := argument[0]
				var f *function
				var ex bool
				var constr = &Task{}

				if typ == IDENTIFIER_VARIABLE {
					//the first token is the thing to overwrite
					f, ex = currentCircuit.findFunctionInBloodline(id.Identifier)
					if !ex {
						p.error(fmt.Sprintf("variable %s not declared", id.Identifier))
					}

					//we also allow overload of array elements. thats why we need to parse in the IV case
					f = p.parseExpression(argument, currentCircuit, constr)
					toOverload.FktInputs = append(toOverload.FktInputs, f)
				}

				if typ == VARIABLE_DECLARE {
					if len(argument) != 1 {
						panic("pure identifier expected")
					}

					if _, ex := currentCircuit.functions[(id.Identifier)]; ex {
						panic(fmt.Sprintf("variable %s already declared", id.Identifier))
					}
					if _, ex := predeclaredFunctionsMap[(id.Identifier)]; ex {
						panic(fmt.Sprintf("cannot redeclare a predefined function. %s already declared", id.Identifier))
					}
					//add placeholder to avaid var a,a = sdaf
					currentCircuit.functions[(id.Identifier)] = &function{}
					constr = id.toConstraint()
				}
				toOverload.Inputs = append(toOverload.Inputs, constr)

				if remm == nil {
					break
				}
			}
			currentCircuit.taskStack.add(varConst)

			var oneOnOneMapping bool //we mimic golang. either one to one assignments, or one to all
			argument, rest := splitAtFirstHighestStringType(expr, ",", "\n")
			for ; ; argument, rest = splitAtFirstHighestStringType(rest, ",", "\n") {
				argument = removeLeadingAndTrailingBreaks(argument)
				constr := &Task{}
				fk := p.parseExpression(argument, currentCircuit, constr)

				if typ == IDENTIFIER_VARIABLE {
					//check if the assignment types match
					if eq, _ := toOverload.FktInputs[0].hasEqualDescription(fk); eq {
						toOverload.FktInputs = toOverload.FktInputs[1:]
						overloadWith.Inputs = append(overloadWith.Inputs, constr)
						// var a,b = c,d
						oneOnOneMapping = true
					} else {
						// var a,b = f()
						if len(toOverload.Inputs) != len(fk.OutputTypes) || oneOnOneMapping {
							p.error("assingment missmatch")
						}
						for i, f := range toOverload.FktInputs {
							if eq, err := f.hasEqualDescription2(fk.OutputTypes[i]); !eq {
								p.error(err)
							}
						}
						overloadWith.Inputs = []*Task{constr}
						break
					}

					if len(toOverload.Inputs) == len(overloadWith.Inputs) {
						break
					}

				}
				//var f = func(bool) (bool,bool){return true,true}
				//var a, b = f(true)
				if typ == VARIABLE_DECLARE {
					//var a,b = f()
					if len(fk.OutputTypes) == len(toOverload.Inputs) && len(fk.InputTypes) == 0 {
						if oneOnOneMapping {
							panic("")
						}
						for i, f := range toOverload.Inputs {
							nfkt := fk.CopyHeaderOnly()
							nfkt.OutputTypes = []returnTypes{nfkt.OutputTypes[i]}
							currentCircuit.functions[f.Description.Identifier] = nfkt
						}
						overloadWith.Inputs = []*Task{constr}
						break
					}
					//var a,b = c,d
					oneOnOneMapping = true
					currentCircuit.functions[toOverload.Inputs[len(overloadWith.Inputs)].Description.Identifier] = fk
					overloadWith.Inputs = append(overloadWith.Inputs, constr)

					if len(toOverload.Inputs) == len(overloadWith.Inputs) {
						break
					}
				}

			}
			p.PreCompile(currentCircuit, rest)
			return
		}

		p.error("%statement", tokens)
	}
	return
}

func (p *Parser) readDimension(toks []Token, infkt *function, inConstr *Task) (rest []Token) {
	if len(toks) == 0 {
		return nil
	}
	if toks[0].Identifier != "[" {
		return toks
	}

	left, right, success := splitAtClosingSquareBrackets(toks)
	if !success {
		p.error("")
	}
	outconst := &Task{}
	outfkt := p.parseExpression(left, infkt, outconst)

	//currently only static array access supported
	if ew, err := outfkt.HasIntegerOutput(); !ew {
		p.error(err)
	}
	inConstr.Inputs = append(inConstr.Inputs, outconst)

	return p.readDimension(right, infkt, inConstr)
}
func (p *Parser) readStaticDimension(toks []Token, in []int64) (rest []Token, out []int64) {
	if len(toks) == 0 {
		return nil, in
	}
	if toks[0].Identifier != "[" {
		return toks, in
	}
	left, right, success := splitAtClosingSquareBrackets(toks)
	if !success {
		p.error("")
	}
	if len(left) != 1 || left[0].Type != DecimalNumberToken {
		p.error("Slice not supported. Only static arrays so far. Integer expected")
	}
	in = append(in, left[0].value.Int64())

	return p.readStaticDimension(right, in)
}

func (p *Parser) loadDimension(toks []Token, in []int64) (res []int64) {
	if len(toks) == 0 {
		return in
	}

	if len(toks) < 3 {
		p.error("invalid array")
	}
	if toks[0].Identifier != "[" || toks[2].Identifier != "]" || toks[1].Type != DecimalNumberToken {
		p.error("invalid array\"")
	}

	return p.loadDimension(toks[3:], append(in, toks[1].value.Int64()))
}

func (p *Parser) resolveArrayName(currentCircuit *function, con *Task) (composedName string, arrayDimension []int64, isArray bool) {

	composedName = con.Description.Identifier
	//if len(c.InputIdentifiers) < 1 {
	//	panic("accessing array index failed")
	//}
	for _, in := range con.Inputs {
		bund, _ := currentCircuit.compile(in, newGateContainer())
		if bund.fac().containsArgument() {
			p.error("cannot access array dynamically in an arithmetic circuit currently")
		}
		if len(bund.fac()) > 1 {
			panic("unexpected")
		}
		tmp, err := strconv.ParseInt(bund.fac()[0].Identifier, 10, 64)
		if err != nil || tmp < 0 {
			p.error(err.Error())
		}
		arrayDimension = append(arrayDimension, tmp)
		composedName += fmt.Sprintf("[%v]", tmp)
		return composedName, arrayDimension, true
	}
	return
}
func (p *Parser) loadArray(current *function, name string, constaint *Task, dim []int64, pos []int, expecedType Token) {

	//single variable assignment
	// bool a = 1
	if dim == nil && constaint.Inputs == nil {
		//staticTypeCheck(constaint)
		return
	}
	if len(constaint.Inputs) != int(dim[0]) {
		p.error("array assignment size missmatch")
	}
	//array assignment
	if len(dim) == 1 {
		st := name
		for _, v := range pos {
			st += fmt.Sprintf("[%v]", v)
		}
		for i := 0; i < int(dim[0]); i++ {
			id := st + fmt.Sprintf("[%v]", i)
			// should we check if the assigned types match not, or at compile time?
			//staticTypeCheck(constaint.InputTypes[i])
			current.functions[id] = constaint.Inputs[i].primitiveReturnfunction(expecedType)
		}
		return
	}
	for i, v := range constaint.Inputs {
		p.loadArray(current, name, v, dim[1:], append(pos, i), expecedType)
	}

}

func (p *Parser) AssertIdentifier(token Token) {
	if token.Type != IDENTIFIER_VARIABLE || token.Identifier == "" {
		p.error("identifier expected")
	}
}
func (p *Parser) Assert(in string, tok Token) {
	if tok.Identifier != in {
		p.error("expected %v, got %v", in, tok.Identifier)
	}
}
func (p *Parser) AssertIdentifiedType(typ TokenType, tok Token) {
	if tok.Identifier == "" {
		p.error("expected identifier")
	}
	if tok.Type != typ {
		p.error("expected type %v, got %v", typ, tok.Type)
	}
}
func (p *Parser) AssertTypes(toks []Token, types ...TokenType) {
	for i, v := range types {
		if toks[i].Type != v {
			p.error("expected %v, got %v", toks[:len(types)], types)
		}
	}
}

func removeTrailingBreaks(in []Token) (out []Token) {
	if len(in) == 0 {
		return
	}
	if in[len(in)-1].Identifier == "\n" {
		return removeLeadingBreaks(in[:len(in)-1])
	}
	return in
}
func removeLeadingAndTrailingBreaks(in []Token) (out []Token) {
	return removeLeadingBreaks(removeTrailingBreaks(in))
}

func removeLeadingBreaks(in []Token) (out []Token) {
	if len(in) == 0 {
		return
	}
	if in[0].Identifier == "\n" {
		return removeLeadingBreaks(in[1:])
	}
	return in

}

func (p *Parser) stackAllTokens() []Token {
	var stack []Token
	for tok := p.nextToken(); tok.Type != EOF; tok = p.nextToken() {
		stack = append(stack, *tok)
	}
	stack = append(stack, Token{
		Type: EOF,
	})
	return stack
}

//splitAt takes takes a string S and a token array and splits st: abScdasdSf -> ( ab,cdas, F  )
//if S does not occur it returns ( in , []Tokens{} )
func splitAt(in []Token, splitAt string) (out [][]Token) {

	for l, r := splitTokensAtFirstString(in, splitAt); ; l, r = splitTokensAtFirstString(r, splitAt) {
		if len(l) > 0 {
			out = append(out, l)
			continue
		}
		if len(r) > 1 && r[0].Identifier == splitAt {
			r = r[1:]
			continue
		}
		return
	}
}

//splitTokensAtFirstString takes takes a string S and a token array and splits st: abScd -> ( ab , Scd )
//if S does not occur it returns ( in , []Tokens{} )
func splitTokensAtFirstString(in []Token, splitAt string) (cutLeft, cutRight []Token) {
	for i := 0; i < len(in); i++ {
		if in[i].Identifier == splitAt {
			return in[:i], in[i:]
		}
	}
	return in, cutRight
}

//splitAtFirstHighestTokenType takes takes a string S and a token array and splits st:
func splitAtFirstHighestStringType(in []Token, splitAt ...string) (cutLeft []Token, cutRight []Token) {
	ctr1 := 0
	ctr2 := 0
	ctr3 := 0
	for i := 0; i < len(in); i++ {

		if ctr1|ctr2|ctr3 == 0 {
			for _, s := range splitAt {
				if in[i].Identifier == s {
					if i == len(in)-1 {
						return in[:i], cutRight
					}
					return in[:i], in[i+1:]
				}
			}
		}

		switch in[i].Identifier {
		case "(":
			ctr1++
		case "{":
			ctr3++
		case "[":
			ctr2++
		case ")":
			ctr1--
		case "}":
			ctr3--
		case "]":
			ctr2--
		default:

		}

	}
	return in, nil
}

//splitAtFirstHighestTokenType takes takes a string S and a token array and splits st:
func splitAtFirstHighestTokenType(in []Token, splitAt TokenType) (cutLeft []Token, tok Token, cutRight []Token) {
	ctr1 := 0
	ctr2 := 0
	ctr3 := 0
	for i := 0; i < len(in); i++ {
		if (in[i].Type&splitAt) != 0 && ctr1|ctr2|ctr3 == 0 {
			if i == len(in)-1 {
				return in[:i], in[i], cutRight
			}
			return in[:i], in[i], in[i+1:]
		}

		switch in[i].Identifier {
		case "(":
			ctr1++
		case "{":
			ctr3++
		case "[":
			ctr2++
		case ")":
			ctr1--
		case "}":
			ctr3--
		case "]":
			ctr2--
		default:

		}

	}
	return in, Token{}, nil
}

func stripOfBrackets(in []Token) []Token {
	if isSurroundedByBrackets(in) && in[0].Type != FUNCTION_CALL {
		return stripOfBrackets(in[1 : len(in)-1])
	}
	return in
}

func isSurroundedByBrackets(in []Token) bool {
	if len(in) == 0 {
		return false
	}
	_, r, b := splitAtClosingBrackets(in)
	if b && len(r) == 0 {
		return true
	}
	return false
}

//splitAtClosingBrackets takes token array asserting that the opening brackets
//are not contained! Returns the slices without the closing bracket in an of them!
//note that this behaviour differs from splitTokensAtFirstString
//example "asdf)jkl" -> "asdf" ,"jkl" ,true
//"(asdf)jkl" -> nil,nil,false
//"(asdf))jkl" -> "(asdf)","jkl",true
func splitAtClosingBrackets(in []Token) (cutLeft, cutRight []Token, success bool) {
	return splitAtClosingStringBrackets(in, "(", ")")
}
func splitNothing(in []Token) (cutLeft, cutRight []Token, success bool) {
	return in, nil, true
}
func splitAtClosingSwingBrackets(in []Token) (cutLeft, cutRight []Token, success bool) {
	return splitAtClosingStringBrackets(in, "{", "}")
}
func splitAtClosingSquareBrackets(in []Token) (cutLeft, cutRight []Token, success bool) {
	return splitAtClosingStringBrackets(in, "[", "]")
}

func splitAtClosingStringBrackets(in []Token, open, close string) (cutLeft, cutRight []Token, success bool) {
	ctr := 1
	start := 1
	if in[0].Identifier != open {
		start = 0
	}

	for i := start; i < len(in); i++ {
		if in[i].Identifier == open {
			ctr++
		}
		if in[i].Identifier == close {
			ctr--
			if ctr == 0 {
				if i == len(in)-1 {
					return in[start:i], nil, true
				}
				return in[start:i], in[i+1:], true
			}
		}
	}
	return nil, nil, false
}

// Task is the data structure of a flat code operation
type Task struct {
	Description Token
	Inputs      []*Task
	FktInputs   []*function
}

func (c Task) String() string {
	return fmt.Sprintf("|%v|", c.Description)
}

//clone returns a deep copy of c
func (c *Task) clone() *Task {
	in := make([]*Task, len(c.Inputs))
	fkts := make([]*function, len(c.FktInputs))
	for i, cc := range c.Inputs {
		in[i] = cc.clone()
	}
	for i, cc := range c.FktInputs {
		fkts[i] = cc.flatCopy()
	}
	return &Task{
		Description: c.Description.copy(),
		Inputs:      in,
		FktInputs:   fkts,
	}
}

//outdated.. delete soon
func ArrayStringBuild(in []int64, res string, coll *[]string) {
	if len(in) == 0 {
		*coll = append(*coll, res)
		return
	}
	for j := int64(0); j < in[0]; j++ {
		str := fmt.Sprintf("%v[%v]", res, j)
		ArrayStringBuild(in[1:], str, coll)
	}
}
func ArrayString(dimension []int64) string {
	return fmt.Sprintf("%v", dimension)
}
