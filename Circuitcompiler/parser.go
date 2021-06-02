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
	constraintChan chan *Constraint

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
	return &Parser{constraintChan: make(chan *Constraint), tokenChannel: make(chan Token), done: make(chan struct{}), lexer: lexer, log: log}
}

func Parse(code string) (p *Program) {

	parser := NewParser(code, false)
	toks := parser.stackAllTokens()
	p = newProgram()

	parser.PreCompile(p.globalFunction, toks)

	return p
}

// write some pattern matcher

//arrayCall := arrayCall[Number] | identifier
//arrayDefine := [Number]arrayDefine | identifier
//functionHeaderDefine := func(TypedArg) | func(TypedArg)(out)
// epx := (exp) | exp Operator exp |  identifier | Number | arrayCall | identifier(call) | functionHeaderDefine{stm} | functionHeaderDefine{stm}(call) | arrayDefine
// TypedArg := " " | identifier Type | TypedArg , identifier Type | identifier arrayDefine | identifier functionHeaderDefine
// out := " " | Type | out , Type | arrayDefine | functionHeaderDefine
//call := exp | call, exp

func (c *Constraint) add(in *Constraint) *Constraint {
	c.Inputs = append(c.Inputs, in)
	return c
}
func (c *Constraint) Set(in Token) *Constraint {
	c.Output = in
	return c
}

//i think i need to revive the contraint part.. the outFkt should only serve the purpose of type checking
func (p *Parser) parseExpression(stack []Token, inFkt *function, inConstr *Constraint) (outFkt *function, currentConstriant *Constraint) {
	//(exp)->exp
	stack = stripOfBrackets(stack)

	if len(stack) == 0 {
		p.error("Expression expected")
		return
	}

	//can only be IN | Number
	if len(stack) == 1 {
		if stack[0].Type == DecimalNumberToken {
			stack[0].Type = FIELD
			return stack[0].primitiveReturnfunction(), inConstr.Set(stack[0])
		}
		if stack[0].Type == True || stack[0].Type == False {
			stack[0].Type = BOOL
			return stack[0].primitiveReturnfunction(), inConstr.Set(stack[0])
		}

		if stack[0].Type == IDENTIFIER_VARIABLE {
			v, ex := inFkt.findFunctionInBloodline(stack[0].Identifier)
			if !ex {
				panic("")
			}
			*inFkt = *v.flatCopy()
			return inFkt, &Constraint{Output: stack[0]}
		}

		p.error("Variable or number expected, got %v ", stack[0])
	}

	//maybe use different split style.. we now read everyting if no operator comes.
	l, binOperation, r := splitAtFirstHighestTokenType(stack, Operator)
	l, r = stripOfBrackets(l), stripOfBrackets(r)

	// exp Operator exp
	if binOperation.Type&Operator != 0 {

		lfkt, lc := p.parseExpression(l, NewCircuit("", inFkt.Context), &Constraint{})
		rfkt, rc := p.parseExpression(r, NewCircuit("", inFkt.Context), &Constraint{})

		return combineFunctions(binOperation.Identifier, lfkt, rfkt, inFkt.Context, lc, rc)

	} else if binOperation.Type != 0 {
		p.error("unsuported operation %v", binOperation)
	}

	if stack[0].Type == FUNCTION_CALL {
		v, ex := inFkt.findFunctionInBloodline(stack[0].Identifier)
		if !ex {
			panic("")
		}
		rest, argFks, argConst := p.parseArguments(inFkt.Context, stack[1:], splitAtClosingBrackets)

		*inFkt = *v.flatCopy()
		inConstr.Output = stack[0]
		inConstr.Inputs = argConst

		inFkt.getsLoadedWith(argFks)
		//whats the taskmap here?
		if len(rest) != 0 {
			if rest[0].Identifier == "(" {
				rest[0].Type = FUNCTION_CALL

				p.parseExpression(rest, inFkt, inConstr)
				return inFkt, inConstr
			}
			panic("")
		}
		return inFkt, inConstr
	}
	if stack[0].Type == FUNCTION_CALL && stack[0].Identifier == "(" {

		rest, argFkt, argConst := p.parseArguments(inFkt.Context, stack[1:], splitAtClosingBrackets)

		inFkt.getsLoadedWith(argFkt)
		inConstr.Inputs = append(inConstr.Inputs, argConst...)

		if len(rest) != 0 {
			if rest[0].Identifier == "(" {
				rest[0].Type = FUNCTION_CALL
				p.parseExpression(rest, inFkt, inConstr)
				return inFkt, inConstr
			}
			panic("")
		}
		return inFkt, inConstr
	}

	if stack[0].Type == FUNCTION_DEFINE {

		p.Assert("(", stack[1])

		inside, statement := splitAtFirstHighestStringType(stack[1:], "{")

		p.PrepareFunctionSignature(inFkt, inside)
		inConstr.Set(stack[0])
		statement, restt, s := splitAtClosingSwingBrackets(statement)
		if !s {
			panic("closing brackets missing")
		}
		p.PreCompile(inFkt, statement)
		inConstr.FktInputs = append(inConstr.FktInputs, inFkt)
		if len(restt) != 0 && restt[0].Identifier == "(" {
			restt[0].Type = FUNCTION_CALL
			ct := &Constraint{}
			inConstr.add(ct)
			p.parseExpression(restt, inFkt, ct)
			return inFkt, inConstr
		} else {
			if len(restt) != 0 {
				panic("")
			}

		}
		//now compile the function

		//check if the returns match the header description

		ex, v := inFkt.taskStack.PeekLast()
		fktReturns := ex && v.Output.Type == RETURN

		if len(inFkt.OutputTypes) != 0 && !fktReturns {
			p.error("return missing")
		}
		b, err := inFkt.checkIfReturnsMatchHeader(v.FktInputs)
		if !b {
			panic(err)
		}
		return inFkt, inConstr
	}

	//if stack[0].Type == ARRAY_CALL {
	//	tok := stack[0]
	//	p.loadDimension()
	//
	//	v, ex := inFkt.findFunctionInBloodline(stack[0].Identifier)
	//	if !ex {
	//		panic("")
	//	}
	//	rest, arguments := p.parseArguments(inFkt.Context, stack[1:], splitAtClosingBrackets)
	//
	//	*inFkt = *v.flatCopy()
	//	inFkt.getsLoadedWith(arguments)
	//	//whats the taskmap here?
	//	if len(rest) != 0 {
	//		if rest[0].Identifier == "(" {
	//			rest[0].Type = FUNCTION_CALL
	//
	//			p.parseExpression(rest, inFkt)
	//			return inFkt
	//		}
	//		panic("")
	//	}
	//	return inFkt
	//
	//	return
	//}
	//if stack[0].Identifier == "[" && stack[len(stack)-1].Identifier == "]" {
	//
	//	cr := &Constraint{Output: Token{
	//		Type: ARRAY_DECLARE,
	//	},
	//	}
	//	constraint.Inputs = append(constraint.Inputs, cr)
	//	restAfterBracketsClosed := p.argumentParse(currentCircuit, stack, splitAtClosingSquareBrackets, cr)
	//	// [1,b,c]
	//	if len(restAfterBracketsClosed) != 0 {
	//		p.error("array fdsaf")
	//	}
	//	return
	//}

	panic("unexpected reach")

	return

}

func (p *Parser) parseArguments(context *function, stack []Token, bracketSplitFunction func(in []Token) (cutLeft, cutRight []Token, success bool)) (rest []Token, args []*function, argConstr []*Constraint) {

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
		nfk := NewCircuit("", context)
		f, l := p.parseExpression(arguments, nfk, &Constraint{})
		args = append(args, f)
		argConstr = append(argConstr, l)
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
		p.prepareFunctionHeader(newFunction, arguments)
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

func (p *Parser) prepareFunctionHeader(current *function, stack []Token) {
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

	if stack[1].Type&Types != 0 {
		//a bool
		tok := Token{
			Type:       stack[1].Type,
			Identifier: stack[0].Identifier,
			isArgument: true,
		}
		//a bool[4][5]
		//if len(stack) > 2 {
		//	tok.dimensions = p.loadDimension(stack[2:], []int64{})
		//	tok.isArray = true
		//	tok.isArgument = true
		//	current.functions[stack[0].Identifier] = tok.primitiveReturnfunction()
		//
		//	arrayPostfixes := []string{}
		//	ArrayStringBuild(tok.dimensions, "", &arrayPostfixes)
		//	for _, post := range arrayPostfixes {
		//		id := fmt.Sprintf("%v%v", stack[0].Identifier, post)
		//		tok := Token{
		//			Type:       stack[1].Type,
		//			Identifier: id,
		//			isArgument: true,
		//		}
		//		current.functions[id] = tok.primitiveReturnfunction()
		//	}
		//	return
		//}
		current.InputTypes = append(current.InputTypes, returnTypes{
			typ: tok,
		})
		current.functions[stack[0].Identifier] = tok.primitiveReturnfunction()
		return
	}
	if stack[1].Type == FUNCTION_DEFINE {
		//TODO rethink what context a function in a function header has
		next := NewCircuit(stack[0].Identifier, nil)
		p.PrepareFunctionSignature(next, stack[2:])
		current.functions[stack[0].Identifier] = next
		current.InputTypes = append(current.InputTypes, returnTypes{
			functionReturn: true,
			fkt:            next,
		})
		return
	}
	p.error("not defined type %v", stack[1].Identifier)
	return

}

func (p *Parser) prepareReturns(current *function, stack []Token) {
	if len(stack) == 0 {
		return
	}

	if stack[0].Type&Types != 0 {
		tok := Token{
			Type: stack[0].Type,
		}
		// that was array stuff
		//if len(stack) > 1 {
		//	tok.dimensions = p.loadDimension(stack[2:], []int64{})
		//	tok.isArray = true
		//	current.functions[stack[0].Identifier] = tok.primitiveReturnfunction()
		//	return
		//}

		current.OutputTypes = append(current.OutputTypes, returnTypes{
			typ: tok,
		})
		return
	}
	if stack[0].Type == FUNCTION_DEFINE {
		next := NewCircuit("", nil)
		p.PrepareFunctionSignature(next, stack[1:])
		current.OutputTypes = append(current.OutputTypes, returnTypes{
			functionReturn: true,
			fkt:            next,
		})
		return
	}
	p.error("not defined")
	return

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

	//first we re

	switch tokens[0].Type {
	case PUBLIC:
		//toks := Tokens{toks: tokens[1:]}
		//tok := toks.next()
		//if tok.Identifier != "{" {
		//	p.error("define publics via: 'public {a,b,c}' , got %v ", tok)
		//}
		////NOTE THAT WE GOT ERROR WHEN USING &Constraint{} instead of Constraint{}, dont understand why
		//buffer := Constraint{}
		//
		//rest := p.parseArguments(currentCircuit, toks.toks, splitAtClosingSwingBrackets, &buffer)
		//for _, publicInput := range buffer.Inputs {
		//	a := currentCircuit.resolveArrayName(publicInput.Output.Identifier, publicInput.Inputs)
		//	currentCircuit.SNARK_Public_Statement = append(currentCircuit.SNARK_Public_Statement, a)
		//}
		//p.PreCompile(currentCircuit, rest)
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
		//we add the function
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
		fktReturns := ex && v.Output.Type == RETURN

		if len(fkt.OutputTypes) != 0 && !fktReturns {
			p.error("return missing")
		}
		b, err := fkt.checkIfReturnsMatchHeader(v.FktInputs)
		if !b {
			panic(err)
		}

		return
	//case IF: //if a<b { }   if (a<b) {
	//
	//	//we add a constraint, its name references to the function body, its inputs hold the condition
	//	identifierLeadingToTheStatement := fmt.Sprintf("if%v", currentCircuit.taskStack.len())
	//
	//	ifFunction := NewCircuit(identifierLeadingToTheStatement, currentCircuit)
	//
	//	//we are sure that there is no collision, because the name is sureley uneque
	//	currentCircuit.functions[identifierLeadingToTheStatement] = ifFunction
	//
	//	ifConstraint := &Constraint{
	//		Output: Token{
	//			Type:       IF_FUNCTION_CALL,
	//			Identifier: identifierLeadingToTheStatement,
	//		},
	//	}
	//	currentCircuit.taskStack.add(ifConstraint)
	//
	//	var fk = func(t []Token) (condition, statement, rest []Token, success bool) {
	//		condition, rest = splitAtFirstHighestStringType(t, "{")
	//		statement, rest, success = splitAtClosingSwingBrackets(rest)
	//		if !success {
	//			panic("")
	//		}
	//		if removeLeadingBreaks(rest)[0].Type != ELSE {
	//			return condition, statement, rest, false
	//		}
	//		return
	//	}
	//
	//	statements := [][]Token{}
	//	conditions := [][]Token{}
	//	c, s, r, contin := fk(tokens)
	//	for ; ; c, s, r, contin = fk(r) {
	//		statements = append(statements, s)
	//		conditions = append(conditions, c)
	//		if !contin {
	//			break
	//		}
	//	}
	//
	//	p.PreCompile(currentCircuit, r)
	//	for i, v := range statements {
	//		identifierLeadingToTheStatement := fmt.Sprintf("ifStatmement%v", ifFunction.taskStack.len())
	//		ifConstraint := &Constraint{
	//			Output: Token{
	//				Type:       IF,
	//				Identifier: identifierLeadingToTheStatement,
	//			},
	//		}
	//		if i == 0 {
	//			p.AssertTypes(conditions[i], IF)
	//			ifConstraint.FktInputs = append(ifConstraint.FktInputs, p.parseExpression(conditions[0][1:], NewCircuit("", currentCircuit)))
	//		} else if i < len(statements)-1 {
	//			p.AssertTypes(conditions[i], ELSE, IF)
	//			ifConstraint.FktInputs = append(ifConstraint.FktInputs, p.parseExpression(conditions[i][2:], NewCircuit("", currentCircuit)))
	//		} else {
	//			if len(conditions[i]) > 1 {
	//				p.AssertTypes(conditions[i], ELSE, IF)
	//				ifConstraint.FktInputs = append(ifConstraint.FktInputs, p.parseExpression(conditions[i][2:], NewCircuit("", currentCircuit)))
	//			} else {
	//				p.AssertTypes(conditions[i], ELSE)
	//				ifConstraint.Output.Type = ELSE
	//				//no condition
	//			}
	//		}
	//
	//		tsk := NewCircuit(identifierLeadingToTheStatement, currentCircuit)
	//
	//		//we are sure that there is no collision, because the name is sureley uneque
	//		ifFunction.functions[identifierLeadingToTheStatement] = tsk
	//
	//		ifFunction.taskStack.add(ifConstraint)
	//
	//		p.PreCompile(tsk, v)
	//	}
	//
	//	return
	case FOR:
		// TODO for loops a just syntactic suggar for a recursive function

		break
	case RETURN:
		re := &Constraint{
			Output: Token{
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

		varConst := &Constraint{
			Output: tokens[0],
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
	case IDENTIFIER_VARIABLE: //variable overloading -> a,b = a * 4 , 7

		overload, expr := splitAtFirstHighestStringType(tokens, "=")
		toOverload := &Constraint{
			Output: Token{
				Type: UNASIGNEDVAR,
			},
		}
		overloadWith := &Constraint{
			Output: Token{
				Type: RETURN,
			},
		}
		varConst := &Constraint{
			Output: Token{
				Type: VARIABLE_OVERLOAD,
			},
			Inputs: []*Constraint{toOverload, overloadWith},
		}

		for arguments, remm := splitAtFirstHighestStringType(overload, ","); ; arguments, remm = splitAtFirstHighestStringType(remm, ",") {
			arguments = removeLeadingAndTrailingBreaks(arguments)
			//todo array overload
			p.AssertTypes(arguments, IDENTIFIER_VARIABLE)
			//the first input is the thing to overwrite
			f, ex := currentCircuit.findFunctionInBloodline(arguments[0].Identifier)
			if !ex {
				p.error(fmt.Sprintf("variable %s not declared", arguments[0].Identifier))
			}
			toOverload.Inputs = append(toOverload.Inputs, &Constraint{
				Output: arguments[0],
			})
			toOverload.FktInputs = append(toOverload.FktInputs, f)

			if remm == nil {
				break
			}
		}
		currentCircuit.taskStack.add(varConst)

		//observation
		//we only us precompile for type checking and preparing the compilation tree
		//in parseExpression we cannot reference to functions by address
		//we still need to reference by name as we've done before.
		//unless.. hmm no I think we must do it the old way. otherwise i
		//see no chance how to du parallel calls such as in fibunacci a(x-1)+a(x-2)
		//cuz in that case

		var found bool //we mimic golang but I dont see why this break of symmetry is needed
		arguments, rest := splitAtFirstHighestStringType(expr, ",", "\n")
		for ; ; arguments, rest = splitAtFirstHighestStringType(rest, ",", "\n") {
			arguments = removeLeadingAndTrailingBreaks(arguments)

			fk, constr := p.parseExpression(arguments, NewCircuit("", currentCircuit), &Constraint{})

			//check if the assignment types match
			if eq, _ := toOverload.FktInputs[0].hasEqualDescription(fk); eq {
				toOverload.FktInputs = toOverload.FktInputs[1:]
				found = true
			} else {
				if len(toOverload.Inputs) != len(fk.OutputTypes) || found {
					p.error("assingment missmatch")
				}
				for i, f := range toOverload.FktInputs {
					if eq, err := f.hasEqualDescription2(fk.OutputTypes[i]); !eq {
						p.error(err)
					}
				}
				overloadWith.Inputs = append(overloadWith.Inputs, constr)
				if len(toOverload.Inputs) == len(overloadWith.FktInputs) {
					break
				}
				break
			}

			overloadWith.Inputs = append(overloadWith.Inputs, constr)
			if len(toOverload.Inputs) == len(overloadWith.Inputs) {
				break
			}
		}

		p.PreCompile(currentCircuit, rest)

		return
	case VARIABLE_DECLARE:
		//var a = smth
		//var a = []bool{true}
		//var a = func()(bool) {return 0/1}
		//var a = func(x field,y field)(bool) {return 0/1}
		assignmentLine, afterBreak := splitAtFirstHighestStringType(tokens, "\n")

		lhs, rhs := splitTokensAtFirstString(assignmentLine, "=")

		id := lhs[1]
		p.AssertIdentifier(id)
		if len(rhs[1:]) == 0 || len(lhs) != 2 {
			p.error("assignment missing")
		}
		if _, ex := currentCircuit.functions[(id.Identifier)]; ex {
			panic(fmt.Sprintf("variable %s already declared", id.Identifier))
		}
		if _, ex := predeclaredFunctionsMap[(id.Identifier)]; ex {
			panic(fmt.Sprintf("variable %s already declared", id.Identifier))
		}
		fkt := NewCircuit(id.Identifier, currentCircuit)
		currentCircuit.functions[(id.Identifier)] = fkt

		p.parseExpression(rhs[1:], fkt, &Constraint{})

		p.PreCompile(currentCircuit, afterBreak)

		return

	default:
		p.error("%v", tokens)
	}
	return
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

func (p *Parser) resolveArrayName(currentCircuit *function, con *Constraint) (composedName string, arrayDimension []int64, isArray bool) {

	composedName = con.Output.Identifier
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
func (p *Parser) loadArray(current *function, name string, constaint *Constraint, dim []int64, pos []int, expecedType Token) {

	//var staticTypeCheck = func(c *Constraint) {
	//	switch c.Output.Type {
	//	case FUNCTION_CALL:
	//		if f, ex := current.findFunctionInBloodline(c.Output.Identifier); ex {
	//			if len(f.OutputTypes) != 1 || f.OutputTypes[0].typ.Type != expecedType {
	//				panic(fmt.Sprintf("assignment missmatch. Expected %v, got %v", expecedType, f.OutputTypes[0].typ.Type))
	//			}
	//		} else {
	//			panic(fmt.Sprintf("variable %s not declared", c.Output.Identifier))
	//		}
	//	case IDENTIFIER_VARIABLE:
	//		if f, ex := current.findFunctionInBloodline(c.Output.Identifier); ex {
	//			if len(f.InputTypes) != 0 || len(f.OutputTypes) != 1 || f.OutputTypes[0].typ.Type != expecedType {
	//				panic(fmt.Sprintf("assignment missmatch. Expected %v, got %v", expecedType, f.OutputTypes[0].typ.Type))
	//			}
	//		} else {
	//			panic(fmt.Sprintf("variable %s not declared", c.Output.Identifier))
	//		}
	//	case DecimalNumberToken:
	//		checkRangeValidity(c.Output.value, expecedType)
	//	case ARRAY_CALL:
	//		if f, ex := current.findFunctionInBloodline(c.Output.Identifier); ex {
	//			if len(f.OutputTypes) != 1 || f.OutputTypes[0].typ.Type != expecedType {
	//				panic(fmt.Sprintf("assignment missmatch. Expected %v, got %v", expecedType, f.OutputTypes[0].typ.Type))
	//			}
	//		} else {
	//			panic(fmt.Sprintf("variable %s not declared", c.Output.Identifier))
	//		}
	//	default:
	//
	//	}
	//}

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

func readTokenTillType(in []Token) (cutLeft, cutRight []Token) {
	for i := 0; i < len(in); i++ {
		if in[i].Type&Types != 0 {
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

// Constraint is the data structure of a flat code operation
type Constraint struct {
	Output    Token
	Inputs    []*Constraint
	FktInputs []*function
}

func (c Constraint) String() string {
	return fmt.Sprintf("|%v|", c.Output)
}

//clone returns a deep copy of c
func (c *Constraint) clone() *Constraint {
	in := make([]*Constraint, len(c.Inputs))
	fkts := make([]*function, len(c.FktInputs))
	for i, cc := range c.Inputs {
		in[i] = cc.clone()
	}
	for i, cc := range c.FktInputs {
		fkts[i] = cc.flatCopy()
	}
	return &Constraint{
		Output:    c.Output,
		Inputs:    in,
		FktInputs: fkts,
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
