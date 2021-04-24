package Circuitcompiler

import (
	"errors"
	"fmt"
	"io/ioutil"
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
func (p *Parser) nextNonBreakToken() *Token {
	tok, done := p.lexer.NextToken()
	if done {
		//close(p.done)
		return &Token{Type: EOF}
	}
	for tok.Type == CommentToken {

		tok = p.nextToken()
	}
	for tok.Identifier == "\n" {
		tok = p.nextToken()
	}
	return tok
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

func newParser(code string, log bool) (p *Parser) {
	lexer := New(code, ProbablyWhitespaceState)
	lexer.Start()
	return &Parser{constraintChan: make(chan *Constraint), tokenChannel: make(chan Token), done: make(chan struct{}), lexer: lexer, log: log}
}

func NewParse(code string) (p *Program) {

	parser := newParser(code, false)
	toks := parser.stackAllTokens()
	p = newProgram()

	parser.NEWPreCompile(p.globalFunction, toks)

	return p
}

// write some pattern matcher
// var identifier = identifier|array

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

// epx := (exp) | exp Operator exp | identifier(arg) | identifier | Number | identifier[exp]
//arg := exp | arg,exp
func (p *Parser) parseExpression(stack []Token, constraint *Constraint) {
	//(exp)->exp
	stack = stripOfBrackets(stack)

	if len(stack) == 0 {
		p.error("Expression expected")
		return
	}

	//can only be IN | Number
	if len(stack) == 1 {
		if stack[0].Type&(DecimalNumberToken|IDENTIFIER_VARIABLE) != 0 {
			constraint.Inputs = append(constraint.Inputs, &Constraint{Output: stack[0]})
			return
		}
		p.error("Variable or number expected, got %v ", stack[0])
	}

	l, binOperation, r := splitAtFirstHighestTokenType(stack, Operator)
	l, r = stripOfBrackets(l), stripOfBrackets(r)

	// exp Operator exp
	if binOperation.Type&Operator != 0 {
		newTok := Token{
			Type:       UNASIGNEDVAR,
			Identifier: combineString(stack),
		}
		c1 := &Constraint{Output: newTok}
		constraint.Inputs = append(constraint.Inputs, c1)
		constraint = c1
		//p.parseExpression(v, c1)
		//
		c1.Inputs = append(c1.Inputs, &Constraint{
			Output: binOperation,
		})

		if len(l) == 1 && len(r) == 1 {
			p.parseExpression(l, constraint)
			p.parseExpression(r, constraint)
			return
		}
		if len(l) == 1 {

			tok := Token{
				Type:       UNASIGNEDVAR,
				Identifier: combineString(r),
			}
			c2 := &Constraint{Output: tok}
			p.parseExpression(l, constraint)
			constraint.Inputs = append(constraint.Inputs, c2)
			p.parseExpression(r, c2)
			return
		}
		if len(r) == 1 {

			tok := Token{
				Type:       UNASIGNEDVAR,
				Identifier: combineString(l),
			}
			c2 := &Constraint{Output: tok}
			constraint.Inputs = append(constraint.Inputs, c2)
			p.parseExpression(l, c2)
			p.parseExpression(r, constraint)
			return
		}

		p.parseExpression(l, constraint)
		p.parseExpression(r, constraint)

		return
	} else if binOperation.Type != 0 {
		p.error("unsuported operation %v", binOperation)
	}

	if stack[0].Type == FUNCTION_CALL {

		cr := &Constraint{Output: stack[0]}
		constraint.Inputs = append(constraint.Inputs, cr)
		restAfterBracketsClosed := p.argumentParse(stack[1:], splitAtClosingBrackets, cr)
		//TODO  fkt(34)*7
		if len(restAfterBracketsClosed) != 0 {
			p.error("%v", restAfterBracketsClosed)
		}
		return
	}

	if stack[0].Type == IDENTIFIER_VARIABLE && stack[1].Identifier == "[" && stack[len(stack)-1].Identifier == "]" {
		cr := &Constraint{Output: Token{
			Type:       ARRAY_CALL,
			Identifier: stack[0].Identifier},
		}
		constraint.Inputs = append(constraint.Inputs, cr)

		l, r, _ := splitAtClosingSquareBrackets(stack[2:])
		p.parseExpression(l, cr)
		for len(r) != 0 {
			l, r, _ = splitAtClosingSquareBrackets(r)
			p.parseExpression(l, cr)
		}
		return
	}
	if stack[0].Identifier == "[" && stack[len(stack)-1].Identifier == "]" {

		cr := &Constraint{Output: Token{
			Type: ARRAY_DECLARE,
		},
		}
		constraint.Inputs = append(constraint.Inputs, cr)
		restAfterBracketsClosed := p.argumentParse(stack, splitAtClosingSquareBrackets, cr)
		// [1,b,c]
		if len(restAfterBracketsClosed) != 0 {
			p.error("array fdsaf")
		}
		return
	}

	panic("unexpected reach")

	return

}

//argument parse expects  bracket-arg-bracket
func (p *Parser) argumentParse(stack []Token, bracketSplitFunction func(in []Token) (cutLeft, cutRight []Token, success bool), constraint *Constraint) (rest []Token) {

	functionInput, rem, success := bracketSplitFunction(stack)

	if !success {
		p.error("closing brackets missing")
	}
	if len(functionInput) == 0 {
		return rem
	}
	//arguments can be expressions, so we need to parse them
	for arguments, remm := splitAtFirstHighestStringType(functionInput, ","); ; arguments, remm = splitAtFirstHighestStringType(remm, ",") {
		arguments = removeLeadingAndTrailingBreaks(arguments)
		p.parseExpression(arguments, constraint)
		if remm == nil {
			break
		}
	}

	//handle what comes after the function

	return rem

}

func (p *Parser) PrepareFunction(newFunction *function, stack []Token) {

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
		current.Inputs = append(current.Outputs, returnTypes{
			typ: tok,
		})
		current.functions[stack[0].Identifier] = tok.primitiveReturnfunction()
		return
	}
	if stack[1].Type == FUNCTION_DEFINE {
		//TODO rethink what context a function in a function header has
		next := NewCircuit(stack[0].Identifier, nil)
		p.PrepareFunction(next, stack[2:])
		current.functions[stack[0].Identifier] = next
		current.Inputs = append(current.Outputs, returnTypes{
			functionReturn: true,
			fkt:            next,
		})
		return
	}

	//arguments can be expressions, so we need to parse them
	// todo what if we parse func( a func(x field,y bool)(field) , c field)
	//then this primitive split does not work

	//handle what comes after the function
	p.error("not defined")
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

		current.Outputs = append(current.Outputs, returnTypes{
			typ: tok,
		})
		return
	}
	if stack[1].Type == FUNCTION_DEFINE {
		next := NewCircuit("", nil)
		p.PrepareFunction(next, stack[2:])
		current.Outputs = append(current.Outputs, returnTypes{
			functionReturn: true,
			fkt:            next,
		})
		return
	}

	//arguments can be expressions, so we need to parse them
	// todo what if we parse func( a func(x field,y bool)(field) , c field)
	//then this primitive split does not work

	//handle what comes after the function
	p.error("not defined")
	return

}

func (p *Parser) NEWPreCompile(currentCircuit *function, tokens []Token) (ret []Token) {

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
		toks := Tokens{toks: tokens[1:]}
		tok := toks.next()
		if tok.Identifier != "{" {
			p.error("define publics via: 'public {a,b,c}' , got %v ", tok)
		}
		//NOTE THAT WE GOT ERROR WHEN USING &Constraint{} instead of Constraint{}, dont understand why
		buffer := Constraint{}

		rest := p.argumentParse(toks.toks, splitAtClosingSwingBrackets, &buffer)
		for _, publicInput := range buffer.Inputs {
			a := currentCircuit.resolveArrayName(publicInput.Output.Identifier, publicInput.Inputs)
			currentCircuit.SNARK_Public_Statement = append(currentCircuit.SNARK_Public_Statement, a)
		}
		p.NEWPreCompile(currentCircuit, rest)
	case IMPORT:
		if len(tokens) == 1 || tokens[1].Type != IDENTIFIER_VARIABLE {
			p.error("import failed")
		}

		dat, err := ioutil.ReadFile(tokens[1].Identifier)
		if err != nil {
			panic(err)
		}
		tmpParser := newParser(string(dat), false)
		t := tmpParser.stackAllTokens()
		//we glue em together, but remove the EOF from t first
		p.NEWPreCompile(currentCircuit, append(t[:len(t)-1], tokens[2:]...))
		//TODO then?
	case FUNCTION_DEFINE:
		p.AssertIdentifiedType(FUNCTION_CALL, tokens[1])
		fkt := NewCircuit(tokens[1].Identifier, currentCircuit)
		if _, ex := currentCircuit.functions[fkt.Name]; ex {
			p.error(fmt.Sprintf("function %s already declared", fkt.Name))
		}
		//we add the function
		currentCircuit.functions[fkt.Name] = fkt
		p.Assert("(", tokens[2])

		inside, rest := splitAtFirstHighestStringType(tokens[2:], "{")

		p.PrepareFunction(fkt, inside)

		inside, rest, success := splitAtClosingSwingBrackets(rest)
		if !success {
			panic("closing brackets missing")
		}

		//we first compile whats outside the function. so
		//this enables us, to access not yet defined functions inside
		//the function scope
		p.NEWPreCompile(currentCircuit, rest)

		//now compile the function
		p.NEWPreCompile(fkt, inside)

		//check if the returns match the header description

		ex, v := fkt.taskStack.PeekLast()
		fktReturns := ex && v.Output.Type == RETURN

		if fktReturns && len(v.Inputs) != len(fkt.Outputs) {
			p.error("expect %v return values, got %v", len(fkt.Outputs), len(v.Inputs))
		}
		if !fktReturns && len(fkt.Outputs) != 0 {
			p.error("expect %v return values, got %v", len(fkt.Outputs), len(v.Inputs))
		}

		return
	case IF: //if a<b { }   if (a<b) {

		//we add a constraint, its name references to the function body, its inputs hold the condition
		identifier := fmt.Sprintf("if%v", currentCircuit.taskStack.len())

		ifFunction := NewCircuit(identifier, currentCircuit)
		currentCircuit.functions[identifier] = ifFunction

		ifConstraint := &Constraint{
			Output: Token{
				Type:       IF_FUNCTION_CALL,
				Identifier: identifier,
			},
		}
		currentCircuit.taskStack.add(ifConstraint)

		condition, rest := splitTokensAtFirstString(tokens, "{")

		p.parseExpression(condition[1:], ifConstraint)

		ifStatement, rest, success := splitAtClosingSwingBrackets(rest)
		if !success {
			panic("closing brackets missing")
		}
		p.NEWPreCompile(ifFunction, ifStatement)
		p.NEWPreCompile(currentCircuit, rest)
		return
	case ELSE:

		if ex, lastTask := currentCircuit.taskStack.PeekLast(); !ex || lastTask.Output.Type != IF_FUNCTION_CALL {
			p.error("if statement must proceed else!")
		}

		identifier := fmt.Sprintf("else%v", currentCircuit.taskStack.len())

		elseFunction := NewCircuit(identifier, currentCircuit)

		currentCircuit.functions[identifier] = elseFunction

		ifConstraint := &Constraint{
			Output: Token{
				Type:       IF_FUNCTION_CALL,
				Identifier: identifier,
			},
		}
		currentCircuit.taskStack.add(ifConstraint)

		condition, rest := splitTokensAtFirstString(tokens, "{")

		p.parseExpression(condition[1:], ifConstraint)

		ifStatement, rest, success := splitAtClosingSwingBrackets(rest)
		if !success {
			panic("closing brackets missing")
		}
		p.NEWPreCompile(elseFunction, ifStatement)
		p.NEWPreCompile(currentCircuit, rest)
		return
	case FOR:
		// TODO for loops a just syntactic suggar for a recursive function
		// for declare; till; increment === var declare; func loop(till,increment){ if till then return; increment(); return loop(till, increment) }
		// for (  a<5 ; a+=1)
		ForConst := &Constraint{Output: Token{Type: FOR}, Inputs: []*Constraint{}}
		var success bool
		if tokens[1].Identifier != "(" {
			p.error("brackets '(' missing, got %v", tokens[1])
		}
		// a = 4;
		//l, r := splitTokensAtFirstString(tokens[2:], ";")
		//if r[0].identifier != ";" {
		//	p.error("';' expected, got %v", r[0])
		//}
		//r = r[1:]
		//if b, err := isVariableAssignment(l[1:]); !b {
		//	p.error(err)
		//}
		//varConst := &Constraint{
		//	Output: Token{
		//		Type:  VARIABLE_DECLARE,
		//		identifier: l[1].identifier,
		//	},
		//}
		//p.parseExpression(l[3:], varConst)
		//ForConst.InputIdentifiers = append(ForConst.InputIdentifiers, varConst)

		// a <5;
		l, r := splitTokensAtFirstString(tokens[2:], ";")
		if r[0].Identifier != ";" {
			p.error("';' expected, got %v", r[0])
		}
		r = r[1:]
		p.parseExpression(l, ForConst)
		//a = a+1)
		l, r, success = splitAtClosingBrackets(r)
		if !success {
			p.error("closing brackets missing")
		}
		//if b, _, err := isVariableAssignment(l); !b {
		//	p.error(err)
		//}

		overload, expr := splitTokensAtFirstString(l, "=")

		////hannes = 42
		//if b, rem, _ := isVariableAssignment(l); b {
		varConst := &Constraint{
			Output: Token{
				Type: VARIABLE_OVERLOAD,
			},
		}
		p.parseExpression(overload, varConst)
		p.parseExpression(expr[1:], varConst)

		ForConst.Inputs = append(ForConst.Inputs, varConst)

		p.constraintChan <- ForConst
		r = removeLeadingBreaks(r)

		if r[0].Identifier != "{" {
			p.error("brackets '{' missing, got %v", r[0])
		}
		l, r, success = splitAtClosingSwingBrackets(r[1:])
		if !success {
			p.error("closing brackets missing")
		}
		//p.statementMode(l)
		p.constraintChan <- &Constraint{
			Output: Token{
				Type: NESTED_STATEMENT_END,
			},
		}
		//p.statementMode(r)
		break
	case IDENTIFIER_VARIABLE: //variable overloading -> a = a * 4
		l, r := splitTokensAtFirstString(tokens, "\n")

		if r != nil && r[0].Identifier != "\n" {
			p.error("linebreak expected")
		}

		overload, expr := splitTokensAtFirstString(l, "=")
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

			//the first input is the thing to overwrite
			p.parseExpression(arguments, toOverload)
			if _, ex := currentCircuit.findFunctionInBloodline(arguments[0].Identifier); !ex {
				p.error(fmt.Sprintf("variable %s not declared", arguments[0].Identifier))
			}

			if remm == nil {
				break
			}
		}
		currentCircuit.taskStack.add(varConst)
		//before we parse the rhs, finish of the rest
		p.NEWPreCompile(currentCircuit, r)

		//the second input is the  'new to assign' expression
		p.parseExpression(expr[1:], overloadWith)

		return
	case BOOL:
		//TODO i should stop treating arrays as something speacial, they are just
		//syntactic suggar for a function
		// say bool[2] a = [k,z] ====  func a(x){ if x =0 return k, else if x =1 reutrn z, else return nil }
		//bool a = 0/1
		//bool[4][4] a = [[],[],[],[]]
		assignmentLine, afterBreak := splitTokensAtFirstString(tokens, "\n")

		lhs, rhs := splitTokensAtFirstString(assignmentLine, "=")

		id := lhs[len(lhs)-1]
		p.AssertIdentifier(id)
		if len(rhs[1:]) == 0 {
			p.error("assignment missing")
		}
		if _, ex := currentCircuit.functions[(id.Identifier)]; ex {
			panic(fmt.Sprintf("variable %s already declared", id.Identifier))
		}

		tok := Token{
			Type:       BOOL,
			Identifier: id.Identifier,
		}

		//read and set the declared array size on the lhs
		if len(lhs) > 2 {
			tok.isArray = true
			tok.dimensions = p.loadDimension(lhs[1:len(lhs)-1], []int64{})
		}
		p.NEWPreCompile(currentCircuit, afterBreak)

		varConst := &Constraint{
			Output: tok,
		}

		p.parseExpression(rhs[1:], varConst)
		p.loadArray(currentCircuit, id.Identifier, varConst.Inputs[0], varConst.Output.dimensions, []int{})
		currentCircuit.functions[(id.Identifier)] = tok.primitiveReturnfunction()
		return
	case RETURN:
		//return (1+a)
		l, _ := splitTokensAtFirstString(tokens, "\n")
		returnCOnstraint := &Constraint{
			Output: Token{
				Type: RETURN,
			},
		}
		p.argumentParse(l[1:], splitNothing, returnCOnstraint)
		currentCircuit.taskStack.add(returnCOnstraint)
		return
	case FUNCTION_CALL:
		//fkt(args...)    equal(a,2) -> creates assertion gates s.t. a=2

		varConst := &Constraint{
			Output: tokens[0],
		}
		if _, ex := currentCircuit.findFunctionInBloodline(tokens[0].Identifier); !ex {
			panic(fmt.Sprintf("function %s not declared", tokens[0].Identifier))
		}

		//todo.. what if we call a function with a function call as argument, of a function that has not yet been declared
		r := p.argumentParse(tokens[1:], splitAtClosingBrackets, varConst)
		currentCircuit.taskStack.add(varConst)

		p.NEWPreCompile(currentCircuit, r)
		return

	default:
		p.error("%v", tokens)
	}
	return ret
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
	nr, err := strconv.ParseInt(toks[1].Identifier, 10, 64)
	if err != nil {
		p.error(err.Error())
	}
	return p.loadDimension(toks[3:], append(in, nr))
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
		tmp, err := strconv.ParseInt(bund.fac()[0].Typ.Identifier, 10, 64)
		if err != nil || tmp < 0 {
			p.error(err.Error())
		}
		arrayDimension = append(arrayDimension, tmp)
		composedName += fmt.Sprintf("[%v]", tmp)
		return composedName, arrayDimension, true
	}
	return
}
func (p *Parser) loadArray(current *function, name string, constaint *Constraint, dim []int64, pos []int) {
	if len(constaint.Inputs) != int(dim[0]) {
		p.error("array assignment size missmatch")
	}
	if len(dim) == 1 {
		st := name
		for _, v := range pos {
			st += fmt.Sprintf("[%v]", v)
		}
		for i := 0; i < int(dim[0]); i++ {
			id := st + fmt.Sprintf("[%v]", i)
			current.functions[id] = constaint.Inputs[i].primitiveReturnfunction()
		}
		return
	}
	for i, v := range constaint.Inputs {
		p.loadArray(current, name, v, dim[1:], append(pos, i))
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

func combineString(in []Token) string {
	out := ""
	for _, s := range in {
		out += s.Identifier
	}
	return out
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

func (p *Parser) stackTillBreak() []Token {
	var stack []Token
	for tok := p.nextToken(); tok.Identifier != "\n" || tok.Type == EOF; tok = p.nextToken() {
		stack = append(stack, *tok)
	}
	return stack
}
func (p *Parser) stackTillSemiCol() []Token {
	var stack []Token
	for tok := p.nextToken(); tok.Identifier != "\n" || tok.Type != EOF; tok = p.nextToken() {
		if tok.Identifier == ";" {
			return stack
		}
		stack = append(stack, *tok)
	}
	p.error("no ';' found")
	return nil
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
func splitAtFirstHighestStringType(in []Token, splitAt string) (cutLeft []Token, cutRight []Token) {
	depth := 0

	for i := 0; i < len(in); i++ {
		if in[i].Identifier == "(" || in[i].Identifier == "[" {
			depth++
		}

		if in[i].Identifier == ")" || in[i].Identifier == "]" {
			depth--
		}

		if in[i].Identifier == splitAt && depth == 0 {
			if i == len(in)-1 {
				return in[:i], cutRight
			}
			return in[:i], in[i+1:]
		}
	}
	return in, nil
}

//splitAtFirstHighestTokenType takes takes a string S and a token array and splits st:
func splitAtFirstHighestTokenType(in []Token, splitAt TokenType) (cutLeft []Token, tok Token, cutRight []Token) {
	ctr1 := 0
	ctr2 := 0
	for i := 0; i < len(in); i++ {
		if in[i].Identifier == "(" {
			ctr1++
		}
		if in[i].Identifier == "[" {
			ctr2++
		}
		if in[i].Identifier == ")" {
			ctr1--
		}
		if in[i].Identifier == "]" {
			ctr2--
		}

		if (in[i].Type&splitAt) != 0 && ctr1 == 0 && ctr2 == 0 {
			if i == len(in)-1 {
				return in[:i], in[i], cutRight
			}
			return in[:i], in[i], in[i+1:]
		}
	}
	return nil, Token{}, nil
}

func stripOfBrackets(in []Token) []Token {
	if isSurroundedByBrackets(in) {
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

func (p *Parser) cutAtSemiCol(in []Token) (cut []Token) {
	if len(in) == 0 {
		return
	}
	if in[len(in)-1].Identifier == ";" {
		return in[:len(in)-1]
	}

	return p.cutAtSemiCol(in[:len(in)-1])
}

// Constraint is the data structure of a flat code operation
type Constraint struct {
	Output Token
	Inputs []*Constraint
}

func (c Constraint) String() string {
	return fmt.Sprintf("|%v|", c.Output)
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

func (c Constraint) PrintConstraintTree() string {
	str := c.String()
	for _, v := range c.Inputs {
		str += v.PrintConstraintTree()
	}
	return str
}
