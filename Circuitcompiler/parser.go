package Circuitcompiler

import (
	"crypto/md5"
	"errors"
	"fmt"
	"hash"
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

//read the code, check syntax and semantics, translate it into an function tree ready for execution
func Parse(code string, checkSemantics bool) (p *Program) {

	parser := newParser(code, false)

	var constraintStack []*Constraint
	toks := parser.stackAllTokens()
	go parser.statementMode(toks)
	//go parser.statementMode(parser.stackAllTokens())
	p = newProgram()
out:
	for {
		select {
		case constraint := <-parser.constraintChan:
			//fmt.Println("#############")
			//fmt.Println(constraint)
			if constraint.Output.Type == PUBLIC {
				a := p.GetMainCircuit().resolveArrayName(constraint.Output.Identifier, constraint.Inputs)
				p.PublicInputs = append(p.PublicInputs, a)
			}
			if checkSemantics {
				constraintStack = append(constraintStack, constraint)
			}
		case <-parser.done:
			break out
		}
	}

	//p = newProgram(big.NewInt(1993), big.NewInt(1993))
	p.globalFunction.preCompile(constraintStack)
	return p
}

func (p *Parser) statementMode(tokens []Token) {

	tokens = removeLeadingAndTrailingBreaks(tokens)

	if len(tokens) == 0 {
		return
	}
	if tokens[0].Type == EOF {
		close(p.done)
		return
	}

	switch tokens[0].Type {
	case PUBLIC:
		toks := Tokens{toks: tokens[1:]}
		tok := toks.next()
		if tok.Identifier != "{" {
			p.error("Function expected, got %v ", tok)
		}
		//NOTE THAT WE GOT ERROR WHEN USING &Constraint{} instead of Constraint{}, dont understand why
		buffer := Constraint{}

		rest := p.argumentParse(toks.toks, splitAtClosingSwingBrackets, &buffer)
		for _, publicInput := range buffer.Inputs {
			publicInput.Output.Type = PUBLIC
			p.constraintChan <- publicInput
		}
		p.statementMode(rest)
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
		p.statementMode(append(t[:len(t)-1], tokens[2:]...))
	case FUNCTION_DEFINE:
		toks := Tokens{toks: tokens}
		var tok = toks.next() //func
		tok = toks.next()     //function name

		FuncConstraint := &Constraint{
			Output: Token{Type: FUNCTION_DEFINE, Identifier: tok.Identifier},
		}

		tok = toks.next()
		if tok.Identifier != "(" {
			p.error("Function expected, got %v ", tok)
		}

		rest := p.argumentParse(toks.toks, splitAtClosingBrackets, FuncConstraint)
		var FuncConstraintInputs []*Constraint
		for _, v := range FuncConstraint.Inputs {

			if v.Output.Type == ARRAY_CALL {
				var arrayDimensions = make([]int64, len(v.Inputs))
				var err error
				for i, in := range v.Inputs {
					if in.Output.Type != DecimalNumberToken {
						p.error("Invalid array declaration in function header, got %v, expect static size", v.Inputs[0])
					}
					arrayDimensions[i], err = strconv.ParseInt(in.Output.Identifier, 10, 64)
					if err != nil || arrayDimensions[i] <= 0 {
						p.error(err.Error())
					}
				}
				arrayPostfixes := []string{}
				ArrayStringBuild(arrayDimensions, "", &arrayPostfixes)
				for _, post := range arrayPostfixes {
					FuncConstraintInputs = append(FuncConstraintInputs, &Constraint{
						Output: Token{
							Type:       ARGUMENT,
							Identifier: fmt.Sprintf("%v%v", v.Output.Identifier, post),
						},
					})
				}
				continue
			}
			if v.Output.Type != IDENTIFIER_VARIABLE {
				p.error("Invalid function header, got %v : %v", v.Output.Identifier, v.Output.Type)
			}
			v.Output.Type = ARGUMENT
			FuncConstraintInputs = append(FuncConstraintInputs, v)
		}
		FuncConstraint.Inputs = FuncConstraintInputs
		toks.toks = rest
		tok = toks.next()
		if tok.Identifier != "{" {
			p.error("invalid function declaration, got %v : %v", tok.Identifier, tok.Type)
		}
		p.constraintChan <- FuncConstraint
		l, r, b := splitAtClosingSwingBrackets(toks.toks)
		if !b {
			panic("closing brackets missing")
		}
		p.statementMode(l)
		p.constraintChan <- &Constraint{
			Output: Token{
				Type: NESTED_STATEMENT_END,
			},
		}
		p.statementMode(r)
		return
	case IF: //if a<b { }   if (a<b) {

		var condition []Token
		var success bool
		rest := tokens

		var handeFunc = func(skip int, pareseExpression bool, typ TokenType) {
			condition, rest = splitTokensAtFirstString(rest, "{")
			IfConst := &Constraint{Output: Token{Type: typ}}
			if pareseExpression {
				p.parseExpression(condition[skip:], IfConst)
			}
			condition, rest, success = splitAtClosingSwingBrackets(rest)
			if !success {
				p.error("closing } brackets missing around IF body")
			}
			p.constraintChan <- IfConst
			p.statementMode(condition)
			p.constraintChan <- &Constraint{Output: Token{Type: NESTED_STATEMENT_END}}
		}

		handeFunc(1, true, IF)

		hasElseIf := false
		for rest[0].Type == ELSE && rest[1].Type == IF {
			handeFunc(2, true, ELSE)
			hasElseIf = true
		}

		if rest[0].Type != ELSE && hasElseIf {
			p.error("you used else if, so a else at the end is required")
		}

		if rest[0].Type == ELSE {
			handeFunc(1, false, ELSE)
		}
		p.constraintChan <- &Constraint{Output: Token{Type: IF_ELSE_CHAIN_END}}

		p.statementMode(rest)
		return
	case FOR:
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
		//ForConst.Inputs = append(ForConst.Inputs, varConst)

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
		if b, _, err := isVariableAssignment(l); !b {
			p.error(err)
		}

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
		p.statementMode(l)
		p.constraintChan <- &Constraint{
			Output: Token{
				Type: NESTED_STATEMENT_END,
			},
		}
		p.statementMode(r)
		break
	case IDENTIFIER_VARIABLE: //variable overloading -> a = a * 4
		l, r := splitTokensAtFirstString(tokens, "\n")
		if r != nil && r[0].Identifier != "\n" {
			p.error("linebreak expected, got %v", r[0])
		}

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
		p.constraintChan <- varConst
		p.statementMode(r)
		return
	case VARIABLE_DECLARE:
		l, r := splitTokensAtFirstString(tokens, "\n")
		if r != nil && r[0].Identifier != "\n" {
			p.error("linebreak expected, got %v", r[0])
		}

		//var x =
		if b, rem, _ := isVariableAssignment(tokens[1:]); b {

			//var a = func(){}
			if rem[0].Type == FUNCTION_DEFINE {
				p.statementMode(append([]Token{rem[0], tokens[1]}, rem[1:]...))
				return
			}

			//var x = expression
			varConst := &Constraint{
				Output: Token{
					Type:       VARIABLE_DECLARE,
					Identifier: l[1].Identifier,
				},
			}
			p.parseExpression(l[3:], varConst)
			p.constraintChan <- varConst
			p.statementMode(r)
			return
		}

		//var hannes[] = {a,b,c,d}
		if b, _ := isArrayAssignment(l[1:]); b {
			arrayConst := &Constraint{
				Output: Token{
					Type:       ARRAY_DECLARE,
					Identifier: l[1].Identifier,
				},
			}

			rest := p.argumentParse(l[5:], splitAtClosingSwingBrackets, arrayConst)
			if len(rest) > 0 {
				panic("unexpected")
			}
			p.constraintChan <- arrayConst
			p.statementMode(r)
			return
		}
		//var a = func(){}
	case RETURN:
		//return (1+a)
		l, r := splitTokensAtFirstString(tokens, "\n")
		returnCOnstraint := &Constraint{
			Output: Token{
				Type: RETURN,
			},
		}
		if len(l) > 1 {
			p.parseExpression(l[1:], returnCOnstraint)
		}

		if len(removeLeadingAndTrailingBreaks(r)) != 0 {
			panic("not supposed")
		}
		p.constraintChan <- returnCOnstraint
		return
	case FUNCTION_CALL:
		//fkt(args...)    equal(a,2) -> creates assertion gates s.t. a=2

		varConst := &Constraint{
			Output: tokens[0],
		}
		r := p.argumentParse(tokens[1:], splitAtClosingBrackets, varConst)
		if r != nil && r[0].Identifier != "\n" {
			p.error("linebreak expected, got %v", r[0])
		}
		//r = r[1:]
		p.constraintChan <- varConst
		p.statementMode(r)
		return

	default:
		p.error("return missing maybe")
	}
}

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
		if len(restAfterBracketsClosed) != 0 {
			p.error("%v", restAfterBracketsClosed)
		}
		return
	}

	if stack[1].Identifier == "[" && stack[len(stack)-1].Identifier == "]" {
		rtok := Token{
			Type:       ARRAY_CALL,
			Identifier: stack[0].Identifier,
		}
		cr := &Constraint{Output: rtok}
		constraint.Inputs = append(constraint.Inputs, cr)

		l, r, _ := splitAtClosingSquareBrackets(stack[2:])
		p.parseExpression(l, cr)
		for len(r) != 0 {
			l, r, _ = splitAtClosingSquareBrackets(r)
			p.parseExpression(l, cr)
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

func isArrayAssignment(stx []Token) (yn bool, err string) {
	if len(stx) < 5 {
		return false, "array assignment needs min 3 tokens: a = b"
	}
	if stx[0].Type != IDENTIFIER_VARIABLE {
		return false, "identifier expected"
	}

	if stx[1].Identifier != "[" || stx[2].Identifier != "]" {
		return false, "brackets  expected"
	}
	if stx[3].Type != AssignmentOperatorToken {
		return false, "assignment  expected"
	}
	if stx[4].Identifier != "{" {
		return false, "assignment  expected"
	}

	return true, ""
}

func isVariableAssignment(stx []Token) (yn bool, rem []Token, err string) {
	if len(stx) < 3 {
		return false, nil, "assignment needs min 3 tokens: a = b"
	}
	if stx[0].Type != IDENTIFIER_VARIABLE {
		return false, nil, "identifier expected"
	}
	if stx[1].Type != AssignmentOperatorToken {
		return false, nil, "assignment  expected"
	}
	return true, stx[2:], ""
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

//splitTokensAtFirstString takes takes a string S and a token array and splits st: abScd -> ( ab , Scd )
//if S does not occur it returns ( in , []Tokens{} )
func SplitAtFirstTokenType(in []Token, splitAt TokenType) (cutLeft []Token, tok Token, cutRight []Token) {
	for i := 0; i < len(in); i++ {
		if in[i].Type == splitAt {
			if i == len(in)-1 {
				return in[:i], in[i], cutRight
			}
			return in[:i], in[i], in[i:]
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
func splitAtClosingSwingBrackets(in []Token) (cutLeft, cutRight []Token, success bool) {
	return splitAtClosingStringBrackets(in, "{", "}")
}
func splitAtClosingSquareBrackets(in []Token) (cutLeft, cutRight []Token, success bool) {
	return splitAtClosingStringBrackets(in, "[", "]")
}

func splitAtClosingStringBrackets(in []Token, open, close string) (cutLeft, cutRight []Token, success bool) {
	ctr := 0
	start := 1
	if in[0].Identifier != open {
		ctr = 1
		start = 0
	}

	for i := 0; i < len(in); i++ {
		if in[i].Identifier == open {
			ctr++
		}
		if in[i].Identifier == close {
			ctr--
			if ctr == 0 {
				if i == len(in)-1 {
					return in[start:i], cutRight, true
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
func (c Constraint) MD5Signature() string {
	md5 := md5.New()
	c.idd(md5)
	return string(md5.Sum(nil))
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

func (c Constraint) idd(h hash.Hash) {
	h.Write([]byte(c.Output.String()))
	for _, v := range c.Inputs {
		v.idd(h)
	}
}

func (c Constraint) PrintConstraintTree() string {
	str := c.String()
	for _, v := range c.Inputs {
		str += v.PrintConstraintTree()
	}
	return str
}
