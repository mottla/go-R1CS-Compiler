package circuitcompiler

import (
	"crypto/md5"
	"errors"
	"fmt"
	"hash"
	"math/big"
)

// Constraint is the data structure of a flat code operation
type Constraint struct {
	Output Token
	Inputs []*Constraint
}

func (c Constraint) String() string {
	//fmt.Sprintf("|%v , %v|", c.Output, c.Inputs)
	//if c.Output.Type == FUNCTION_CALL {
	//	return fmt.Sprintf("|%v , %v|", c.Output, c.Inputs)
	//}
	return fmt.Sprintf("|%v|", c.Output)
}
func (c Constraint) MD5Signature() string {
	md5 := md5.New()
	c.idd(md5)
	return string(md5.Sum(nil))
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

//isDeterministicExpression takes a constraint and checks if all leaves are numbers.
func isDeterministicExpression(c *Constraint) bool {
	if c.Output.Type == NumberToken {
		return true
	}
	if len(c.Inputs) == 0 {
		return false
	}
	res := true
	for _, v := range c.Inputs {
		res = res && isDeterministicExpression(v)
	}
	return res
}

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
	for tok.Value == "\n" {
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

func Parse(code string, fieldOrder *big.Int) (p *Program) {

	parser := newParser(code, false)

	var constraintStack []*Constraint
	go parser.libraryMode()
out:
	for {
		select {
		case constraint := <-parser.constraintChan:


			constraintStack = append(constraintStack, constraint)

		case <-parser.done:
			break out
		}
	}
	p = newProgram(fieldOrder, fieldOrder)
	p.internal(nil, constraintStack)
	return p
}

func (p *Parser) libraryMode() {
	tok := p.nextNonBreakToken()

	if tok.Type == FUNCTION_DEFINE {
		p.functionMode()
		p.libraryMode()
		return
	}
	for tok.Value != "" {
		tok = p.nextToken()
	}
	close(p.done)
}

func (p *Parser) functionMode() {
	tok := p.nextToken()
	if tok.Type != FUNCTION_CALL {
		p.error("Function Identifier expected, got %v : %v", tok.Value, tok.Type)
	}

	FuncConstraint := &Constraint{
		Output: Token{Type: FUNCTION_DEFINE, Value: tok.Value},
	}

	tok = p.nextToken()

	if tok.Value != "(" {
		p.error("Function expected, got %v ", tok)
	}

	for {
		tok = p.nextToken()
		if tok.Type == IdentToken {
			FuncConstraint.Inputs = append(FuncConstraint.Inputs, &Constraint{Output: Token{Type: ARGUMENT, Value: tok.Value}})

			continue
		}
		if tok.Value == "," {
			continue
		}
		if tok.Value == ")" {
			break
		}
		p.error("Invalid function header, got %v : %v", tok.Value, tok.Type)

	}
	tok = p.nextToken()
	if tok.Value != "{" {
		p.error("invalid function declaration, got %v : %v", tok.Value, tok.Type)
	}
	p.constraintChan <- FuncConstraint
	toks := p.stackTillSwingBracketsClose() //we collect everything inside the function body
	p.statementMode(toks)
	return

}

func (p *Parser) statementMode(tokens []Token) {

	tokens = removeLeadingAndTrailingBreaks(tokens)
	if len(tokens) == 0 {
		return
	}

	switch tokens[0].Type {
	//TODO
	case IF: //if a<b { }   if (a<b) {
		ifStatement, rest := splitTokensAtFirstString(tokens, "{")
		if len(rest) == 0 || rest[0].Value != "{" {
			p.error("if statement requires { }")
		}
		insideIf, outsideIf, success := splitAtClosingSwingBrackets(rest[1:])
		if !success {
			p.error("closing } brackets missing around IF body")
		}

		ifStatement = removeTrailingBreaks(ifStatement)
		IfConst := &Constraint{
			Output: Token{
				Type: IF,
			},
		}
		p.parseExpression(ifStatement[1:], IfConst)
		p.constraintChan <- IfConst
		p.statementMode(insideIf)
		p.statementMode(outsideIf)
		return
	case FOR:
		// for (  a<5 ; a+=1)
		ForConst := &Constraint{Output: Token{Type: FOR}, Inputs: []*Constraint{}}
		var success bool
		if tokens[1].Value != "(" {
			p.error("brackets '(' missing, got %v", tokens[1])
		}
		// a = 4;
		//l, r := splitTokensAtFirstString(tokens[2:], ";")
		//if r[0].Value != ";" {
		//	p.error("';' expected, got %v", r[0])
		//}
		//r = r[1:]
		//if b, err := isVariableAssignment(l[1:]); !b {
		//	p.error(err)
		//}
		//varConst := &Constraint{
		//	Output: Token{
		//		Type:  VARIABLE_DECLARE,
		//		Value: l[1].Value,
		//	},
		//}
		//p.parseExpression(l[3:], varConst)
		//ForConst.Inputs = append(ForConst.Inputs, varConst)

		// a <5;
		l, r := splitTokensAtFirstString(tokens[2:], ";")
		if r[0].Value != ";" {
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
		varConst := &Constraint{
			Output: Token{
				Type:  VARIABLE_OVERLOAD,
				Value: l[0].Value,
			},
		}
		p.parseExpression(l[2:], varConst)
		ForConst.Inputs = append(ForConst.Inputs, varConst)

		p.constraintChan <- ForConst
		r = removeLeadingBreaks(r)

		if r[0].Value != "{" {
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
	case IdentToken: //variable overloading -> a = a * 4
		l, r := splitTokensAtFirstString(tokens, "\n")
		if r != nil && r[0].Value != "\n" {
			p.error("linebreak expected, got %v", r[0])
		}
		//hannes = 42
		if b, rem, _ := isVariableAssignment(l); b {
			varConst := &Constraint{
				Output: Token{
					Type:  VARIABLE_OVERLOAD,
					Value: l[0].Value,
				},
			}
			p.parseExpression(rem, varConst)
			p.constraintChan <- varConst
			p.statementMode(r)
			return
		}
		panic("array element overloading not supported yet")
	case VARIABLE_DECLARE:
		l, r := splitTokensAtFirstString(tokens, "\n")
		if r != nil && r[0].Value != "\n" {
			p.error("linebreak expected, got %v", r[0])
		}

		//var x =
		if b, rem, _ := isVariableAssignment(tokens[1:]); b {

			//var a = func(){}
			if rem[0].Type == FUNCTION_DEFINE_Internal {

				toks := Tokens{toks: rem}
				var tok = toks.next() //func

				FuncConstraint := &Constraint{
					Output: Token{Type: FUNCTION_DEFINE_Internal, Value: tokens[1].Value},
				}

				tok = toks.next()
				if tok.Value != "(" {
					p.error("Function expected, got %v ", tok)
				}

				rest := p.argumentParse(toks.toks, splitAtClosingBrackets, FuncConstraint)
				for _, v := range FuncConstraint.Inputs {
					if v.Output.Type != IdentToken {
						p.error("Invalid function header, got %v : %v", v.Output.Value, v.Output.Type)
					}
				}
				toks.toks = rest
				tok = toks.next()
				if tok.Value != "{" {
					p.error("invalid function declaration, got %v : %v", tok.Value, tok.Type)
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
				//got a function definition plus its call
				//var a = func(){}()
				if r[0].Value == "(" {
					panic("instantly calling function not supported yet")
				}
				p.statementMode(r)
				return
			}

			//var x = expression
			varConst := &Constraint{
				Output: Token{
					Type:  VARIABLE_DECLARE,
					Value: l[1].Value,
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
					Type:  ARRAY_Define,
					Value: l[1].Value,
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
		if r != nil && r[0].Value != "\n" {
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

// epx := (exp) | exp Operator exp | Identifier(arg) | Identifier | Number | Identifier[exp]
//arg := exp | arg,exp
func (p *Parser) parseExpression(stack []Token, constraint *Constraint) {
	//(exp)->exp
	//helpText := combineString(stack)
	//helpText += ""
	stack = stripOfBrackets(stack)

	if len(stack) == 0 {
		p.error("Expression expected")
		return
	}

	//can only be IN | Number
	if len(stack) == 1 {
		if stack[0].Type&(NumberToken|IdentToken) != 0 {
			constraint.Inputs = append(constraint.Inputs, &Constraint{Output: stack[0]})
			return
		}
		p.error("Variable or number expected, got %v ", stack[0])
	}

	l, binOperation, r := splitAtFirstHighestTokenType(stack, binOp)
	l, r = stripOfBrackets(l), stripOfBrackets(r)

	// exp Operator exp
	if binOperation.Type&binOp != 0 {
		newTok := Token{
			Type:  UNASIGNEDVAR,
			Value: combineString(stack),
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
				Type:  UNASIGNEDVAR,
				Value: combineString(r),
			}
			c2 := &Constraint{Output: tok}
			constraint.Inputs = append(constraint.Inputs, c2)
			p.parseExpression(l, constraint)
			p.parseExpression(r, c2)
			return
		}
		if len(r) == 1 {

			tok := Token{
				Type:  UNASIGNEDVAR,
				Value: combineString(l),
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

	if stack[1].Value == "[" && stack[len(stack)-1].Value == "]" {
		rtok := Token{
			Type:  ARRAY_CALL,
			Value: stack[0].Value,
		}
		cr := &Constraint{Output: rtok}
		constraint.Inputs = append(constraint.Inputs, cr)
		p.parseExpression(stack[2:len(stack)-1], cr)
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

	arguments := splitAt(functionInput, ",")

	//arguments can be expressions, so we need to parse them
	for _, v := range arguments {
		//TODO check soon
		if len(v) == 0 {
			p.error("argument missing at function %v", constraint.Output)
		}
		p.parseExpression(v, constraint)

	}

	//handle what comes after the function

	return rem

}

func removeTrailingBreaks(in []Token) (out []Token) {
	if len(in) == 0 {
		return
	}
	if in[len(in)-1].Value == "\n" {
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
	if in[0].Value == "\n" {
		return removeLeadingBreaks(in[1:])
	}
	return in

}

func combineString(in []Token) string {
	out := ""
	for _, s := range in {
		out += s.Value
	}
	return out
}

func isArrayAssignment(stx []Token) (yn bool, err string) {
	if len(stx) < 5 {
		return false, "array assignment needs min 3 tokens: a = b"
	}
	if stx[0].Type != IdentToken {
		return false, "identifier expected"
	}

	if stx[1].Value != "[" || stx[2].Value != "]" {
		return false, "brackets  expected"
	}
	if stx[3].Type != AssignmentOperatorToken {
		return false, "assignment  expected"
	}
	if stx[4].Value != "{" {
		return false, "assignment  expected"
	}

	return true, ""
}

//func isAssignment(stx []Token) (yn bool,tok Token,rest []Token) {
//	if stx[0].Type != IdentToken {
//		return
//	}
//	if stx[1].Value=="["{
//		arg,rest,success:=splitAtClosingStringBrackets(stx,"[","]")
//		if !success{
//			panic("")
//		}
//	}
//
//}

func isVariableAssignment(stx []Token) (yn bool, rem []Token, err string) {
	if len(stx) < 3 {
		return false, nil, "assignment needs min 3 tokens: a = b"
	}
	if stx[0].Type != IdentToken {
		return false, nil, "identifier expected"
	}
	if stx[1].Type != AssignmentOperatorToken {
		return false, nil, "assignment  expected"
	}
	return true, stx[2:], ""
}

func (p *Parser) stackTillBracketsClose() (tokens []Token) {
	return p.stackTillBrackets("(", ")")
}

func (p *Parser) stackTillSwingBracketsClose() (tokens []Token) {
	return p.stackTillBrackets("{", "}")
}

//the closing } is not in the returned tokens array
func (p *Parser) stackTillBrackets(open, close string) (tokens []Token) {
	var stack []Token
	ctr := 1
	for tok := p.nextToken(); tok.Type != EOF; tok = p.nextToken() {
		if tok.Value == open {
			ctr++
		}
		if tok.Value == close {
			ctr--
			if ctr == 0 {
				return stack
			}
		}
		stack = append(stack, *tok)
	}
	p.error("closing %v missing", close)
	return
}

func (p *Parser) stackTillBreak() []Token {
	var stack []Token
	for tok := p.nextToken(); tok.Value != "\n" || tok.Type == EOF; tok = p.nextToken() {
		stack = append(stack, *tok)
	}
	return stack
}
func (p *Parser) stackTillSemiCol() []Token {
	var stack []Token
	for tok := p.nextToken(); tok.Value != "\n" || tok.Type != EOF; tok = p.nextToken() {
		if tok.Value == ";" {
			return stack
		}
		stack = append(stack, *tok)
	}
	p.error("no ';' found")
	return nil
}

//splitAt takes takes a string S and a token array and splits st: abScdasdSf -> ( ab,cdas, f  )
//if S does not occur it returns ( in , []Tokens{} )
func splitAt(in []Token, splitAt string) (out [][]Token) {

	for l, r := splitTokensAtFirstString(in, splitAt); ; l, r = splitTokensAtFirstString(r, splitAt) {
		if len(l) > 0 {
			out = append(out, l)
			continue
		}
		if len(r) > 1 && r[0].Value == splitAt {
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
		if in[i].Value == splitAt {
			return in[:i], in[i:]
		}
	}
	return in, cutRight
}

//splitAtFirstHighestTokenType takes takes a string S and a token array and splits st:
func splitAtFirstHighestTokenType(in []Token, splitAt TokenType) (cutLeft []Token, tok Token, cutRight []Token) {
	ctr1 := 0
	ctr2 := 0
	for i := 0; i < len(in); i++ {
		if in[i].Value == "(" {
			ctr1++
		}
		if in[i].Value == "[" {
			ctr2++
		}
		if in[i].Value == ")" {
			ctr1--
		}
		if in[i].Value == "]" {
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

func splitAtClosingStringBrackets(in []Token, open, close string) (cutLeft, cutRight []Token, success bool) {
	ctr := 0
	start := 1
	if in[0].Value != open {
		ctr = 1
		start = 0
	}

	for i := 0; i < len(in); i++ {
		if in[i].Value == open {
			ctr++
		}
		if in[i].Value == close {
			ctr--
			if ctr == 0 {
				if i == len(in)-1 {
					return in[1:i], cutRight, true
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
	if in[len(in)-1].Value == ";" {
		return in[:len(in)-1]
	}

	return p.cutAtSemiCol(in[:len(in)-1])
}
