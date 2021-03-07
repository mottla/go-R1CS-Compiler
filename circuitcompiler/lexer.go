package circuitcompiler

// This package provides a Lexer that globalFunction similarly to Rob Pike's discussion
// about lexer design in this [talk](https://www.youtube.com/watch?v=HxaD_trXwRE).

import (
	"errors"
	"fmt"
	"strings"
	"unicode/utf8"
)

type StateFunc func(*Lexer) StateFunc

type TokenType int

const (
	EOFRune rune = -1
)

type Tokens struct {
	toks []Token
}

type Token struct {
	Type       TokenType
	Identifier string
}

func (ch Token) String() string {
	return fmt.Sprintf("(%v <> %v)", ch.Identifier, ch.Type)
}

func (t *Tokens) next() (r Token) {
	if len(t.toks) == 0 {
		return Token{}
	}
	r = t.toks[0]
	t.toks = t.toks[1:]
	return r
}

var numberTokens = "0123456789"
var syntaxTokens = "():,;\n{}[]"
var operationTokens = "=-+*/&|><!"
var commentToken = '#'

var assignmentOperator = []string{"=", "-=", "+=", "*=", "/="}
var arithmeticOperator = []string{"-", "+", "*", "/"}
var booleanOperator = []string{"||", "&&"}
var bitOperator = []string{">>", "<<"}
var binaryComperator = []string{"==", "!=", ">", ">=", "<", "<="}

//var unaryOperator = []string{"++", "--"}

var operationMap = make(map[string]TokenType)
var keyWordMap map[string]TokenType

func init() {

	for _, v := range assignmentOperator {
		operationMap[v] = AssignmentOperatorToken
	}
	for _, v := range arithmeticOperator {
		operationMap[v] = ArithmeticOperatorToken
	}
	for _, v := range booleanOperator {
		operationMap[v] = BooleanOperatorToken
	}
	for _, v := range bitOperator {
		operationMap[v] = BitOperatorToken
	}
	for _, v := range binaryComperator {
		operationMap[v] = BinaryComperatorToken
	}
	//for _, v := range unaryOperator {
	//	operationMap[v] = UnaryOperatorToken
	//}
	keyWordMap = map[string]TokenType{
		"return": RETURN,
		"var":    VARIABLE_DECLARE,
		"if":     IF,
		"else":   ELSE,
		"for":    FOR,
		"func":   FUNCTION_DEFINE,
		"import": IMPORT,
		"public": PUBLIC,
	}

}

var Operator = BinaryComperatorToken | ArithmeticOperatorToken | BooleanOperatorToken | BitOperatorToken | AssignmentOperatorToken
var IN = IDENTIFIER_VARIABLE | ARGUMENT | VARIABLE_DECLARE | UNASIGNEDVAR

const (
	NumberToken TokenType = 1 << iota
	SyntaxToken
	CommentToken
	AssignmentOperatorToken
	ArithmeticOperatorToken
	BooleanOperatorToken
	BitOperatorToken
	BinaryComperatorToken
	//UnaryOperatorToken
	EOF
	IMPORT
	PUBLIC
	IDENTIFIER_VARIABLE
	FUNCTION_DEFINE
	FUNCTION_CALL
	IF_FUNCTION_CALL
	VARIABLE_DECLARE
	VARIABLE_OVERLOAD
	ARRAY_DECLARE
	ARRAY_CALL
	UNASIGNEDVAR
	ARGUMENT
	IF
	ELSE
	FOR
	NESTED_STATEMENT_END
	IF_ELSE_CHAIN_END
	RETURN
)

func (ch TokenType) String() string {
	switch ch {

	case UNASIGNEDVAR:
		return "UNASIGNEDVAR"
	case IDENTIFIER_VARIABLE:
		return "identifier"
	case CommentToken:
		return "commentToken"
	case AssignmentOperatorToken:
		return "AssignmentOperatorToken"
	case ArithmeticOperatorToken:
		return "ArithmeticOperatorToken"
	case BooleanOperatorToken:
		return "BooleanOperatorToken"
	case BitOperatorToken:
		return "BitOperatorToken"
	case BinaryComperatorToken:
		return "BinaryComperatorToken"
	case IMPORT:
		return "import"
	case NESTED_STATEMENT_END:
		return "For ends"
	case SyntaxToken:
		return "syntaxToken"
	case NumberToken:
		return "numberToken"
	case FUNCTION_DEFINE:
		return "def"
	case FUNCTION_CALL:
		return "call"
	case IF_FUNCTION_CALL:
		return "if_function_call"
	case VARIABLE_DECLARE:
		return "var"
	case VARIABLE_OVERLOAD:
		return "VARIABLE_OVERLOAD"
	case IF:
		return "if"
	case ELSE:
		return "else"
	case FOR:
		return "for"
	case ARGUMENT:
		return "Argument"
	case RETURN:
		return "RETURN"
	case ARRAY_CALL:
		return "arrayAccess"
	case ARRAY_DECLARE:
		return "arrayDefine"
	case PUBLIC:
		return "public"

	default:
		panic("unknown TOken")
		return "unknown Token"
	}
}

type Lexer struct {
	source          string
	start, position int
	startState      StateFunc
	Err             error
	tokens          chan Token
	ErrorHandler    func(e string)
	rewind          runeStack
	currentLine     int
	alreadyNewline  bool
}

// New creates a returns a lexer ready to parse the given source code.
func New(src string, start StateFunc) *Lexer {
	return &Lexer{
		source:     src,
		startState: start,
		start:      0,
		position:   0,
		rewind:     newRuneStack(),
	}
}

// Start begins executing the Lexer in an asynchronous manner (using a goroutine).
func (l *Lexer) Start() {
	// Take half the string length as a buffer size.
	buffSize := len(l.source) / 2
	if buffSize <= 0 {
		buffSize = 1
	}
	l.tokens = make(chan Token, buffSize)
	go l.run()
}

func (l *Lexer) StartSync() {
	// Take half the string length as a buffer size.
	buffSize := len(l.source) / 2
	if buffSize <= 0 {
		buffSize = 1
	}
	l.tokens = make(chan Token, buffSize)
	l.run()
}

// Current returns the value being being analyzed at this moment.
func (l *Lexer) Current() string {
	return l.source[l.start:l.position]
}

// Emit will receive a token type and push a new token with the current analyzed
// value into the tokens channel.
func (l *Lexer) Emit(t TokenType) {
	tok := Token{
		Type:       t,
		Identifier: l.Current(),
	}
	l.tokens <- tok
	l.start = l.position
	l.rewind.clear()
}

// Ignore clears the rewind stack and then sets the current beginning position
// to the current position in the source which effectively ignores the section
// of the source being analyzed.
func (l *Lexer) Ignore() {
	l.rewind.clear()
	l.start = l.position
}

// Peek performs a Next operation immediately followed by a Rewind returning the
// peeked rune.
func (l *Lexer) Peek() rune {
	r := l.Next()
	l.Rewind()

	return r
}

// Peek performs a Next operation immediately followed by a Rewind returning the
// peeked rune.
func (l *Lexer) PeekTwo() string {
	r := string(l.Next()) + string(l.Next())
	l.Rewind()
	l.Rewind()
	return r
}

// Rewind will take the last rune read (if any) and rewind back. Rewinds can
// occur more than once per call to Next but you can never rewind past the
// last point a token was emitted.
func (l *Lexer) Rewind() {
	r := l.rewind.pop()
	if r > EOFRune {
		size := utf8.RuneLen(r)
		l.position -= size
		if l.position < l.start {
			l.position = l.start
		}
	}
	if r == '\n' {
		l.currentLine--
	}
}

// Next pulls the next rune from the Lexer and returns it, moving the position
// forward in the source.
func (l *Lexer) Next() rune {
	var (
		r rune
		s int
	)
	str := l.source[l.position:]
	if len(str) == 0 {
		r, s = EOFRune, 0
	} else {
		r, s = utf8.DecodeRuneInString(str)
	}
	l.position += s
	l.rewind.push(r)
	if r == '\n' {
		l.currentLine++
	}
	return r
}

// Take receives a string containing all acceptable strings and will contine
// over each consecutive character in the source until a token not in the given
// string is encountered. This should be used to quickly pull token parts.
func (l *Lexer) Take(chars string) {
	r := l.Next()
	for strings.ContainsRune(chars, r) {
		r = l.Next()
	}
	l.Rewind() // last next wasn't a match
}

// nextToken returns the next token from the lexer and a value to denote whether
// or not the token is finished.
func (l *Lexer) NextToken() (*Token, bool) {
	if tok, ok := <-l.tokens; ok {
		//this way we only return the first \n we encounter, if multiple \n\n\n.. follow, we skip the consecutive ones
		if tok.Identifier == "\n" && !l.alreadyNewline {
			l.alreadyNewline = true
		} else if tok.Identifier == "\n" && l.alreadyNewline {
			return l.NextToken()
		} else {
			l.alreadyNewline = false
		}
		return &tok, false
	} else {
		return nil, true
	}
}

// Partial yyLexer implementation

func (l *Lexer) Error(e string) {
	if l.ErrorHandler != nil {
		l.Err = errors.New(e)
		l.ErrorHandler(e)
	} else {
		panic(fmt.Sprintf("%v at line %v", e, l.currentLine))
	}
}

// Private methods
func (l *Lexer) run() {
	state := l.startState
	for state != nil {
		state = state(l)
	}
	close(l.tokens)
}

//##############################################################################

func isWhitespace(ch rune) bool {
	return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}

func isLetter(ch rune) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
}
func isDigit(ch rune) bool {
	return (ch >= '0' && ch <= '9')
}

func NumberState(l *Lexer) StateFunc {
	l.Take(numberTokens)
	l.Emit(NumberToken)
	return ProbablyWhitespaceState(l)
}

func readIdent(l *Lexer) {

	r := l.Next()
	for isLetter(r) || isDigit(r) || r == '_' {
		r = l.Next()
	}
	l.Rewind()
}
func readCommentLine(l *Lexer) {

	for r := l.Next(); r != EOFRune && r != '\n'; {
		r = l.Next()
	}
	l.Rewind()
}

func IdentState(l *Lexer) StateFunc {
	peek := l.Peek()

	if isDigit(peek) {
		return NumberState
	}
	if strings.ContainsRune(syntaxTokens, peek) {
		l.Next()
		l.Emit(SyntaxToken)
		return ProbablyWhitespaceState
	}

	if peek == '"' {
		l.Next()
		l.Ignore()
		for l.Peek() != '"' { // we read everything inside " ", used for import
			l.Next()
		}
		l.Emit(IDENTIFIER_VARIABLE)
		l.Next()
		l.Ignore()
		return ProbablyWhitespaceState
	}

	//look if its a operators that has two runes (==,+=,..)
	if val, ex := operationMap[l.PeekTwo()]; ex {
		l.Next()
		l.Next()
		l.Emit(val)
		return ProbablyWhitespaceState
	}

	if val, ex := operationMap[string(peek)]; ex {
		l.Next()
		l.Emit(val)
		return ProbablyWhitespaceState
	}

	if peek == commentToken {
		readCommentLine(l)
		l.Emit(CommentToken)
		return WhitespaceState
	}

	//read the next word and push it on stack
	readIdent(l)

	if val, ex := keyWordMap[l.Current()]; ex {
		l.Emit(val)
		return ProbablyWhitespaceState
	}

	peek = l.Peek()
	if peek == '(' {
		//l.Next()
		l.Emit(FUNCTION_CALL)
		return ProbablyWhitespaceState
	}

	//it wasnt a keyword, so we assume its an identifier
	//identifiers do not require a whitespace (like func foo(), has '(' after identifier 'foo')
	l.Emit(IDENTIFIER_VARIABLE)
	return ProbablyWhitespaceState
}

func ProbablyWhitespaceState(l *Lexer) StateFunc {

	r := l.Peek()
	if r == EOFRune {
		return nil
	}

	//l.Take(" \t\n\r")
	l.Take(" \t\r")
	l.Ignore()

	return IdentState
}

func WhitespaceState(l *Lexer) StateFunc {

	r := l.Peek()
	if r == EOFRune {
		return nil
	}

	if !isWhitespace(r) {
		l.Error(fmt.Sprintf("unexpected token %q", r))
		return nil
	}

	//l.Take(" \t\n\r")
	l.Take(" \t\r")
	l.Ignore()

	return IdentState
}
