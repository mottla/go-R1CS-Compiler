package Circuitcompiler

import (
	"errors"
	"fmt"
	"github.com/mottla/go-R1CS-Compiler/utils"
	"math/big"
	"strings"
	"unicode/utf8"
)

type StateFunc func(*Lexer) StateFunc

type TokenType int

const (
	EOFRune rune = -1
)

type Tokens []Token

type Token struct {
	Type       TokenType
	Identifier string
	value      *big.Int
	isArray    bool
	isArgument bool
	dimensions []int64
	readInLine int
}

func (ch Token) String() string {
	if ch.Type == 0 {
		return ""
	}
	if ch.isArray {
		fmt.Sprintf("%v[%v]", ch.Type, ch.dimensions)
	}
	return fmt.Sprintf("%v", ch.Type)
}
func (t Token) copy() (r Token) {
	r = Token{
		Type:       t.Type,
		Identifier: t.Identifier,
		isArray:    t.isArray,
		isArgument: t.isArgument,
		dimensions: t.dimensions,
		readInLine: t.readInLine,
	}
	if t.value == nil {
		r.value = bigOne
		return
	}
	r.value = t.value
	return
}
func (t Token) equalDescription(a Token) bool {
	return t.isArgument == a.isArgument && t.isArray == a.isArray && t.Identifier == a.Identifier && ArrayString(t.dimensions) == ArrayString(a.dimensions)
}

func (t Tokens) next() (r Token) {
	if len(t) == 0 {
		return Token{}
	}
	r = t[0]
	t = t[1:]
	return r
}

var decimalNumberTokens = "0123456789"
var hexTokens = "0123456789abcdefABCDEF"
var syntaxTokens = "():,;\n{}[]"
var operationTokens = "=-+*/&|><!^"
var commentToken = '#'

var assignmentOperator = []string{"="}
var arithmeticOperator = []string{"-", "+", "*", "/", "**"}
var booleanOperator = []string{"||", "&&"}
var bitOperator = []string{">>", "<<", "<<<", ">>>", "|", "^", "&"}
var binaryComperator = []string{"==", "!=", ">", ">=", "<", "<="}
var predeclaredFunctions = []string{"SPLIT", "equal ", "BREAK", "ADD"}

//var unaryOperator = []string{"++", "--"}

var operationMap = make(map[string]TokenType)
var predeclaredFunctionsMap = make(map[string]bool)
var keyWordMap map[string]TokenType
var handle = make(map[string]func(...*function))

func init() {

	for _, v := range predeclaredFunctions {
		predeclaredFunctionsMap[v] = true
	}
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
		"field":  FIELD,
		"bool":   BOOL,
		"u8":     U8,
		"u16":    U16,
		"u32":    U32,
		"u64":    U64,
		"true":   True,
		"false":  False,
	}

}

var Operator = BinaryComperatorToken | ArithmeticOperatorToken | BooleanOperatorToken | BitOperatorToken | AssignmentOperatorToken
var IN = IDENTIFIER_VARIABLE | ARGUMENT | VARIABLE_DECLARE | UNASIGNEDVAR
var Types = BOOL | U8 | U16 | U32 | U64 | FIELD

const (
	DecimalNumberToken TokenType = 1 << iota
	HexNumberToken
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
	RETURN
	FIELD
	BOOL
	U8
	U16
	U32
	U64
	True
	False
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
	case SyntaxToken:
		return "syntaxToken"
	case DecimalNumberToken:
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
	case FIELD:
		return "field type"
	case BOOL:
		return "boolean type"
	case U8:
		return "8 bit type"
	case U16:
		return "16 bit type"
	case U32:
		return "32 bit type"
	case U64:
		return "64 bit type"

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
	var tok Token
	if t == DecimalNumberToken {
		value, success := utils.Field.ArithmeticField.StringToFieldElement(l.Current())
		if !success {
			panic("not possible")
		}
		tok = Token{
			Type:       DecimalNumberToken,
			readInLine: l.currentLine,
			value:      value,
		}
	} else if t == HexNumberToken {
		value, success := new(big.Int).SetString(l.Current()[2:], 16)
		if !success {
			panic("not possible")
		}
		tok = Token{
			Type:       DecimalNumberToken,
			readInLine: l.currentLine,
			value:      value,
		}
	} else {
		tok = Token{
			Type:       t,
			Identifier: l.Current(),
			readInLine: l.currentLine,
			//TODO safes space/time but could lead into trouble
			value: bigOne,
		}
	}

	l.tokens <- tok
	if tok.Identifier == "\n" {
		l.currentLine = l.currentLine + 1
	}
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
func isDecimalDigit(ch rune) bool {
	return ch >= '0' && ch <= '9'
}
func isOperator(ch rune) bool {
	return strings.ContainsRune(operationTokens, ch)
}

func DecimalNumberState(l *Lexer) StateFunc {
	l.Take(decimalNumberTokens)
	l.Emit(DecimalNumberToken)
	return ProbablyWhitespaceState(l)
}
func HexNumberState(l *Lexer) StateFunc {
	l.Take(hexTokens)
	l.Emit(HexNumberToken)
	return ProbablyWhitespaceState(l)
}

func readIdent(l *Lexer) {

	r := l.Next()
	for isLetter(r) || isDecimalDigit(r) || r == '_' {
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
	if l.PeekTwo() == "0x" {
		l.Next()
		l.Next()
		return HexNumberState
	}

	peek := l.Peek()

	if isDecimalDigit(peek) {
		return DecimalNumberState
	}

	if strings.ContainsRune(syntaxTokens, peek) {
		l.Next()
		l.Emit(SyntaxToken)
		return ProbablyWhitespaceState
	}

	//for import.. renew this
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

	if isOperator(peek) {
		l.Take(operationTokens)
		if val, ex := operationMap[l.Current()]; ex {
			l.Emit(val)
			return ProbablyWhitespaceState
		} else {
			l.Error("invalid operator")
		}
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
	if peek == '[' {
		//l.Next()
		l.Emit(ARRAY_CALL)
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
