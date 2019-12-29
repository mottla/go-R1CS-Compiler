package circuitcompiler

import (
	"fmt"
	//"fmt"
	////"math/big"
	//"strings"
	"testing"
)

var CircuitCorrectnessTest = []string{
	//3 gates needed. nice dude
	//	`
	//def foo(a){
	//return a*a
	//}
	//	def main( x ) {
	//		return foo(x)*foo(x*x)
	//		}`,

	`
def main( a,b,c) {	
	var a[]	= {1,b,3,a}
	var k = a[a[0]*8/8]
	foo(a)
	var d = foo(a)
	return k * b *d
  }
def foo(a){
	return a}
`,
	//`def main( a) {
	//return (a*a)*a*a
	//}`,
	//`def main( a) {
	//return a*a*a*a
	//}`,
}

func TestPrintTree(t *testing.T) {

	for _, test := range CircuitCorrectnessTest {

		program := Parse(test, Order)

		fmt.Println("\n unreduced")
		fmt.Println(test)

		gates := program.CompileToGates()

		for _, g := range gates {
			fmt.Printf("\n %v", g)
		}

		//fmt.Println("\n generating R1CS")
		//r1cs := program.GatesToR1CS(gates)
		//fmt.Println(r1cs.Lexer)
		//fmt.Println(r1cs.R)
		//fmt.Println(r1cs.O)

	}

}
