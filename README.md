#R1CS Compiler (Help wanted)

Compile a special purpose language into the Rank 1 Constraint System.
This is very usefull for general purpose prove systems like zkSNARKs.
The process chain goes as follows:
- we translate our own special purpose language into an arithmetic circuit defined over a finite field
- the translate the arithmetic circuit into the desired R1CS form


**Circuit Language**
The compiler is fully written in go.
The language aims to be similar to go.
Currently supported:
- declare multiple functions via:
 func identifier (arguments...){...}
- declare variable: var x = expression
- variable overloading: x = expression
- array declaration: var k[]={expression,expression,..}
- equality check. call equal(a,b), to ensure that a is equal to b at a given point of code execution.
 use this to verify signatures etc.
 - for loop
 - functions are first class entities

This language then gets compiled into a R1CS form, with focus on gate reduction.
We reuse gates whenever possible, exploit commutative properties of the gates, extract constant factors as long as possible etc.

**Not yet supported**
- support for conditional statements
- binary operations on/with variables (the for-loop conditions a<b etc. are translated during pre compiling, but not part of the R1CS)
- storing R1CS in sparse representation

**Example** 

call Parse with the program code as a string, and tie FieldOrder as *big.Int (must be a prime)
```
program := Parse(code, FieldOrder)
```
Perform the compilation to arithmetic circuit gates
```
gates := program.CompileToGates()
```

final step from gates to R1CS
```
r1cs := program.GatesToR1CS(gates)
```

A full example of code and the corresponding output:
```
	def main(x,z,w) {
		var arra[]={x,1,2,3}
        #we overload the function mul
		var mul = func(a,b){
			return x*b*7
		}
		var a =1
		var c = w
		
		for( a<3;a=a+1){
			var b = 3
			for( b<4;b=b+2){
				c = mul(c,c)
			}				
		}

		#arra[2]=3
		var k = mul(z,z)
		var l = k*k
		return l*(k*arra[2])*x*x
	}

	def mul(a,b){
	    return a*b
	}


R1SC form of the code above. 
Note that the big numbers are because we do arithmetic on a finite field and we extract factors as long as possible to reduce gates.
so inverses and negative numbers are likely huge.

[[0 0 0 1 0 0 0 0 0 0 0] [0 0 0 0 1 0 0 0 0 0 0] [0 0 1 0 0 0 0 0 0 0 0] [0 1 0 0 0 0 0 0 0 0 0] [0 0 0 0 0 0 1 0 0 0 0] [0 0 0 0 0 0 1 0 0 0 0] [0 0 0 0 0 0 0 0 1 0 0]]
[[0 0 0 1 0 0 0 0 0 0 0] [0 1 0 0 0 0 0 0 0 0 0] [0 1 0 0 0 0 0 0 0 0 0] [0 1 0 0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 1 0 0 0] [0 0 0 0 0 0 1 0 0 0 0] [0 0 0 0 0 0 0 0 0 1 0]]
[[0 0 0 0 1 0 0 0 0 0 0] [0 0 0 0 0 3126891838834182174606629392179610726935480628630862049099743455225115499374 0 0 0 0 0] [0 0 0 0 0 0 1 0 0 0 0] [0 0 0 0 0 0 0 1 0 0 0] [0 0 0 0 0 0 0 0 1 0 0] [0 0 0 0 0 0 0 0 0 1 0] [0 0 0 0 0 0 0 0 0 0 10752679078439993804514633726168661377318948692332658270883811677661876768255]]
input
[3 2 328329]
witness
[1 3 2 328329 107799932241 2263798577061 6 9 54 36 1333584]
--- PASS: TestCorrectness (0.00s)
PASS
