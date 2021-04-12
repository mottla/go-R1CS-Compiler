package testPrograms

import "math/big"

type InOut struct {
	Inputs []*big.Int
	Result *big.Int
}

type TraceCorrectnessTest struct {
	Skip bool
	Code string
	IO   []InOut
}

var bigNumberResult1, _ = new(big.Int).SetString("2297704271284150716235246193843898764109352875", 10)
var pubkeyOf42OnBn256_G1, _ = new(big.Int).SetString("4312786488925573964619847916436127219510912864504589785209181363209026354996", 10)

var sudoku = func() []*big.Int {
	field := [9][9]int64{
		{9, 5, 7, 6, 1, 3, 2, 8, 4},
		{4, 8, 3, 2, 5, 7, 1, 9, 6},
		{6, 1, 2, 8, 4, 9, 5, 3, 7},
		{1, 7, 8, 3, 6, 4, 9, 5, 2},
		{5, 2, 4, 9, 7, 1, 3, 6, 8},
		{3, 6, 9, 5, 2, 8, 7, 4, 1},
		{8, 4, 5, 7, 9, 2, 6, 1, 3},
		{2, 9, 1, 4, 3, 6, 8, 7, 5},
		{7, 3, 6, 1, 8, 5, 4, 2, 9},
	}
	res := []*big.Int{}
	for i := 0; i < 9; i++ {
		for j := 0; j < 9; j++ {
			res = append(res, new(big.Int).SetInt64(field[i][j]))
		}
	}
	return res
}

var TestPrograms = []TraceCorrectnessTest{
	{
		Skip: false,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(3)), big.NewInt(int64(2))},
			Result: big.NewInt(int64(49)),
		}},
		Code: `
	func main( x,y) {
	public{
		x
	}	

		return (x|y)*7
	}
`,
	},
	{
		Skip: true,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(7)), big.NewInt(int64(11))},
			Result: big.NewInt(int64(1729500084900343)),
		}, {
			Inputs: []*big.Int{big.NewInt(int64(365235)), big.NewInt(int64(11876525))},

			Result: bigNumberResult1,
		}},
		Code: `
	func main( x  ,  z ) {
	public{
	x,
	z
	}
		return do(z) + add(x,x)
	}		

	func do(x){
		var e = x * 5
		var b = e * 6
		var c = b * 7
		var F = c * 1
		var d = c * F
		var ll = d * mul(d,e)
		return ll
	}
	
	func add(x ,k){
		var z = k * x
		return do(x) + mul(x,z)
	}

	func mul(a,b){
		return a * b
	}`,
	},
	{
		Skip: true,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(3))},
		}},

		Code: `
func main(x){
	public{
	x
	}
	return (x*fubunaci(7))
}
func fubunaci(a){
	if a==0{
		return 1
	}
	if a==1{
		return 1
	}
	return addGateConstraint(fubunaci(a-1),fubunaci(a-2))
}
`},
	{
		Skip: true,
		IO: []InOut{{
			Inputs: sudoku(),
		}},
		Code: `
func main(sudoku[9][9]){	
	public{
	sudoku[3][4]
	}	
	# we check if all inputs are in the range 1 to 9	
	func iterate(fromX,toX,stepSize, call){       
		if fromX==toX{           
			return
		}			
		call(fromX)
		iterate(fromX+stepSize,toX,stepSize,call)
	}	

    var product  = 2*3*4*5*6*7*8*9	
	
	#we check if all columns  	
	var checkRange = func(x){		
		var EQ = func(y){			
			equal(constraint(sudoku[x][y]),0)
		}
		iterate(0,9,1,EQ)
	}
	
    var col = func(x){
		var prod = 1
		var EQ = func(y){	
			prod = prod * sudoku[x][y]			
		}
		iterate(0,9,1,EQ)
		equal(prod,product)
	}
    var row = func(x){
		var prod = 1
		var EQ = func(y){	
			prod = prod * sudoku[y][x]			
		}
		iterate(0,9,1,EQ)
		equal(prod,product)
	}

	iterate(0,9,1,checkRange)
    iterate(0,9,1,col)
    iterate(0,9,1,row)		
	
	var i = 0
	for (i<9;i=i+3){			
		var j = 0
		for (j<9;j=j+3){
			var colProduct = 1	
			var k = 0
			for (k<3;k=k+1){
				var l = 0
					for (l<3;l=l+1){
						colProduct = colProduct * sudoku[i+k][j+l]	
				}
			}
			equal(colProduct,product)		
		}		
	}
	return
}
func constraint(x){
	return (x-1)*(x-2)*(x-3)*(x-4)*(x-5)*(x-6)*(x-7)*(x-8)*(x-9)
}

`},
	{
		Skip: true,
		IO: []InOut{{
			Inputs: sudoku(),
		}},
		Code: `

func main(x[9][9]){		
	# we check if all inputs are in the range 1 to 9
	var i = 0
	for (i<9;i=i+1){
		var j = 0
			for (j<9;j=j+1){
				equal(constraint(x[i][j]),0)
		}
	}
	#we check if all columns  
	i = 0
	for (i<9;i=i+1){
		var colProduct = 1		
		var j = 0
		for (j<9;j=j+1){
			colProduct = colProduct * x[i][j]			
		}
		equal(colProduct,2*3*4*5*6*7*8*9)
	}
	#we check if all rows  
	i = 0
	for (i<9;i=i+1){
		var colProduct = 1		
		var j = 0
		for (j<9;j=j+1){
			colProduct = colProduct * x[j][i]			
		}
		equal(colProduct,2*3*4*5*6*7*8*9)
	}
	#we check if each 3x3 box is satisfied
	i = 0
	for (i<9;i=i+3){			
		var j = 0
		for (j<9;j=j+3){
			var colProduct = 1	
			var k = 0
			for (k<3;k=k+1){
				var l = 0
					for (l<3;l=l+1){
						colProduct = colProduct * x[i+k][j+l]	
				}
			}
			equal(colProduct,2*3*4*5*6*7*8*9)		
		}
		
	}
	
	return
}
func constraint(x){
	return (x-1)*(x-2)*(x-3)*(x-4)*(x-5)*(x-6)*(x-7)*(x-8)*(x-9)
}

`},
	{
		Skip: true,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(3)), big.NewInt(int64(4))},
		}},
		Code: `
func main(x,y){	
	var multiply = func(b,h){
		return b*h*6
	}
	var execute = func(j,m){
		return j(m)
	}
	#var kk = x*7	
	var k = execute(multiply,x) 
	
	return iterate(2,4,1,k)*y*x
}
	func iterate(fromX,toX,stepSize, call){       
		if fromX==toX{           
			return 1
		}			
		call(fromX)
		return iterate(fromX+stepSize,toX,stepSize,call)
		 
	}	
`},
	{
		Skip: true,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(1))},
		}},

		Code: `
#import "/github.com/mottla/go-R1CS-Compiler/utils/basicOperations"

func main(x){
	public{x}
	return (1*fubunaci(8,x))
}

func fubunaci(a,v){
	var dyn[] = {v,v}
	var i = 2
	for (i<a;i=i+1){
		var n = addGateConstraint(dyn[0],dyn[1])
		dyn[0] = dyn[1]
		dyn[1] = n
	}
    return dyn[1]
}
`},
	{
		Skip: true,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(1))},
		}},

		Code: `
func main(x){
	return (x*fubunaci(8,x))
}

func fubunaci(a,v){
	var dyn[] = {v,v}
	var i = 2
	for (i<a;i=i+1){
		var n = dyn[0]+dyn[1]
		dyn[0] = dyn[1]
		dyn[1] = n
	}
    return dyn[1]
}
`},
	{
		Skip: true,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(3)), big.NewInt(int64(5)), big.NewInt(int64(7))},
		}},

		Code: `
func main(x,y,z){
	return mul(add(mul(x,y),z)-132,z)
}

func mul(a,b){
    return a*b
}
func add(a,b){
	return a+b
	}

`},
	{
		Skip: true,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(3))},
		}},

		Code: `
func main(x){
	return (1*fubunaci(8))*(x*x)
}
var dyn[] = {1,1}
func fubunaci(a){
	var i = 2
	for (i<a;i=i+1){
		var n = dyn[0]+dyn[1]
		dyn[0] = dyn[1]
		dyn[1] = n
	}
    return dyn[1]
}
`},

	{
		Skip: true,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(3))},
		}},

		Code: `
func main(x){
	return (1*fubunaci(8))*(x*x)
}
var dyn[] = {1,1,0,0,0,0,0,0,0}
func fubunaci(a){
	var i = 2
	for (i<a;i=i+1){
		dyn[i] = dyn[i-1]+dyn[i-2]
	}
    return dyn[a-1]
}
`},
	{
		Skip: true,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(3))},
		}},

		Code: `
func main(x){
	var a = func(i){
		if i == 0 {
			return
		}
		i = i-1
		return x*a(i)			
	}
	var b = 7
	var c = 123 * b    
	 return mul(1/c,a(array[3]*2))
}

var xx = 4
var array[] = {1,4,7,xx}

func mul(a,b){
    return a*b
}
`},
	{
		Skip: true,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(3))},
		}},
		Code: `
	func main(x){
		var a = func(c,b){
			return (c*c)*b
		}
		var c[] = {x, 2*x,a }
		return a(applyFunction(c[0],a),x)
	}

	func applyFunction(a,fkt){
		return fkt(a,a)
	}
`,
	},
	{Skip: true,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(3)), big.NewInt(int64(2)), big.NewInt(328329)},
		}},

		Code: `
	func main(x,z,w) {
		if ( (4*7) == 28){
			x=x*x
		}else{
			x=z*z
		}
		var arra[]={x,1,2,3}
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

	func mul(a,b){
	return a*b
	}
	
`,
	}, {Skip: true,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(7)), big.NewInt(int64(11))},
			Result: big.NewInt(int64(2160900)),
		}},

		Code: `
	func main( x  ,  z ) {
		if ( 1==2){
			x=x*x
		}else if 3==3{
			x=z*z
		}else{
			x=x*z
		}
		if ( 1==2){
			x=x*x
		}else if 3==3{
			x=x*x
		}else{
			x=x*z
		}
	#	var b = x*x
		
		return
		}		
`,
	},
	{
		Skip: true,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(7)), big.NewInt(int64(11))},
			Result: big.NewInt(int64(2160900)),
		}},

		Code: `


	func main( x  ,  z ) {
		var a =1
		var c = 45345146
		for( a<3;a=a+1){
			var b = 3
			c = foo(x,c)*x
			for( b<4;b=b+2){
				c = foo(x,c)*x
			}	
			x = x*x+1
		}
		return
	}	

	func foo(x,y){
		return x*y
	}
	
	func fooX(x,y){
		return x/y
	}`,
	},
}
