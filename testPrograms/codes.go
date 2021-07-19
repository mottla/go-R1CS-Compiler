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

var TestFibonacci = []TraceCorrectnessTest{
	{
		Skip: false,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(3))},
		}},

		Code: `
func main(x field) field{
	return (1*Fibonacci(8,dyn))*(x*x)*dyn[3]
}
var dyn = [9]field{1,1,0,0,0,0,0,0,0}

func Fibonacci(a field, dyn [9]field) field{
	var i = 12
	for (i = 2 ; i<a;i=i+1){
		dyn[i] = dyn[i-1]+dyn[i-2]
	}
    return dyn[a-1]
}
`},

	{
		Skip: false,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(3))},
		}},

		Code: `
func main(x field)(field){
	public{
	x
	}
	
	return (x*Fibonacci(7))
}
func Fibonacci(a field)(field){
	if a==0{
		return 1
	}
	if a==1{
		return 1
	}
	return Fibonacci(a-1)+Fibonacci(a-2)
}
`},
	{
		Skip: false,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(2))},
		}},

		Code: `

func main(u field) field{
	public{u}
	return Fibonacci(8,1)*u*u
}

func Fibonacci(a field,v field) field{
	var dyn = [2]field{v,v}
	
	for (var i = 2 ; i<a; i =i+1){
		var n = (dyn[0] + dyn[1])
		dyn[0] = dyn[1]
		dyn[1] = n
	}
    return dyn[1]
}
`},
	{
		Skip: false,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(3))},
		}},

		Code: `
func main(x field) field{
	return (1*Fibonacci(8))*(x*x)
}
var dyn = [2]field{1,1}
func Fibonacci(a field) field{
	
	for (var i = 2; i<a ; i=i+1){
		var n = dyn[0]+dyn[1]
		dyn[0] = dyn[1]
		dyn[1] = n
	}
    return dyn[1]
}
`},

	{
		Skip: false,
		IO: []InOut{{
			Inputs: []*big.Int{big.NewInt(int64(3))},
		}},

		Code: `
func main(x field) field{
	return (1*Fibonacci(8))*(x*x)
}
var dyn = [9]field{1,1,0,0,0,0,0,0,0}

func Fibonacci(a field) field{
	var i = 12
	for (i = 2 ; i<a;i=i+1){
		dyn[i] = dyn[i-1]+dyn[i-2]
	}
    return dyn[a-1]
}
`},
}

var TestSudoku = []TraceCorrectnessTest{
	{
		Skip: true,
		IO: []InOut{{
			Inputs: sudoku(),
		}},
		Code: `

func main(x [9][9]field){	
public{
	x[0][0],x[5][3]
}
	var prod = 2*3*4*5*6*7*8*9
	var i = 0
	# we check if all inputs are in the range 1 to 9
	for (i = 0;i<9;i=i+1){		
			for (var j = 0;j<9;j=j+1){
				equal(constraint(x[i][j]),0)
		}
	}
	
	
	return
}
func constraint(x field)field{
	return (x-1)*(x-2)*(x-3)*(x-4)*(x-5)*(x-6)*(x-7)*(x-8)*(x-9)
}

`},
	{
		Skip: false,
		IO: []InOut{{
			Inputs: sudoku(),
		}},
		Code: `
func main(sudoku [9][9]field){	
	public{
	sudoku[3][4]
	}	
	# we check if all inputs are in the range 1 to 9	
	func iterate(fromX field,toX field ,stepSize field, call func(a field)){       
		if fromX==toX{           
			return
		}			
		call(fromX)
		iterate(fromX+stepSize,toX,stepSize,call)
	}	

    var product  = 2*3*4*5*6*7*8*9	
	
	#we check if all columns  	
	var checkRange = func(x field){		
		var EQ = func(y field){			
			equal(constraint(sudoku[x][y]),0)
		}
		iterate(0,9,1,EQ)
	}
	
   

	iterate(0,9,1,checkRange)
  
	return
}
func constraint(x field) field{
	return (x-1)*(x-2)*(x-3)*(x-4)*(x-5)*(x-6)*(x-7)*(x-8)*(x-9)
}

`},
	{
		Skip: false,
		IO: []InOut{{
			Inputs: sudoku(),
		}},
		Code: `
func main(sudoku [9][9]field){	
	public{
	sudoku[3][4]
	}	
	# we check if all inputs are in the range 1 to 9	
	func iterate(fromX field,toX field ,stepSize field, call func(a field)){       
		if fromX==toX{           
			return
		}			
		call(fromX)
		iterate(fromX+stepSize,toX,stepSize,call)
	}	

    var product  = 2*3*4*5*6*7*8*9	
	
	#we check if all columns  	
	var checkRange = func(x field){		
		var EQ = func(y field){			
			equal(constraint(sudoku[x][y]),0)
		}
		iterate(0,9,1,EQ)
	}
	
    var col = func(x field){
		var prod = 1
		var EQ = func(y field){	
			prod = prod * sudoku[x][y]			
		}
		iterate(0,9,1,EQ)
		equal(prod,product)
	}
    var row = func(x field){
		var prod = 1
		var EQ = func(y field){	
			prod = prod * sudoku[y][x]			
		}
		iterate(0,9,1,EQ)
		equal(prod,product)
	}

	iterate(0,9,1,checkRange)
    iterate(0,9,1,col)
    iterate(0,9,1,row)		
	
	
	for (var i = 0;i<9;i=i+3){	
		for (var j = 0;j<9;j=j+3){
			var colProduct = 1				
			for (var k = 0;k<3;k=k+1){				
					for (var l = 0;l<3;l=l+1){
						colProduct = colProduct * sudoku[i+k][j+l]	
				}
			}
			equal(colProduct,product)		
		}		
	}
	return
}
func constraint(x field) field{
	return (x-1)*(x-2)*(x-3)*(x-4)*(x-5)*(x-6)*(x-7)*(x-8)*(x-9)
}

`},
	{
		Skip: false,
		IO: []InOut{{
			Inputs: sudoku(),
		}},
		Code: `

func main(x [9][9]field){	
public{
	x[0][0],x[5][3]
}
	var prod = 2*3*4*5*6*7*8*9
	var i = 0
	# we check if all inputs are in the range 1 to 9
	for (i = 0;i<9;i=i+1){		
			for (var j = 0;j<9;j=j+1){
				equal(constraint(x[i][j]),0)
		}
	}
	#we check if all columns  	
	for (i = 0;i<9;i=i+1){
		var colProduct = 1				
		for (var j = 0;j<9;j=j+1){
			colProduct = colProduct * x[i][j]			
		}
		equal(colProduct,prod)
	}
	#we check if all rows  
	
	for (i = 0;i<9;i=i+1){
		var colProduct = 1		
		for (var j = 0;j<9;j=j+1){
			colProduct = colProduct * x[j][i]			
		}
		equal(colProduct,prod)
	}
	#we check if each 3x3 box is satisfied
	for (i = 0;i<9;i=i+3){		
		for (var j = 0;j<9;j=j+3){
			var colProduct = 1			
			for (var k = 0;k<3;k=k+1){				
					for (var l = 0;l<3;l=l+1){
						colProduct = colProduct * x[i+k][j+l]	
				}
			}
			equal(colProduct,prod)		
		}
		
	}
	
	return
}
func constraint(x field)field{
	return (x-1)*(x-2)*(x-3)*(x-4)*(x-5)*(x-6)*(x-7)*(x-8)*(x-9)
}

`},
}
