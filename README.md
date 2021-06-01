# go-R1CS Compiler

**UNDER CONSTRUCTION**


**Circuit Language**
A subset of Golang (with slight modifications), suitable for the creation of Non-Interactive Succinct Arguments, such as zkSNARKs.
This projects goal is to provide a compiler that translates Go-code into R1CS, which is the compilation target
and starting point for various zkSNARK constructions.

-This compiler currently supports the creation of Groth16 zkSNARKs.

-Comes with JS graph rendering support to visualize the generated arithmetic circuit if needed. 

# Language

## main

as in Go, every program starts with the function 'main'

```
func main(){
#this is a comment
}
```

main can be fed with an arbitrary amount of single arguments and n-dimensional static size arrays of single values.
Main does not support functions as inputs.

```
func main(a bool,b [2]uint32, c field, d uint64){

}
```

## Declare public inputs

In order to declare, which of the Main-Function inputs will be part of the public statement of the SNARK, write

```
func main(a bool,b [2][3]uint32, c field, d uint64){
    public{
        a, 
        b[1][1],
        c
    }
}
```


## variables

Variable assignment and overloading follows the same logic as in Golang: 

```
    var a = 42*17  # is now a field type element
    # var a = x*x   -> Error- variable a already declared
    # a = true   -> type missmatch. Field expected, got bool
    var b = uint32(235)

    #declare array
    var c = [2]uint32{b,b+2}
    
    #declare function. Functions can return functions or take them as arguments
    var d = func(x field, b func()(bool) , c [4]bool)( func()(field)) {
               return 42
            }

}
```


## Function preloading

At this point we extended Go by this cool functionality.
One now can partially preload a function

```
func main(x field){
    var multiply = func(a field,b field)(field){return a*b}
    var multiplyBy5 = multiply(5)
    multiplyBy5(x) #is now the same as multiply(5,x) 
}
```

## Operations

| Operator   |      Description      |  Remark |
|----------|:-------------:|------:|
| + |  addition |  |
| * |    multiplication   |   |
| - | subtraction |     |
| / | division|     |
| ** | exponentiation |    *|
| << | left shift|   * |
| &gt;&gt; | right-shift |    *  |
| <<< | rotate-left |   * |
| &gt;&gt;&gt; | rotate-right |    * |
| & | bitwise-and |     |
| &#124; | bitwise-or |     |
| ^ | bitwise-xor |     |
| == | equality |     |
| != | un-equality |     |

*rhs must be fixed at compile-time


## if-else if-else

in order to create braching conditions, write:

```
if expression1{ 
    ...
}else if expression2{
    ...
}else if expression3{
    ...
}else{
    ...
}
```

note that we currently only support static decidable branching conditions

## arrays

tba.

# SNARK stuff

## euquality assertion gate

in order to create an equality assertion constraint write

```
equal(expression1,expression2)
```

example: in order to prove knowledge of a square root of some input y, write

```
func main(x field,y field){
   public{y}
   equal(x*x,y)
}
```

## split

to split a value into its bit representatives write

```
SPLIT(x)
```

now the i'th bit of x can be accessed with x[i], where x[0] is the least significant bit
