# def do(x):
#     e = x * 5
#     b = e * 6
#     c = b * 7
#     f = c * 1
#     d = c * f
#     return d * mul(d,e)
#
# def add(x ,k):
#     z = k * x
#     return do(x) + mul(x,z)
#
#
# def mul(a,b):
#     return a * b
#
# def main():
#     x=365235
#     z=11876525
#     print(do(z) + add(x,x))
#######################
def main():
    #var res = do(x)+do((3)*x)
    print( do(7)+do((3)*7) )#444675)

def do(x):
    e = x * 5
    b = e * e
    c = b * 7
    f = c * c
    d = c + f
    return d

################################

# def add(x ,k):
#     z = k * x
#     return 6 + mul(x,z)

# def asdf(a,b):
#     d = b + b
#     c = a * d
#     e = c - a
#     return e * c
#
# def asdf(a,b):
#     c = a + b
#     e = c - a
#     f = e + b
#     g = f + 2
#     return g * a

##############################
# def doSomething(x ,k):
#     z = k * x
#     return 6 + mul(x,z)
#
# def mul(a,b):
#     return a * b
#
# def main():
#     x=64341
#     z=76548465
#
#     print(mul(x,z) - doSomething(x,x))

#######################
#
# def mul(a,b):
#     return a * b
#
# def asdf(a):
#     b = a * a
#     c = 4 - b
#     d = 5 * c
#     return  mul(d,c) /  mul(b,b)
############################

# def go(a,b,c,d):
#     e = a * b
#     f = c * d
#     g = e * f
#     h = g / e
#     i = h * 5
#     return  g * i
#
# def main():
#     print(go(3,5,7,11))

if __name__ == '__main__':
    #pascal(8)
    main()
