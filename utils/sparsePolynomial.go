package utils

import (
	"fmt"
	"math/big"
)

// Mul multiplies two polinomials over the Finite Field
func (fq Fq) MulSparse(a, b *AvlTree) *AvlTree {
	r := NewAvlTree()

	for L := range a.ChannelNodes(true) {
		for R := range b.ChannelNodes(true) {

			r.Put(L.Key+R.Key, fq.Mul(R.Value, L.Value), fq.Add)
		}
	}
	return r
}

// Mul multiplies a sparse polynomail with a scalar over the Finite Field
func (fq Fq) MulSparseScalar(a *AvlTree, scalar *big.Int) *AvlTree {
	a = a.Clone()
	for L := range a.ChannelNodes(true) {
		a.Put(L.Key, scalar, fq.Mul)
	}
	return a
}

type Entry struct {
	Key   uint
	Value *big.Int
}

// Div divides two polinomials over the Finite Field, returning the result and the remainder
func (fq Fq) DivideSparse(a, b *AvlTree) (result, rem *AvlTree) {
	result = NewAvlTree()
	rem = a.Clone()
	maxA, maxB := rem.MaxNode(), b.MaxNode()
	for ; maxA != nil && maxB != nil && maxA.Key() >= maxB.Key(); maxA = rem.MaxNode() {
		l := fq.Div(maxA.Value(), maxB.Value())
		pos := maxA.Key() - maxB.Key()
		aux := NewAvlTree()
		aux.InsertNoOverwriteAllowed(pos, l)
		result.InsertNoOverwriteAllowed(pos, l)
		mul := fq.MulSparse(b, aux)
		rem = fq.SubToSparse(rem, mul)
	}
	return result, rem
}

// Add adds two polinomials over the Finite Field
func (a *AvlTree) Clone() *AvlTree {
	r := NewAvlTree()

	for v := range a.ChannelNodes(false) {
		r.Insert(v.Key, v.Value)
	}

	return r
}

func (fq Fq) AddToSparse(a, b *AvlTree) *AvlTree {
	//r := NewAvlTree()

	//for v := range a.ChannelNodes(false) {
	//	r.Put(v.Key, v.Value, fq.Add)
	//}
	a = a.Clone()
	for v := range b.ChannelNodes(false) {
		a.Put(v.Key, v.Value, fq.Add)
	}
	return a
}

//EvalPoly Evaluates a sparse polynomial
func (fq Fq) EvalSparsePoly(poly *AvlTree, at *big.Int) (result *big.Int) {
	//get the number of bits of the highest degree
	nBits := len(fmt.Sprintf("%b", poly.MaxNode().Key()))
	if nBits < poly.Size() {
		//fmt.Println("WARN, ur polynomial is not very sparse. a casual array type polynomial becomes more efficient at some point. not necessarily in this case however.")
	}
	if at.Cmp(bigZero) == 0 {
		if v, b := poly.Get(0); b == nil {
			return new(big.Int).Set(v)
		}
		return big.NewInt(0)
	}
	//tree that stores intermediate results of exponent ladder. Key is the exponent. value is at^key
	alredyComputedExponents := NewAvlTree()
	alredyComputedExponents.InsertNoOverwriteAllowed(1, at)
	result = new(big.Int).SetInt64(0)
	for j := range poly.ChannelNodes(true) {
		rem := j.Key
		q := uint(0)
		nextPower := new(big.Int).SetInt64(1)
		//apply a greedy algorithm to tackle the knapsack problem we face her. we want to create the
		//next power by reusing already computed powers, starting from the biggest.
		//example: we want x^15, we have x^8,x^3,x
		//we get x^15 = (x^8)^1 * (x^3)^2 * (x)^1
		for _, highestAlreadyComputedExponent := range alredyComputedExponents.DecendingNodes() {
			q, rem = euclid(rem, highestAlreadyComputedExponent.Key)
			vv := fq.ExpInt(highestAlreadyComputedExponent.Value, q)
			alredyComputedExponents.Insert(q*highestAlreadyComputedExponent.Key, vv)
			nextPower = fq.Mul(nextPower, vv)
			if rem == 0 {
				break
			}
		}
		result = fq.Add(result, fq.Mul(j.Value, nextPower))
	}
	return result
}

//euclid returns q,r s.t. a=bq+r
func euclid(a, b uint) (q, r uint) {
	return a / b, a % b
}

// Sub subtracts two polinomials over the Finite Field
func (fq Fq) SubToSparse(a, b *AvlTree) *AvlTree {

	a = a.Clone()
	for v := range b.ChannelNodes(false) {
		a.Put(v.Key, fq.Neg(v.Value), fq.Add)
	}
	return a
}

// LagrangeInterpolation performs the Lagrange Interpolation / Lagrange Polynomials operation
func (f Fq) InterpolateSparseArray(dataArray *AvlTree, degree int) (polynom *AvlTree) {
	// https://en.wikipedia.org/wiki/Lagrange_polynomial
	if dataArray.MaxPower() >= uint(degree) {
		panic("interpolation degree cannot be smaller then highest degree in the polynomial")
	}
	var base = func(pointPos, totalPoints int) (r *AvlTree) {

		if v, ex := f.bases[baseLengthPair{baseIndex: pointPos, Length: totalPoints}]; ex {
			return v
		}
		//r = NewAvlTree()
		facBig := big.NewInt(1)

		for i := 0; i < pointPos; i++ {
			facBig = f.Mul(facBig, big.NewInt(int64(pointPos-i)))
		}
		for i := pointPos + 1; i < totalPoints; i++ {
			facBig = f.Mul(facBig, big.NewInt(int64(pointPos-i)))
		}

		r = NewSparseArrayWith(uint(0), new(big.Int).SetInt64(1))
		for i := 0; i < totalPoints; i++ {
			if i != pointPos {
				r = f.MulSparse(r, NewSparseArrayFromArray([]*big.Int{big.NewInt(int64(-i)), big.NewInt(int64(1))}))
			}
		}
		hf := f.Inverse(facBig)
		r = f.MulSparseScalar(r, hf)
		f.bases[baseLengthPair{baseIndex: pointPos, Length: totalPoints}] = r
		return r
	}
	//if  IsZeroArray(dataArray.ToArray(degree)){
	//	//at position -1 we store the all zero polynomial
	//	if v,ex:=f.bases[baseLengthPair{baseIndex: -1,Length: degree}];ex{
	//		return v
	//	}
	//	p:= PolynomialField{
	//		F: f,
	//	}
	//	DomainPoly := NewSparseArrayFromArray(p.DomainPolynomial(degree))
	//	f.bases[baseLengthPair{baseIndex: -1,Length: degree}]= DomainPoly
	//	return DomainPoly
	//}
	polynom = NewAvlTree()
	for v := range dataArray.ChannelNodes(true) {
		prod := f.MulSparseScalar(base(int(v.Key), degree), v.Value)
		polynom = f.AddToSparse(polynom, prod)
	}
	return polynom
}
func (f Fq) SparseScalarProduct(a *AvlTree, b []*big.Int) (res *big.Int) {
	res = big.NewInt(0)
	for v := range a.ChannelNodes(true) {
		res = res.Add(res, f.Mul(v.Value, b[v.Key]))
	}
	return
}

func (f Fq) Combine(a *AvlTree, w []*big.Int) (scaledPolynomial *AvlTree) {
	for v := range a.ChannelNodes(true) {
		a.Put(v.Key, w[v.Key], f.Mul)
	}
	return a
}

//
func (f Fq) LinearCombine(polynomials []*AvlTree, w []*big.Int) (scaledPolynomials []*AvlTree) {
	scaledPolynomials = make([]*AvlTree, len(w))
	for i := 0; i < len(w); i++ {
		scaledPolynomials[i] = f.MulSparseScalar(polynomials[i], w[i])

	}
	return
}
func (f Fq) AddPolynomials(polynomials []*AvlTree) (sumPoly *AvlTree) {
	sumPoly = NewAvlTree()
	for i := 0; i < len(polynomials); i++ {
		sumPoly = f.AddToSparse(sumPoly, polynomials[i])
	}
	return
}

func TransposeSparse(matrix []*AvlTree, witness int) (tra []*AvlTree) {
	r := make([]*AvlTree, witness)
	for i := 0; i < witness; i++ {
		r[i] = NewAvlTree()
	}
	for y, tree := range matrix {
		//if k := int(tree.MaxPower()); k > max {
		//	max = k
		//}
		for val := range tree.ChannelNodes(true) {
			//for int(val.Key)+1 > len(r) {
			//	r = append(r, NewAvlTree())
			//}
			r[int(val.Key)].InsertNoOverwriteAllowed(uint(y), val.Value)
		}
	}
	return r
}
