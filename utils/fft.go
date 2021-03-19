package utils

import (
	"math/big"
	"math/bits"
)

type FFT_PrecomputedParas struct {
	RootOfUnity                  *big.Int
	RootOfUnitys_SquareSteps     []*big.Int // w,w^2,w^4,w^8..
	RootOfUnity_inv              *big.Int
	RootOfUnity_invs_SquareSteps []*big.Int // w^-1,w^-2,w^-4,w^-8..
	RootOfUnitys                 []*big.Int // 1,w,w^2,w^3,w^4..,
	RootOfUnity_invs             []*big.Int // 1,w^-1,w^-2,w^-3,w^-4..
	Size                         int        //number of points we want to consider, rounded to the next power of two
	pf                           *PolynomialField
	Domain                       []*big.Int //p(x) = x^size - 1
}

func (pf *PolynomialField) PrepareFFT(length int) *FFT_PrecomputedParas {
	paras := new(FFT_PrecomputedParas)
	paras.pf = pf
	adicity, _ := pf.F.Adicity()
	//TODO analyse where this number comes from. sage tells us, that it indeed has order 2^adicity (over bn256)
	//change of curve will cause things to break here
	rootOfUnity, _ := new(big.Int).SetString("19103219067921713944291392827692070036145651957329286315305642004821462161904", 10)
	//now find the closest possible power of two, to fill up the datapoints
	bit := bits.Len(uint(NextPowerOfTwo(length)))
	// 1000 = 1 << 3, so we remove 1 from bit
	bit = bit - 1
	if adicity < bit {
		panic("field has two low adicity to support fft for that many values")
	}
	paras.Size = 1 << bit
	paras.Domain = ArrayOfBigZeros(paras.Size + 1)
	paras.Domain[paras.Size] = new(big.Int).SetInt64(1)
	paras.Domain[0] = pf.F.Neg(new(big.Int).SetInt64(1)) // -1
	//now determine the root of unity
	// exponent = c*2^(adicity-(bit+1))
	// now for any k in Field, (k^(exponent))^2^(bit+1)==1 mod P
	//fmt.Println(c.String())
	exponent := new(big.Int).Lsh(new(big.Int).SetInt64(1), uint(adicity-bit-1))

	alphaSqrt := pf.F.Exp(rootOfUnity, exponent)
	//fmt.Println(alphaSqrt.String())
	alpha := pf.F.Mul(alphaSqrt, alphaSqrt)
	alphaInv := pf.F.Inverse(alpha)
	if pf.F.Exp(alpha, new(big.Int).Lsh(new(big.Int).SetInt64(1), uint(bit))).Cmp(bigOne) != 0 {
		panic("(k^(exponent))^2^(bit+1) != 1 mod P ")
	}

	bigAlphas_SS := make([]*big.Int, bit)
	bigAlphas_Inv_SS := make([]*big.Int, bit)
	bigAlphas_Inv := make([]*big.Int, 1<<(bit))
	bigAlphas := make([]*big.Int, 1<<(bit))

	bigAlphas_SS[0] = alpha
	bigAlphas_Inv_SS[0] = alphaInv
	for i := 1; i < bit; i++ {
		bigAlphas_SS[i] = pf.F.Mul(bigAlphas_SS[i-1], bigAlphas_SS[i-1])
		bigAlphas_Inv_SS[i] = pf.F.Mul(bigAlphas_Inv_SS[i-1], bigAlphas_Inv_SS[i-1])
	}

	bigAlphas[0] = new(big.Int).SetInt64(1)
	bigAlphas_Inv[0] = new(big.Int).SetInt64(1)
	bigAlphas[1<<(bit-1)] = pf.F.Neg(bigAlphas[0])
	bigAlphas_Inv[1<<(bit-1)] = pf.F.Neg(bigAlphas_Inv[0])
	for i := 1; i < 1<<(bit-1); i++ {
		bigAlphas[i] = pf.F.Mul(bigAlphas[i-1], alpha)
		bigAlphas_Inv[i] = pf.F.Mul(bigAlphas_Inv[i-1], alphaInv)
		bigAlphas[i+(1<<(bit-1))] = pf.F.Neg(bigAlphas[i])
		bigAlphas_Inv[i+(1<<(bit-1))] = pf.F.Neg(bigAlphas_Inv[i])
	}

	paras.RootOfUnity = alpha
	paras.RootOfUnity_inv = alphaInv
	paras.RootOfUnity_invs_SquareSteps = bigAlphas_Inv_SS
	paras.RootOfUnitys_SquareSteps = bigAlphas_SS
	paras.RootOfUnitys = bigAlphas
	paras.RootOfUnity_invs = bigAlphas_Inv

	if pf.F.Exp(bigAlphas_SS[len(bigAlphas_SS)-1], new(big.Int).SetInt64(2)).Cmp(bigOne) != 0 {
		panic("cannot happen")
	}

	return paras
}

func (p *FFT_PrecomputedParas) InvDFFT(ValuesAtRoots []*big.Int, shift *big.Int) (coefficients []*big.Int) {
	c1 := make(chan []*big.Int)
	go p._dfft(p.RootOfUnity_invs_SquareSteps, p.RootOfUnity_invs, p.ExtendPolyToDomain(ValuesAtRoots), c1)
	coefficients = <-c1
	div := new(big.Int).SetInt64(int64(len(coefficients)))
	for i, v := range coefficients {
		coefficients[i] = p.pf.F.Div(v, div)
	}
	return p.pf.shift(shift, true, coefficients)
	//return coefficients
}

func (p *FFT_PrecomputedParas) DFFT(polynomial []*big.Int, shift *big.Int) (evaluatedAtRoots []*big.Int) {
	c1 := make(chan []*big.Int)
	extd := p.ExtendPolyToDomain(polynomial)

	go p._dfft(p.RootOfUnitys_SquareSteps, p.RootOfUnitys, p.pf.shift(shift, false, extd), c1)
	return <-c1
}

func (p *FFT_PrecomputedParas) ExtendPolyToDomain(in []*big.Int) []*big.Int {
	if len(in) < p.Size {
		rest := p.Size - len(in)
		//fmt.Printf("\npolysize %v, filled up to next power of two 2^%v. Add %v dummy values", len(polynomial), bit, rest)
		in = append(in, ArrayOfBigZeros(rest)...)
	}
	return in
}

func (p *FFT_PrecomputedParas) _dfft(bigAlphas_SS, bigAlphas, data []*big.Int, in chan []*big.Int) {
	if len(data) == 1 {
		in <- data
		return
	}
	even := []*big.Int{}
	odd := []*big.Int{}
	for k, _ := range data {
		if k%2 == 1 {
			odd = append(odd, data[k])
		} else {
			even = append(even, data[k])
		}
	}

	removeUneven := make([]*big.Int, 0, len(bigAlphas)/2)
	for k, v := range bigAlphas {
		if k%2 == 0 {
			removeUneven = append(removeUneven, v)
		}
	}
	c1, c2 := make(chan []*big.Int, 1), make(chan []*big.Int, 1)

	go p._dfft(bigAlphas_SS[1:], removeUneven, even, c1)
	go p._dfft(bigAlphas_SS[1:], removeUneven, odd, c2)
	y, y2 := <-c1, <-c2

	res := make([]*big.Int, len(data))
	for k := 0; k < len(data)/2; k++ {
		wy := p.pf.F.Mul(bigAlphas[k], y2[k])
		res[k] = p.pf.F.Add(y[k], wy)
		res[k+(len(data)/2)] = p.pf.F.Sub(y[k], wy)
	}
	in <- res
	return
}
