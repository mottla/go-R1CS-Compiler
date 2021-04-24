package Circuitcompiler

import "testing"

//factors are essential to identify, if a specific Gate has been computed already
//eg. if we can extract a factor from a Gate that is independent of commutativity, multiplicativitz we will do much better, in finding and reusing old outputs do
//minimize the multiplication Gate number
// for example the Gate a*b == Gate b*a hence, we only need to compute one of both.

func TestNewFactors(t *testing.T) {

}
