package utils

import "testing"

func TestNextPowerOfTwo(t *testing.T) {
	for i := 0; i < 63; i++ {
		if NextPowerOfTwo(1<<i) != 1<<i {
			t.Error("err")
		}
	}
	for i := 0; i < 62; i++ {
		if NextPowerOfTwo(1+(1<<i)) != 1<<(i+1) {
			t.Error("err")
		}
	}
	for i := 2; i < 62; i++ {
		if NextPowerOfTwo((1<<i)-1) != 1<<(i) {
			t.Error("err")
		}
	}
}
