package Circuitcompiler

import (
	"flag"
	"fmt"
	"testing"
)

var addr = flag.String("addr", "127.0.0.1:8081", "http service address")

func Test(t *testing.T) {
	StartServer(addr)
	fmt.Println("Server is running")
	<-make(chan bool)

}
