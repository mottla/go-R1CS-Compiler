package main

import (
	"flag"
	"fmt"
	"github.com/mottla/go-R1CS-Compiler/Circuitcompiler"
)

var addr = flag.String("addr", "127.0.0.1:8081", "http service address")

func main() {
	fmt.Println("asdf")
	Circuitcompiler.StartServer(addr)
	fmt.Println("Server is running")
	<-make(chan bool)
}
