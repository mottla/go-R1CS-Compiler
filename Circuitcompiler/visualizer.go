package Circuitcompiler

import (
	"github.com/mottla/go-R1CS-Compiler/testPrograms"
)

func start(hub *Hub) {

	code := testPrograms.TestPrograms[3]

	program := Parse(code.Code, true)
	container := program.Execute()
	gates := container.OrderedGates()
	res := newAgentStateMsg()

	for _, g := range gates {

		//if g.gateType == additionGate {
		//	res.ColorCha = append(res.ColorCha, colorChange{
		//		Id:    g.identifier,
		//		Color: "#FFD6E7",
		//		Label: "+",
		//	})
		//}
		//if g.gateType == multiplicationGate {
		res.ColorCha = append(res.ColorCha, colorChange{
			Id:    g.identifier,
			Color: "#AAD8D8",
			Label: "x",
		})
		//}

		for _, l := range g.leftIns {

			if l.Typ.Type == DecimalNumberToken {
				res.ColorCha = append(res.ColorCha, colorChange{
					Id:    g.identifier,
					Color: "#F6C3B7",
					//Label: l.String(),
				})
			}

			res.AddLinks = append(res.AddLinks, linkChange{
				Target: g.identifier,
				Source: l.Typ.Identifier,
			})
		}
		for _, l := range g.rightIns {
			if l.Typ.Type == DecimalNumberToken {
				res.ColorCha = append(res.ColorCha, colorChange{
					Id:    g.identifier,
					Color: "#BDEFDB",
					//Label: l.String(),
				})
			}

			res.AddLinks = append(res.AddLinks, linkChange{
				Target: g.identifier,
				Source: l.Typ.Identifier,
			})
		}
		for _, l := range g.outIns {

			res.AddLinks = append(res.AddLinks, linkChange{
				Target: g.identifier,
				Source: l.Typ.Identifier,
			})
		}

	}
	hub.broadcast <- res

	return
}
