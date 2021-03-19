// Copyright 2013 The Gorilla WebSocket Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package Circuitcompiler

import (
	"fmt"
	"time"
)

// Hub maintains the set of active clients and broadcasts messages to the
// clients.
type Hub struct {
	// Registered clients.
	clients map[*Client]bool

	// Inbound messages from the clients.
	broadcast chan interface{}

	// Register requests from the clients.
	register chan *Client

	// Unregister requests from clients.
	unregister chan *Client
}

func newHub() *Hub {
	return &Hub{
		broadcast:  make(chan interface{}),
		register:   make(chan *Client),
		unregister: make(chan *Client),
		clients:    make(map[*Client]bool),
	}
}

func newAddNodeMsg() addNodeAndLinksMsg {
	return addNodeAndLinksMsg{Typeus: "addNode", Links: []int{}}
}

type addNodeAndLinksMsg struct {
	Typeus string `json:"type"`
	Id     int    `json:"id"`
	Links  []int  `json:"connections"`
}

func newAgentStateMsg() agentStateMsg {
	return agentStateMsg{Typeus: "stateChanged", ColorCha: []colorChange{}, AddLinks: []linkChange{}, RemLinks: []linkChange{}}
}

type agentStateMsg struct {
	Typeus   string        `json:"type"`
	ColorCha []colorChange `json:"colorChange"`
	AddLinks []linkChange  `json:"linkAdd"`
	RemLinks []linkChange  `json:"linkRem"`
}
type colorChange struct {
	Id    string `json:"target"`
	Color string `json:"color"`
	Label string `json:"label"`
}
type linkChange struct {
	Target string `json:"target"`
	Source string `json:"source"`
}
type agentState struct {
	colC colorChange
	addL linkChange
	remL linkChange
}

func (h *Hub) run() {

	tkr := time.NewTicker(time.Second * 15)

	for {
		select {
		case client := <-h.register:
			fmt.Println("Client registered")
			h.clients[client] = true
			go start(h)

		case client := <-h.unregister:
			fmt.Println("Client tries to unregister")
			if _, ok := h.clients[client]; ok {
				delete(h.clients, client)
				close(client.send)

				fmt.Println("Client unregistered")
			}
		case message := <-h.broadcast:

			switch cast := message.(type) {
			case addNodeAndLinksMsg:
				for client := range h.clients {
					select {
					case client.send <- message:
					default:
						close(client.send)
						delete(h.clients, client)
					}
				}
			case agentStateMsg:
				for client := range h.clients {
					select {
					case client.send <- message:
					default:
						close(client.send)
						delete(h.clients, client)
					}
				}
			case []byte:
				fmt.Printf("byte messag type dude %v", string(cast))

				for client := range h.clients {
					select {
					case client.send <- string(cast):
					default:
						close(client.send)
						delete(h.clients, client)
					}
				}
			default:
				fmt.Printf("dunno this messag type dude ")

			}
		case c := <-tkr.C:
			for client := range h.clients {
				select {
				case client.send <- c.Local().UTC().String():
				//case client.send <- []byte(`{"num":6,"strs":["a","b"]}`):
				default:
					close(client.send)
					delete(h.clients, client)
				}
			}
		}
	}
}
