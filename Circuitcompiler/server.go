package Circuitcompiler

import (
	"log"
	"net/http"
)

func StartServer(addr *string) *Hub {
	hub := newHub()
	go hub.run()

	http.Handle("/", http.FileServer(http.Dir("./Circuitcompiler/web_g6")))

	http.HandleFunc("/ws", func(w http.ResponseWriter, r *http.Request) {
		serveWs(hub, w, r)
	})
	go func() {
		err := http.ListenAndServe(*addr, nil)
		if err != nil {

			log.Fatal("ListenAndServe: ", err)
		}
	}()

	return hub
}
