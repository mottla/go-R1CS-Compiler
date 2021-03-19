/*global Viva*/
var colors = [
    0x1f77b4ff, 0xaec7e8ff,
    0xff7f0eff, 0xffbb78ff,
    0x2ca02cff, 0x98df8aff,
    0xd62728ff, 0xff9896ff,
    0x9467bdff, 0xc5b0d5ff,
    0x8c564bff, 0xc49c94ff,
    0xe377c2ff, 0xf7b6d2ff,
    0x7f7f7fff, 0xc7c7c7ff,
    0xbcbd22ff, 0xdbdb8dff,
    0x17becfff, 0x9edae5ff
];



function onLoad() {

    const container = document.getElementById('graph1');
    const width = container.scrollWidth;
    const height = container.scrollHeight || 500;
    const graph = new G6.Graph({
        container: 'graph1',
        width,
        height,
        modes: {
            default: ['drag-canvas', 'drag-node', 'zoom-canvas'],
        },
        animate: true,
        defaultNode: {
            size: 20,
            style: {
                lineWidth: 2,
                stroke: '#5B8FF9',
                fill: '#C6E5FF',
            },
        },
        defaultEdge: {
            size: 2,
            color: '#e2e2e2',
            style: {
                endArrow: {
                    path: 'M 0,0 L 8,4 L 8,-4 Z',
                    fill: '#e2e2e2',
                },
            },
        },layout: {
            type: 'gForce',
            gpuEnable : true,
           // nodeStrength: 30,
            //collideStrength: 0.7,
           // alphaDecay: 0.01,
           // preventOverlap: true,
            maxIteration: 1000,
        },

    });
    const model = {
        id: 'node',
        label: 'node',
        address: 'cq',
        x: 200,
        y: 150,
        style: {
            fill: 'blue',
        },
        size: 100,
    };
    const model2 = {
        id: 'node2',
        label: 'node',
        address: 'cq',
        x: 200,
        y: 150,
        style: {
            fill: 'red',
        },
        size: 100,
    };
    graph.addItem('node', model);
    const g = graph.findById('node2')
    if (g == undefined) {
        graph.addItem('node', model2);
        graph.addItem('edge', {source: 'node', target: 'node2'})
        graph.addItem('edge', {source: 'node', target: 'node2'})
    }


    graph.render()
    const forceLayout = graph.get('layoutController').layoutMethod;
    graph.on('node:dragstart', function (e) {
        graph.layout();
        refreshDragedNodePosition(e);
    });
    graph.on('node:drag', function (e) {
        forceLayout.execute();
        refreshDragedNodePosition(e);
    });
    graph.on('node:dragend', function (e) {
        e.item.get('model').fx = null;
        e.item.get('model').fy = null;
    });

    if (typeof window !== 'undefined')
        window.onresize = () => {
            if (!graph || graph.get('destroyed')) return;
            if (!container || !container.scrollWidth || !container.scrollHeight) return;
            graph.changeSize(container.scrollWidth, container.scrollHeight);
        };


    function refreshDragedNodePosition(e) {
        const model = e.item.get('model');
        model.fx = e.x;
        model.fy = e.y;
    }


    var conn;
    var msg = document.getElementById("msg");
    var log = document.getElementById("log");

    function appendLog(item) {
        var doScroll = log.scrollTop > log.scrollHeight - log.clientHeight - 1;
        log.appendChild(item);
        if (doScroll) {
            log.scrollTop = log.scrollHeight - log.clientHeight;
        }
    }

    document.getElementById("form").onsubmit = function () {
        if (!conn) {
            return false;
        }
        if (!msg.value) {
            return false;
        }
        conn.send(msg.value);
        msg.value = "";
        return false;
    };


    if (window["WebSocket"]) {
        conn = new WebSocket("ws://" + document.location.host + "/ws");
        conn.onclose = function (evt) {
            var item = document.createElement("div");
            item.innerHTML = "<b>Connection closed.</b>";
            appendLog(item);
        };

        conn.onmessage = function (evt) {


            var msg = JSON.parse(evt.data);


            switch (msg.type) {
                case "message":
                    var messages = msg.text.split('\n');
                    for (var i = 0; i < messages.length; i++) {
                        var item = document.createElement("div");
                        item.innerText = messages[i];
                        appendLog(item);
                    }
                    break;

                case "stateChanged":
                    console.log("Put a message here.")

                    for (var i = 0; i < msg.linkAdd.length; i++) {
                        if (graph.findById(msg.linkAdd[i].target) == undefined) {
                            graph.addItem('node', {id: '' + msg.linkAdd[i].target,});
                        }
                        if (graph.findById(msg.linkAdd[i].source) == undefined) {
                            graph.addItem('node', {id: '' + msg.linkAdd[i].source,});
                        }

                        const e = graph.findById('edge' + msg.linkAdd[i].target + msg.linkAdd[i].source)
                        if (e != undefined) {
                            e.update({type: 'arc', curveOffset: (((e.getTarget()).getEdges()).length) * 10})
                            graph.addItem('edge', {
                                source: '' + msg.linkAdd[i].source,
                                target: '' + msg.linkAdd[i].target,
                                type: 'arc',
                                curveOffset: (((e.getTarget()).getEdges()).length) * -10,
                            });

                        } else {
                            graph.addItem('edge', {
                                id: 'edge' + msg.linkAdd[i].target + msg.linkAdd[i].source,
                                source: '' + msg.linkAdd[i].source,
                                target: '' + msg.linkAdd[i].target,

                            });
                        }
                    }
                    for (var i = 0; i < msg.colorChange.length; i++) {
                        const e = graph.findById('' + msg.colorChange[i].target)
                        if (e != undefined) {
                            e.update({
                                label: msg.colorChange[i].label,
                                style : {color : msg.colorChange[i].color},

                            })
                        }
                    }
                    graph.refresh();
                    break;
                case
                "graph"
                :

                    var item = document.createElement("div");
                    item.innerText = "graph " + msg.type;
                    appendLog(item);
                    break;
                default:
                    var item = document.createElement("div");
                    item.innerText = msg;
                    appendLog(item);
                    break;
            }
        }
        ;



    } else {
        var item = document.createElement("div");
        item.innerHTML = "<b>Your browser does not support WebSockets.</b>";
        appendLog(item);
    }
}
