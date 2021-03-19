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


function beginRemoveNodesLoop(graph) {
    var nodesLeft = [];
    graph.forEachNode(function (node) {
        nodesLeft.push(node.id);
    });

    var removeInterval = setInterval(function () {
        var nodesCount = nodesLeft.length;

        if (nodesCount > 0) {
            var nodeToRemove = Math.min((Math.random() * nodesCount) << 0, nodesCount - 1);

            graph.removeNode(nodesLeft[nodeToRemove]);
            nodesLeft.splice(nodeToRemove, 1);
        }

        if (nodesCount === 0) {
            clearInterval(removeInterval);
            setTimeout(function () {
                beginAddNodesLoop(graph);
            }, 100);
        }
    }, 100);
}

function beginAddNodesLoop(graph) {
    var i = 0, m = 10, n = 50;
    var addInterval = setInterval(function () {
        graph.beginUpdate();

        for (var j = 0; j < m; ++j) {
            var node = i + j * n;
            if (i > 0) {
                graph.addLink(node, i - 1 + j * n);
            }
            if (j > 0) {
                graph.addLink(node, i + (j - 1) * n);
            }
        }
        i++;
        graph.endUpdate();

        if (i >= n) {
            clearInterval(addInterval);
            setTimeout(function () {
                beginRemoveNodesLoop(graph);
            }, 10000);
        }
    }, 100);
}

function onLoad() {
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


    var graph = Viva.Graph.graph();

    var layout = Viva.Graph.Layout.forceDirected(graph, {
        springLength: 20,
        springCoeff: 0.0003,
        dragCoeff: 0.09,
        gravity: -3,
        theta: 0.08
    });

    var graphics = Viva.Graph.View.webglGraphics();
    graphics
        .node(function (node) {
            return Viva.Graph.View.webglSquare(20, 0xc49c94ff);
        })
    .link(function (link) {
        return Viva.Graph.View.webglLine(colors[(Math.random() * colors.length) << 0]);
    });

    var renderer = Viva.Graph.View.renderer(graph,
        {
            layout: layout,
            graphics: graphics,
            container: document.getElementById('graph1'),
            renderLinks: true
        });
    var multiSelectOverlay;
    renderer.run(50);

    //graph.addLink(1, 2)
    //beginAddNodesLoop(graph);
    l = layout;

    j = 1000;

    if (window["WebSocket"]) {
        conn = new WebSocket("ws://" + document.location.host + "/ws");
        conn.onclose = function (evt) {
            var item = document.createElement("div");
            item.innerHTML = "<b>Connection closed.</b>";
            appendLog(item);
        };

        document.addEventListener('keydown', function(e) {
            if (e.which === 16 && !multiSelectOverlay) { // shift key
                multiSelectOverlay = startMultiSelect(graph, renderer, layout);
            }
        });
        document.addEventListener('keyup', function(e) {
            if (e.which === 16 && multiSelectOverlay) {
                multiSelectOverlay.destroy();
                multiSelectOverlay = null;
            }
        });

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
                    for (var i = 0; i < msg.linkAdd.length; i++) {
                        if (!graph.getNode(msg.linkAdd[i].target)) {
                            graph.addNode(msg.linkAdd[i].target);
                        }
                        if (!graph.getNode(msg.linkAdd[i].source)) {
                            graph.addNode(msg.linkAdd[i].source);
                        }
                        if (!graph.getLink(msg.linkAdd[i].target, msg.linkAdd[i].source)) {
                            graph.addLink(msg.linkAdd[i].target, msg.linkAdd[i].source);
                        }
                    }

                    for (var i = 0; i < msg.linkRem.length; i++) {
                       //   var link = graph.hasLink(msg.linkRem[i].id, msg.linkRem[i].link)
                        //  graph.removeLink(link);
                    }
                    for (var i = 0; i < msg.colorChange.length; i++) {
                     //   var nod = graphics.getNodeUI(msg.colorChange[i].id);
                      //  nod.color = msg.colorChange[i].color;
                    }
                    renderer.rerender()

                    break;
                case "graph":

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
        };

    } else {
        var item = document.createElement("div");
        item.innerHTML = "<b>Your browser does not support WebSockets.</b>";
        appendLog(item);
    }
}

function startMultiSelect(graph, renderer, layout) {
    var graphics = renderer.getGraphics();
    var domOverlay = document.querySelector('.graph-overlay');
    var overlay = createOverlay(domOverlay);
    overlay.onAreaSelected(handleAreaSelected);

    return overlay;

    function handleAreaSelected(area) {
        // For the sake of this demo we are using silly O(n) implementation.
        // Could be improved with spatial indexing if required.
        var topLeft = graphics.transformClientToGraphCoordinates({
            x: area.x,
            y: area.y
        });

        var bottomRight = graphics.transformClientToGraphCoordinates({
            x: area.x + area.width,
            y: area.y + area.height
        });

        graph.forEachNode(higlightIfInside);
        renderer.rerender();

        return;

        function higlightIfInside(node) {
            var nodeUI = graphics.getNodeUI(node.id);
            if (isInside(node.id, topLeft, bottomRight)) {
                nodeUI.color = 0xFFA500ff;
                nodeUI.size = 20;
            }
            // else {
            //     nodeUI.color = 0x009ee8ff;
            //     nodeUI.size = 10;
            // }
        }

        function isInside(nodeId, topLeft, bottomRight) {
            var nodePos = layout.getNodePosition(nodeId);
            return (topLeft.x < nodePos.x && nodePos.x < bottomRight.x &&
                topLeft.y < nodePos.y && nodePos.y < bottomRight.y);
        }
    }
}

function createOverlay(overlayDom) {
    var selectionClasName = 'graph-selection-indicator';
    var selectionIndicator = overlayDom.querySelector('.' + selectionClasName);
    if (!selectionIndicator) {
        selectionIndicator = document.createElement('div');
        selectionIndicator.className = selectionClasName;
        overlayDom.appendChild(selectionIndicator);
    }

    var notify = [];
    var dragndrop = Viva.Graph.Utils.dragndrop(overlayDom);
    var selectedArea = {
        x: 0,
        y: 0,
        width: 0,
        height: 0
    };
    var startX = 0;
    var startY = 0;

    dragndrop.onStart(function(e) {
        startX = selectedArea.x = e.clientX;
        startY = selectedArea.y = e.clientY;
        selectedArea.width = selectedArea.height = 0;

        updateSelectedAreaIndicator();
        selectionIndicator.style.display = 'block';
    });

    dragndrop.onDrag(function(e) {
        recalculateSelectedArea(e);
        updateSelectedAreaIndicator();
        notifyAreaSelected();
    });

    dragndrop.onStop(function() {
        selectionIndicator.style.display = 'none';
    });

    overlayDom.style.display = 'block';

    return {
        onAreaSelected: function(cb) {
            notify.push(cb);
        },
        destroy: function () {
            overlayDom.style.display = 'none';
            dragndrop.release();
        }
    };

    function notifyAreaSelected() {
        notify.forEach(function(cb) {
            cb(selectedArea);
        });
    }

    function recalculateSelectedArea(e) {
        selectedArea.width = Math.abs(e.clientX - startX);
        selectedArea.height = Math.abs(e.clientY - startY);
        selectedArea.x = Math.min(e.clientX, startX);
        selectedArea.y = Math.min(e.clientY, startY);
    }

    function updateSelectedAreaIndicator() {
        selectionIndicator.style.left = selectedArea.x + 'px';
        selectionIndicator.style.top = selectedArea.y + 'px';
        selectionIndicator.style.width = selectedArea.width + 'px';
        selectionIndicator.style.height = selectedArea.height + 'px';
    }
}
