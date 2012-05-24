function Anekdar(server, port) {
	this.server = server;
	this.port = port;
}

Anekdar.prototype = {
	connect : function(callback) {
		if ("MozWebSocket" in window) {
			WebSocket = MozWebSocket;
		}
		if ("WebSocket" in window) {
			// browser supports websockets
			ws = new WebSocket("ws://" + this.server + ":" + this.port + "/ws");
			ws.onopen = function() {
				// websocket is connected
				console.info("websocket connection established");
			};

			ws.onmessage = function (evt) {
				callback(evt.data);
			};

			ws.onclose = function() {
				// websocket was closed
				console.info("websocket connection closed");
			};
			this.ws = ws;
		}
		else {
			// websocket not supported
			// TODO: long polling stuff will be implemented here
			console.info("websocket is not supported");
		}
	},
	subscribe : function(Channel) {
		this.ws.send("sub::" + Channel);
	},
	publish : function(Channel, Message) {
		this.ws.send("pub::" + Channel + "::" + JSON.stringify(Message));
	}
}
