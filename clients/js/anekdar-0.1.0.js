function Anekdar(server, port) {
	this.server = server;
	this.port = port;
	this.protocol = new Anekdar_protocol();
	this.callbacks = {
		publish : [],
		subscribe: []
	};
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
				callback();
			};
			self = this;
			ws.onmessage = function (evt) {
				msg = evt.data;
				type = msg.charAt(0);
				rest = msg.substring(1);
				switch(type)
				{
					case self.protocol.reply.success_int:
						// publish response
						// pass count as param
						c = self.callbacks.publish.shift();
						c(rest);
						break;
					case self.protocol.reply.success_str:
						// subscribe
						data = split(rest, self.protocol.delimiter, 2);
						channel = data[0];
						message = data[1];
						self.callbacks.subscribe[channel](message);
						break;
					case self.protocol.reply.error:
						console.error("error occurred: " + rest)
						break;
					default:
						console.error('unknown message type: ' + type);
					break;
				}
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
	subscribe : function(channel, callback) {
		this.callbacks.subscribe[channel] = callback;
		this.ws.send(this.protocol.command.subscribe + this.protocol.delimiter + channel);
	},
	publish : function(channel, message, callback) {
		this.callbacks.publish.push(callback);
		this.ws.send(this.protocol.command.publish + this.protocol.delimiter + channel + this.protocol.delimiter + message);
	},
	unsubscribe : function (channel) {
		this.ws.send(this.protocol.command.unsubscribe + this.protocol.delimiter + channel);
	},
	disconnect : function () {
		this.ws.send(this.protocol.command.quit);
	}
};

function Anekdar_protocol() {
	this.command = {};
	this.command.publish = 'pub';
	this.command.subscribe = 'sub';
	this.command.unsubscribe = 'unsub';
	this.command.ping = 'ping';
	this.command.quit = 'quit';
	this.reply = {};
	this.reply.success_str = '+';
	this.reply.success_int = ':';
	this.reply.error = '-';
	this.delimiter = ' ';
}

function split(string, delimiter, n_parts)
{
	parts = string.split(delimiter, n_parts - 1);
	var index = 0;
	for (i in parts) {
		index += (parts[i].length + 1)
	}
	parts.push(string.substring(index));
	return parts;
}
