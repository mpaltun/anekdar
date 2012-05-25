function Anekdar(server, port) {
	this.server = server;
	this.port = port;
	this.parser = new Anekdar_message_parser();
	this.callbacks = {};
	self = this;
	this.connect = function(callback) {
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
			parser = this.parser;
			ws.onmessage = function (evt) {
				data = split(evt.data, parser.delimiter, 3);
				command = data[0];
				channel = data[1];
				message = data[2];
				switch(command) {
					case parser.command.publish:
						self.callbacks[channel](message);
					break;
					case parser.command.subscribe:
						// to be implemented
					break;
					case parser.command.count:
						// to be implemented
					break;
					default:
						console.error('unknown message type: ' + command);
					break;
				}
			};

			ws.onclose = function() {
				// websocket was closed
				console.info("websocket connection closed");
			};
			self.ws = ws;
		}
		else {
			// websocket not supported
			// TODO: long polling stuff will be implemented here
			console.info("websocket is not supported");
		}
	};
	this.subscribe = function(channel, callback) {
		self.callbacks[channel] = callback;
		self.ws.send("sub " + channel);
	};
	this.publish = function(channel, message) {
		self.ws.send("pub " + channel + " " + JSON.stringify(message));
	}
}

function Anekdar_message_parser() {
	this.command = {};
	this.command.publish = 'pub';
	this.command.subscribe = 'sub';
	this.command.count = 'count';
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
