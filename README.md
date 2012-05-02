### What is anekdar

Anekdar is an experimental REST based pub/sub server written in Erlang. 

### Installing and running

First of all you need to install Erlang to your box. 

On Ubuntu with ```apt-get```

    apt-get install erlang

On Mac OS X with ```brew```

    brew install erlang

After that

Clone anekdar from git

    git://github.com/rtgvd/anekdar.git
    cd anekdar

Get dependencies and compile

    make
    
And run!
    
    make start

### Using

It's accessible from ```localhost``` and uses ```9999``` port.

To subscribe to a channel

    GET     /sub/{channel}

Example

    curl http://localhost:9999/sub/channel_1/

Publishing a message to channel

    POST    /pub/{channel} [message_body]

Example

    curl -H "Content-Type: application/json" -X POST -d '{"realtime":["publish", "subscribe"], "server" : "anekdar"}' http://localhost:9999/pub/channel_1/
    
Thats it!

### License

Anekdar is a open-source project and licenced with MIT. Feel free to fork and contribute.
