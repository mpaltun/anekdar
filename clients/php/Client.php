<?php

/*
 * This file is part of the Anekdar project.
 *
 * (c) Osman Ungur <osmanungur@gmail.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Anekdar;

class ClientException extends \RuntimeException {
    
}

class Client {
    
    private $socket;
    private $host;
    private $port;

    const EOL = "\r\n";
    const TIMEOUT = 5;
    const TCP_PREFIX = 'tcp://';
    const SEPERATOR = ':';
    const REPLY_STATUS_SUCCESS = '+';
    const REPLY_STATUS_INTEGER = ':';
    const REPLY_STATUS_ERROR = '-';
    const REPLY_OK = 'OK';
    
    const COMMAND_PUB = 'pub';
    const COMMAND_SUB = 'sub';
    const COMMAND_PING = 'ping';
    const COMMAND_QUIT = 'quit';

    public function __construct($host = '127.0.0.1', $port = 9998) 
    {
        $this->host = $host;
        $this->port = $port;
    }
    
    public function connect() 
    {
        $connection = stream_socket_client(self::TCP_PREFIX . $this->host . self::SEPERATOR . $this->port, $errno, $errstr, self::TIMEOUT, STREAM_CLIENT_CONNECT);
        if ($connection)
            $this->socket = $connection;
            return true;
            
        throw new ClientException(sprintf('Cant connect to Anekdar server %s:%s Error : %s, %s', $this->host, $this->port, $errno, $errstr), 1);
    }
    
    public function disconnect()
    {
        return fclose($this->socket);
    }
    
    private function write($command) 
    {
        $write = fwrite($this->socket, $command . self::EOL);
        if ($write === false)
            throw new ClientException('No bytes have been written', 1);
        
        return true;
    }

    private function read() 
    {
        $line = fgets($this->socket);
        if ($line === false || $line === '')
            throw new ClientException('Cant read line from socket.', 1);
        
        return $line;
    }
    
    public function parse($line) 
    {
        $kind = $line[0];
        $data = substr($line, 1, -2);
        switch ($kind) {
            case self::REPLY_STATUS_SUCCESS:
                return $data;
                break;

            case self::REPLY_STATUS_INTEGER:
                return (int) $data;
                break;

            case self::REPLY_STATUS_ERROR:
                throw new ClientException(sprintf('An error occured: %s', $data));
                break;

            default:
                throw new ClientException(sprintf('Unexpected message from server: %s', $data));
                break;
        }
    }
    
    public function execute($command)
    {
        $this->write($command);
        return $this->parse($this->read());
    }

    public function getConnectionInfo() 
    {
        return stream_get_meta_data($this->socket);
    }
    
    public function pub($channel, $payload) 
    {
        return $this->execute(sprintf('%s %s %s', self::COMMAND_PUB, $channel, $payload));
    }

    public function sub($channel, $payload) 
    {
        return explode(chr(32), $this->execute(sprintf('%s %s', self::COMMAND_SUB, $channel)), 2);
    }

    public function ping() 
    {
        return $this->execute(self::COMMAND_PING);
    }
    
    public function quit() 
    {
        return $this->execute(self::COMMAND_QUIT);
    }

    public function __destruct() 
    {
        if (is_resource($this->socket)) fclose($this->socket);
    }
    
}