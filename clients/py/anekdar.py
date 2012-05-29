#!/usr/bin/env python
# -*- coding: utf-8 -*-

import socket

class anekdar:
    def __init__(self, host, port):
        print "trying to connect anekdar server"
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((host, port))
        print "connected"
        self.socket = s
        self.BUFFER_SIZE = 1024
        self.CRLF="\r\n"
        self.DELIMITER = " "

    def sub(self, channel):
        #data = self.socket.recv(BUFFER_SIZE)
        # to be implemented
        pass
    def pub(self, Channel, Message):
        self.socket.send("pub" + self.DELIMITER + Channel + self.DELIMITER + Message + self.CRLF)

    def disconnect(self):
        self.socket.close()
