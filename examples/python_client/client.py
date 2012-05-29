#!/usr/bin/python
#-*-coding: utf-8-*-
import os,sys
from time import time

client_path = os.path.abspath('../../clients/py')
sys.path.append(client_path)
from anekdar import anekdar

anekdar = anekdar("localhost", 9998)
now = time()
for i in range(1000000):
	anekdar.pub('a', str(i));

print "1000000 message published in " + str(time() - now) + ' seconds'
anekdar.disconnect()