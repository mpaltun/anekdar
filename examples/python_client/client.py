#!/usr/bin/python
#-*-coding: utf-8-*-
import os,sys
from time import time

client_path = os.path.abspath('../../clients/py')
sys.path.append(client_path)
from anekdar import anekdar

now = time()
for i in range(100):
    a = anekdar("localhost", 9998)
    print str(i) + " " + a.pub('a', str(i))
    a.disconnect()

print "100 message published in " + str(time() - now) + ' seconds'