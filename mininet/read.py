#!/usr/bin/python

import routers_pb2
import sys

routers = routers_pb2.Routers()

f = open(sys.argv[1], "rb")
routers.ParseFromString(f.read())
f.close()

print routers
