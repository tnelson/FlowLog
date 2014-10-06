#!/bin/bash

# To run controller listening on port 9999
#PYTHONPATH=. ../../pox/pox.py log.level --DEBUG openflow.of_01 --port=9999 seen

PYTHONPATH=. ../../pox/pox.py log.level --DEBUG seen
