#!/bin/bash
PYTHONPATH=. ../../pox/pox.py log.level --DEBUG openflow.of_01 --port=9999 seen
