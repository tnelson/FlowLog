#!/bin/bash
echo 'Measuring' $1
echo 'rtr:' `sudo ovs-ofctl dump-flows $1-rtr | grep "cookie" | wc -l`
echo 'acl:' `sudo ovs-ofctl dump-flows $1-acl | grep "cookie" | wc -l`
echo 'tr:' `sudo ovs-ofctl dump-flows $1-tr | grep "cookie" | wc -l`
echo 'nat:' `sudo ovs-ofctl dump-flows $1-nat | grep "cookie" | wc -l`

