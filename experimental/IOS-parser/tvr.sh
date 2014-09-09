# Just 2 routers
# ./ios2flowlog --path tests/vr1/ configA.txt configC.txt



# (almost) triangle; includes 4 subnets
 #./ios2flowlog --path tests/vr1/ configA.txt configC.txt configD.txt

./ios2flowlog --path tests/vr1/ --conn conns.txt configA.txt configB.txt configC.txt configD.txt int.txt extracl.txt
