all:
	raco exe ios2flowlog.ss

protobuf:
	protoc --proto_path=../../mininet/ --plugin=`which protoc-gen-racket` --racket_out=. ../../mininet/routers.proto
	protoc --proto_path=../../mininet/ --python_out=../../mininet/ ../../mininet/routers.proto
