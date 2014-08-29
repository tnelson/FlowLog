#lang racket/base
;; Generated using protoc-gen-racket v1.1
(require (planet murphy/protobuf:1/syntax))

(define-message-type
 subnet
 ((optional primitive:string addr 1)
  (optional primitive:int32 mask 2)
  (optional primitive:string gw 3)
  (optional primitive:string ifname 6)
  (repeated primitive:int32 physical-portid 7)))
(define-message-type
 port
 ((optional primitive:int32 id 1)
  (optional primitive:string name 2)
  (optional primitive:string vlan-type 3)))
(define-message-type
 connection
 ((optional primitive:string router1 1)
  (optional primitive:string router2 2)
  (optional primitive:string iface1 3)
  (optional primitive:string iface2 4)))
(define-message-type
 network
 ((optional primitive:string addr 1) (optional primitive:int32 mask 2)))
(define-message-type
 peer
 ((optional primitive:string ip 1)
  (optional primitive:int32 mask 2)
  (optional primitive:string mac 3)
  (repeated struct:network networks 4)))
(define-message-type
 router
 ((optional primitive:string name 1)
  (optional primitive:string self-dpid 2)
  (optional primitive:string nat-dpid 3)
  (optional primitive:string tr-dpid 6)
  (optional primitive:string acl-dpid 7)
  (optional primitive:string vlan-dpid 8)
  (optional primitive:int32 num-physical 10)
  (repeated struct:subnet subnets 4)
  (repeated struct:peer peers 5)
  (repeated struct:port ports 9)))
(define-message-type
 routers
 ((repeated struct:router routers 1)
  (optional primitive:string subnet-base-dpid 2)
  (repeated struct:connection connections 3)))

(provide (all-defined-out))
