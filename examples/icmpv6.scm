#!/usr/bin/csi -s

(import (scheme base) sisl)

(define ipv6-version-value #x6)
(define icmpv6-next-header 58)

(define-input-format ipv6-packet
  (make-uint "verison-field" 4 ipv6-version-value)
  (make-uint "traffic-class" 8 #x0)
  (make-uint "flow-label" 20 #x0)
  (make-uint "payload-length" 16 4) ;; 4 bytes
  (make-uint "next-header" 8 icmpv6-next-header)
  (make-uint "hop-limit" 8 #x42)
  (make-uint "src-addr" 128 #xffffffffffffffffffffffffffffffff)
  (make-uint "dest-addr" 128 #xffffffffffffffffffffffffffffffff)
  (make-uint "payload" 32 #xdeadbeef))

(write-format ipv6-packet)
