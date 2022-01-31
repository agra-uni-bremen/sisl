#!/usr/bin/csi -s

(import (scheme base) sisl)

(define ipv6-version-value #x6)
(define icmpv6-next-header 58)

(define-input-format (ipv6-packet next-hdr payload-field)
  (make-uint "verison-field" 4 ipv6-version-value)
  (make-uint "traffic-class" 8 #x0)
  (make-uint "flow-label" 20 #x0)
  (make-uint "payload-length" 16 (field-bytesize payload-field))
  (make-uint "next-header" 8 next-hdr)
  (make-uint "hop-limit" 8 #x42)
  (make-uint "src-addr" 128 #xffffffffffffffffffffffffffffffff)
  (make-uint "dest-addr" 128 #xffffffffffffffffffffffffffffffff)
  payload-field)

(write-format
  (ipv6-packet
    icmpv6-next-header
    (make-uint "payload" 32 #xdeadbeef)))
