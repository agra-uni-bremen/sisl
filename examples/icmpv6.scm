#!/usr/bin/csi -s

(import sisl)

(define ipv6-version-value #x6)
(define ipv6-loopback #u8(#x00 #x00 #x00 #x00
                          #x00 #x00 #x00 #x00
                          #x00 #x00 #x00 #x00
                          #x00 #x00 #x00 #x01))

(define icmpv6-next-header 58)

(define-input-format (ipv6-packet next-hdr &encapsulate payload)
  (make-uint "verison-field" 4 ipv6-version-value)
  (make-uint "traffic-class" 8 #x0)
  (make-uint "flow-label" 20 #x0)
  (make-uint "payload-length" 16 (input-format-bytsize payload))
  (make-uint "next-header" 8 next-hdr)
  (make-uint "hop-limit" 8 #x42)
  (make-symbolic "src-addr" 128)
  (make-concrete "dest-addr" 128 ipv6-loopback))

(define-input-format icmpv6-packet
  (make-uint "payload" 32 #xdeadbeef))

(write-format
  (ipv6-packet
    icmpv6-next-header
    icmpv6-packet))
