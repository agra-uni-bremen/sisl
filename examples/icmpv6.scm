#!/usr/bin/csi -s

(import sisl)

;; IPv6 constants
(define ipv6-version-value #x6)
(define ipv6-loopback #u8(#x00 #x00 #x00 #x00
                          #x00 #x00 #x00 #x00
                          #x00 #x00 #x00 #x00
                          #x00 #x00 #x00 #x01))

;; ICMPv6 constants
(define icmpv6-next-header 58)
(define icmpv6-echo-req 128)
(define icmpv6-echo-rep 129)

;; See https://datatracker.ietf.org/doc/html/rfc8200#section-3
(define-input-format (ipv6-packet next-hdr &encapsulate payload)
  (make-uint 'verison-field 4 ipv6-version-value)
  (make-uint 'traffic-class 8 #x0)
  (make-uint 'flow-label 20 #x0)
  (make-uint 'payload-length 16 (input-format-bytsize payload))
  (make-uint 'next-header 8 next-hdr)
  (make-uint 'hop-limit 8 #x42)
  (make-symbolic 'src-addr 128)
  (make-concrete 'dest-addr 128 ipv6-loopback))

;; See https://datatracker.ietf.org/doc/html/rfc4443#section-2.1
(define-input-format (icmpv6-packet &encapsulate body)
  (make-symbolic 'type 8 `((Or
                              (Eq type ,icmpv6-echo-req)
                              (Eq type ,icmpv6-echo-rep))))
  (make-symbolic 'code 8)
  (make-symbolic 'checksum 16))

(write-format
  (ipv6-packet
    icmpv6-next-header
    (icmpv6-packet
      (make-input-format
        (make-symbolic 'body (bytes->bits 8))))))
