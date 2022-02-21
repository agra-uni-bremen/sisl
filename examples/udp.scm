#!/usr/bin/csi -s

(import sisl)

(define ipv6-version-value #x6)
(define udp-next-header 17)

;; See https://datatracker.ietf.org/doc/html/rfc8200#section-3
(define-input-format (ipv6-packet next-hdr &encapsulate payload)
  (make-uint 'verison-field 4 ipv6-version-value)
  (make-uint 'traffic-class 8 #x0)
  (make-uint 'flow-label 20 #x0)
  (make-uint 'payload-length 16 (input-format-bytesize payload))
  (make-uint 'next-header 8 next-hdr)
  (make-uint 'hop-limit 8 #x42)
  (make-symbolic 'src-addr 128)
  (make-symbolic 'dst-addr 128))

;; See https://datatracker.ietf.org/doc/html/rfc768
(define-input-format (udp-datagram &encapsulate payload)
  (make-uint 'src-port 16 2342)
  (make-symbolic 'dst-port 16)
  (make-uint 'length 16 (+ 8 (input-format-bytesize payload)))
  (make-symbolic 'checksum 16))

(write-format
  (ipv6-packet
    udp-next-header
    (udp-datagram
      (make-input-format
        (make-symbolic 'udp-payload (bytes->bits 32))))))
