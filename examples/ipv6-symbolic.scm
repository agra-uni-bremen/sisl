#!/usr/bin/csi -s

(import sisl)

(define ipv6-version-value #x6)

;; See https://datatracker.ietf.org/doc/html/rfc8200#section-3
(define-input-format (ipv6-packet &encapsulate payload)
  (make-uint 'verison-field 4 ipv6-version-value)
  (make-uint 'traffic-class 8 #x0)
  (make-uint 'flow-label 20 #x0)
  (make-uint 'payload-length 16 (input-format-bytesize payload))
  (make-symbolic 'next-header 8)
  (make-uint 'hop-limit 8 #x42)
  (make-symbolic 'addresses 256))

(write-format
  (ipv6-packet
    (make-input-format
      (make-symbolic 'ipv6-body (bytes->bits 32)))))
