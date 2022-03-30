#!/usr/bin/csi -s

(import sisl)

;; IPv6 constants
(define ipv6-version-value #x6)
(define ipv6-multicast-mdns #u8(#xff #x02 #x00 #x00
                                #x00 #x00 #x00 #x00
                                #x00 #x00 #x00 #x00
                                #x00 #x00 #x00 #xfb)) ;; ff02::fb

;; UDP constants
(define udp-next-header 17)
(define udp-port 5353)

;; See https://datatracker.ietf.org/doc/html/rfc8200#section-3
(define-input-format (ipv6-packet next-hdr &encapsulate payload)
  (make-uint 'verison-field 4 ipv6-version-value)
  (make-uint 'traffic-class 8 #x0)
  (make-uint 'flow-label 20 #x0)
  (make-uint 'payload-length 16 (input-format-bytesize payload))
  (make-uint 'next-header 8 next-hdr)
  (make-uint 'hop-limit 8 #x42)
  (make-symbolic 'src-addr 128)
  (make-concrete 'dst-addr 128 ipv6-multicast-mdns))

;; See https://datatracker.ietf.org/doc/html/rfc768
(define-input-format (udp-datagram &encapsulate payload)
  (make-symbolic 'src-port 16)
  (make-uint 'dst-port 16 udp-port)
  (make-uint 'length 16 (+ 8 (input-format-bytesize payload)))
  (make-symbolic 'checksum 16))

;; See https://datatracker.ietf.org/doc/html/rfc1035#section-4.1.1
(define-input-format (mdns-packet &encapsulate queries)
  (make-uint 'dns-id 16 #x2342)
  (make-uint 'dns-qr-and-opcode 5 0)
  (make-symbolic 'dns-flags-remaining 11)
  (make-symbolic 'dns-counts (* 4 16)))

(write-format
  (ipv6-packet
    udp-next-header
    (udp-datagram
      (mdns-packet
        (make-input-format
          (make-symbolic 'dns-queries (bytes->bits 32)))))))
