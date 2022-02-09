#!/usr/bin/csi -s

(import sisl)

(define input-byte-size 64)

(write-format
  (make-input-format
    (make-symbolic 'input (bytes->bits input-byte-size))))
