(import r7rs test sisl)

(define (test-format name expected format)
  (test name expected (format->bencode format)))

(define (test-field name expected field)
  (test name expected (field->bencode field)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "input format"
  (test-format "concrete input format"
    #(#("c1" 16 #(23 42)))
    (make-input-format
      (make-field "c1" 16 #u8(23 42))))

  (test-format "symbolic input format"
    #(#("s1" 8 #("Eq w8 s1 42")))
    (make-input-format
      (make-field "s1" 8 #("Eq w8 s1 42"))))

  (test-format "mixed input format"
    #(#("s1" 8 #("Eq w8 s1 5"))
      #("c1" 16 #(#x23 #x42)))
    (make-input-format
      (make-field "s1" 8 #("Eq w8 s1 5"))
      (make-field "c1" 16 #(#x23 #x42)))))

(test-group "integer abstractions"
  (test-field "make-uint single byte"
    #("c1" 8 #(255))
    (make-uint "c1" 8 #xff))

  (test-field "make-uint two bytes"
    #("bytes" 16 #(#x23 #x42))
    (make-uint "bytes" 16 #x2342))

  (test-field "make-uint pad single byte"
    #("padded" 16 #(#x00 #x23))
    (make-uint "padded" 16 #x23))

  (test-field "make-unit single nibble"
    #("nibble" 4 #(#x0f))
    (make-uint "nibble" 4 #xf))

  (test-error "make-uint value exceeds size"
    (make-uint "byte" 8 (+ #xff 1))))

;; Exit with non-zero exit status on test failure.
(test-exit)
