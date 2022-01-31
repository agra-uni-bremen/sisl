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
      (make-concrete-field "c1" 16 #u8(23 42))))

  (test-format "symbolic input format"
    #(#("s1" 8 #("(Eq w8 s1 42)")))
    (make-input-format
      (make-symbolic-field "s1" 8 `((Eq w8 s1 42)))))

  (test-format "unconstrained symbolic input"
    #(#("symbolic" 16 #()))
    (make-input-format
      (make-symbolic-field "symbolic" 16)))

  (test-format "mixed input format"
    #(#("s1" 8 #("(Eq w8 s1 5)"))
      #("c1" 16 #(#x23 #x42)))
    (make-input-format
      (make-symbolic-field "s1" 8 `((Eq w8 s1 5)))
      (make-concrete-field "c1" 16 #(#x23 #x42)))))

(test-group "make-uint"
  (test-field "single byte"
    #("c1" 8 #(255))
    (make-uint "c1" 8 #xff))

  (test-field "two bytes"
    #("bytes" 16 #(#x23 #x42))
    (make-uint "bytes" 16 #x2342))

  (test-field "pad single byte"
    #("padded" 16 #(#x00 #x23))
    (make-uint "padded" 16 #x23))

  (test-field "single nibble"
    #("nibble" 4 #(#x0f))
    (make-uint "nibble" 4 #xf))

  (test-error "value exceeds size"
    (make-uint "byte" 8 (+ #xff 1))))

(test-group "make-sint"
  (test-field "single positive byte"
    #("c1" 8 #(127))
    (make-sint "c1" 8 127))

  (test-field "single negative byte"
    #("negative" 8 #(#b10000000))
    (make-sint "negative" 8 -128))

  (test-field "pad positive"
    #("padded" 16 #(#x00 23))
    (make-sint "padded" 16 23))

  (test-field "pad negative"
    #("padded" 16 #(#x00 #b11111110))
    (make-sint "padded" 16 -2))

  (test-error "negative value exceeds size"
    (make-sint "error" 8 -129))

  (test-error "positive value exceeds size"
    (make-sint "error" 8 128)))

;; Exit with non-zero exit status on test failure.
(test-exit)
