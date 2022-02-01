(import r7rs test sisl)

(define (test-format name expected format)
  (test name expected (format->bencode format)))

(define (test-field name expected field)
  (test name expected (field->bencode field)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "make-uint"
  (test-field "single byte"
    #("c1" 8 #(255))
    (make-uint 'c1 8 #xff))

  (test-field "two bytes"
    #("bytes" 16 #(#x23 #x42))
    (make-uint 'bytes 16 #x2342))

  (test-field "pad single byte"
    #("padded" 16 #(#x00 #x23))
    (make-uint 'padded 16 #x23))

  (test-field "single nibble"
    #("nibble" 4 #(#x0f))
    (make-uint 'nibble 4 #xf))

  (test-field "big endian integer"
    #("be" 16 #(#x23 #x42))
    (field-be (make-uint 'be 16 #x2342)))

  (test-field "little endian integer"
    #("le" 16 #(#x42 #x23))
    (field-le (make-uint 'le 16 #x2342)))

  (test-field "little endian with padding"
    #("le" 32 #(#x00 #x00 #x42 #x23))
    (field-le (make-uint 'le 32 #x2342)))

  (test-error "value exceeds size"
    (make-uint 'byte 8 (+ #xff 1))))

(test-group "make-sint"
  (test-field "single positive byte"
    #("c1" 8 #(127))
    (make-sint 'c1 8 127))

  (test-field "single negative byte"
    #("negative" 8 #(#b10000000))
    (make-sint 'negative 8 -128))

  (test-field "pad positive"
    #("padded" 16 #(#x00 23))
    (make-sint 'padded 16 23))

  (test-field "pad negative"
    #("padded" 16 #(#x00 #b11111110))
    (make-sint 'padded 16 -2))

  (test-error "negative value exceeds size"
    (make-sint 'error 8 -129))

  (test-error "positive value exceeds size"
    (make-sint 'error 8 128)))

(test-group "input format"
  (test-format "concrete input format"
    #(#("c1" 16 #(23 42)))
    (make-input-format
      (make-concrete 'c1 16 #u8(23 42))))

  (test-format "symbolic input format"
    #(#("s1" 8 #("(Eq w8 s1 42)")))
    (make-input-format
      (make-symbolic 's1 8 `((Eq w8 s1 42)))))

  (test-format "unconstrained symbolic input"
    #(#("symbolic" 16 #()))
    (make-input-format
      (make-symbolic 'symbolic 16)))

  (test-format "mixed input format"
    #(#("s1" 8 #("(Eq w8 s1 5)"))
      #("c1" 16 #(#x23 #x42)))
    (make-input-format
      (make-symbolic 's1 8 `((Eq w8 s1 5)))
      (make-concrete 'c1 16 #(#x23 #x42))))

  (test-format "byteorder"
    #(#("c1" 16 #(#x42 #x23))
      #("c2" 4  #(#xf))
      #("c3" 24 #(#x42 #xff #x23))
      #("c4" 16 #(#x23 #x42))
      #("c5" 32 #(#x00 #x00 #x42 #x23)))
    (make-input-format
      (field-le (make-concrete 'c1 16 #(#x23 #x42)))
      (field-be (make-concrete 'c2 4 #(#xf)))
      (field-le (make-uint 'c3 24 #x23ff42))
      (field-be (make-uint 'c4 16 #x2342))
      (field-le (make-uint 'c5 32 #x2342)))))

;; Exit with non-zero exit status on test failure.
(test-exit)
