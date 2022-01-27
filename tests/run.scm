(import r7rs test sisl)

(define (test-format name expected format)
  (test name expected (format->bencode format)))

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

;; Exit with non-zero exit status on test failure.
(test-exit)
