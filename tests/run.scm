(import r7rs test sisl)

(define (test-format name expected format)
  (test name expected (format->bencode format)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "input format"
  (test-format "concrete input format"
    #(#("c1" 8 #(42)))
    (make-input-format
      (make-field "c1" 8 #u8(42)))))
