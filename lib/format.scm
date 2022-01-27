;; Type Field represents a bit field for the input format.

(define-record-type Field
  (%make-field name size value)
  field?
  ;; string with human-readable name of field.
  (name field-name)
  ;; size of field in bits, *not* bytes.
  (size field-size)
  ;; vector of strings (symbolic field) or vector of bytes (concrete field).
  (value field-value))

(: make-field (string fixnum (or (vector-of string) bytevector) -> (struct Field)))
(define (make-field name size value)
  ;; TODO: Can be empty for unconstrained fields.
  ;; XXX: Also needs to be fixed in SymEx-VP.
  (if (*vector-empty? value)
    (error "field value cannot be empty")
    (if (eqv? (bits->bytes* size) (*vector-length value))
      (%make-field name size
                   (if (bytevector? value)
                     (bytevector->vector value)
                     value)) ;; vector? => #t
      (error "size in bits does not match value vector length"))))

(: field-symbolic? ((struct Field) -> boolean))
(define (field-symbolic? field)
  (string? (vector-ref (field-value field) 0)))

;; Type Input-Format represents the specification of an input format
;; consisting of different bit fields. The argument fields is a list of
;; Field types as described above.

(define-record-type Input-Format
  (%make-input-format fields)
  input-format?
  (fields input-format-fields))

(: make-input-format ((list-of (struct Field)) -> (struct Input-Format)))
(define (make-input-format . body)
  (%make-input-format (list->vector body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Procedure number->bytevector returns a bytevector representing the
;; given value with optional padding to the given amount of bits.

(: number->bytevector (fixnum fixnum -> bytevector))
(define (number->bytevector numbits value)
  (let ((numbytes (bits->bytes* numbits))
        (ret (byte-fold (lambda (byte vec)
                          (bytevector-append vec (bytevector byte)))
                        #u8() value)))
    (if (< (bytevector-length ret) numbytes)
      (bytevector-append
        (make-bytevector (- numbytes (bytevector-length ret)) #x00)
        ret)
      ret)))

;; Procedure make-uint creates a fixed-size unsigned integer field with
;; a boundary check. The size of the field is given in bits. If the
;; given value exceeds the maximum value representable in the given
;; amount of bits, an error is raised.

(: make-uint (string fixnum fixnum -> (struct Field)))
(define (make-uint name numbits value)
  (if (> value (dec (expt 2 numbits)))
    (error "value not representable in given amount of bits")
    (make-field
      name
      numbits
      (number->bytevector numbits value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Field-Vector (vector string fixnum (or (vector-of string) (vector-of fixnum))))

(: field->bencode ((struct Field) -> Field-Vector))
(define (field->bencode field)
  (vector
    (field-name field)
    (field-size field)
    (field-value field)))

(define-type Format-Vector (vector-of Field-Vector))

(: format->bencode ((struct Input-Format) -> Format-Vector))
(define (format->bencode format)
  (vector-map field->bencode (input-format-fields format)))
