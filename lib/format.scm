;; Type Field represents a bit field for the input format.

(define-record-type Field
  (%make-field name size value padding)
  field?
  ;; string with human-readable name of field.
  (name field-name)
  ;; size of field in bits, *not* bytes.
  (size field-size)
  ;; vector of strings (symbolic field) or vector of bytes (concrete field).
  (value field-value)
  ;; optional padding added to value (i.e. by make-sint or make-uint).
  (padding field-padding))

(: make-field (string fixnum (or (vector-of string) bytevector) #!optional fixnum -> (struct Field)))
(define (make-field name size value #!optional (padding 0))
  (if (> size 0)
    (let ((field (%make-field name size
                     (if (bytevector? value)
                       (bytevector->vector value)
                       value)
                     padding))) ;; vector? => #t
      (if (or
            (field-symbolic? field)
            (eqv? (bits->bytes* size) (*vector-length value)))
        field
        (error "size in bits does not match value vector length")))
    (error "field size must be a positive non-zero fixnum")))

(: field-symbolic? ((struct Field) -> boolean))
(define (field-symbolic? field)
  (let ((v (field-value field)))
    (or
      (*vector-empty? v)
      (string? (vector-ref v 0)))))

(: field-byteswap ((struct Field) -> (struct Field)))
(define (field-byteswap field)
  (unless (field-symbolic? field)
    (let ((value (field-value field))
          (padding (field-padding field)))
      (make-field
        (field-name field)
        (field-size field)
        (vector-append
          (make-vector padding #x00)
          (vector-reverse (vector-copy value padding)))
        padding))))

(define (field-le field) (field-byteswap field))
(define (field-be field) field)

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

;; Macro to define new input format.

(define-syntax define-input-format
  (syntax-rules ()
    ((define-format NAME BODY ...)
     (define NAME
       (make-input-format BODY ...)))
    ((define-format (ARGS ...) BODY ...)
     (define (ARGS ...)
       (make-input-format BODY ...)))))

;; Create a new concrete field.

(: make-concrete-field (string fixnum bytevector -> (struct Field)))
(define (make-concrete-field name numbits bv)
  (make-field
    name
    numbits
    bv))

;; Create a new symbolic field with optional constraints.

(: make-symbolic-field (string fixnum #!optional (list-of (list-of symbol)) -> (struct Field)))
(define (make-symbolic-field name numbits #!optional (constraints '()))
  (define (constraint->string constraint)
    (call-with-port
      (open-output-string)
      (lambda (port)
        (write constraint port)
        (get-output-string port))))
  (define (constraints->list constraints)
    (map constraint->string constraints))

  (make-field
    name
    numbits
    (list->vector (constraints->list constraints))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Procedure number->bytevector returns a bytevector representing the
;; given value with optional padding to the next byte boundary according
;; to the given amount of bits.

;(: number->bytevector (fixnum fixnum -> fixnum bytevector))
(define (number->bytevector numbits value)
  (let* ((numbytes (bits->bytes* numbits))
         (ret (byte-fold (lambda (byte vec)
                          (bytevector-append vec (bytevector byte)))
                        #u8() value))
         (pad (- numbytes (bytevector-length ret))))
    (values
      pad
      (if (zero? pad)
        ret
        (bytevector-append
          (make-bytevector pad #x00)
          ret)))))

;; Procedure make-uint creates a fixed-size unsigned integer field with
;; a boundary check. The size of the field is given in bits. If the
;; given value exceeds the maximum value representable in the given
;; amount of bits, an error is raised.

(: make-uint (string fixnum fixnum -> (struct Field)))
(define (make-uint name numbits value)
  (if (> value (dec (expt 2 numbits)))
    (error "value not representable in given amount of bits")
    (let-values (((pad bv) (number->bytevector numbits value)))
      (make-field name numbits bv pad))))

(define (u8  name value) (make-uint name 8  value))
(define (u16 name value) (make-uint name 16 value))
(define (u32 name value) (make-uint name 32 value))
(define (u64 name value) (make-uint name 64 value))

;; Procedure make-sint creates a fixed-size signed integer field with a
;; boundary check. Signed integers are represented in two's complement.
;; The size of the two's complement field is given in bits. If the given
;; value exceeds the maximum value representable in the given amount of
;; bits, an error is raised.

(: make-sint (string fixnum fixnum -> (struct Field)))
(define (make-sint name numbits value)
  (let ((max (/ (expt 2 numbits) 2)))
    (if (or (>= value max)
            (< value (* -1 max)))
      (error "value not representable in given amount of bits")
      (let*-values (((v) (if (negative? value)
                           (- value (arithmetic-shift 1 (- numbits)))
                           value))
                    ((pad bv) (number->bytevector numbits v)))
        (make-field name numbits bv pad)))))

(define (s8  name value) (make-sint name 8  value))
(define (s16 name value) (make-sint name 16 value))
(define (s32 name value) (make-sint name 32 value))
(define (s64 name value) (make-sint name 64 value))

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

;; Write the representation of format to a textual output port.

(define write-format
  (case-lambda
    ((format port)
     (write-string
       (bencode->string (format->bencode format))
       port))
    ((format) (write-format format (current-output-port)))))
