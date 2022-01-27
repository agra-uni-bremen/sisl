(define (inc n) (+ n 1))
(define (dec n) (- n 1))

;; Type alias for R7RS bytevectors (somehow not defined by the R7RS egg).
(define-type bytevector u8vector)

;; Like display but prints multiple objects and adds trailing newline.

(define (fprintln port . objs)
  (for-each (lambda (obj) (display obj port)) objs)
  (newline port))

(define (println . objs)
  (apply fprintln (current-output-port) objs))

;; Convert R7RS bytevector to list.

(: bytevector->list (bytevector -> list))
(define (bytevector->list bv)
  (map (lambda (n) (bytevector-u8-ref bv n))
       (iota (bytevector-length bv))))

;; Convert R7RS bytevector to vector.

(: bytevector->vector (bytevector -> vector))
(define (bytevector->vector bv)
  (list->vector (bytevector->list bv)))

;; Returns the length of a given vector or bytevector.

(: *vector-length ((or vector bytevector) -> fixnum))
(define (*vector-length v)
  (if (bytevector? v)
    (bytevector-length v)
    (vector-length v)))

;; Returns true if the given vector or bytevector is empty.

(: *vector-empty ((or vector bytevector) -> boolean))
(define (*vector-empty? v)
  (zero? (*vector-length v)))

;; Convert amount of bytes to amount of bits.

(: bytes->bits (fixnum -> fixnum))
(define (bytes->bits bytes)
  (* bytes 8))

;; Convert amount of bits to amount of bytes.
;; Errors out if amount of bits is not byte-alinged.

(: bits->bytes (fixnum -> fixnum))
(define (bits->bytes bits)
  (if (not (zero? (modulo bits 8)))
    (error "bits value not byte-aligned")
    (/ bits 8)))

;; Like bits->bytes but rounds up if not byte-aligned.

(: bits->bytes* (fixnum -> fixnum))
(define (bits->bytes* bits)
  (let ((rem (modulo bits 8)))
    (if (zero? rem)
      (bits->bytes bits)
      (/ (+ (- bits rem) 8) 8))))

;; Get the nth byte in the given number.

(: get-byte (fixnum fixnum -> fixnum))
(define (get-byte value nth)
  (let ((mask  (- (expt 2 8) 1))
        (shamt (* -1 (* nth 8))))
    (bitwise-and (arithmetic-shift value shamt) mask)))

;; Return the amount of bytes needed to represent number.

(: byte-length (fixnum -> fixnum))
(define (byte-length number)
  (let* ((bit-length (integer-length number))
         (rem (modulo bit-length 8)))
    (/
      (if (zero? rem)
        bit-length
        (+ (- bit-length rem) 8)) ;; Round to next byte boundary
      8)))

;; Fold over individual bytes of given fixnum.

(: byte-fold ((fixnum * -> *) * fixnum -> *))
(define (byte-fold proc seed number)
  (define (recur n)
    (if (>= n (byte-length number))
      seed
      (proc (get-byte number n)
            (recur (+ n 1)))))

  (if (zero? number)
    (proc 0 seed)
    (recur 0)))
