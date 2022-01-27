(define-library (sisl)
  (import (scheme base)
          (scheme file)
          (scheme write)
          (scheme case-lambda)

          (srfi 1)
          (srfi 151)

          (chicken type)
          (bencode))

  ;; Input-Format
  (export make-input-format input-format?)
  ;; Field
  (export field? make-concrete-field make-symbolic-field)
  ;; Integer abstractions
  (export make-uint make-sint)
  ;; Serialization
  (export format->bencode field->bencode write-format)

  (include "lib/util.scm"
           "lib/format.scm"))
