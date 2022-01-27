(define-library (sisl)
  (import (scheme base)
          (scheme file)
          (scheme write)

          (srfi 1)
          (srfi 151)

          (chicken type))

  ;; Input-Format
  (export make-input-format input-format?)
  ;; Field
  (export make-field field?)
  ;; Integer abstractions
  (export make-uint)
  ;; Serialization
  (export format->bencode field->bencode)

  (include "lib/util.scm"
           "lib/format.scm"))
