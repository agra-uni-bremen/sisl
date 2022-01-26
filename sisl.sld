(define-library (sisl)
  (import (scheme base)
          (scheme file)
          (scheme write)

          (srfi 1)

          (chicken type))

  (export make-input-format make-field)
  (export format->bencode)

  (include "lib/util.scm"
           "lib/format.scm"))
