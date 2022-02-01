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
  (export define-input-format make-input-format input-format?
          input-format-bitsize input-format-bytsize)
  ;; Field
  (export make-concrete make-symbolic field?
          field-bitsize field-bytesize field-le field-be)
  ;; Integer abstractions
  (export make-uint u8 u16 u32 u64 make-sint s8 s16 s32 s64)
  ;; Serialization
  (export format->bencode field->bencode write-format)
  ;; Utility procedures.
  (export bits->bytes bytes->bits)

  (include "lib/util.scm"
           "lib/format.scm"))
