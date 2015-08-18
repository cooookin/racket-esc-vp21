#lang typed/racket/base
;
; ESC/VP.net Stripping
;

(require racket/tcp)

(provide tcp-connect/esc/vp-net)


(: tcp-connect/esc/vp-net
  (-> String Positive-Integer (values Input-Port Output-Port)))
(define (tcp-connect/esc/vp-net hostname port-no)
  (define-values (in out)
    (tcp-connect hostname port-no))

  (write-bytes #"ESC/VP.net\x10\x03\x00\x00\x00\x00" out)
  (flush-output out)

  (unless (equal? (read-bytes 16 in)
                  #"ESC/VP.net\x10\x03\x00\x00\x20\x00")
    (error 'tcp-connect/escvp-net "failed to negotiate connection"))

  (values in out))


; vim:set ts=2 sw=2 et:
