#lang typed/racket/base
;
; Apollo Art PEx System Control
;

(require racket/match
         typed/racket/class)

(require mordae/syntax
         mordae/match)

(require esc/vp-net)

(provide Power-Status
         Projector-Aspect
         Projector-Format
         Projector%
         projector%)


(define-logger esc-vp21)


(define-type Power-Status
  (U 'offline
     'online
     'warmup
     'cooldown
     'standby
     'abnormal
     'av-standby
     'unknown))

(define-type Projector-Aspect
  (U 'normal
     '4:3
     '16:9
     'auto
     'full
     'zoom
     'native))

(define-type Projector-Format
  (U '4:3 '16:9 '16:10))


(define-type Projector%
  (Class
    (init-field (host String))

    (command (-> String String * String))

    (get-power-status (-> Power-Status))

    (online! (-> Void))
    (offline! (-> Void))

    (set-aspect! (-> Projector-Aspect Void))
    (get-aspect (-> Projector-Aspect))

    (set-format! (-> Projector-Format Void))
    (get-format (-> Projector-Format))

    (set-mute! (-> Boolean Void))
    (set-freeze! (-> Boolean Void))

    (get-mute? (-> Boolean))
    (get-freeze? (-> Boolean))

    (send-key (-> Byte Void))))


(: projector% Projector%)
(define projector%
  (class object%
    (init-field host)

    ;; Semaphore to be locked when a request/response transaction atomicity
    ;; needs to be guaranteed. That is, every time a reply is expected.
    (define lock
      (make-semaphore 1))

    (: in Input-Port)
    (: out Output-Port)
    (define-values (in out)
      (tcp-connect/esc/vp-net host 3629))

    (define/public (command fmt . args)
      (with-semaphore lock
        ;; Send our command.
        (let ((cmd (apply format fmt args)))
          (log-esc-vp21-info "-> ~a" cmd)
          (write-string (string-append cmd "\r\n") out)
          (flush-output out))

        ;; Read our reply.
        (let/ec return : String
          (loop
            (cond
              ((equal? #\: (peek-char in))
               (log-esc-vp21-info "<- ack")
               (read-char in)
               (return ""))

              (else
                (let ((line (read-line in 'any)))
                  (when (eof-object? line)
                    (error 'ESC/VP21 "connection lost"))

                  (read-char in)

                  (cond
                    ((regexp-match? #rx"^IMEVENT=" line)
                     (log-esc-vp21-debug "<- ~a" line))

                    (else
                     (log-esc-vp21-info "<- ~a" line)
                     (return line))))))))))

    (define/public (get-power-status)
      (match (command "PWR?")
        ("PWR=00" 'offline)
        ("PWR=01" 'online)
        ("PWR=02" 'warmup)
        ("PWR=03" 'cooldown)
        ("PWR=04" 'standby)
        ("PWR=05" 'abnormal)
        ("PWR=09" 'av-standby)
        (else 'unknown)))

    (define/public (online!)
      (void (command "PWR ON")))

    (define/public (offline!)
      (void (command "PWR OFF")))

    (define/public (set-aspect! aspect)
      (void (command "ASPECT ~a" (aspect->code aspect))))

    (define/public (get-aspect)
      (match (command "ASPECT?")
        ("ASPECT=00" 'normal)
        ("ASPECT=10" '4:3)
        ("ASPECT=20" '16:9)
        ("ASPECT=30" 'auto)
        ("ASPECT=40" 'full)
        ("ASPECT=50" 'zoom)
        ("ASPECT=60" 'native)
        ("ERR"       'normal)
        (else        'normal)))

    (define/public (set-format! format)
      (void (command "SCFORMAT ~a" (format->code format))))

    (define/public (get-format)
      (match (command "SCFORMAT?")
        ("SCFORMAT=01" '4:3)
        ("SCFORMAT=02" '16:9)
        ("SCFORMAT=03" '16:10)
        ("ERR"         '4:3)))

    (define/public (set-mute! mute?)
      (void (command "MUTE ~a" (if mute? "ON" "OFF"))))

    (define/public (set-freeze! freeze?)
      (void (command "FREEZE ~a" (if freeze? "ON" "OFF"))))

    (define/public (get-mute?)
      (equal? "MUTE=ON" (command "MUTE?")))

    (define/public (get-freeze?)
      (equal? "FREEZE=ON" (command "FREEZE?")))

    (define/public (send-key code)
      (void (command "KEY ~a" (number->string code 16))))

    (super-new)))


(: aspect->code (-> Projector-Aspect (U "00" "10" "20" "30" "40" "50" "60")))
(define (aspect->code aspect)
  (match aspect
    ('normal "00")
    ('4:3 "10")
    ('16:9 "20")
    ('auto "30")
    ('full "40")
    ('zoom "50")
    ('native "60")))

(: format->code (-> Projector-Format (U "01" "02" "03")))
(define (format->code format)
  (match format
    ('4:3 "01")
    ('16:9 "02")
    ('16:10 "03")))

; vim:set ts=2 sw=2 et:
