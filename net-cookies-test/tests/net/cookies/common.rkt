#lang racket

(require net/cookies/common
         rackcheck
         rackunit)

(module+ main
  (require rackunit/text-ui)
  (run-tests cookie-name-tests)
  (run-tests cookie-name-prop-tests)
  (run-tests cookie-value-tests)
  (run-tests cookie-value-prop-tests)
  (run-tests p/e-value-tests))

(define-syntax test-cookie-pred
  (syntax-rules (valid invalid)
    [(_ label ok? bytes-too? (valid v ...) (invalid inv ...))
     (test-begin
      (test-case (string-append "Valid " label)
                 (check-true (ok? v) v)
                 ...
                 (when bytes-too?
                   (check-true (ok? (string->bytes/utf-8 v)) v)
                   ...))
      (test-case (string-append "Invalid " label)
                 (check-false (ok? inv) inv)
                 ...
                 (when bytes-too?
                   (check-false (ok? (string->bytes/utf-8 inv)) inv)
                   ...)))]))

(define-test-suite cookie-name-tests
  (test-cookie-pred "cookie names" cookie-name? #t
    (valid "HI" "hi" "Hi" "modestlyLongCookieName"
           "somewhatTremendouslyOverlongCookieNameThatTakesAWhileToType")
    (invalid "\0" "hello\n" "\t" "\u7F" "(ugh)" "\"" "\"argh\"" "<tags>" "foo@bar"
             ",,,,,chameleon" "this;that" "this:that" "[bracketed]" "{braced}"
             "slashed/" "back\\slashed" "what?" "x=y" "spaced out" "\ttabbed")))

(define gen:token
  (apply
   gen:choice
   (for*/list ([code (in-range 32 127)] ;; excluding CTLs
               [char (in-value (integer->char code))]
               #:unless (memv char '(#\( #\) #\< #\> #\@
                                     #\, #\; #\: #\\ #\"
                                     #\/ #\[ #\] #\? #\=
                                     #\{ #\} #\space #\tab)))
     (gen:const char))))

(define gen:cookie-name
  (gen:let ([t0 gen:token]
            [ts (gen:list gen:token)])
    (apply string t0 ts)))

(define-test-suite cookie-name-prop-tests
  (check-property
   (property ([name gen:cookie-name])
     (check-true (cookie-name? name)))))

(define-test-suite cookie-value-tests
  (test-cookie-pred "cookie values" cookie-value? #t
    (valid "value" "(" "!" ")" ")!" "(!" "(!)" "!)" "\"hey!\"" "a=b=c" "`a")
    (invalid "a;b" "a,b" "a b" "a\tb" "a=\"foo\"")))

;; cookie-octet      = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
;;                       ; US-ASCII characters excluding CTLs,
;;                       ; whitespace DQUOTE, comma, semicolon,
;;                       ; and backslash
(define gen:cookie-octet
  (apply
   gen:choice
   (map
    (compose1 gen:const integer->char)
    (append '(#x21)
            (range #x23 (add1 #x2B))
            (range #x2D (add1 #x3A))
            (range #x3C (add1 #x5B))
            (range #x5D (add1 #x7E))))))

(define gen:cookie-value
  (gen:choice
   (gen:string gen:cookie-octet)
   (gen:let ([value (gen:string gen:cookie-octet)])
     (string-append "\"" value "\""))))

(define-test-suite cookie-value-prop-tests
  (check-property
   (property ([value gen:cookie-value])
     (check-true (cookie-value? value)))))

(define-test-suite p/e-value-tests
  (test-cookie-pred "path/extension values" path/extension-value? #f
    (valid "abc=123"
           "def=(define (forever x) (forever x))"
           "You're so \"cool\"")
    (invalid "x;y" "\000" (string #\rubout))))

(module+ test (require (submod ".." main))) ; for raco test & drdr
