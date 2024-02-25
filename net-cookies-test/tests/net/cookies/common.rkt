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
  (run-tests p/e-value-tests)
  (run-tests p/e-value-prop-tests))

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

;; Excludes CTLs (0-31, 127) by default.
(define (gen:ascii-char exceptions [lo 32] [hi 127])
  (apply
   gen:choice
   (for*/list ([code (in-range lo hi)]
               [char (in-value (integer->char code))]
               #:unless (memv char exceptions))
     (gen:const char))))

(define separator-chars
  '(#\( #\) #\< #\> #\@
    #\, #\; #\: #\\ #\"
    #\/ #\[ #\] #\? #\=
    #\{ #\} #\space #\tab))
(define separator-octets
  (map char->integer separator-chars))
(define gen:token
  (gen:ascii-char separator-chars))

(define gen:cookie-name
  (gen:let ([t0 gen:token]
            [ts (gen:list gen:token)])
    (apply string t0 ts)))

(define gen:invalid-cookie-name
  (gen:string
   (gen:ascii-char
    (for/list ([i (in-range 32 127)]
               #:unless (memv i separator-octets))
      (integer->char i))
    0 128)))

(define-test-suite cookie-name-prop-tests
  (test-case "cookie-name? property tests (valid)"
    (check-property
     (property ([name gen:cookie-name])
       (check-true (cookie-name? name)))))
  (test-case "cookie-name? property tests (invalid)"
    (check-property
     (property ([name gen:invalid-cookie-name])
       (check-false (cookie-name? name))))))

(define-test-suite cookie-value-tests
  (test-cookie-pred "cookie values" cookie-value? #t
    (valid "value" "(" "!" ")" ")!" "(!" "(!)" "!)" "\"hey!\"" "a=b=c" "`a")
    (invalid "a;b" "a,b" "a b" "a\tb" "a=\"foo\"")))

;; cookie-octet      = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
;;                       ; US-ASCII characters excluding CTLs,
;;                       ; whitespace DQUOTE, comma, semicolon,
;;                       ; and backslash
(define valid-cookie-octets
  (append '(#x21)
          (range #x23 (add1 #x2B))
          (range #x2D (add1 #x3A))
          (range #x3C (add1 #x5B))
          (range #x5D (add1 #x7E))))

(define gen:cookie-octet
  (apply gen:choice (map (compose1 gen:const integer->char) valid-cookie-octets)))

(define gen:cookie-value
  (gen:choice
   (gen:string gen:cookie-octet)
   (gen:let ([value (gen:string gen:cookie-octet)])
     (string-append "\"" value "\""))))

(define gen:invalid-cookie-octet
  (apply
   gen:choice
   (for/list ([i (in-range 0 128)]
              #:unless (memv i valid-cookie-octets))
     (gen:const (integer->char i)))))

(define gen:invalid-cookie-value
  (gen:let ([o0 gen:invalid-cookie-octet]
            [os (gen:string gen:invalid-cookie-octet)])
    (string-append (string o0) os)))

(define-test-suite cookie-value-prop-tests
  (test-case "cookie-value? property tests (valid)"
    (check-property
     (property ([value gen:cookie-value])
       (check-true (cookie-value? value)))))
  (test-case "cookie-value? property tests (invalid)"
    (check-property
     (property ([value gen:invalid-cookie-value])
       (check-false (cookie-value? value))))))

(define-test-suite p/e-value-tests
  (test-cookie-pred "path/extension values" path/extension-value? #f
    (valid "abc=123"
           "def=(define (forever x) (forever x))"
           "You're so \"cool\"")
    (invalid "x;y" "\000" (string #\rubout))))

(define av-octet-exception-chars
  '(#\# #\\ #\;))
(define av-octet-exception-octets
  (map char->integer av-octet-exception-chars))
(define gen:av-octet
  (gen:ascii-char av-octet-exception-chars))
(define gen:invalid-av-octet
  (gen:ascii-char
   (for/list ([i (in-range 32 127)]
              #:unless (memv i av-octet-exception-octets))
     (integer->char i))
   0 127))

(define gen:p/e-value
  (gen:string gen:av-octet))
(define gen:invalid-p/e-value
  (gen:let ([o0 gen:invalid-av-octet]
            [os (gen:string gen:invalid-av-octet)])
    (string-append (string o0) os)))

(define-test-suite p/e-value-prop-tests
  (test-case "path/extension-value? property tests (valid)"
    (check-property
     (property ([p/e gen:p/e-value])
       (check-true (path/extension-value? p/e)))))
  (test-case "path/extension-value? property tests (invalid)"
    (check-property
     (property ([p/e gen:invalid-p/e-value])
       (check-false (path/extension-value? p/e))))))

(module+ test (require (submod ".." main))) ; for raco test & drdr
