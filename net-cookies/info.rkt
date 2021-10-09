#lang info

;; for net-cookies

(define collection 'multi)

(define name "HTTP Cookies (RFC6265)")

(define deps '("net-cookies-lib"
               "net-cookies-doc"))

(define implies '("net-cookies-lib"
                  "net-cookies-doc"))

(define version "1.1.3")

(define license
  '(Apache-2.0 OR MIT))
