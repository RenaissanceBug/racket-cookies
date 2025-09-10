#lang racket/base

(require rackunit
         racket/match
         racket/class
         net/cookies/user-agent
         (only-in net/url string->url)
         ;web-server/http/request-structs
         (only-in racket/date date->seconds))

;;;; Date-related constants ;;;;

(define exp-date:rfc1123 "Mon, 16 Feb 2015 01:41:26 GMT")
(define exp-date:rfc850  "Monday, 16-Feb-15 01:41:26 GMT")
(define exp-date:asctime "Mon Feb 16 01:41:26 2015")
;; TODO use a date struct instead of seconds to test. See FIXMEs below.
(define exp-seconds 1424079686)

;;;; Processing the Set-Cookie header ;;;;

;; URLs for testing:
(define example-url (string->url "http://example.com/"))
(define https://example.com/ (string->url "https://example.com/"))
(define example.com/abc/d (string->url "http://example.com/abc/d"))
(define example.com/x/y (string->url "http://example.com/x/y"))
(define example.com/x/y/z (string->url "http://example.com/x/y/z"))
(define test-example-url (string->url "http://test.example.com/"))
(define test.example.com/abc/e (string->url "http://test.example.com/abc/e"))
(define test.example.com/x/y (string->url "http://test.example.com/x/y"))
(define test.example.com/x/y/z (string->url "http://test.example.com/x/y/z"))

(define racket-lang-url (string->url "http://racket-lang.org/"))

(module+ main
  (require rackunit/text-ui)
  (run-tests extract-cookies-tests)
  (run-tests extract-cookies-bytes-tests)
  (run-tests cookie-jar-tests)
  (run-tests cookie-jar-bytes-test)
  (run-tests cookie-saving-tests)
  (run-tests default-path-tests)
  (run-tests date-parsing-tests1)
  (run-tests date-parsing-tests2))

(module+ test (require (submod ".." main)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing extract-cookies

;; Given a list of headers, a URL, and a template expected cookie, produces
;; a test case that runs extract-cookies on the headers and URL, checking the
;; output as follows:
;;  1) that it be a singleton list containing one cookie c
;;  2) that all fields of c other than ctime and atime be equal to those of the
;;     template cookie
;;  3) that the ctime and atime of c be approximately equal to the current
;;     time.
(define (test-cookie-from-header message headers url template [maxage #f])
  (define now (current-seconds))
  (test-single-cookie-OK? message
     (extract-cookies headers url)
     (struct-copy ua-cookie template
                  [expiration-time
                   (if maxage (+ now maxage) max-cookie-seconds)]
                  [creation-time now]
                  [access-time now])))

;; Checks that extracted is a singleton list containing a UA cookie
;; that matches expected and has creation/access times within
;; (current-time-tolerance) of the expected times.
(define (test-single-cookie-OK? message extracted expected)
  (test-case message
    (check-match extracted (list (? ua-cookie?)))
    (check-cookie-with-approx-ctime/atime? (car extracted) expected)))

(define current-time-tolerance (make-parameter 2))
;; Check the actual cookie against the expected one, with all parts equal
;; except for the ctime and atime fields, which must be within the
;; current-time-tolerance (in seconds) of expected.
(define-binary-check (check-cookie-with-approx-ctime/atime? actual expected)
  (check-pred ua-cookie? actual)
  (check-pred ua-cookie? expected)
  (match expected
    [(ua-cookie name1 val1 dom1 path1 exp1 ctime1 atime1 p?1 h?1 s?1 http-only?1)
     (check-match actual
       (ua-cookie name2 val2 dom2 path2 exp2 ctime2 atime2 p?2 h?2 s?2 http-only?2)
       (let ([tol (current-time-tolerance)])
         (and (andmap equal?
                      (list name1 val1 dom1 path1 exp1 p?1 h?1 s?1 http-only?1)
                      (list name2 val2 dom2 path2 exp2 p?2 h?2 s?2 http-only?2))
              (<= (- ctime1 tol) ctime2 (+ ctime1 tol))
              (<= (- atime1 tol) atime2 (+ atime1 tol)))))]
    [_ ; can't happen
     (fail "problem with library test; please file an issue on github")]))

(define-test-suite extract-cookies-tests
  (test-cookie-from-header
    "extract cookie with no exp-time or options"
    '((#"Set-Cookie" . #"foo=bar"))
    example-url
    (ua-cookie "foo" "bar" "example.com" "/" 1 1 1
               #f #t #f #f))
  (test-cookie-from-header
    "extract cookie with max-age only"
    '((#"Set-Cookie" . #"foo=bar; Max-Age=51"))
    example-url
    (ua-cookie "foo" "bar" "example.com" "/" 1 1 1
               #t #t #f #f)
    51)
  (test-cookie-from-header
    "extract cookie with max-age and domain"
    '((#"Set-Cookie" . #"foo=bar; Max-Age=52; Domain=example.com"))
    example-url
    (ua-cookie "foo" "bar" "example.com" "/" 1 1 1
               #t #f #f #f)
    52)
  (test-cookie-from-header
    "extract cookie with max-age, secure, domain"
    '((#"Set-Cookie" . #"foo=bar; Max-Age=53; Secure; Domain=example.com"))
    example-url
    (ua-cookie "foo" "bar" "example.com" "/" 1 1 1
               #t #f #t #f)
    53)
  (test-cookie-from-header
    "extract cookie with httponly, domain"
    '((#"Set-Cookie" . #"foo=bar; httpOnly; Domain=example.com"))
    example-url
    (ua-cookie "foo" "bar" "example.com" "/" 1 1 1
               #f #f #f #t))
  (test-cookie-from-header
    "extract cookie with path"
    '((#"Set-Cookie" . #"foo=bar; Path=/abc/def"))
    example-url
    (ua-cookie "foo" "bar" "example.com" "/abc/def" 1 1 1
               #f #t #f #f))
  (test-cookie-from-header
    "extract cookie with domain, path"
    '((#"Set-Cookie" . #"foo=bar; Domain=test.example.com; Path=/abc/de/f"))
    test-example-url
    (ua-cookie "foo" "bar" "test.example.com" "/abc/de/f" 1 1 1
                          #f #f #f #f))
  (test-cookie-from-header
    "extract cookie -- use last domain given (1)"
    '((#"Set-Cookie" . #"foo=bar; Domain=test.example.com; Domain=example.com"))
    test-example-url
    (ua-cookie "foo" "bar" "example.com" "/" 1 1 1 #f #f #f #f))
  (test-cookie-from-header
    "extract cookie -- use last domain given (2)"
    '((#"Set-Cookie"
       . #"foo=bar; Domain=example.com; Domain=test.example.com;"))
    test-example-url
    (ua-cookie "foo" "bar" "test.example.com" "/" 1 1 1 #f #f #f #f))

  (test-equal? "cookies that should be ignored"
               (extract-cookies
                '((#"Set-Cookie" . #"foo=bar; Domain=foo.com") ; wrong dom
                  (#"Set-Cookie" . #"foo=bar; Domain=test.example.com") ;subdom
                  (#"Set-Cookie"
                   . #"foo=bar; Domain=test.example.com; Path=/abc/de/f")) ;subdom
                example-url)
               '()))

(define-test-suite extract-cookies-bytes-tests
  (test-cookie-from-header
    "bytes: extract cookie with no exp-time or options"
    '(#"Set-Cookie: foo=bar")
    example-url
    (ua-cookie "foo" "bar" "example.com" "/" 1 1 1 #f #t #f #f))
  (test-cookie-from-header
    "extract cookie with max-age only"
    '(#"Set-Cookie: foo=bar; Max-Age=51")
    example-url
    (ua-cookie "foo" "bar" "example.com" "/" 1 1 1 #t #t #f #f)
    51)
  (test-cookie-from-header
    "extract cookie with max-age and domain"
    '(#"Set-Cookie: foo=bar; Max-Age=52; Domain=example.com")
    example-url
    (ua-cookie "foo" "bar" "example.com" "/" 1 1 1 #t #f #f #f)
    52)
  (test-cookie-from-header
    "extract cookie with max-age, secure, domain"
    '(#"Set-Cookie: foo=bar; Max-Age=53; Secure; Domain=example.com")
    example-url
    (ua-cookie "foo" "bar" "example.com" "/" 1 1 1 #t #f #t #f)
    53)
  (test-cookie-from-header
    "extract cookie with httponly, domain"
    '(#"Set-Cookie: foo=bar; httpOnly; Domain=example.com")
    example-url
    (ua-cookie "foo" "bar" "example.com" "/" 1 1 1 #f #f #f #t))
  (test-cookie-from-header
    "extract cookie with path"
    '(#"Set-Cookie: foo=bar; Path=/abc/def")
    example-url
    (ua-cookie "foo" "bar" "example.com" "/abc/def" 1 1 1 #f #t #f #f))
  (test-cookie-from-header
    "extract cookie with domain, path"
    '(#"Set-Cookie: foo=bar; Domain=test.example.com; Path=/abc/de/f")
    test-example-url
    (ua-cookie "foo" "bar" "test.example.com" "/abc/de/f" 1 1 1
               #f #f #f #f))
  (test-cookie-from-header
    "extract cookie -- use last domain given (1)"
    '(#"Set-Cookie: foo=bar; Domain=test.example.com; Domain=example.com")
    test-example-url
    (ua-cookie "foo" "bar" "example.com" "/" 1 1 1 #f #f #f #f))
  (test-cookie-from-header
    "extract cookie -- use last domain given (2)"
    '(#"Set-Cookie: foo=bar; Domain=example.com; Domain=test.example.com;")
    test-example-url
    (ua-cookie "foo" "bar" "test.example.com" "/" 1 1 1 #f #f #f #f))

  (test-equal? "cookies that should be ignored"
               (extract-cookies
                '(#"Set-Cookie: foo=bar; Domain=foo.com"          ; wrong dom
                  #"Set-Cookie: foo=bar; Domain=test.example.com" ; subdom
                  #"Set-Cookie: foo=bar; Domain=test.example.com; Path=/abc/de/f") ;subdom
                example-url)
               '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cookie jars: Storing & Retrieving, Generating Headers


(define-test-suite cookie-jar-tests
  (parameterize ([current-cookie-jar (new list-cookie-jar%)])
    (define now (current-seconds))
    (test-false "cookie-header: empty jar" (cookie-header test-example-url))

    (extract-and-save-cookies!
     '((#"X-Test-Header" . #"ignore this header")
       (#"Set-Cookie" . #"a=b; Max-Age=2000; Path=/")
       (#"Another-Dummy" . #"another value to ignore")
       ; This next one won't get saved:
       (#"Set-Cookie" . #"c=d; Max-Age=3; Domain=example.com; Path=/x/y")
       (#"Set-Cookie" . #"user=bob; Max-Age=4; Path=/x")
       )
     test.example.com/x/y)
    (test-equal? "cookie-header: one match"
                 (cookie-header test-example-url) ; sic, NOT -url3
                 #"a=b")
    (test-equal? "cookie-header: one match (via domain & path)"
                 (cookie-header example.com/x/y/z)
                 #"c=d")
    (test-equal? "cookie-header: multiple matches"
                 (cookie-header test.example.com/x/y/z)
                 #"c=d; user=bob; a=b")
    (sleep 1) ; so next cookie's ctime is later than a's.
    
    (extract-and-save-cookies!
     '((#"Dummy-Header" . #"something to ignore")
       ; This cookie, being inserted later, should follow #"a=b" in the header:
       (#"Set-Cookie" . #"x=y; Domain=test.example.com; Path=/"))
     test.example.com/x/y)
    (test-false "cookie-header: no matches" (cookie-header racket-lang-url))
    (test-equal? "cookie-header: multiple matches, order w/ties broken by ctime"
                 (cookie-header test.example.com/x/y)
                 #"user=bob; a=b; x=y")

    (extract-and-save-cookies!
     '((#"Set-Cookie" . #"user=; Max-Age=6; Path=/x"))
     test.example.com/x/y)
    (test-equal? "cookie-header: after replacing a cookie"
                 (cookie-header test.example.com/x/y)
                 #"user=; a=b; x=y")
    (test-equal? "cookie-header: 4 cookies, after replacing a cookie"
                 (cookie-header test.example.com/x/y/z)
                 #"c=d; user=; a=b; x=y")
    (sleep 4) ; move past c's expiration

    (extract-and-save-cookies! ; This should expire the "c" cookie but not "user"
     '((#"Set-Cookie" . #"timeToExpire=now; Domain=test.example.com; Path=/x"))
     test.example.com/x/y)
    (test-equal? "cookie-header: after expiring a cookie"
                 (cookie-header test.example.com/x/y/z)
                 #"user=; timeToExpire=now; a=b; x=y") ; x is later by ctime
    (sleep 3) ; move past user's expiration

    (extract-and-save-cookies! ; This should expire the "user" cookie.
     '((#"Set-Cookie" . #"timeToExpire=NOW; Domain=test.example.com; Path=/x"))
     test.example.com/x/y)
    (test-equal? "cookie-header: after expiring another cookie"
                 (cookie-header test.example.com/x/y/z)
                 #"timeToExpire=NOW; a=b; x=y")
    (sleep 1) ; so next timeToExpire cookie's ctime is later
    
    (extract-and-save-cookies!
     '((#"Set-Cookie" . #"timeToExpire=SOON; Domain=example.com; Path=/x"))
     test.example.com/x/y)
    (test-equal? "cookie-header: same cookie on 2 domains: earlier ctime first"
                 (cookie-header test.example.com/x/y/z)
                 #"timeToExpire=NOW; timeToExpire=SOON; a=b; x=y")

    (extract-and-save-cookies!  ; Clear both timeToExpire cookies:
     '((#"Set-Cookie" . #"timeToExpire=; Max-Age=-1; Domain=example.com; Path=/x")
       (#"Set-Cookie"
        . #"timeToExpire=; Max-Age=-1; Domain=test.example.com; Path=/x"))
     test.example.com/x/y)
    (test-equal? "cookie-header: clearing cookies"
                 (cookie-header test.example.com/x/y/z)
                 #"a=b; x=y")
    
    (extract-and-save-cookies!
     '((#"Set-Cookie" . #"supersecret=yeah; Secure"))
     https://example.com/)
    (test-false "cookie-header: don't send a secure cookie over insecure HTTP"
                (cookie-header example-url))
    (test-equal? "cookie-header: do send a secure cookie over HTTPS"
                 (cookie-header https://example.com/)
                 #"supersecret=yeah"))
  
  (parameterize ([current-cookie-jar (new list-cookie-jar%)])
    (extract-and-save-cookies!
     '((#"Set-Cookie" . #"okToReplace=NO; Path=/; Domain=example.com; HttpOnly"))
     example-url)
    (test-equal? "cookie-header: an HTTPOnly cookie" (cookie-header example-url)
                 #"okToReplace=NO")
    (save-cookie! (parse-cookie #"okToReplace=please?; Path=/; Domain=example.com"
                                example-url)
                  #f)
    (test-equal? "save-cookie!: non-HTTP cookie can't replace an HTTPOnly one"
                 (cookie-header example-url)
                 #"okToReplace=NO")))

;; The first of the above tests, but using byte-string headers instead of pairs:
(define-test-suite cookie-jar-bytes-test
  (parameterize ([current-cookie-jar (new list-cookie-jar%)])
    (define now (current-seconds))
    (extract-and-save-cookies!
     '(#"X-Test-Header: ignore this header"
       #"Set-Cookie: a=b; Max-Age=2000; Path=/"
       #"Another-Dummy: another value to ignore"
       ; This next one won't get saved:
       #"Set-Cookie: c=d; Max-Age=3; Domain=example.com; Path=/x/y"
       #"Set-Cookie: user=bob; Max-Age=4; Path=/x")
     test.example.com/x/y)
    (test-equal? "cookie-header: one match"
                 (cookie-header test-example-url) ; sic, NOT -url3
                 #"a=b")
    (test-equal? "cookie-header: one match (via domain & path)"
                 (cookie-header example.com/x/y/z)
                 #"c=d")
    (test-equal? "cookie-header: multiple matches"
                 (cookie-header test.example.com/x/y/z)
                 #"c=d; user=bob; a=b")
    ))

;; Testing repeated insertion of cookies. Test from Alexis King; see
;; https://github.com/RenaissanceBug/racket-cookies/issues/14.
;; TODO resume here — test-ua-cookies-match isn't catching the duplicates
(define-test-suite repeat-two-cookies-test
  (parameterize ([current-cookie-jar (new list-cookie-jar%)])
    (define now (current-seconds))
    (define headers '(#"set-cookie: cookie1=value1; Max-Age=100000; path=/; httponly"
                      #"set-cookie: cookie2=value2; Max-Age=100000; path=/; httponly"))
    ;; The cookies should look like this but with different times:
    (define cookie1 (ua-cookie "cookie1" "value1" "example.com" "/" 1681348246 1681248243 1681248246 #t #t #f #t))
    (define cookie2 (ua-cookie "cookie2" "value2" "example.com" "/" 1681348246 1681248243 1681248246 #t #t #f #t))
    (define url (string->url "https://example.com/"))
    
    (extract-and-save-cookies! headers url)
    (test-pred "two cookies"
               (test-ua-cookies-match (list cookie2 cookie1))
               (send (current-cookie-jar) cookies-matching url))
    (sleep 1)
    (extract-and-save-cookies! headers url)
    (test-pred "two cookies after 2nd insert"
               (test-ua-cookies-match (list cookie2 cookie1))
               (send (current-cookie-jar) cookies-matching url))
    (sleep 1)
    (extract-and-save-cookies! headers url)
    (test-pred "two cookies after 3rd insert"
               (test-ua-cookies-match (list cookie2 cookie1))
               (send (current-cookie-jar) cookies-matching url))
    (sleep 1)
    (extract-and-save-cookies! headers url)
    (test-pred "two cookies after 4th insert"
               (test-ua-cookies-match (list cookie2 cookie1))
               (send (current-cookie-jar) cookies-matching url))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cookies used in subsequent tests:

(define test-cookie1
  (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds 1 1 #f #t #f #f))
(define test-cookie2
  (ua-cookie "baz" "" "example.com" "/abc" (- max-cookie-seconds 1)
             1 1 #f #f #f #f))
(define test-cookie3
  (ua-cookie "ugh" "" "test.example.com" "/x" max-cookie-seconds 1 1 #f #f #f #f))
(define test-cookie4 ; replaces test-cookie2
  (ua-cookie "baz" "42" "example.com" "/abc" max-cookie-seconds 1 1 #f #f #f #f))
(define test-cookie5
  (ua-cookie "qux" "" "racket-lang.org" "/" max-cookie-seconds 1 1 #f #f #f #f))
(define test-cookie-expired ; removes test-cookie2
  (ua-cookie "baz" "" "example.com" "/abc" 100 1 1 #f #f #f #f))

(define-test-suite cookie-saving-tests
  ;; Inserting via save-cookie! procedure:
  (parameterize ([current-cookie-jar (new list-cookie-jar%)])
    (test-begin
     (define the-jar (current-cookie-jar))
     (check-equal? (send the-jar cookies-matching example-url) '()
                   "cookies-matching on an empty jar")
     (save-cookie! test-cookie1)
     (check-equal? (send the-jar cookies-matching example-url)
                   (list test-cookie1)
                   "cookies-matching, 1 in the jar")
     (check-equal? (send the-jar cookies-matching test-example-url)
                   (list test-cookie1)
                   "cookies-matching, 1 cookie in the jar, subdomain matches")
     (save-cookie! test-cookie2)
     (check-equal? (send the-jar cookies-matching example-url)
                   (list test-cookie1)
                   "cookies-matching, 2 in the jar; path only matches 1")
     (check-equal? (send the-jar cookies-matching example.com/abc/d)
                   (list test-cookie2 test-cookie1)
                   "cookies-matching, 2 in the jar; longer path s.b. first")
     (check-equal? (send the-jar cookies-matching test-example-url)
                   (list test-cookie1)
                   "cookies-matching with 2 in the jar; subdomain; match 1")
     (check-equal? (send the-jar cookies-matching test.example.com/abc/e)
                   (list test-cookie2 test-cookie1)
                   "cookies-matching with 2 in the jar; subdomain; match 2")
     (save-cookie! test-cookie3)
     (check-equal? (send the-jar cookies-matching example.com/abc/d)
                   (list test-cookie2 test-cookie1)
                   "cookies-matching, 3 in the jar, exclude subdomain cookie")
     (check-equal? (send the-jar cookies-matching test.example.com/abc/e)
                   (list test-cookie2 test-cookie1)
                   "cookies-matching, 3 in the jar; subdomain; path excludes 1")
     (check-equal? (send the-jar cookies-matching test.example.com/x/y)
                   (list test-cookie3 test-cookie1)
                   "cookies-matching, 3 in the jar; subdomain; path excludes 1")
     (save-cookie! test-cookie4)
     (check-equal? (send the-jar cookies-matching test.example.com/abc/e)
                   (list test-cookie4 test-cookie1)
                   "cookies-matching, 3 in jar, after replacing a cookie")
     (check-equal? (send the-jar cookies-matching racket-lang-url) '()
                   "totally different URL")
     (save-cookie! test-cookie5)
     (check-equal? (send the-jar cookies-matching example-url)
                   (list test-cookie1)
                   "cookies-matching, 4 in the jar, diff domains")
     (check-equal? (send the-jar cookies-matching racket-lang-url)
                   (list test-cookie5)
                   "cookies-matching, 4 in the jar, diff domains")
     (save-cookie! test-cookie-expired)
     (check-equal? (send the-jar cookies-matching test.example.com/abc/e)
                   (list test-cookie1)
                   "cookies-matching, check that cookies are expired")
     ))
  
  ;; Inserting via save-cookie! method. (Same sequence of insertions and
  ;; checks as above for the procedure.)
  (parameterize ([current-cookie-jar (new list-cookie-jar%)])
    (test-begin
     (define the-jar (current-cookie-jar))
     (check-equal? (send the-jar cookies-matching example-url) '()
                   "cookies-matching on an empty jar")
     (send the-jar save-cookie! test-cookie1)
     (check-equal? (send the-jar cookies-matching example-url)
                   (list test-cookie1)
                   "cookies-matching, 1 in the jar")
     (check-equal? (send the-jar cookies-matching test-example-url)
                   (list test-cookie1)
                   "cookies-matching, 1 cookie in the jar, subdomain matches")
     (send the-jar save-cookie! test-cookie2)
     (check-equal? (send the-jar cookies-matching example-url)
                   (list test-cookie1)
                   "cookies-matching, 2 in the jar; path only matches 1")
     (check-equal? (send the-jar cookies-matching example.com/abc/d)
                   (list test-cookie2 test-cookie1)
                   "cookies-matching, 2 in the jar; longer path s.b. first")
     (check-equal? (send the-jar cookies-matching test-example-url)
                   (list test-cookie1)
                   "cookies-matching with 2 in the jar; subdomain; match 1")
     (check-equal? (send the-jar cookies-matching test.example.com/abc/e)
                   (list test-cookie2 test-cookie1)
                   "cookies-matching with 2 in the jar; subdomain; match 2")
     (send the-jar save-cookie! test-cookie3)
     (check-equal? (send the-jar cookies-matching example.com/abc/d)
                   (list test-cookie2 test-cookie1)
                   "cookies-matching, 3 in the jar, exclude subdomain cookie")
     (check-equal? (send the-jar cookies-matching test.example.com/abc/e)
                   (list test-cookie2 test-cookie1)
                   "cookies-matching, 3 in the jar; subdomain; path excludes 1")
     (check-equal? (send the-jar cookies-matching test.example.com/x/y)
                   (list test-cookie3 test-cookie1)
                   "cookies-matching, 3 in the jar; subdomain; path excludes 1")
     (send the-jar save-cookie! test-cookie4)
     (check-equal? (send the-jar cookies-matching test.example.com/abc/e)
                   (list test-cookie4 test-cookie1)
                   "cookies-matching, 3 in jar, after replacing a cookie")
     (check-equal? (send the-jar cookies-matching racket-lang-url) '()
                   "totally different URL")
     (send the-jar save-cookie! test-cookie5)
     (check-equal? (send the-jar cookies-matching example-url)
                   (list test-cookie1)
                   "cookies-matching, 4 in the jar, diff domains")
     (check-equal? (send the-jar cookies-matching racket-lang-url)
                   (list test-cookie5)
                   "cookies-matching, 4 in the jar, diff domains")
     (send the-jar save-cookie! test-cookie-expired)
     (check-equal? (send the-jar cookies-matching test.example.com/abc/e)
                   (list test-cookie1)
                   "cookies-matching, check that cookies are expired")
     ))
  
  ;; Inserting via save-cookies! method:
  (parameterize ([current-cookie-jar (new list-cookie-jar%)])
    (test-begin
     (define the-jar (current-cookie-jar))
     (send the-jar save-cookies! (list test-cookie1 test-cookie2 test-cookie5))
     (check-equal? (send the-jar cookies-matching test.example.com/abc/e)
                   (list test-cookie2 test-cookie1)
                   "inserted cookies with save-cookies! method: domain 1")
     (check-equal? (send the-jar cookies-matching racket-lang-url)
                   (list test-cookie5)
                   "inserted cookies with save-cookies! method: domain 2")
     ))
  
  (parameterize ([current-cookie-jar (new list-cookie-jar%)])
    (test-begin
     (send (current-cookie-jar) save-cookie!
           (ua-cookie "x" "y" "example.com" "/" max-cookie-seconds 1 1
                      #f #t #f #t)
           #f)
     (check-equal? (send (current-cookie-jar) cookies-matching example-url)
                   '()
                   "don't save an HTTPOnly cookie that didn't come in on HTTP")
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing cookies and extracting them from headers

;; Helpers: Cookie times will vary depending on when the test is run;
;; the following two fns ignore differences in ctime and atime.
(define ((ua-cookie-matches expected-uac) uac)
  (and (ua-cookie? uac)
       (match (list uac expected-uac)
         [(list (ua-cookie name value dom path exp _ _ p? h? s? http-only?)
                (ua-cookie name value dom path exp _ _ p? h? s? http-only?))
          #t]
         [_ #f])))

(define ((test-ua-cookies-match expected-ls) ls)
  (and (= (length expected-ls) (length ls))
       (for/and ([expected expected-ls]
                 [cookie ls])
         (and (ua-cookie? cookie)
              ((ua-cookie-matches expected) cookie)))))

;; TODO (low priority) we could check that atime/ctime are roughly current
;; (see "Testing extract-cookies" above) -- but if the tests above are
;; finding correct times, the time-setting code is already being exercised
(define-test-suite cookie-parsing-tests
  (test-pred "extracting from HTTP Response headers"
             (test-ua-cookies-match
              (list (ua-cookie "foo" "bar" "example.com" "/"
                               max-cookie-seconds
                               1 1 #f #t #f #f)
                    (ua-cookie "baz" "qux" "example.com" "/"
                               max-cookie-seconds
                               1 1 #f #t #f #t)))
             (extract-cookies
              '((#"X-Test-Padding" . #"notacookie")
                (#"Set-Cookie" . #"foo=bar")
                (#"set-Cookie" . #"baz=qux; HttpOnly"))
              example-url))
  
  (test-pred "extracting cookies from byte alist"
             (test-ua-cookies-match
              (list (ua-cookie "qux" "" "example.com" "/"
                               max-cookie-seconds 1 1 #f #f #f #f)
                    (ua-cookie "mum" "" "example.com" "/"
                               max-cookie-seconds 1 1 #f #f #f #f)))
             (extract-cookies '((#"foo" . #"bar")
                                (#"Set-Cookie" . #"qux; ")
                                (#"set-Cookie" . #"mum"))
                              example-url))
  
  (test-pred "parse-cookie: simple"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #t #f #f))
             (parse-cookie #"foo=bar" example-url))
  (test-pred "parse-cookie: simple, no value"
             (ua-cookie-matches
              (ua-cookie "foo" "" "example.com" "/" max-cookie-seconds
                         1 1 #f #t #f #f))
             (parse-cookie #"foo=" example-url))
  (test-pred "parse-cookie: multiple semicolons in a row"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #t #f #f))
             (parse-cookie #"foo=bar;;;" example-url))
  (test-pred "parse-cookie: ignore av-pair without name"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #t #f #f))
             (parse-cookie #"foo=bar;=xyz" example-url))
  (test-pred "parse-cookie: ignore empty av-pair"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #t #f #f))
             (parse-cookie #"foo=bar;=;" example-url))
  
  (test-pred "parse-cookie: domain"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #f #f #f))
             (parse-cookie #"foo=bar; Domain=example.com" example-url))
  (test-pred "parse-cookie: ignore empty domain"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "test.example.com" "/" max-cookie-seconds
                         1 1 #f #t #f #f))
             (parse-cookie #"foo=bar; Domain=" test-example-url))
  (test-pred "parse-cookie: domain - uppercase & space don't matter"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #f #f #f))
             (parse-cookie #"foo=bar; DOMAIN = example.com" example-url))
  (test-pred "parse-cookie: subdomain"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #f #f #f))
             (parse-cookie #"foo=bar; Domain=example.com" test-example-url))
  (test-pred "parse-cookie: subdomain with leading dot (which must be removed)"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #f #f #f))
             (parse-cookie #"foo=bar; Domain=.example.com" test-example-url))
  (test-pred "parse-cookie: subdomain that must be lowercased"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #f #f #f))
             (parse-cookie #"foo=bar; Domain=Example.Com" test-example-url))
  (test-pred "parse-cookie: multiple domains: use the final domain"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #f #f #f))
             (parse-cookie #"foo=bar; Domain=test.example.com; Domain=example.com"
                           test-example-url))
  
  (test-pred "parse-cookie: ignore empty path"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #t #f #f))
             (parse-cookie #"foo=bar; Path=" example-url))
  (test-pred "parse-cookie: ignore non-absolute path"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #t #f #f))
             (parse-cookie #"foo=bar; Path=some/place/" example-url))
  (test-pred "parse-cookie: non-empty path"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/some/place"
                         max-cookie-seconds 1 1 #f #t #f #f))
             (parse-cookie #"foo=bar; Path=/some/place" example-url))
  (let ([current-time (current-seconds)])
    (test-pred "parse-cookie: max-age"
               (ua-cookie-matches
                (ua-cookie "foo" "bar" "example.com" "/" (+ current-time 56)
                           1 1 #t #t #f #f))
               (parse-cookie #"foo=bar; Max-age=56" example-url)))
  (test-pred "parse-cookie: negative max-age"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" min-cookie-seconds
                         1 1 #t #t #f #f))
             (parse-cookie #"foo=bar; Max-age=-1" example-url))
  (test-pred "parse-cookie: invalid max-age (non-initial dash)"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #t #f #f))
             (parse-cookie #"foo=bar; Max-age=123-456" example-url))
  (test-pred "parse-cookie: invalid max-age (invalid starting char)"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #t #f #f))
             (parse-cookie #"foo=bar; Max-age=/123456" example-url))
  (test-pred "parse-cookie: invalid max-age (other non-digit chars)"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds
                         1 1 #f #t #f #f))
             (parse-cookie #"foo=bar; Max-age=123*456" example-url))
  
  (test-pred "parse-cookie: Expires (RFC1123)"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" exp-seconds
                         1 1 #t #t #f #f))
             (parse-cookie (bytes-append #"foo=bar; Expires="
                                         (string->bytes/utf-8
                                          exp-date:rfc1123))
                           example-url))
  (test-pred "parse-cookie: Expires (RFC850)"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" exp-seconds
                         1 1 #t #t #f #f))
             (parse-cookie (bytes-append #"foo=bar; Expires="
                                         (string->bytes/utf-8
                                          exp-date:rfc850))
                           example-url))
  (test-pred "parse-cookie: Expires (ANSI ctime)"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" exp-seconds
                         1 1 #t #t #f #f))
             (parse-cookie (bytes-append #"foo=bar; Expires="
                                         (string->bytes/utf-8
                                          exp-date:asctime))
                           example-url))
  (let ([now (current-seconds)])
    (test-pred "parse-cookie: Max-Age overrides Expires"
               (ua-cookie-matches
                (ua-cookie "foo" "bar" "example.com" "/" (+ now 86400)
                           1 1 #t #t #f #f))
               (parse-cookie (bytes-append #"foo=bar; "
                                           #"Max-age=86400; "
                                           #"Expires="
                                           (string->bytes/utf-8
                                            exp-date:rfc1123))
                             example-url)))
  (let ([now (current-seconds)])
    (test-pred "parse-cookie: Max-Age overrides Expires that comes first"
               (ua-cookie-matches
                (ua-cookie "foo" "bar" "example.com" "/" (+ now 86400)
                           1 1 #t #t #f #f))
               (parse-cookie (bytes-append #"foo=bar; "
                                           #"Expires="
                                           (string->bytes/utf-8
                                            exp-date:rfc1123)
                                           #"; Max-age=86400")
                             example-url)))
  
  (test-pred "parse-cookie: secure flag"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds 1 1
                         #f #t #t #f))
             (parse-cookie #"foo=bar; Secure;" example-url))
  (test-pred "parse-cookie: HttpOnly flag"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds 1 1
                         #f #t #f #t))
             (parse-cookie #"foo=bar; HttpOnly;" example-url))
  (test-pred "parse-cookie: httponly flag, case doesn't matter"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds 1 1
                         #f #t #f #t))
             (parse-cookie #"foo=bar; httponly;" example-url))
  (test-pred "parse-cookie: both Secure and HttpOnly"
             (ua-cookie-matches
              (ua-cookie "foo" "bar" "example.com" "/" max-cookie-seconds 1 1
                         #f #t #t #t))
             (parse-cookie #"foo=bar; Secure ; HttpOnly;" example-url))
  
  (let ([current-time (current-seconds)])
    (test-pred "parse-cookie: secure and max-age"
               (ua-cookie-matches
                (ua-cookie "foo" "bar" "example.com" "/"
                           (+ current-time 20000) 1 1
                           #t #t #t #f))
               (parse-cookie
                #"foo=bar; Secure; Max-Age=20000"
                example-url)))
  
  (let ([now (current-seconds)])
    (test-pred "parse-cookie: all but domain"
               (ua-cookie-matches
                (ua-cookie "x" "y" "test.example.com" "/apps/special"
                           (+ now 20000) 1 1
                           #t #t #t #t))
               (parse-cookie
                (bytes-append #"x=y; Secure; Max-Age=20000; Expires="
                              (string->bytes/utf-8 exp-date:rfc1123)
                              #"; HttPOnly; SECURE; "
                              #"Path=/apps/special")
                test-example-url)))
  (let ([now (current-seconds)])
    (test-pred "parse-cookie: kitchen sink"
               (ua-cookie-matches
                (ua-cookie "x" "y" "example.com" "/apps/special"
                           (+ now 20000) 1 1
                           #t #f #t #t))
               (parse-cookie
                (bytes-append #"x=y; Secure; Max-Age=20000; Expires="
                              (string->bytes/utf-8 exp-date:rfc1123)
                              #"; HttPOnly; SECURE; Domain=.example.com; "
                              #"Path=/apps/special")
                test-example-url)))
  
  ;; Cookie parsing failures:
  (test-false "empty Set-Cookie header" (parse-cookie #"" example-url))
  (test-false "no equals in nvpair" (parse-cookie #"foo" example-url))
  (test-false "no equals in nvpair" (parse-cookie #"foo;" example-url))
  (test-false "no cookie name" (parse-cookie #"=foo" example-url))
  (test-false "domain doesn't match"
              (parse-cookie #"foo=bar; Domain=yahoo.com" example-url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test-suite default-path-tests
  (test-equal? "default path with no slashes"
               (default-path (string->url "http://example.com"))
               "/")
  (test-equal? "default path with one slash"
               (default-path example-url)
               "/")
  (test-equal? "default path with multiple slashes"
               (default-path (string->url "http://example.com/foo/bar"))
               "/foo")
  (test-equal? "default path with empty string URL"
               (default-path (string->url ""))
               "/")
  (test-equal? "default path with no host and no slash"
               (default-path (string->url "foo"))
               "/")
  (test-equal? "default path with no host and one slash"
               (default-path (string->url "/foo"))
               "/")
  (test-equal? "default path with no host and several parts"
               (default-path (string->url "/foo/bar/baz"))
               "/foo/bar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date-parsing tests

(define-test-suite date-parsing-tests1
  #; ;; FIXME test fails; is date->seconds the culprit?
  (test-equal? "parse RFC1123 date"
               (date->seconds (parse-date exp-date:rfc1123))
               exp-seconds)
  (test-equal? "parse RFC1123 date (4-digit yr >= 2000)"
               (parse-date "Thu, 19 Feb 2015 09:23:26 GMT")
               (date 26 23 9 19 2 2015 0 0 #f 0))
  (test-equal? "parse RFC1123 date (2-digit yr >= 2000)"
               (parse-date "Thu, 19 Feb 15 09:23:26 GMT")
               (date 26 23 9 19 2 2015 0 0 #f 0))
  (test-equal? "parse RFC1123 date (max 2-digit yr >= 2000)"
               (parse-date "Thu, 19 Feb 69 09:23:26 GMT")
               (date 26 23 9 19 2 2069 0 0 #f 0))
  (test-equal? "parse RFC1123 date (2-digit yr < 2000)"
               (parse-date "Thu, 19 Feb 78 09:23:26 GMT")
               (date 26 23 9 19 2 1978 0 0 #f 0))
  (test-equal? "parse bare-minimum date"
               (parse-date "19 Feb 15 09:23:26")
               (date 26 23 9 19 2 2015 0 0 #f 0)))

(define months
  (map symbol->string
       '(jan feb mar apr may jun jul aug sep oct nov dec)))
(define (randtest n)
  (if (zero? n)
      (void)
      (let ([day (+ 1 (random 28))]
            [mon (list-ref months (random 12))]
            [y (+ 2000 (random 100))]
            [h (random 24)]
            [m (random 60)]
            [s (random 60)])
        (define d1
          (parse-date
           (format "~a ~a, ~a ~a:~a:~a GMT" mon day y h m s)))
        (define d2
          (parse-date
           (format "~a:~a:~a GMT on ~a ~a, ~a" h m s mon day y)))
        (define d3
          (parse-date
           (format "~a:~a:~a GMT on ~a ~a ~a" h m s day mon y)))
        (define d4
          (parse-date
           (format "~a ~a ~a ~a:~a:~a" y mon day h m s)))
        (check-equal? d1 d2)
        (check-equal? d1 d3)
        (check-equal? d1 d4)
        (randtest (sub1 n)))))
;(randtest 100)

(define-test-suite date-parsing-tests2
  #; ;; FIXME tests fail; is date->seconds the culprit?
  (test-case
   "parsing other date formats"
   (test-equal? "parse RFC850 date"
                (date->seconds (parse-date exp-date:rfc850))
                exp-seconds)
   (test-equal? "parse asctime() date"
                (date->seconds (parse-date exp-date:asctime))
                exp-seconds))
  (test-case
   "invalid date component tests"
   (test-false "invalid day"
               (parse-date "Mon, 30 Feb 2015 01:41:26 GMT"))
   (test-false "invalid day"
               (parse-date "Mon, 32 Jan 2015 01:41:26 GMT"))
   (test-false "invalid day"
               (parse-date "Mon, 31 Apr 2015 01:41:26 GMT"))
   (test-false "missing day"
               (parse-date "Mon, Dec 2015 01:41:26 GMT"))
   (test-false "invalid month"
               (parse-date "Mon, 31 Erb 2015 01:41:26 GMT"))
   (test-false "missing month"
               (parse-date "Mon, 31 2015 01:41:26 GMT"))
   (test-false "invalid year"
               (parse-date "Mon, 31 Jan 1515 01:41:26 GMT"))
   (test-false "invalid year"
               (parse-date "Mon, 31 Jan 19999 01:41:26 GMT"))
   (test-false "invalid year"
               (parse-date "Mon, 31 Jan -5 01:41:26 GMT"))
   (test-false "invalid year"
               (parse-date "Mon, 31 Jan 0 01:41:26 GMT"))
   (test-false "missing year"
               (parse-date "Mon, 31 Jan 01:41:26 GMT"))
   (test-false "invalid hours"
               (parse-date "Mon, 31 Jan 2015 24:41:26 GMT"))
   (test-false "invalid minutes"
               (parse-date "Mon, 31 Jan 2015 10:60:26 GMT"))
   (test-false "invalid seconds"
               (parse-date "Mon, 31 Jan 2015 22:41:60 GMT"))))
