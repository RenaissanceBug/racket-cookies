
# TODO

 * Bump library version 1.2 -> 2.0.
 * Add changelog
 * Address comment in `parse-cookie`: 'TODO: allow UA to "reject public suffixes", sec 5.3'

## net-cookies-lib

### common.rkt

 * Consider adding a `same-site-value?` predicate

### server.rkt

 * Update `cookie` struct def and contract to add SameSite field.
 * Update `make-cookie` fn & contract to add `#:same-site` keyword
 * Update `cookie->string` to emit SameSite attribute
 * Emit a log message with `log-warning` when user attempts to create a cookie with SameSite=None and Secure=false:
```
(log-warning 'cookies
             (~a "In cookie «" cookie
                 "»: Browsers will reject cookies having SameSite=None without Secure also set"))
```

### user-agent.rkt

Adding SameSite:

 * Update `ua-cookie` struct def to add `same-site-enforcement` field
 * Update `extract-and-save-cookies!` to handle new field
 * Update `cookies-matching` in `list-cookie-jar%` to add SameSite enforcement logic:
   * add `#:initiating-url [initiating-url #f]` and `#:method [method 'get]` parameters
   * if `initiating-url` is provided, filter cross-site requests according to RFC6265bis:
     * exclude `'strict`
     * include `'lax` if `method` is a safe method
     * include `'none` and `'default` cookies
   * send all cookies if `initiating-url` is `#f`
 * Update `match-define`s in `insert` method of `list-cookie-jar%`, around lines 138/145
 * Locate all calls to `ua-cookie` constructor and update to add field
 * Update `remove-cookie-matching` use of `match-lambda` on `ua-cookie` struct. Around line 187
 * Update `cookie-matching` use of `match` around line 199
 * Update `parse-cookie-attributes` to handle SameSite attribute

Also:
 * Add `#:method [method 'get]` as an arg to both `cookies-matching` in `list-cookie-jar%` and the contract given in the `cookie-jar<%>` interface
 * Add `#:method [method 'get]` to `cookie-header`, and propagate through call to `cookies-matching`
 * Update access time to now on selected cookies in `cookies-matching` — decide whether to update via `set!` on cookie jar's list or `set-ua-cookie-access-time!`
 * Remove exposure of `#:mutable` field on struct `ua-cookie`
 * Add `make-ua-cookie` function with `#:same-site-enforcement` keyword
 * Add `#:omit-constructor` to `ua-cookie` struct def

## net-cookies-test

### server.rkt

 * Confirm that all calls to `make-cookie` are unaffected by field addition
 * Add cookie-making-tests for `make-cookie` that use new field
 * Add cookie-header-parsing-tests for `make-cookie` that use new field
 * Test `make-cookie` for warning issued when `SameSite=None` and not `Secure`

### user-agent.rkt

Constructing cookies directly:
 * Update all calls to `ua-cookie` constructor (63) / modify existing `parse-cookie` tests to verify `SameSite=default`
 * Update all match pattern instances of `ua-cookie` (4: 2 in `check-cookie-with-approx-ctime/atime?` helper, 2 in `ua-cookie-matches` helper)
 * Add tests for `ua-cookie` construction via new `make-ua-cookie` function

Parsing:
 * Test that `parse-cookie` (via `parse-cookie-attributes`) correctly parses all three legal values for `SameSite`, in variants of differing case

Retrieval:
 * Test `initiating-url` parameter of `cookies-matching`:
   * Same-site request: cookies sent regardless of enforcement mode
   * Cross-site request:
     * `'strict` mode cookies NOT sent
     * `'lax` mode cookies sent if top-level nav with safe method XXX review
     * `'none` mode cookies sent
     * `'default` mode cookies: test "Lax-allowing-unsafe" behavior

Misc (later):
 * Parameterize cookie-jar tests over a jar-constructing thunk, to allow testing any class that implements `cookie-jar<%>`

## net-cookies-doc

 * DONE cite the bis draft in the lib docs intro

### common

 * Document `same-site-value?` predicate if added

### server

 * Document modification to `make-cookie` in net/cookies/server and cite bis draft
 * Document warning re SameSite=None without Secure in docs for `make-cookie`
 * Add a suitable bibliography ref — [MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Cookies)? — re SameSite=None without Secure
 * Document possible new output from `cookie->set-cookie-header`
 * Add example to `cookie->string` demo showing SameSite

### user-agent

 * Document new `same-site-enforcement` field in `ua-cookie` struct
 * Document `cookie-jar<%>` obligation of `cookies-matching` to do same-site filtering if `initiating-url` is provided
 * Document `make-ua-cookie` function
 * Document removal of `#:mutable` from `ua-cookie` interface, e.g.:
```
@history[#:changed "2.0" @elem{The @racket[access-time] field of @racket[ua-cookie] is no longer mutable; @racket[set-ua-cookie-access-time!] is no longer provided.}]
```
 * Add `@history` comment to `cookies-matching` documenting that `cookie-jar<%>` implementations are obliged to update cookie atimes and that the new version of `list-cookie-jar%` does this.

## known dependencies

 * submit PR adding new field to [typed net/cookies repo](https://github.com/racket/typed-racket/blob/b61ba231adaec253008fa60b714e0e49f384d1b0/typed-racket-more/typed/net/cookies/server.rkt) and same-site-value? predicate to [common.rkt in same](https://github.com/racket/typed-racket/blob/b61ba231adaec253008fa60b714e0e49f384d1b0/typed-racket-more/typed/net/cookies/common.rkt)
