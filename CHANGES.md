# Changelog

## v1.4 _(forthcoming)_

### net/cookies/user-agent

- Added `make-ua-cookie`, a stable keyword-argument constructor for `ua-cookie`
  structs. Prefer this over calling the `ua-cookie` struct constructor directly.
- Direct use of the `ua-cookie` struct constructor is now deprecated.

## v1.3

### net/cookies/common

- Added `same-site-value?` predicate.

### net/cookies/server

- `make-cookie` accepts a new `#:same-site` keyword argument (a
  `same-site-value?` or `#f`, defaulting to `#f`). Existing calls are
  unaffected.
- `make-cookie` and other procedures here now create and use
  `cookie/same-site` (a subtype of `cookie`) instead of `cookie`, having an
  additional `same-site` field.
- `cookie->string` (and by extension `cookie->set-cookie-header`) now emits a
  `SameSite` attribute when the cookie's `same-site` field is set.
- A `log-warning` is issued at cookie-creation time if `SameSite=None` is
  requested without `Secure` also being set, as browsers will reject such
  cookies.
