# Changelog

## v1.4 _(forthcoming)_

### net/cookies/user-agent

- Added `make-ua-cookie`, a stable keyword-argument constructor for `ua-cookie`
  structs. Prefer this over calling the `ua-cookie` struct constructor directly.
- Direct use of the `ua-cookie` struct constructor is now deprecated. It will be
  removed in v2.0, when a new field will be added to `ua-cookie` that would make
  positional construction a source of silent breakage.

---

## v1.3

### net/cookies/common

- Added `same-site-value?` predicate.

### net/cookies/server

- `make-cookie` accepts a new `#:same-site` keyword argument (a
  `same-site-value?` or `#f`, defaulting to `#f`). Existing calls are
  unaffected.
- `cookie->string` (and by extension `cookie->set-cookie-header`) now emits a
  `SameSite` attribute when the cookie's `same-site` field is set.
- A `log-warning` is issued at cookie-creation time if `SameSite=None` is
  requested without `Secure` also being set, as browsers will reject such
  cookies.
