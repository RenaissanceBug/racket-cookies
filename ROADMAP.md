# Roadmap: SameSite support

## v1.3 — Server-side SameSite (additive, no breaking changes)

### 1. Common predicate

- [X] `common.rkt`: add `same-site-value?` predicate
- [X] Docs: document `same-site-value?`

### 2. Server

- [X] `server.rkt`: update `cookie` struct to add SameSite field (with default)
- [X] `server.rkt`: update `make-cookie` + contract with `#:same-site` keyword
- [X] `server.rkt`: update `cookie->string` to emit SameSite attribute
- [X] `server.rkt`: emit `log-warning` when SameSite=None and Secure=false
- [X] Tests: confirm all existing `make-cookie` calls are unaffected
- [X] Tests: add `make-cookie` tests exercising the new field
- [X] Tests: add `cookie->string` tests showing SameSite output
- [X] Tests: test warning is issued for SameSite=None + not Secure
- [X] Docs: document `#:same-site` param in `make-cookie`
- [X] Docs: document warning behaviour
- [X] Docs: document new possible output from `cookie->string`, add example
- [X] Docs: add MDN bibliography ref re SameSite=None without Secure
- [X] Bump version 1.2 → 1.3 and write changelog entry

---

## v1.4 — UA constructor migration (additive, no breaking changes)

- [ ] `user-agent.rkt`: add `make-ua-cookie` function with keyword args mirroring all current `ua-cookie` fields
- [ ] `user-agent.rkt`: add deprecation warning on direct `ua-cookie` construction
- [ ] Tests: add `make-ua-cookie` construction tests
- [ ] Docs: document `make-ua-cookie`
- [ ] Docs: add deprecation notice for direct `ua-cookie` construction
- [ ] Bump version 1.3 → 1.4 and write changelog entry

---

## v2.0 — UA-side SameSite (breaking changes)

### 3. UA struct & constructor refactor

- [ ] `user-agent.rkt`: add `same-site-enforcement` field to `ua-cookie` struct
- [ ] `user-agent.rkt`: add `#:omit-constructor` to `ua-cookie` struct def (completing the v1.4 deprecation)
- [ ] `user-agent.rkt`: remove exposure of `#:mutable` field
- [ ] `user-agent.rkt`: add `#:same-site-enforcement` keyword to `make-ua-cookie` (added in v1.4)
- [ ] `user-agent.rkt`: update all `ua-cookie` constructor calls throughout the lib
- [ ] `user-agent.rkt`: update `match-define`s on `ua-cookie` (~lines 138/145)
- [ ] `user-agent.rkt`: update `remove-cookie-matching` use of `match-lambda` (~line 187)
- [ ] `user-agent.rkt`: update `cookie-matching` use of `match` (~line 199)
- [ ] Tests: update all 63 `ua-cookie` constructor calls in test files (switch to `make-ua-cookie`)
- [ ] Tests: update 4 `ua-cookie` match patterns (`check-cookie-with-approx-ctime/atime?` and `ua-cookie-matches` helpers)
- [ ] Docs: document `same-site-enforcement` field in `ua-cookie`
- [ ] Docs: update `make-ua-cookie` docs for new `#:same-site-enforcement` keyword
- [ ] Docs: add `@history` note — `access-time` no longer mutable; `set-ua-cookie-access-time!` no longer provided

### 4. UA parsing

- [ ] `user-agent.rkt`: update `parse-cookie-attributes` to handle SameSite attribute
- [ ] `user-agent.rkt`: update `extract-and-save-cookies!` to populate the new field
- [ ] Tests: verify `parse-cookie` parses all three SameSite values in varied case
- [ ] Tests: update existing `parse-cookie` tests to assert `same-site-enforcement: 'default`

### 5. UA retrieval & filtering

- [ ] `user-agent.rkt`: update `cookies-matching` in `list-cookie-jar%` with `#:initiating-url` and `#:method` params
- [ ] `user-agent.rkt`: implement cross-site filtering logic in `cookies-matching` per RFC6265bis
- [ ] `user-agent.rkt`: update `cookie-jar<%>` interface contract to include new params
- [ ] `user-agent.rkt`: update `cookie-header` to accept and propagate `#:method`
- [ ] `user-agent.rkt`: implement atime update on selected cookies in `cookies-matching`
- [ ] Tests: same-site requests — all enforcement modes send cookies
- [ ] Tests: cross-site, `'strict` — cookies NOT sent
- [ ] Tests: cross-site, `'lax` — cookies sent only for safe methods on top-level nav
- [ ] Tests: cross-site, `'none` — cookies sent
- [ ] Tests: cross-site, `'default` — Lax-allowing-unsafe behaviour
- [ ] Docs: document filtering obligation on `cookies-matching` when `initiating-url` provided
- [ ] Docs: document atime update behaviour
- [ ] Docs: add `@history` note to `cookies-matching` re new params and atime obligation
- [ ] Bump version 1.3 → 2.0 and write changelog entry (highlight breaking struct changes)

---

## Post-release

- [ ] Submit PR to typed-racket repo: add `same-site-value?` to `common.rkt` and new struct field to `server.rkt`

---

## Deferred

- `parse-cookie`: allow UA to reject public suffixes (sec 5.3 of RFC6265bis)
- Parameterize cookie-jar tests over a jar-constructing thunk
