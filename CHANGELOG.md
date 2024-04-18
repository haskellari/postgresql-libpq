0.10.1.0
--------

- Fix issue with empty binary values (https://github.com/haskellari/postgresql-libpq/issues/54)

0.10.0.0
--------

There are technicallly two breaking changes in this release,
but they shouldn't affect anyone not doing anything weird.

- Binary parameters are passed without copying.
- FFI functions are imported without `unsafe`. Most uses were incorrect.
  We make all calls "safe", as checking whether libpq functions do IO
  or may call a notifier (potentially calling back into Haskell),
  is virtually impossible for all versions of libpq.
  (The above properties are not specified in the documentation).

0.9.5.0
-------

- Drop support for GHCs prior 8.6
- Require libpq >=10.22 (when using pkgconfig).
- Use CApiFFI for some parts of the API
  (we cannot use for all due https://gitlab.haskell.org/ghc/ghc/-/issues/22043)

0.9.4.3
-------

- Support `bytestring-0.11`
- Allow `Win32-2.10.0.0`
- Make `PQconsumeInput` FFI call `safe`

0.9.4.2
-------

- Support GHC-8.6.1
- Add simple smoke test

