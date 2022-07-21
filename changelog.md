### 2.0.1

* Improve portability of C and C++ code.
* [Make `Lift` instance more efficient](https://github.com/haskell/text/pull/413)
* [Make `toCaseFold` idempotent](https://github.com/haskell/text/pull/402)
* [Add `fromPtr0`](https://github.com/haskell/text/pull/423)
* [Add `Data.Text.foldr'`](https://github.com/haskell/text/pull/436)
* [Add `withCString`](https://github.com/haskell/text/pull/431)
* [Add `spanM` and `spanEndM`](https://github.com/haskell/text/pull/437)

### 2.0

* [Switch internal representation of text from UTF-16 to UTF-8](https://github.com/haskell/text/pull/365):
  * Functions in `Data.Text.Array` now operate over arrays of `Word8` instead of `Word16`.
  * Rename constructors of `Array` and `MArray` to `ByteArray` and `MutableByteArray`.
  * Rename functions and types in `Data.Text.Foreign` to reflect switch
    from `Word16` to `Word8`.
  * Rename slicing functions in `Data.Text.Unsafe` to reflect switch
    from `Word16` to `Word8`.
  * Rename `Data.Text.Internal.Unsafe.Char.unsafeChr` to `unsafeChr16`.
  * Change semantics and order of arguments of `Data.Text.Array.copyI`:
    pass length, not end offset.
  * Extend `Data.Text.Internal.Encoding.Utf8` to provide more UTF-8 related routines.
  * Extend interface of `Data.Text.Array` with more utility functions.
  * Add `instance Show Data.Text.Unsafe.Iter`.
  * Add `Data.Text.measureOff`.
  * Extend `Data.Text.Unsafe` with `iterArray` and `reverseIterArray`.
  * Export `Data.Text.Internal.Lazy.equal`.
  * Export `Data.Text.Internal.append`.
  * Add `Data.Text.Internal.Private.spanAscii_`.
  * Replacement characters in `decodeUtf8With` are no longer limited to Basic Multilingual Plane.
* [Disable implicit fusion rules](https://github.com/haskell/text/pull/348)
* [Add `Data.Text.Encoding.decodeUtf8Lenient`](https://github.com/haskell/text/pull/342)
* [Remove `Data.Text.Internal.Unsafe.Shift`](https://github.com/haskell/text/pull/343)
* [Remove `Data.Text.Internal.Functions`](https://github.com/haskell/text/pull/354)
* [Bring type of `Data.Text.Unsafe.reverseIter` in line with `iter`](https://github.com/haskell/text/pull/355)
* [Add `instance Bounded FPFormat`](https://github.com/haskell/text/pull/355)
* [Add HasCallStack to partial functions](https://github.com/haskell/text/pull/388)

### 1.2.5.0

* [Support sized primitives from GHC 9.2](https://github.com/haskell/text/pull/305)
* [Allow `template-haskell-2.18.0.0`](https://github.com/haskell/text/pull/320)
* [Add `elem :: Char -> Text -> Bool` to `Data.Text` and `Data.Text.Lazy`](https://github.com/haskell/text/pull/274)
* [Replace surrogate code points in `Data.Text.Internal.Builder.{singleton,fromString}`](https://github.com/haskell/text/pull/281)
* [Use `unsafeWithForeignPtr` when available](https://github.com/haskell/text/pull/325)
* [Use vectorized CPU instructions for decoding and encoding](https://github.com/haskell/text/pull/302)
* [Regenerate case mapping in accordance to Unicode 13.0](https://github.com/haskell/text/pull/334)
* [Fix UTF-8 decoding of lazy bytestrings](https://github.com/haskell/text/pull/333)

### 1.2.4.1

* Support `template-haskell-2.17.0.0`
* Support `bytestring-0.11`
* Add `take . drop` related RULE

### 1.2.4.0

* Add TH `Lift` instances for `Data.Text.Text` and `Data.Text.Lazy.Text` (gh-232)

* Update Haddock documentation to better reflect fusion eligibility; improve fusion
  rules for `takeWhileEnd` and `length` (gh-241, ghc-202)

* Optimise `Data.Text.replicate`. Rather than calling `memcpy` `n` times,
  call it only `O(log n)` times on chunks of increasing size. The total
  asymptotic complexity remains `O(nm)`. (gh-209)

* Support `base-4.13.0.0`

### 1.2.3.1

* Make `decodeUtf8With` fail explicitly for unsupported non-BMP
  replacement characters instead silent undefined behaviour (gh-213)

* Fix termination condition for file reads via `Data.Text.IO`
  operations (gh-223)

* A serious correctness issue affecting uses of `take` and `drop` with
  negative counts has been fixed (gh-227)

* A bug in the case-mapping functions resulting in unreasonably large
  allocations with large arguments has been fixed (gh-221)

### 1.2.3.0

* Spec compliance: `toCaseFold` now follows the Unicode 9.0 spec
  (updated from 8.0).

* Bug fix: the lazy `takeWhileEnd` function violated the
  [lazy text invariant](https://github.com/bos/text/blob/1.2.3.0/Data/Text/Internal/Lazy.hs#L51)
  (gh-184).

* Bug fix: Fixed usage of size hints causing incorrect behavior (gh-197).

* New function: `unsnoc` (gh-173).

* Reduce memory overhead in `encodeUTF8` (gh-194).

* Improve UTF-8 decoder error-recovery (gh-182).

* Minor documentation improvements (`@since` annotations, more
  examples, clarifications).

#### 1.2.2.2

* The `toTitle` function now correctly handles letters that
  immediately follow punctuation. Before, `"there's"` would turn into
  `"There'S"`. Now, it becomes `"There's"`.

* The implementation of unstreaming is faster, resulting in operations
  such as `map` and `intersperse` speeding up by up to 30%, with
  smaller code generated.

* The optimised length comparison function is now more likely to be
  used after some rewrite rule tweaking.

* Bug fix: an off-by-one bug in `takeEnd` is fixed.

* Bug fix: a logic error in `takeWord16` is fixed.

#### 1.2.2.1

* The switch to `integer-pure` in 1.2.2.0 was apparently mistaken.
  The build flag has been renamed accordingly.  Your army of diligent
  maintainers apologizes for the churn.

* Spec compliance: `toCaseFold` now follows the Unicode 8.0 spec
  (updated from 7.0)

* An STG lint error has been fixed

### 1.2.2.0

* The `integer-simple` package, upon which this package optionally
  depended, has been replaced with `integer-pure`.  The build flag has
  been renamed accordingly.

* Bug fix: For the `Binary` instance, If UTF-8 decoding fails during a
  `get`, the error is propagated via `fail` instead of an uncatchable
  crash.

* New function: `takeWhileEnd`

* New instances for the `Text` types:
    * if `base` >= 4.7: `PrintfArg`
    * if `base` >= 4.9: `Semigroup`

#### 1.2.1.3

* Bug fix: As it turns out, moving the literal rewrite rules to simplifier
  phase 2 does not prevent competition with the `unpack` rule, which is
  also active in this phase. Unfortunately this was hidden due to a silly
  test environment mistake. Moving literal rules back to phase 1 finally
  fixes GHC Trac #10528 correctly.

#### 1.2.1.2

* Bug fix: Run literal rewrite rules in simplifier phase 2.
  The behavior of the simplifier changed in GHC 7.10.2,
  causing these rules to fail to fire, leading to poor code generation
  and long compilation times. See
  [GHC Trac #10528](https://ghc.haskell.org/trac/ghc/ticket/10528).

#### 1.2.1.1

* Expose unpackCString#, which you should never use.

### 1.2.1.0

* Added Binary instances for both Text types. (If you have previously
  been using the text-binary package to get a Binary instance, it is
  now obsolete.)

#### 1.2.0.6

* Fixed a space leak in UTF-8 decoding

#### 1.2.0.5

* Feature parity: repeat, cycle, iterate are now implemented for lazy
  Text, and the Data instance is more complete

* Build speed: an inliner space explosion has been fixed with toCaseFold

* Bug fix: encoding Int to a Builder would infinite-loop if the
  integer-simple package was used

* Deprecation: OnEncodeError and EncodeError are deprecated, as they
  are never used

* Internals: some types that are used internally in fusion-related
  functions have moved around, been renamed, or been deleted (we don't
  bump the major version if .Internal modules change)

* Spec compliance: toCaseFold now follows the Unicode 7.0 spec
  (updated from 6.3)

#### 1.2.0.4

* Fixed an incompatibility with base < 4.5

#### 1.2.0.3

* Update formatRealFloat to correspond to the definition in versions
  of base newer than 4.5 (https://github.com/bos/text/issues/105)

#### 1.2.0.2

* Bumped lower bound on deepseq to 1.4 for compatibility with the
  upcoming GHC 7.10

#### 1.2.0.1

* Fixed a buffer overflow in rendering of large Integers
  (https://github.com/bos/text/issues/99)

## 1.2.0.0

* Fixed an integer overflow in the replace function
  (https://github.com/bos/text/issues/81)

* Fixed a hang in lazy decodeUtf8With
  (https://github.com/bos/text/issues/87)

* Reduced codegen bloat caused by use of empty and single-character
  literals

* Added an instance of IsList for GHC 7.8 and above

### 1.1.1.0

* The Data.Data instance now allows gunfold to work, via a virtual
  pack constructor

* dropEnd, takeEnd: new functions

* Comparing the length of a Text against a number can now
  short-circuit in more cases

#### 1.1.0.1

* streamDecodeUtf8: fixed gh-70, did not return all unconsumed bytes
  in single-byte chunks

## 1.1.0.0

* encodeUtf8: Performance is improved by up to 4x.

* encodeUtf8Builder, encodeUtf8BuilderEscaped: new functions,
  available only if bytestring >= 0.10.4.0 is installed, that allow
  very fast and flexible encoding of a Text value to a bytestring
  Builder.

  As an example of the performance gain to be had, the
  encodeUtf8BuilderEscaped function helps to double the speed of JSON
  encoding in the latest version of aeson! (Note: if all you need is a
  plain ByteString, encodeUtf8 is still the faster way to go.)

* All of the internal module hierarchy is now publicly exposed.  If a
  module is in the .Internal hierarchy, or is documented as internal,
  use at your own risk - there are no API stability guarantees for
  internal modules!

#### 1.0.0.1

* decodeUtf8: Fixed a regression that caused us to incorrectly
  identify truncated UTF-8 as valid (gh-61)

# 1.0.0.0

* Added support for Unicode 6.3.0 to case conversion functions

* New function toTitle converts words in a string to title case

* New functions peekCStringLen and withCStringLen simplify
  interoperability with C functions

* Added support for decoding UTF-8 in stream-friendly fashion

* Fixed a bug in mapAccumL

* Added trusted Haskell support

* Removed support for GHC 6.10 (released in 2008) and older
