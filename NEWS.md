# EmStanS (development version)

## Changes to results

* Cut scores are now taken from the exact minimum of the ESS inconsistency
  profile. Previously the profile (weights divided by SD) was rounded to two
  decimals before the minimum was found, which created false ties and could
  pick a different cut. Across the example studies this changed 30 of 634
  grade-level cuts, mostly on theta-scale locations with a scale-score SD,
  where the rounding merged real differences. Values are still rounded to
  nine decimals before comparison, which removes floating-point noise only,
  so ties in the exact profile are still broken by the lower count, then the
  lower cut.

## Bug fixes

* The package installs on case-sensitive file systems (macOS, Linux): the
  stale `Collate` field in DESCRIPTION has been removed.
* Examinee data in the wide layout (repeated Score/Freq/Grade blocks) is
  read again.
* The item review no longer drops a grade that has no inconsistent items,
  which made `emstans()` fail for such grades.
* The item review takes level numbers from the level labels, so grades in
  which no item is aligned to some level are compared at the right level.
* Levels numbered 10 and above are parsed correctly.
* `update()` matches manual cut scores to grades by name, so argument order
  no longer matters; unknown grade names and wrong numbers of cuts are
  errors. Its first argument is now `object`, matching the generic.
* Item locations are attached to ratings by `Item_ID` rather than by row
  position.

## Input checks

* `emstans()` now stops with a clear message when the location column is
  missing, non-numeric or incomplete, when item IDs are duplicated, when SD
  or EC are not numbers, when an ALD label is not one of the grade's Level
  Options (previously it became `LevelNA`), or when a panelist did not rate
  every item in the grade exactly once.

## Performance

* ESS-Count and ESS-Weight profiles are computed with sorted cumulative sums
  instead of an items x candidates table (identical results; 30x to 3,700x
  faster for that step, depending on test length).
* Modal ALDs across panelists are computed for all items at once (identical
  results; about 1,000x faster for that step).
* End to end, typical studies run about 2x faster.

## Tests

* Added a testthat suite: hand-computed and brute-force checks of the
  Count/Weight profiles and the selection rule, equivalence of the modal ALD
  rule, input validation, `update()` by name, and a snapshot of cut scores
  on simulated data.
