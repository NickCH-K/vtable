# vtable 1.3.2

- Fixes an issue where `format(digits = 0)` was still being used despite no longer being allowed
- Fixes the use of sample weights combined with factor variables that have missing values.

# vtable 1.3.3

- Fixes an error that occurs if a character variable with lots of different values is specified in `vars` in `sumtable`.

# vtable 1.3.4

- Fixes an issue with `haven_labelled` class variables where value labels weren't recognized in `vtable`.
- Fixes an issue where custom star markers are not recognized in `sumtable`.
- Date variables maintain class before being evaluated for `summ` in `vtable`, and also `lush = TRUE` produces medians and `nuniq()` for dates instead of mean, SD, and `nuniq`.

# vtable 1.4.1

- Fixes an issue where group headers didn't show up properly in `sumtable()` with `out = 'kable'` if run outside of **knitr**.
- Adds variable formatting options to `sumtable()`.
- Adds count and frequency columns to `labeltable()`.
- Other minor fixes.

# vtable 1.4.2

- `format` option of `sumtable()` changed to `numformat` to avoid namespace issues.
- Added `obs.function` option to `sumtable()` to make it easier to swap out the function used to count observations.
- Made it possible to use a named vector/list to change `numformat` option for only some variables in `sumtable()`, instead of having to specify every single variable.
- Updated vignettes.
- Fixed issue where default `out` option did not work running inside of `knitr` rendering to Word.
- Fixed issue with escaped characters in `kable`s.
- Added `formatfunc()` function to easily customize `numformat` options.

# vtable 1.4.3

- Fixed bug introduced by 1.4.2 relating to grouped sumtables when one group has all-missing values of a variable.
