cran-comments
================

<!-- cran-comments.md is generated from cran-comments.Rmd. -->

## New submission

This patch comes with the following changes:

- The different intercepts / different slopes model with pooled mean
  square error (dids.pmse) is generally also reported besides the dids
  model obtained from fitting individual linear models.
- The assessment of stability data of a single batch is now possible.
- A new parameter of the plot functions allows selection of the model to
  be displayed. On the other hand, the show_grouping parameter is
  deprecated.
- The function ggplot2::coord_cartesian() is used to prevent the loss of
  information displayed on graphs when the intervals have values beyond
  the edges of a plot.
- For specifications with two sides it is possible to perform the
  assessment for both sides in one run. The estimated shelf lives are
  reported together with the side where the shelf life is shorter.
- The release limit must be smaller or greater than the corresponding
  specification limit, after rounding to the number of significant
  figures for the specification limit.
- The determination of the relaxed shelf life and release specification
  limits was corrected.
- The cyclomatic complexity was reduced as far a possible.
- The examples were slimmed down to reduce execution time.

## Test environments

- Local:
  - Platform windows-x86_64-w64-mingw32/x64, R 4.3.0
- win-builder:
  - Platform windows-x86_64-w64-mingw32/x64, R 4.2.3
  - Platform windows-x86_64-w64-mingw32/x64, R 4.3.3
  - Platform windows-x86_64-w64-mingw32/x64, R-devel
- R-hub:
  - Platform Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Platform Fedora Linux, R-devel, clang, gfortran
  - Platform Fedora Linux, R-devel, GCC
  - Platform Debian Linux, R-devel, GCC
  - Platofrm Debian Linux, R-devel, GCC ASAN/UBSAN
  - Platform Debian Linux, R-release, GCC
  - Platform Windows Server 2022, R-devel, 64 bit
  - Platform Windows Server 2022, R-release, 32/64 bit

## R CMD check results

There were no ERRORs or WARNINGs. There were 4 NOTEs, but only on R-hub
when the testing was performed with the function
`rhub::check_for_cran()`.

- On the platforms `Ubuntu Linux 20.04.1 LTS, R-release, GCC` and
  `Fedora Linux, R-devel, clang, gfortran`, the following notes were
  reported:

      * checking HTML version of manual ... NOTE
      Skipping checking HTML validation: no command 'tidy' found
      Skipping checking math rendering: package 'V8' unavailable

- On the platform `Windows Server 2022, R-devel, 64 bit`, the following
  notes were reported:

      * checking HTML version of manual ... NOTE
      Skipping checking math rendering: package 'V8' unavailable

      * checking for non-standard things in the check directory ... NOTE
      Found the following files/directories:
        ''NULL''

      * checking for detritus in the temp directory ... NOTE
      Found the following files/directories:
        'lastMiKTeXException'

When the test at the platforms
`Ubuntu Linux 20.04.1 LTS, R-release, GCC`,
`Fedora Linux, R-devel, clang, gfortran` and
`Windows Server 2022, R-devel, 64 bit` were performed one at a time by
using the function `rhub::check()`, no ERRORs, no WARNINGs and no NOTEs
were reported.

The notes that were reported when using the `rhub::check_for_cran()`
function therefore probably can be ignored because they are not related
to this package:

- NOTE `no command 'tidy' found`: This note is related to the
  corresponding platforms. It is reported with many submissions (see
  e.g. [R-hub issue \#560](https://github.com/r-hub/rhub/issues/548)).
- NOTE `package 'V8' unavailable`: This note is related to the
  corresponding platforms where the `V8` package is not installed.
- NOTE concerning `non-standard things in the check directory`: This
  note is exclusively reported on R-hub with the platform
  `Windows Server 2022, R-devel, 64 bit` (see e.g. [R-hub issue
  \#560](https://github.com/r-hub/rhub/issues/560)).
- NOTE concerning `lastMiKTeXException`: This note is exclusively
  reported on R-hub with the platform
  `Windows Server 2022, R-devel, 64 bit` (see e.g. [R-hub issue
  \#503](https://github.com/r-hub/rhub/issues/503)).

## Downstream dependencies

There are no downstream dependencies for this package.
