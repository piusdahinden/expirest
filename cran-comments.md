cran-comments
================

<!-- cran-comments.md is generated from cran-comments.Rmd. -->

## New submission

This patch comes with the following changes:

- The check_ancova() function was modified to favor the different
  intercepts / different slopes (dids) model when ANCOVA suggests that
  the common intercepts / different slopes (cids) model is appropriate,
  as the cids model is not practically relevant.
- References to the corresponding plotting functions was added to the
  help sections of the expirest_osle() and expirest_wisle() function.
- To the help sections of all internal functions the @noRd tag was
  added.
- Remove expect_length() tests that were used for ggplot graph labels. A
  planned update of ggplot2 would affect these tests because ggplot2
  will not populate the plot\$labels field before plot building anymore.
  For this reason, the test assumptions would be violated. The tests are
  thus removed to prevent errors.

## Test environments

- Local:
  - Platform windows-x86_64-w64-mingw32/x64, R 4.4.2 (2024-10-31)
- win-builder:
  - Platform windows-x86_64-w64-mingw32/x64, R 4.4.2 (2024-10-31)
  - Platform windows-x86_64-w64-mingw32/x64, R 4.3.3 (2024-02-29)
  - Platform windows-x86_64-w64-mingw32/x64, R-devel (2025-02-23)
- R-hub v2:
  - Platform Ubuntu 24.04.1 LTS \[linux\], R-devel (2025-02-23)
  - Platform Ubuntu 22.04.5 LTS \[ubuntu-gcc12\], R-devel (2025-02-23)
  - Platform Fedora Linux 38 \[atlas-linux\], R-devel (2025-02-23)
  - Platform macOS Sonoma 14.7.2 \[m1-san\], R-devel (2025-02-23)
  - Platform macOS Sonoma 14.7.2 \[macos-arm64\], R-devel (2025-02-23)
  - Platform Ubuntu 24.04.5 LTS \[ubuntu-next-linux\], R 4.4.3 RC
    (2025-02-20)
  - Platform Ubuntu 22.04.5 LTS \[ubuntu-release-linux\], R 4.4.2
    (2024-10-31)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

There are no downstream dependencies for this package.
