cran-comments
================

<!-- cran-comments.md is generated from cran-comments.Rmd. -->

## First submission

This patch comes with the following changes:

-   The graphical output of the dids model was not appropriate because
    the prediction was based on the whole model with interaction term
    (group x time, where group is a categorical variable and time is a
    continuous variable). Now, the prediction is based on individual
    models fitted to the different levels of group.
-   Before, a decimal point was shown in case of numbers without decimal
    digits.
-   Comparisons of type class() == “string” were replaced by inherits()
    calls.

## Test environments

-   Local windows-x86_64-w64-mingw32/x64 (R 4.2.0)
-   Win-builder windows-x86_64-w64-mingw32/x64 (R 4.1.3)
-   Win-builder windows-x86_64-w64-mingw32/x64 (R 4.2.0)
-   Win-builder windows-x86_64-w64-mingw32/x64 (R-devel)
-   R-hub Windows Server 2022, 64-bit (R-devel)
-   R-hub Ubuntu Linux 20.04.1 LTS, GCC (R-release)
-   R-hub Fedora Linux, gfortran (R-devel)
-   R-hub macOS 10.13.6 High Sierra (R-release)

## R CMD check results

0 errors \| 0 warnings \| 1 note

There is one NOTE that is only found on Windows Server 2022 (64-bit,
R-devel):

    '*' checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'

As noted in [R-hub issue
\#503](https://github.com/r-hub/rhub/issues/503), this could be due to a
bug/crash in MiKTeX. It is not a problem related to expirest.

Possibly misspelled words in DESCRIPTION: the indicated words are names
or abbreviations. None of the words is misspelled.

## Downstream dependencies

There are no downstream dependencies for this package.
