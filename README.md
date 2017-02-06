[![Build Status](https://travis-ci.org/biggis-project/soh.svg)](https://travis-ci.org/biggis-project/soh)
[![codecov.io](https://codecov.io/github/biggis-project/soh/coverage.svg?branch=master)](https://codecov.io/github/biggis-project/soh?branch=master)


## Summary
The R package **soh** ...

## How to cite
To cite properly, call the R built-in command `citation("soh")` as follows:
```r
library(soh)
citation("soh")
```

## Basic example
```r
library(soh)
# TODO ...
```

## Download and Install
To download the development version of the package, type the following at the R command line:
```r
install.packages("devtools")
devtools::install_github("biggis-project/soh", build_vignettes = TRUE)
```

### How to contribute
- Fork, clone, edit, commit, push, create pull request
- Use RStudio
- Unit-testing: press `CTRL+SHIFT+T` in RStudio
  - we know that is hard to write tests especially for a visual package like this

### Reporting bugs and other issues
If you encounter a clear bug, please file a minimal reproducible example on github.

### How to perform static code analysis and style checks
We use `lintr` which also performs the analysis on Travis-CI.
Configuration for `lintr` is in `.lintr` file.
Lints are treated as warnings, but we strive to be lint-free.

In RStudio, you can run `lintr` from the console as follows:
```r
> lintr::lint_package()
```

### Who do I talk to? ###
- [Viliam Simko](https://github.com/vsimko) (main)
- [Julian Bruns](https://github.com/JulianBruns) (backup)
