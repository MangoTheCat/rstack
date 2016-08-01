


# stack

> Stack data type as an 'R6' class

[![Linux Build Status](https://travis-ci.org/MangoTheCat/stack.svg?branch=master)](https://travis-ci.org/MangoTheCat/stack)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/MangoTheCat/stack?svg=true)](https://ci.appveyor.com/project/MangoTheCat/stack)
[![](http://www.r-pkg.org/badges/version/stack)](http://www.r-pkg.org/pkg/stack)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/stack)](http://www.r-pkg.org/pkg/stack)
[![Coverage Status](https://img.shields.io/codecov/c/github/MangoTheCat/stack/master.svg)](https://codecov.io/github/MangoTheCat/stack?branch=master)

An extremely simple stack data type, implemented with 'R6' classes. The size
of the stack increases as needed, and the amortized time complexity is O(1).
The stack may contain arbitrary objects.

## Installation


```r
devtools::install_github("MangoTheCat/stack")
```

## Usage


```r
library(stack)
#' S <- stack$new()
#' S$push(1L)
#' S$peek()
#' S$pop()
#' S$size()
#'
#' S$push(NULL)
#' S$push(iris)
#' colnames(S$peek())
#' S$pop()
#' $$peek()
```

## License

MIT © [Mango Solutions](https://github.com/mangothecat)
