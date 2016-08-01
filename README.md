


# stack

> Stack data type as an 'R6' class

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Linux Build Status](https://travis-ci.org/MangoTheCat/stack.svg?branch=master)](https://travis-ci.org/MangoTheCat/stack)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/MangoTheCat/stack?svg=true)](https://ci.appveyor.com/project/gaborcsardi/stack)
[![](http://www.r-pkg.org/badges/version/stack)](http://www.r-pkg.org/pkg/stack)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/stack)](http://www.r-pkg.org/pkg/stack)
[![Coverage Status](https://img.shields.io/codecov/c/github/MangoTheCat/stack/master.svg)](https://codecov.io/github/MangoTheCat/stack?branch=master)

An extremely simple stack data type, implemented with 'R6' classes. The size
of the stack increases as needed, and the amortized time complexity is O(1).
The stack may contain arbitrary objects.

## Installation


```r
source("https://install-github.me/MangoTheCat/stack")
```

## Usage


```r
library(stack)
S <- stack$new()
S$push(1L)
S$peek()
```

```
#> [1] 1
```

```r
S$pop()
```

```
#> [1] 1
```

```r
S$size()
```

```
#> [1] 0
```


```r
S$push(NULL)
S$push(iris)
colnames(S$pop())
```

```
#> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
#> [5] "Species"
```

```r
S$peek()
```

```
#> NULL
```

## License

MIT © [Mango Solutions](https://github.com/mangothecat)
