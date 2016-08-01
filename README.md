


# stack

> Stack data type as an 'R6' class

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
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
S <- stack$new()
S$push(1L)
```

```
#> $data
#> function (..., list = character(), package = NULL, lib.loc = NULL, 
#>     verbose = getOption("verbose"), envir = .GlobalEnv) 
#> {
#>     fileExt <- function(x) {
#>         db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
#>         ans <- sub(".*\\.", "", x)
#>         ans[db] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", 
#>             x[db])
#>         ans
#>     }
#>     names <- c(as.character(substitute(list(...))[-1L]), list)
#>     if (!is.null(package)) {
#>         if (!is.character(package)) 
#>             stop("'package' must be a character string or NULL")
#>         if (any(package %in% "base")) 
#>             warning("datasets have been moved from package 'base' to package 'datasets'")
#>         if (any(package %in% "stats")) 
#>             warning("datasets have been moved from package 'stats' to package 'datasets'")
#>         package[package %in% c("base", "stats")] <- "datasets"
#>     }
#>     paths <- find.package(package, lib.loc, verbose = verbose)
#>     if (is.null(lib.loc)) 
#>         paths <- c(path.package(package, TRUE), if (!length(package)) getwd(), 
#>             paths)
#>     paths <- unique(normalizePath(paths[file.exists(paths)]))
#>     paths <- paths[dir.exists(file.path(paths, "data"))]
#>     dataExts <- tools:::.make_file_exts("data")
#>     if (length(names) == 0L) {
#>         db <- matrix(character(), nrow = 0L, ncol = 4L)
#>         for (path in paths) {
#>             entries <- NULL
#>             packageName <- if (file_test("-f", file.path(path, 
#>                 "DESCRIPTION"))) 
#>                 basename(path)
#>             else "."
#>             if (file_test("-f", INDEX <- file.path(path, "Meta", 
#>                 "data.rds"))) {
#>                 entries <- readRDS(INDEX)
#>             }
#>             else {
#>                 dataDir <- file.path(path, "data")
#>                 entries <- tools::list_files_with_type(dataDir, 
#>                   "data")
#>                 if (length(entries)) {
#>                   entries <- unique(tools::file_path_sans_ext(basename(entries)))
#>                   entries <- cbind(entries, "")
#>                 }
#>             }
#>             if (NROW(entries)) {
#>                 if (is.matrix(entries) && ncol(entries) == 2L) 
#>                   db <- rbind(db, cbind(packageName, dirname(path), 
#>                     entries))
#>                 else warning(gettextf("data index for package %s is invalid and will be ignored", 
#>                   sQuote(packageName)), domain = NA, call. = FALSE)
#>             }
#>         }
#>         colnames(db) <- c("Package", "LibPath", "Item", "Title")
#>         footer <- if (missing(package)) 
#>             paste0("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), 
#>                 "\n", "to list the data sets in all *available* packages.")
#>         else NULL
#>         y <- list(title = "Data sets", header = NULL, results = db, 
#>             footer = footer)
#>         class(y) <- "packageIQR"
#>         return(y)
#>     }
#>     paths <- file.path(paths, "data")
#>     for (name in names) {
#>         found <- FALSE
#>         for (p in paths) {
#>             if (file_test("-f", file.path(p, "Rdata.rds"))) {
#>                 rds <- readRDS(file.path(p, "Rdata.rds"))
#>                 if (name %in% names(rds)) {
#>                   found <- TRUE
#>                   if (verbose) 
#>                     message(sprintf("name=%s:\t found in Rdata.rds", 
#>                       name), domain = NA)
#>                   thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
#>                   thispkg <- sub("_.*$", "", thispkg)
#>                   thispkg <- paste0("package:", thispkg)
#>                   objs <- rds[[name]]
#>                   lazyLoad(file.path(p, "Rdata"), envir = envir, 
#>                     filter = function(x) x %in% objs)
#>                   break
#>                 }
#>                 else if (verbose) 
#>                   message(sprintf("name=%s:\t NOT found in names() of Rdata.rds, i.e.,\n\t%s\n", 
#>                     name, paste(names(rds), collapse = ",")), 
#>                     domain = NA)
#>             }
#>             if (file_test("-f", file.path(p, "Rdata.zip"))) {
#>                 warning("zipped data found for package ", sQuote(basename(dirname(p))), 
#>                   ".\nThat is defunct, so please re-install the package.", 
#>                   domain = NA)
#>                 if (file_test("-f", fp <- file.path(p, "filelist"))) 
#>                   files <- file.path(p, scan(fp, what = "", quiet = TRUE))
#>                 else {
#>                   warning(gettextf("file 'filelist' is missing for directory %s", 
#>                     sQuote(p)), domain = NA)
#>                   next
#>                 }
#>             }
#>             else {
#>                 files <- list.files(p, full.names = TRUE)
#>             }
#>             files <- files[grep(name, files, fixed = TRUE)]
#>             if (length(files) > 1L) {
#>                 o <- match(fileExt(files), dataExts, nomatch = 100L)
#>                 paths0 <- dirname(files)
#>                 paths0 <- factor(paths0, levels = unique(paths0))
#>                 files <- files[order(paths0, o)]
#>             }
#>             if (length(files)) {
#>                 for (file in files) {
#>                   if (verbose) 
#>                     message("name=", name, ":\t file= ...", .Platform$file.sep, 
#>                       basename(file), "::\t", appendLF = FALSE, 
#>                       domain = NA)
#>                   ext <- fileExt(file)
#>                   if (basename(file) != paste0(name, ".", ext)) 
#>                     found <- FALSE
#>                   else {
#>                     found <- TRUE
#>                     zfile <- file
#>                     zipname <- file.path(dirname(file), "Rdata.zip")
#>                     if (file.exists(zipname)) {
#>                       Rdatadir <- tempfile("Rdata")
#>                       dir.create(Rdatadir, showWarnings = FALSE)
#>                       topic <- basename(file)
#>                       rc <- .External(C_unzip, zipname, topic, 
#>                         Rdatadir, FALSE, TRUE, FALSE, FALSE)
#>                       if (rc == 0L) 
#>                         zfile <- file.path(Rdatadir, topic)
#>                     }
#>                     if (zfile != file) 
#>                       on.exit(unlink(zfile))
#>                     switch(ext, R = , r = {
#>                       library("utils")
#>                       sys.source(zfile, chdir = TRUE, envir = envir)
#>                     }, RData = , rdata = , rda = load(zfile, 
#>                       envir = envir), TXT = , txt = , tab = , 
#>                       tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , 
#>                       txt.bz2 = , txt.xz = assign(name, read.table(zfile, 
#>                         header = TRUE, as.is = FALSE), envir = envir), 
#>                       CSV = , csv = , csv.gz = , csv.bz2 = , 
#>                       csv.xz = assign(name, read.table(zfile, 
#>                         header = TRUE, sep = ";", as.is = FALSE), 
#>                         envir = envir), found <- FALSE)
#>                   }
#>                   if (found) 
#>                     break
#>                 }
#>                 if (verbose) 
#>                   message(if (!found) 
#>                     "*NOT* ", "found", domain = NA)
#>             }
#>             if (found) 
#>                 break
#>         }
#>         if (!found) 
#>             warning(gettextf("data set %s not found", sQuote(name)), 
#>                 domain = NA)
#>     }
#>     invisible(names)
#> }
#> <bytecode: 0x7fad05ae0b08>
#> <environment: namespace:utils>
#> 
#> $res
#> [1] 1
```

```r
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
```

```
#> $data
#> function (..., list = character(), package = NULL, lib.loc = NULL, 
#>     verbose = getOption("verbose"), envir = .GlobalEnv) 
#> {
#>     fileExt <- function(x) {
#>         db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
#>         ans <- sub(".*\\.", "", x)
#>         ans[db] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", 
#>             x[db])
#>         ans
#>     }
#>     names <- c(as.character(substitute(list(...))[-1L]), list)
#>     if (!is.null(package)) {
#>         if (!is.character(package)) 
#>             stop("'package' must be a character string or NULL")
#>         if (any(package %in% "base")) 
#>             warning("datasets have been moved from package 'base' to package 'datasets'")
#>         if (any(package %in% "stats")) 
#>             warning("datasets have been moved from package 'stats' to package 'datasets'")
#>         package[package %in% c("base", "stats")] <- "datasets"
#>     }
#>     paths <- find.package(package, lib.loc, verbose = verbose)
#>     if (is.null(lib.loc)) 
#>         paths <- c(path.package(package, TRUE), if (!length(package)) getwd(), 
#>             paths)
#>     paths <- unique(normalizePath(paths[file.exists(paths)]))
#>     paths <- paths[dir.exists(file.path(paths, "data"))]
#>     dataExts <- tools:::.make_file_exts("data")
#>     if (length(names) == 0L) {
#>         db <- matrix(character(), nrow = 0L, ncol = 4L)
#>         for (path in paths) {
#>             entries <- NULL
#>             packageName <- if (file_test("-f", file.path(path, 
#>                 "DESCRIPTION"))) 
#>                 basename(path)
#>             else "."
#>             if (file_test("-f", INDEX <- file.path(path, "Meta", 
#>                 "data.rds"))) {
#>                 entries <- readRDS(INDEX)
#>             }
#>             else {
#>                 dataDir <- file.path(path, "data")
#>                 entries <- tools::list_files_with_type(dataDir, 
#>                   "data")
#>                 if (length(entries)) {
#>                   entries <- unique(tools::file_path_sans_ext(basename(entries)))
#>                   entries <- cbind(entries, "")
#>                 }
#>             }
#>             if (NROW(entries)) {
#>                 if (is.matrix(entries) && ncol(entries) == 2L) 
#>                   db <- rbind(db, cbind(packageName, dirname(path), 
#>                     entries))
#>                 else warning(gettextf("data index for package %s is invalid and will be ignored", 
#>                   sQuote(packageName)), domain = NA, call. = FALSE)
#>             }
#>         }
#>         colnames(db) <- c("Package", "LibPath", "Item", "Title")
#>         footer <- if (missing(package)) 
#>             paste0("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), 
#>                 "\n", "to list the data sets in all *available* packages.")
#>         else NULL
#>         y <- list(title = "Data sets", header = NULL, results = db, 
#>             footer = footer)
#>         class(y) <- "packageIQR"
#>         return(y)
#>     }
#>     paths <- file.path(paths, "data")
#>     for (name in names) {
#>         found <- FALSE
#>         for (p in paths) {
#>             if (file_test("-f", file.path(p, "Rdata.rds"))) {
#>                 rds <- readRDS(file.path(p, "Rdata.rds"))
#>                 if (name %in% names(rds)) {
#>                   found <- TRUE
#>                   if (verbose) 
#>                     message(sprintf("name=%s:\t found in Rdata.rds", 
#>                       name), domain = NA)
#>                   thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
#>                   thispkg <- sub("_.*$", "", thispkg)
#>                   thispkg <- paste0("package:", thispkg)
#>                   objs <- rds[[name]]
#>                   lazyLoad(file.path(p, "Rdata"), envir = envir, 
#>                     filter = function(x) x %in% objs)
#>                   break
#>                 }
#>                 else if (verbose) 
#>                   message(sprintf("name=%s:\t NOT found in names() of Rdata.rds, i.e.,\n\t%s\n", 
#>                     name, paste(names(rds), collapse = ",")), 
#>                     domain = NA)
#>             }
#>             if (file_test("-f", file.path(p, "Rdata.zip"))) {
#>                 warning("zipped data found for package ", sQuote(basename(dirname(p))), 
#>                   ".\nThat is defunct, so please re-install the package.", 
#>                   domain = NA)
#>                 if (file_test("-f", fp <- file.path(p, "filelist"))) 
#>                   files <- file.path(p, scan(fp, what = "", quiet = TRUE))
#>                 else {
#>                   warning(gettextf("file 'filelist' is missing for directory %s", 
#>                     sQuote(p)), domain = NA)
#>                   next
#>                 }
#>             }
#>             else {
#>                 files <- list.files(p, full.names = TRUE)
#>             }
#>             files <- files[grep(name, files, fixed = TRUE)]
#>             if (length(files) > 1L) {
#>                 o <- match(fileExt(files), dataExts, nomatch = 100L)
#>                 paths0 <- dirname(files)
#>                 paths0 <- factor(paths0, levels = unique(paths0))
#>                 files <- files[order(paths0, o)]
#>             }
#>             if (length(files)) {
#>                 for (file in files) {
#>                   if (verbose) 
#>                     message("name=", name, ":\t file= ...", .Platform$file.sep, 
#>                       basename(file), "::\t", appendLF = FALSE, 
#>                       domain = NA)
#>                   ext <- fileExt(file)
#>                   if (basename(file) != paste0(name, ".", ext)) 
#>                     found <- FALSE
#>                   else {
#>                     found <- TRUE
#>                     zfile <- file
#>                     zipname <- file.path(dirname(file), "Rdata.zip")
#>                     if (file.exists(zipname)) {
#>                       Rdatadir <- tempfile("Rdata")
#>                       dir.create(Rdatadir, showWarnings = FALSE)
#>                       topic <- basename(file)
#>                       rc <- .External(C_unzip, zipname, topic, 
#>                         Rdatadir, FALSE, TRUE, FALSE, FALSE)
#>                       if (rc == 0L) 
#>                         zfile <- file.path(Rdatadir, topic)
#>                     }
#>                     if (zfile != file) 
#>                       on.exit(unlink(zfile))
#>                     switch(ext, R = , r = {
#>                       library("utils")
#>                       sys.source(zfile, chdir = TRUE, envir = envir)
#>                     }, RData = , rdata = , rda = load(zfile, 
#>                       envir = envir), TXT = , txt = , tab = , 
#>                       tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , 
#>                       txt.bz2 = , txt.xz = assign(name, read.table(zfile, 
#>                         header = TRUE, as.is = FALSE), envir = envir), 
#>                       CSV = , csv = , csv.gz = , csv.bz2 = , 
#>                       csv.xz = assign(name, read.table(zfile, 
#>                         header = TRUE, sep = ";", as.is = FALSE), 
#>                         envir = envir), found <- FALSE)
#>                   }
#>                   if (found) 
#>                     break
#>                 }
#>                 if (verbose) 
#>                   message(if (!found) 
#>                     "*NOT* ", "found", domain = NA)
#>             }
#>             if (found) 
#>                 break
#>         }
#>         if (!found) 
#>             warning(gettextf("data set %s not found", sQuote(name)), 
#>                 domain = NA)
#>     }
#>     invisible(names)
#> }
#> <bytecode: 0x7fad05ae0b08>
#> <environment: namespace:utils>
#> 
#> $res
#> NULL
```

```r
S$push(iris)
```

```
#> $data
#> function (..., list = character(), package = NULL, lib.loc = NULL, 
#>     verbose = getOption("verbose"), envir = .GlobalEnv) 
#> {
#>     fileExt <- function(x) {
#>         db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
#>         ans <- sub(".*\\.", "", x)
#>         ans[db] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", 
#>             x[db])
#>         ans
#>     }
#>     names <- c(as.character(substitute(list(...))[-1L]), list)
#>     if (!is.null(package)) {
#>         if (!is.character(package)) 
#>             stop("'package' must be a character string or NULL")
#>         if (any(package %in% "base")) 
#>             warning("datasets have been moved from package 'base' to package 'datasets'")
#>         if (any(package %in% "stats")) 
#>             warning("datasets have been moved from package 'stats' to package 'datasets'")
#>         package[package %in% c("base", "stats")] <- "datasets"
#>     }
#>     paths <- find.package(package, lib.loc, verbose = verbose)
#>     if (is.null(lib.loc)) 
#>         paths <- c(path.package(package, TRUE), if (!length(package)) getwd(), 
#>             paths)
#>     paths <- unique(normalizePath(paths[file.exists(paths)]))
#>     paths <- paths[dir.exists(file.path(paths, "data"))]
#>     dataExts <- tools:::.make_file_exts("data")
#>     if (length(names) == 0L) {
#>         db <- matrix(character(), nrow = 0L, ncol = 4L)
#>         for (path in paths) {
#>             entries <- NULL
#>             packageName <- if (file_test("-f", file.path(path, 
#>                 "DESCRIPTION"))) 
#>                 basename(path)
#>             else "."
#>             if (file_test("-f", INDEX <- file.path(path, "Meta", 
#>                 "data.rds"))) {
#>                 entries <- readRDS(INDEX)
#>             }
#>             else {
#>                 dataDir <- file.path(path, "data")
#>                 entries <- tools::list_files_with_type(dataDir, 
#>                   "data")
#>                 if (length(entries)) {
#>                   entries <- unique(tools::file_path_sans_ext(basename(entries)))
#>                   entries <- cbind(entries, "")
#>                 }
#>             }
#>             if (NROW(entries)) {
#>                 if (is.matrix(entries) && ncol(entries) == 2L) 
#>                   db <- rbind(db, cbind(packageName, dirname(path), 
#>                     entries))
#>                 else warning(gettextf("data index for package %s is invalid and will be ignored", 
#>                   sQuote(packageName)), domain = NA, call. = FALSE)
#>             }
#>         }
#>         colnames(db) <- c("Package", "LibPath", "Item", "Title")
#>         footer <- if (missing(package)) 
#>             paste0("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), 
#>                 "\n", "to list the data sets in all *available* packages.")
#>         else NULL
#>         y <- list(title = "Data sets", header = NULL, results = db, 
#>             footer = footer)
#>         class(y) <- "packageIQR"
#>         return(y)
#>     }
#>     paths <- file.path(paths, "data")
#>     for (name in names) {
#>         found <- FALSE
#>         for (p in paths) {
#>             if (file_test("-f", file.path(p, "Rdata.rds"))) {
#>                 rds <- readRDS(file.path(p, "Rdata.rds"))
#>                 if (name %in% names(rds)) {
#>                   found <- TRUE
#>                   if (verbose) 
#>                     message(sprintf("name=%s:\t found in Rdata.rds", 
#>                       name), domain = NA)
#>                   thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
#>                   thispkg <- sub("_.*$", "", thispkg)
#>                   thispkg <- paste0("package:", thispkg)
#>                   objs <- rds[[name]]
#>                   lazyLoad(file.path(p, "Rdata"), envir = envir, 
#>                     filter = function(x) x %in% objs)
#>                   break
#>                 }
#>                 else if (verbose) 
#>                   message(sprintf("name=%s:\t NOT found in names() of Rdata.rds, i.e.,\n\t%s\n", 
#>                     name, paste(names(rds), collapse = ",")), 
#>                     domain = NA)
#>             }
#>             if (file_test("-f", file.path(p, "Rdata.zip"))) {
#>                 warning("zipped data found for package ", sQuote(basename(dirname(p))), 
#>                   ".\nThat is defunct, so please re-install the package.", 
#>                   domain = NA)
#>                 if (file_test("-f", fp <- file.path(p, "filelist"))) 
#>                   files <- file.path(p, scan(fp, what = "", quiet = TRUE))
#>                 else {
#>                   warning(gettextf("file 'filelist' is missing for directory %s", 
#>                     sQuote(p)), domain = NA)
#>                   next
#>                 }
#>             }
#>             else {
#>                 files <- list.files(p, full.names = TRUE)
#>             }
#>             files <- files[grep(name, files, fixed = TRUE)]
#>             if (length(files) > 1L) {
#>                 o <- match(fileExt(files), dataExts, nomatch = 100L)
#>                 paths0 <- dirname(files)
#>                 paths0 <- factor(paths0, levels = unique(paths0))
#>                 files <- files[order(paths0, o)]
#>             }
#>             if (length(files)) {
#>                 for (file in files) {
#>                   if (verbose) 
#>                     message("name=", name, ":\t file= ...", .Platform$file.sep, 
#>                       basename(file), "::\t", appendLF = FALSE, 
#>                       domain = NA)
#>                   ext <- fileExt(file)
#>                   if (basename(file) != paste0(name, ".", ext)) 
#>                     found <- FALSE
#>                   else {
#>                     found <- TRUE
#>                     zfile <- file
#>                     zipname <- file.path(dirname(file), "Rdata.zip")
#>                     if (file.exists(zipname)) {
#>                       Rdatadir <- tempfile("Rdata")
#>                       dir.create(Rdatadir, showWarnings = FALSE)
#>                       topic <- basename(file)
#>                       rc <- .External(C_unzip, zipname, topic, 
#>                         Rdatadir, FALSE, TRUE, FALSE, FALSE)
#>                       if (rc == 0L) 
#>                         zfile <- file.path(Rdatadir, topic)
#>                     }
#>                     if (zfile != file) 
#>                       on.exit(unlink(zfile))
#>                     switch(ext, R = , r = {
#>                       library("utils")
#>                       sys.source(zfile, chdir = TRUE, envir = envir)
#>                     }, RData = , rdata = , rda = load(zfile, 
#>                       envir = envir), TXT = , txt = , tab = , 
#>                       tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , 
#>                       txt.bz2 = , txt.xz = assign(name, read.table(zfile, 
#>                         header = TRUE, as.is = FALSE), envir = envir), 
#>                       CSV = , csv = , csv.gz = , csv.bz2 = , 
#>                       csv.xz = assign(name, read.table(zfile, 
#>                         header = TRUE, sep = ";", as.is = FALSE), 
#>                         envir = envir), found <- FALSE)
#>                   }
#>                   if (found) 
#>                     break
#>                 }
#>                 if (verbose) 
#>                   message(if (!found) 
#>                     "*NOT* ", "found", domain = NA)
#>             }
#>             if (found) 
#>                 break
#>         }
#>         if (!found) 
#>             warning(gettextf("data set %s not found", sQuote(name)), 
#>                 domain = NA)
#>     }
#>     invisible(names)
#> }
#> <bytecode: 0x7fad05ae0b08>
#> <environment: namespace:utils>
#> 
#> $res
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1            5.1         3.5          1.4         0.2     setosa
#> 2            4.9         3.0          1.4         0.2     setosa
#> 3            4.7         3.2          1.3         0.2     setosa
#> 4            4.6         3.1          1.5         0.2     setosa
#> 5            5.0         3.6          1.4         0.2     setosa
#> 6            5.4         3.9          1.7         0.4     setosa
#> 7            4.6         3.4          1.4         0.3     setosa
#> 8            5.0         3.4          1.5         0.2     setosa
#> 9            4.4         2.9          1.4         0.2     setosa
#> 10           4.9         3.1          1.5         0.1     setosa
#> 11           5.4         3.7          1.5         0.2     setosa
#> 12           4.8         3.4          1.6         0.2     setosa
#> 13           4.8         3.0          1.4         0.1     setosa
#> 14           4.3         3.0          1.1         0.1     setosa
#> 15           5.8         4.0          1.2         0.2     setosa
#> 16           5.7         4.4          1.5         0.4     setosa
#> 17           5.4         3.9          1.3         0.4     setosa
#> 18           5.1         3.5          1.4         0.3     setosa
#> 19           5.7         3.8          1.7         0.3     setosa
#> 20           5.1         3.8          1.5         0.3     setosa
#> 21           5.4         3.4          1.7         0.2     setosa
#> 22           5.1         3.7          1.5         0.4     setosa
#> 23           4.6         3.6          1.0         0.2     setosa
#> 24           5.1         3.3          1.7         0.5     setosa
#> 25           4.8         3.4          1.9         0.2     setosa
#> 26           5.0         3.0          1.6         0.2     setosa
#> 27           5.0         3.4          1.6         0.4     setosa
#> 28           5.2         3.5          1.5         0.2     setosa
#> 29           5.2         3.4          1.4         0.2     setosa
#> 30           4.7         3.2          1.6         0.2     setosa
#> 31           4.8         3.1          1.6         0.2     setosa
#> 32           5.4         3.4          1.5         0.4     setosa
#> 33           5.2         4.1          1.5         0.1     setosa
#> 34           5.5         4.2          1.4         0.2     setosa
#> 35           4.9         3.1          1.5         0.2     setosa
#> 36           5.0         3.2          1.2         0.2     setosa
#> 37           5.5         3.5          1.3         0.2     setosa
#> 38           4.9         3.6          1.4         0.1     setosa
#> 39           4.4         3.0          1.3         0.2     setosa
#> 40           5.1         3.4          1.5         0.2     setosa
#> 41           5.0         3.5          1.3         0.3     setosa
#> 42           4.5         2.3          1.3         0.3     setosa
#> 43           4.4         3.2          1.3         0.2     setosa
#> 44           5.0         3.5          1.6         0.6     setosa
#> 45           5.1         3.8          1.9         0.4     setosa
#> 46           4.8         3.0          1.4         0.3     setosa
#> 47           5.1         3.8          1.6         0.2     setosa
#> 48           4.6         3.2          1.4         0.2     setosa
#> 49           5.3         3.7          1.5         0.2     setosa
#> 50           5.0         3.3          1.4         0.2     setosa
#> 51           7.0         3.2          4.7         1.4 versicolor
#> 52           6.4         3.2          4.5         1.5 versicolor
#> 53           6.9         3.1          4.9         1.5 versicolor
#> 54           5.5         2.3          4.0         1.3 versicolor
#> 55           6.5         2.8          4.6         1.5 versicolor
#> 56           5.7         2.8          4.5         1.3 versicolor
#> 57           6.3         3.3          4.7         1.6 versicolor
#> 58           4.9         2.4          3.3         1.0 versicolor
#> 59           6.6         2.9          4.6         1.3 versicolor
#> 60           5.2         2.7          3.9         1.4 versicolor
#> 61           5.0         2.0          3.5         1.0 versicolor
#> 62           5.9         3.0          4.2         1.5 versicolor
#> 63           6.0         2.2          4.0         1.0 versicolor
#> 64           6.1         2.9          4.7         1.4 versicolor
#> 65           5.6         2.9          3.6         1.3 versicolor
#> 66           6.7         3.1          4.4         1.4 versicolor
#> 67           5.6         3.0          4.5         1.5 versicolor
#> 68           5.8         2.7          4.1         1.0 versicolor
#> 69           6.2         2.2          4.5         1.5 versicolor
#> 70           5.6         2.5          3.9         1.1 versicolor
#> 71           5.9         3.2          4.8         1.8 versicolor
#> 72           6.1         2.8          4.0         1.3 versicolor
#> 73           6.3         2.5          4.9         1.5 versicolor
#> 74           6.1         2.8          4.7         1.2 versicolor
#> 75           6.4         2.9          4.3         1.3 versicolor
#> 76           6.6         3.0          4.4         1.4 versicolor
#> 77           6.8         2.8          4.8         1.4 versicolor
#> 78           6.7         3.0          5.0         1.7 versicolor
#> 79           6.0         2.9          4.5         1.5 versicolor
#> 80           5.7         2.6          3.5         1.0 versicolor
#> 81           5.5         2.4          3.8         1.1 versicolor
#> 82           5.5         2.4          3.7         1.0 versicolor
#> 83           5.8         2.7          3.9         1.2 versicolor
#> 84           6.0         2.7          5.1         1.6 versicolor
#> 85           5.4         3.0          4.5         1.5 versicolor
#> 86           6.0         3.4          4.5         1.6 versicolor
#> 87           6.7         3.1          4.7         1.5 versicolor
#> 88           6.3         2.3          4.4         1.3 versicolor
#> 89           5.6         3.0          4.1         1.3 versicolor
#> 90           5.5         2.5          4.0         1.3 versicolor
#> 91           5.5         2.6          4.4         1.2 versicolor
#> 92           6.1         3.0          4.6         1.4 versicolor
#> 93           5.8         2.6          4.0         1.2 versicolor
#> 94           5.0         2.3          3.3         1.0 versicolor
#> 95           5.6         2.7          4.2         1.3 versicolor
#> 96           5.7         3.0          4.2         1.2 versicolor
#> 97           5.7         2.9          4.2         1.3 versicolor
#> 98           6.2         2.9          4.3         1.3 versicolor
#> 99           5.1         2.5          3.0         1.1 versicolor
#> 100          5.7         2.8          4.1         1.3 versicolor
#> 101          6.3         3.3          6.0         2.5  virginica
#> 102          5.8         2.7          5.1         1.9  virginica
#> 103          7.1         3.0          5.9         2.1  virginica
#> 104          6.3         2.9          5.6         1.8  virginica
#> 105          6.5         3.0          5.8         2.2  virginica
#> 106          7.6         3.0          6.6         2.1  virginica
#> 107          4.9         2.5          4.5         1.7  virginica
#> 108          7.3         2.9          6.3         1.8  virginica
#> 109          6.7         2.5          5.8         1.8  virginica
#> 110          7.2         3.6          6.1         2.5  virginica
#> 111          6.5         3.2          5.1         2.0  virginica
#> 112          6.4         2.7          5.3         1.9  virginica
#> 113          6.8         3.0          5.5         2.1  virginica
#> 114          5.7         2.5          5.0         2.0  virginica
#> 115          5.8         2.8          5.1         2.4  virginica
#> 116          6.4         3.2          5.3         2.3  virginica
#> 117          6.5         3.0          5.5         1.8  virginica
#> 118          7.7         3.8          6.7         2.2  virginica
#> 119          7.7         2.6          6.9         2.3  virginica
#> 120          6.0         2.2          5.0         1.5  virginica
#> 121          6.9         3.2          5.7         2.3  virginica
#> 122          5.6         2.8          4.9         2.0  virginica
#> 123          7.7         2.8          6.7         2.0  virginica
#> 124          6.3         2.7          4.9         1.8  virginica
#> 125          6.7         3.3          5.7         2.1  virginica
#> 126          7.2         3.2          6.0         1.8  virginica
#> 127          6.2         2.8          4.8         1.8  virginica
#> 128          6.1         3.0          4.9         1.8  virginica
#> 129          6.4         2.8          5.6         2.1  virginica
#> 130          7.2         3.0          5.8         1.6  virginica
#> 131          7.4         2.8          6.1         1.9  virginica
#> 132          7.9         3.8          6.4         2.0  virginica
#> 133          6.4         2.8          5.6         2.2  virginica
#> 134          6.3         2.8          5.1         1.5  virginica
#> 135          6.1         2.6          5.6         1.4  virginica
#> 136          7.7         3.0          6.1         2.3  virginica
#> 137          6.3         3.4          5.6         2.4  virginica
#> 138          6.4         3.1          5.5         1.8  virginica
#> 139          6.0         3.0          4.8         1.8  virginica
#> 140          6.9         3.1          5.4         2.1  virginica
#> 141          6.7         3.1          5.6         2.4  virginica
#> 142          6.9         3.1          5.1         2.3  virginica
#> 143          5.8         2.7          5.1         1.9  virginica
#> 144          6.8         3.2          5.9         2.3  virginica
#> 145          6.7         3.3          5.7         2.5  virginica
#> 146          6.7         3.0          5.2         2.3  virginica
#> 147          6.3         2.5          5.0         1.9  virginica
#> 148          6.5         3.0          5.2         2.0  virginica
#> 149          6.2         3.4          5.4         2.3  virginica
#> 150          5.9         3.0          5.1         1.8  virginica
```

```r
colnames(S$peek())
```

```
#> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
#> [5] "Species"
```

```r
S$pop()
```

```
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1            5.1         3.5          1.4         0.2     setosa
#> 2            4.9         3.0          1.4         0.2     setosa
#> 3            4.7         3.2          1.3         0.2     setosa
#> 4            4.6         3.1          1.5         0.2     setosa
#> 5            5.0         3.6          1.4         0.2     setosa
#> 6            5.4         3.9          1.7         0.4     setosa
#> 7            4.6         3.4          1.4         0.3     setosa
#> 8            5.0         3.4          1.5         0.2     setosa
#> 9            4.4         2.9          1.4         0.2     setosa
#> 10           4.9         3.1          1.5         0.1     setosa
#> 11           5.4         3.7          1.5         0.2     setosa
#> 12           4.8         3.4          1.6         0.2     setosa
#> 13           4.8         3.0          1.4         0.1     setosa
#> 14           4.3         3.0          1.1         0.1     setosa
#> 15           5.8         4.0          1.2         0.2     setosa
#> 16           5.7         4.4          1.5         0.4     setosa
#> 17           5.4         3.9          1.3         0.4     setosa
#> 18           5.1         3.5          1.4         0.3     setosa
#> 19           5.7         3.8          1.7         0.3     setosa
#> 20           5.1         3.8          1.5         0.3     setosa
#> 21           5.4         3.4          1.7         0.2     setosa
#> 22           5.1         3.7          1.5         0.4     setosa
#> 23           4.6         3.6          1.0         0.2     setosa
#> 24           5.1         3.3          1.7         0.5     setosa
#> 25           4.8         3.4          1.9         0.2     setosa
#> 26           5.0         3.0          1.6         0.2     setosa
#> 27           5.0         3.4          1.6         0.4     setosa
#> 28           5.2         3.5          1.5         0.2     setosa
#> 29           5.2         3.4          1.4         0.2     setosa
#> 30           4.7         3.2          1.6         0.2     setosa
#> 31           4.8         3.1          1.6         0.2     setosa
#> 32           5.4         3.4          1.5         0.4     setosa
#> 33           5.2         4.1          1.5         0.1     setosa
#> 34           5.5         4.2          1.4         0.2     setosa
#> 35           4.9         3.1          1.5         0.2     setosa
#> 36           5.0         3.2          1.2         0.2     setosa
#> 37           5.5         3.5          1.3         0.2     setosa
#> 38           4.9         3.6          1.4         0.1     setosa
#> 39           4.4         3.0          1.3         0.2     setosa
#> 40           5.1         3.4          1.5         0.2     setosa
#> 41           5.0         3.5          1.3         0.3     setosa
#> 42           4.5         2.3          1.3         0.3     setosa
#> 43           4.4         3.2          1.3         0.2     setosa
#> 44           5.0         3.5          1.6         0.6     setosa
#> 45           5.1         3.8          1.9         0.4     setosa
#> 46           4.8         3.0          1.4         0.3     setosa
#> 47           5.1         3.8          1.6         0.2     setosa
#> 48           4.6         3.2          1.4         0.2     setosa
#> 49           5.3         3.7          1.5         0.2     setosa
#> 50           5.0         3.3          1.4         0.2     setosa
#> 51           7.0         3.2          4.7         1.4 versicolor
#> 52           6.4         3.2          4.5         1.5 versicolor
#> 53           6.9         3.1          4.9         1.5 versicolor
#> 54           5.5         2.3          4.0         1.3 versicolor
#> 55           6.5         2.8          4.6         1.5 versicolor
#> 56           5.7         2.8          4.5         1.3 versicolor
#> 57           6.3         3.3          4.7         1.6 versicolor
#> 58           4.9         2.4          3.3         1.0 versicolor
#> 59           6.6         2.9          4.6         1.3 versicolor
#> 60           5.2         2.7          3.9         1.4 versicolor
#> 61           5.0         2.0          3.5         1.0 versicolor
#> 62           5.9         3.0          4.2         1.5 versicolor
#> 63           6.0         2.2          4.0         1.0 versicolor
#> 64           6.1         2.9          4.7         1.4 versicolor
#> 65           5.6         2.9          3.6         1.3 versicolor
#> 66           6.7         3.1          4.4         1.4 versicolor
#> 67           5.6         3.0          4.5         1.5 versicolor
#> 68           5.8         2.7          4.1         1.0 versicolor
#> 69           6.2         2.2          4.5         1.5 versicolor
#> 70           5.6         2.5          3.9         1.1 versicolor
#> 71           5.9         3.2          4.8         1.8 versicolor
#> 72           6.1         2.8          4.0         1.3 versicolor
#> 73           6.3         2.5          4.9         1.5 versicolor
#> 74           6.1         2.8          4.7         1.2 versicolor
#> 75           6.4         2.9          4.3         1.3 versicolor
#> 76           6.6         3.0          4.4         1.4 versicolor
#> 77           6.8         2.8          4.8         1.4 versicolor
#> 78           6.7         3.0          5.0         1.7 versicolor
#> 79           6.0         2.9          4.5         1.5 versicolor
#> 80           5.7         2.6          3.5         1.0 versicolor
#> 81           5.5         2.4          3.8         1.1 versicolor
#> 82           5.5         2.4          3.7         1.0 versicolor
#> 83           5.8         2.7          3.9         1.2 versicolor
#> 84           6.0         2.7          5.1         1.6 versicolor
#> 85           5.4         3.0          4.5         1.5 versicolor
#> 86           6.0         3.4          4.5         1.6 versicolor
#> 87           6.7         3.1          4.7         1.5 versicolor
#> 88           6.3         2.3          4.4         1.3 versicolor
#> 89           5.6         3.0          4.1         1.3 versicolor
#> 90           5.5         2.5          4.0         1.3 versicolor
#> 91           5.5         2.6          4.4         1.2 versicolor
#> 92           6.1         3.0          4.6         1.4 versicolor
#> 93           5.8         2.6          4.0         1.2 versicolor
#> 94           5.0         2.3          3.3         1.0 versicolor
#> 95           5.6         2.7          4.2         1.3 versicolor
#> 96           5.7         3.0          4.2         1.2 versicolor
#> 97           5.7         2.9          4.2         1.3 versicolor
#> 98           6.2         2.9          4.3         1.3 versicolor
#> 99           5.1         2.5          3.0         1.1 versicolor
#> 100          5.7         2.8          4.1         1.3 versicolor
#> 101          6.3         3.3          6.0         2.5  virginica
#> 102          5.8         2.7          5.1         1.9  virginica
#> 103          7.1         3.0          5.9         2.1  virginica
#> 104          6.3         2.9          5.6         1.8  virginica
#> 105          6.5         3.0          5.8         2.2  virginica
#> 106          7.6         3.0          6.6         2.1  virginica
#> 107          4.9         2.5          4.5         1.7  virginica
#> 108          7.3         2.9          6.3         1.8  virginica
#> 109          6.7         2.5          5.8         1.8  virginica
#> 110          7.2         3.6          6.1         2.5  virginica
#> 111          6.5         3.2          5.1         2.0  virginica
#> 112          6.4         2.7          5.3         1.9  virginica
#> 113          6.8         3.0          5.5         2.1  virginica
#> 114          5.7         2.5          5.0         2.0  virginica
#> 115          5.8         2.8          5.1         2.4  virginica
#> 116          6.4         3.2          5.3         2.3  virginica
#> 117          6.5         3.0          5.5         1.8  virginica
#> 118          7.7         3.8          6.7         2.2  virginica
#> 119          7.7         2.6          6.9         2.3  virginica
#> 120          6.0         2.2          5.0         1.5  virginica
#> 121          6.9         3.2          5.7         2.3  virginica
#> 122          5.6         2.8          4.9         2.0  virginica
#> 123          7.7         2.8          6.7         2.0  virginica
#> 124          6.3         2.7          4.9         1.8  virginica
#> 125          6.7         3.3          5.7         2.1  virginica
#> 126          7.2         3.2          6.0         1.8  virginica
#> 127          6.2         2.8          4.8         1.8  virginica
#> 128          6.1         3.0          4.9         1.8  virginica
#> 129          6.4         2.8          5.6         2.1  virginica
#> 130          7.2         3.0          5.8         1.6  virginica
#> 131          7.4         2.8          6.1         1.9  virginica
#> 132          7.9         3.8          6.4         2.0  virginica
#> 133          6.4         2.8          5.6         2.2  virginica
#> 134          6.3         2.8          5.1         1.5  virginica
#> 135          6.1         2.6          5.6         1.4  virginica
#> 136          7.7         3.0          6.1         2.3  virginica
#> 137          6.3         3.4          5.6         2.4  virginica
#> 138          6.4         3.1          5.5         1.8  virginica
#> 139          6.0         3.0          4.8         1.8  virginica
#> 140          6.9         3.1          5.4         2.1  virginica
#> 141          6.7         3.1          5.6         2.4  virginica
#> 142          6.9         3.1          5.1         2.3  virginica
#> 143          5.8         2.7          5.1         1.9  virginica
#> 144          6.8         3.2          5.9         2.3  virginica
#> 145          6.7         3.3          5.7         2.5  virginica
#> 146          6.7         3.0          5.2         2.3  virginica
#> 147          6.3         2.5          5.0         1.9  virginica
#> 148          6.5         3.0          5.2         2.0  virginica
#> 149          6.2         3.4          5.4         2.3  virginica
#> 150          5.9         3.0          5.1         1.8  virginica
```

```r
S$peek()
```

```
#> NULL
```

## License

MIT © [Mango Solutions](https://github.com/mangothecat)
