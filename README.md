# simutils

[![Build Status](https://travis-ci.org/yourgithub/simutils.png?branch=master)](https://travis-ci.org/yourgithub/simutils)  [![codecov](https://codecov.io/gh/yourgithub/simutils/branch/master/graph/badge.svg)](https://codecov.io/gh/yourgithub/simutils)

## How to finish setting up your new package

Now that you've got a working package skeleton, there are a few steps to finish setting up all the integrations:

## Git(Hub)

Go to https://github.com/yourgithub and create a new repository. Then, in the directory where this package is, create your git repository from the command line, add the files, and push it to GitHub:

    git init
    git add --all
    git commit -m "Initial commit of package skeleton"
    git remote add origin git@github.com:yourgithub/simutils.git
    git push -u origin master

## Installing

<!-- If you're putting `simutils` on CRAN, it can be installed with

    install.packages("simutils") -->

The pre-release version of the package can be pulled from GitHub using the [devtools](https://github.com/hadley/devtools) package:

    # install.packages("devtools")
    devtools::install_github("yourgithub/simutils", build_vignettes=TRUE)

## For developers

The repository includes a Makefile to facilitate some common tasks.

## Running tests

`$ make test`. Requires the [testthat](https://github.com/hadley/testthat) package. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=logging`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the `testthat` package.

## Updating documentation

`$ make doc`. Requires the [roxygen2](https://github.com/klutometis/roxygen) package.
