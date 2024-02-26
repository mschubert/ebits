ebits: bioinformatics-related R modules
=======================================

[![Build Status](https://github.com/mschubert/ebits/workflows/R-check/badge.svg?branch=master)](https://github.com/mschubert/ebits/actions)

This repository is meant as a collection of bioinformatics-related R
code that simplify workflows and can be shared with other people.

For details, see the `README.md` files and API documentation in the respective
directories.

Requirements
------------

This collection of tools requires `modules`, install using using `devtools`:

```r
# install.packages('devtools')
devtools::install_github('klmr/modules')
```

The scripts also depend on a range of R packages. If using only a few modules,
you may want to install only the packages you need. Otherwise you can
list/install all of them by typing:

```sh
./dependencies.sh # list all packages
make deps # install all packages (warning: there's a lot)
```

Setup
-----

```bash
cd your_local_directory
git clone https://github.com/EBI-predocs/ebits.git
cd ebits && git submodule --init --recursive
```

Additionally, you need load the `modules` package and tell R where to find the
`ebits` module. In order to do this, set your `import.path` in your
*~/.Rprofile*:

```r
library(modules) # can also load manually
options(import.path=c("/path/to/ebits",
                      "/path/to/other/module")
```

Usage
-----

Modules can be imported and used as described in the
[`modules` documentation](https://github.com/klmr/modules)

```r
mod = import('module') # imports the module
mod$func(x=5)          # calls the module function
```
