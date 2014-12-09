ebits: R modules for general reuse
==================================

This repository is meant as a collection of bioinformatics-related R
code that simplify workflows and can be shared with other people.

We currently provide the following modules:

 * [base](base) - some basic functionality, in part overwriting R's `base` functions
 * [io](io) - file reading and writing
 * [array](array) - functions to simplify array programming in R.
 * [hpc](hpc) - functions to send R function calls as LSF jobs.

Requirements
------------

This collection of tools requires `modules`, install using:

```r
devtools::install_github('klmr/modules')
```

This command requires `devtools` to be installed.

The following R libraries are currently required to run `ebits`,
you can install all of them by executing:

```r
install.packages(c('BatchJobs','gtools','plyr','dplyr','abind','reshape2','xlsx'))
# implicitly installed: stringr, magrittr
```

Setup
-----

```bash
cd your_local_directory
git clone https://github.com/EBI-predocs/ebits.git
```

Additionally, you need to tell R where to find the `ebits` module.
In order to do this, set your `import.path` in your *~/.Rprofile*:

```r
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
