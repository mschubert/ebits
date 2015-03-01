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

The following R libraries are currently required to run `ebits`,
you can install all the missing packages by executing:

```r
req = c('devtools','BatchJobs','gtools','plyr','dplyr','abind','reshape2','xlsx')
# implicit dependencies: stringr, magrittr
new = req[!(req %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)
```

This collection of tools requires `modules`, install using using `devtools`:

```r
devtools::install_github('klmr/modules')
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
