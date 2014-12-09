ebits: R modules for general reuse
==================================

This repository is meant as a collection of bioinformatics-related R
code that simplify workflows and can be shared with other people.

Requirements
------------

This collection of tools requires `modules`, install using:

```r
devtools::install_github('klmr/modules')
```

This command requires `devtools to be installed`.

The following R libraries are currently required to run `ebits`:

 * stringr
 * BatchJobs
 * gtools
 * plyr
 * dplyr
 * abind
 * magrittr
 * reshape2
 * xlsx

You can install all of them by executing:

```r
install.packages(c('strings','BatchJobs','gtools','plyr','dplyr','abind','magrittr','reshape2','xlsx'))
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

We currently provide the modules listed below. Each of them can be
loaded using the `import` command:

```r
mod = import('module') # imports the module
mod$func(x=5)          # calls the module function
```

#### [base](base)

Some basic functionality, in part overwriting R's `base` functions.

#### [io](io)

File reading and writing.

#### [array](array)

Functions to simplify array programming in R.

#### [hpc](hpc)

Functions to send R function calls as LSF jobs.
