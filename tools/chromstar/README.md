Chromstar
=========

This is using the Chromstar
([Bioconductor](http://bioconductor.org/packages/release/bioc/html/chromstaR.html),
[Github](https://github.com/ataudt/chromstaR)) package to build univariate and
multivariate HMMs from chromatin modification data.

Command-line interface
----------------------

### Building univariate models

`univ.r` can be run from the command-line to build univariate models given a
sample sheet and the referenced BAM files.

The following arguments are supported:

* `samples` - A YAML file listing all the bam files. Fields required are:
   * `assembly` - Identifier of the genome assembly, e.g. `GRCm38`
   * `paired_end` - Whether paired end reads are used, e.g. `TRUE`
   * `directory` - Directory where to look for BAM files
   * `sample` - List of sample identifiers with the following contents:
      * `input` - Control file to normalize to
      * `marks` - List of modification IDs, containing their BAM files
* `outfile` - File name where to save resulting univariate models as RData
* `beddir` - Directory name where to save BED tracks to
* `plotfile` - File name of a PDF to save univariate plots to

### Building multivariate models

`combine.r` can be run from the command-line to combine multiple univariate
models into a set of multivariate models.

The following arguments are supported:

* `samples` - A YAML file containing a `comparisons` field with ID keys. Within:
   * `samples` - `sample` ID from univariate config
   * `marks` - DNA modification
   * `mode` - Mode to combine models, e.g. `combinatorial`, `differential`,
     `separate`
* `unifile` - Input file of built univariate models
* `outfile` - File name where to save resulting models as RData
* `beddir` - Directory name where to save BED tracks to
* `plotfile` - File name of a PDF to save multivariate plots to
