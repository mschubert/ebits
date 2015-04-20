File reading and writing routines
=================================

### `read_table()`

Like `read.table()`, but (a) guesses the separator from the file
extension, and (b) does by default not coerce strings into factors.

### `read_full_table()`

The same as `read_table()`, but assigns column- and row names
if the first line does not have one element fewer than the others
(discarding the top leftmost element).

### `load()`

The same as the normal `load()` except, that it returns the
object instead of loading it into the global namespace.

### `file_path()`

Like `file.path()`, but allows specifying an `ext` parameter
for the file extension.

### h5 functions

Experimental support for HDF5 loading and saving. Objects are
deconstructed in `value` and `names_<dimension>`, and reconstructed
upon loading.

For now, this needs a patched version of the `rhdf5` package. Install
using

```r
devtools::install_github("mschubert/rhdf5")
```
