.pkg = import_package('narray')
for (n in ls(.pkg))
    assign(n, .pkg[[n]])

warning("'array' module is deprecated. Use 'narray' package instead.")
