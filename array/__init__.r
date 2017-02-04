.pkg = import_package('narray')
for (n in ls(.pkg))
    assign(n, .pkg[[n]])
