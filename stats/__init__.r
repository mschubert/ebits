export_submodule('./nmf')
export_submodule('./util')
export_submodule('./roc')
batch = import_('./batch')
cor = import_('./correlation')

.wrap = import_('../data_frame/wrap_formula_indexing')
for (fname in list.files(module_file('export_indexed'), recursive=TRUE)) {
    .mod = import_(paste0("./export_indexed/", sub("\\.r$", "", fname)))
    .FUN = ls(.mod)
    assign(.FUN, .wrap$wrap_formula_indexing(.mod[[.FUN]]))
}
