export_submodule('./nmf')
export_submodule('./util')
batch = import('./batch')

.wrap = import('../data_frame/wrap_formula_indexing')
for (fname in list.files(module_file('export_indexed'), recursive=TRUE)) {
    .mod = import(paste0("./export_indexed/", sub("\\.r$", "", fname)))
    .FUN = ls(.mod)
    assign(.FUN, .wrap$wrap_formula_indexing(.mod[[.FUN]]))
}
