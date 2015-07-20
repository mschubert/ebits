export_submodule('./ml')
export_submodule('./nmf')
export_submodule('./util')
batch = import('./batch')

.wrap = import('../data_frame/formula_indexing')
.assocs = import('./assocs')
for (FUN in ls(.assocs))
    assign(FUN, .wrap$wrap_formula_indexing(.assocs[[FUN]]))
