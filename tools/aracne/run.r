import_package('igraph', attach=TRUE)
b = import('base')
io = import('io')
aracne = import('.')

INFILE = commandArgs(TRUE)[1] %or% "../data/expr/LUAD/cancer.RData"
OUTFILE = commandArgs(TRUE)[2] %or% "aracne/LUAD/cancer.RData"

expr = io$load(INFILE)
tfs = io$read_table('./tfs.txt', header=FALSE)

net = aracne$aracne(mat = expr, tfs = tfs)

g = graph_from_data_frame(net, directed=FALSE)

g = set_vertex_attr(g, "tf", index=V(g), value=V(g)$name %in% tfs)

dg = decompose.graph(g)
if (length(dg) > 1) {
    warning("Graph is not connected, using only first component")
    message("Component size: ", paste(sapply(dg, function(x)
            length(V(x))), collapse=", "))

    g = dg[[1]]
}

save(g, file=OUTFILE)
