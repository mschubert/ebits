import_package('dplyr', attach=TRUE)
import_package('igraph', attach=TRUE)
b = import('ebits/base')
io = import('ebits/io')
tfs = import('../util/tfs')$tfs

OUTFILE = commandArgs(TRUE)[1] %or% "genenet/LIHC/snormal.RData"
INFILES = OUTFILE %>%
    sub("\\.RData", "", .) %>%
    paste0(sprintf("/%i.RData", 1:100))

#' Loads a network data.frame, keeps only TF edges, and orders node1/2
load_fun = function(fname) {
    message(fname)
    re = io$load(fname) %>%
        mutate(node1 = as.character(node1),
               node2 = as.character(node2)) %>%
        filter(node1 %in% tfs | node2 %in% tfs,
               qval < 0.01)

    swap = mapply(`>`, re$node1, re$node2)
    tmp = re$node1[swap]
    re$node1[swap] = re$node2[swap]
    re$node2[swap] = tmp

    message("TF edges: ", dim(re))
    re
}

gdf = lapply(INFILES, load_fun) %>%
    bind_rows()

message("# of edges across all models: ", dim(gdf))

median_pos = ceiling(length(INFILES)/2)

gdf = gdf %>%
    group_by(node1, node2) %>%
    filter(length(unique(sign(pcor))) == 1) %>%
    summarize(pcor = sign(pcor[1]) * sort(abs(pcor))[median_pos],
              qval = 10^(sort(log10(qval))[median_pos])) %>%
    filter(abs(pcor) > .Machine$double.eps)

message("# of edges after filtering: ", dim(gdf))

g = graph_from_data_frame(gdf, directed=FALSE)

# write node attribute whether gene is TF
set_vertex_attr(g, "tf", index=V(g), value=V(g)$name %in% tfs)

dg = decompose.graph(g)
if (length(dg) > 1) {
    warning("Graph is not connected, using only first component")
    message("Component size: ", paste(sapply(dg, function(x)
            length(V(x))), collapse=", "))

    g = dg[[1]]
}

save(g, file=OUTFILE)
