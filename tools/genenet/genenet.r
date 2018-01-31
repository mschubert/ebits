#' Infers a partial correlation network from expression data
#'
#' @param expr      A gene expression matrix [genes x samples]
#' @param directed  Infer a directed network
#' @param qval      Only return edges with qvalue<cutoff
#' @param verbose   Print messages
#' @return          A data.frame of edges and their significance
infer_network = function(expr, directed=FALSE, qval=NULL, verbose=TRUE) {
    pm = GeneNet::ggm.estimate.pcor(t(expr))
    pm = GeneNet::network.test.edges(pm, direct=directed, plot=FALSE, verbose=verbose)

    if (!is.null(qval))
        pm = GeneNet::extract.network(pm, method.ggm="qval", cutoff.ggm=qval)

    pm$node1 = factor(rownames(expr)[pm$node1])
    pm$node2 = factor(rownames(expr)[pm$node2])
    pm
}

if (is.null(module_name())) {
    b = import('base')
    io = import('io')

    INFILE = commandArgs(TRUE)[1] %or% "../data/expr/STAD/cancer.RData"
    OUTFILE = commandArgs(TRUE)[2] %or% "genenet/STAD/1.RData"

    expr = io$load(INFILE)

    set.seed(strtoi(paste0("0x", substr(digest::digest(OUTFILE), 1, 5))))
    expr = expr[,sample(colnames(expr), replace=TRUE)]

    net = infer_network(expr, qval=0.1)

    save(net, file=OUTFILE)
}
