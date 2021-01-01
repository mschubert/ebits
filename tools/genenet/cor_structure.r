import_package("dplyr", attach=TRUE)
import_package("ggplot2", attach=TRUE)
import_package("ggraph", attach=TRUE)
.st = import('../../stats')

#' Plot a correlation matrix
#'
#' @param mat  data matrix [samples x features]
#' @return     ggplot2 object
plot_cor_matrix = function(mat, title=NULL) {
    p.mat = .st$cor$test(mat)
    cmat = cor(mat)

    col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corrplot::corrplot(cmat, method="color", col=col(200),
        type="upper", order="hclust", mar=c(0,0,2,0), # title cut off otherwise
        addCoef.col = "black", # add coefficient of correlation
        tl.col="black", tl.srt=45, #text label color and rotation
        p.mat = p.mat, sig.level = 0.05, insig = "blank",
        diag=FALSE, title=title)
}

#' Infer partial correlation network using GeneNet
#'
#' @param mat  data matrix [features x samples]
#' @param fdr  FDR cutoff for each individual bootstrap
#' @return     data.frame with node1, node2, pval, qval
pcor = function(mat, fdr=1) {
    pm = GeneNet::ggm.estimate.pcor(t(mat), lambda=0)
    pm = GeneNet::network.test.edges(pm, direct=FALSE, plot=FALSE, verbose=TRUE)
    pm$node1 = factor(rownames(mat)[pm$node1])
    pm$node2 = factor(rownames(mat)[pm$node2])

    pm %>%
        filter(qval < fdr + .Machine$double.eps) %>%
        select(node1, node2, pcor, pval, qval)
}

#' Plot correlation network
#'
#' @param mat  data matrix [samples x features]
#' @param pval p-value cutoff
#' @param fdr  FDR cutoff for each individual bootstrap
#' @return     ggplot2 object
plot_pcor_net = function(pm, pval=1, fdr=1, node_text=6, edge_text=2.5, layout="auto") {
    pval_filter = pval
    dirx = factor(sign(pm$pcor))
    levels(dirx) = c("red", "blue")
    g = pm %>%
        mutate(dir = dirx, #factor(sign(pcor)),
               lab = sprintf("pcor %.2f\np %.2g", pcor, pval)) %>%
        tidygraph::as_tbl_graph() %>%
        tidygraph::activate(edges) %>%
        tidygraph::filter(pval <= pval_filter, qval <= fdr)

    p = ggraph::ggraph(g, layout=layout) # no edges produce plotting error if geom_edge_link set
    if (igraph::gsize(g) > 0)
        p = p +
            ggraph::geom_edge_link(aes(label=lab, width=pcor^2*10, color=dir,
                alpha=abs(pcor)/2), angle_calc='along', label_size=edge_text)
    p = p +
        ggraph::geom_node_text(aes(label=name), size=node_text) +
        theme_void() +
        ggtitle(sprintf("p<%.2g, FDR<%.2g", pval, fdr))
}

#' Plot bootstrapped correlation network
#'
#' @param mat  data matrix [samples x features]
#' @param fdr  FDR cutoff for each individual bootstrap
#' @param n    number of bootstraps
#' @param noise  Standard deviations of noise to add to data
#' @param show_edge_if  logical indicating how often an edge must be < fdr
#' @return     ggplot2 object
plot_bootstrapped_pcor = function(mat, fdr=0.3, n=100, noise=0.01, show_edge_if=round(n/10), layout="auto") {
    do_bs = function(mat) {
        mat = mat[sample(seq_len(nrow(mat)), replace=TRUE),]
        sds = apply(mat, 2, sd)
        mat = t(t(mat) + sds * noise * rnorm(length(mat)))
        pm = pcor(t(mat), fdr=fdr)
    }
    bs = replicate(n, do_bs(mat), simplify=FALSE)
    g = bs %>%
        dplyr::bind_rows() %>%
        group_by(node1, node2) %>%
        summarize(pcor = median(pcor),
                  dir = as.factor(sign(median(pcor))),
                  n = n()) %>%
        filter(n >= show_edge_if) %>%
        tidygraph::as_tbl_graph()

    ggraph(g, layout=layout) +
        geom_edge_link(aes(label=n, color=dir, alpha=abs(pcor), width=n/10)) +#,
                       #angle_calc='along', size=2.5) +
        geom_node_text(aes(label=name), size=6) +
        theme_void() +
        ggtitle(sprintf("%i bootstraps, edges if fdr<%.2f in at least %i runs",
                        n, fdr, show_edge_if))
}

#' Plot the partial correlations for one variable
#'
#' @param pm     data.frame with fields: node1, node2, pcor, pval
#' @param field  Character vector of field to use as anchor (in node1 or node2)
plot_pcor_table = function(pm, field) {
    res = pm %>%
        mutate(full = paste0(node1, node2)) %>%
        filter(grepl(field, full)) %>%
        arrange(qval, pval) %>%
        transmute(node = sub(field, "", full),
                  pcor = sprintf("%.2f", pcor),
                  pval = sprintf("%.2g", pval),
                  #pval = metap::two2one(pval, invert=pcor<0),
                  fdr = sprintf("%.2g", p.adjust(pval, method="fdr")))

    gridExtra::grid.arrange(top=paste("Full-rank partial correlations with",
                            field, "(two-sided test)"),
                            gridExtra::tableGrob(res))
}
