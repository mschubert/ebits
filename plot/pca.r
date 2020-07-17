#' GGplot2 PCA plot of different base R objects
#'
#' @param obj    A PCA object (e.g. from prcomp function)
#' @param aes    Aesthetics mapping; use "PC<num>" for principal components
#' @param annot  Additional data.frame with sample annotation
#' @return       ggplot2 object including PCs and sample annotations
pca = function(obj, aes, annot=NULL, biplot=FALSE, ...) {
    UseMethod("pca")
}

pca.prcomp = function(obj, aes=ggplot2::aes(), annot=NULL, repel=TRUE,
                      biplot=FALSE, bi_color="red", bi_size=5, bi_arrow=0.2) {
    # adapted: https://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
    data = cbind(annot, obj$x)
    rot = data.frame(varnames=rownames(obj$rotation), obj$rotation)
    x = rlang::quo_text(aes[["x"]])
    y = rlang::quo_text(aes[["y"]])

    mult = min(
        (max(data[,y]) - min(data[,y])/(max(rot[,y])-min(rot[,y]))),
        (max(data[,x]) - min(data[,x])/(max(rot[,x])-min(rot[,x])))
    )
    rot = transform(rot,
        v1 = .7 * mult * rot[[x]],
        v2 = .7 * mult * rot[[y]]
    )

    p = ggplot(data=data, mapping=aes)

    if (repel) {
        textfun = function(...) ggrepel::geom_text_repel(..., min.segment.length=Inf)
    } else {
        textfun = geom_text
    }

    if (biplot) {
        p = p + textfun(data=rot, aes(x=v1, y=v2, label=varnames),
                size = bi_size, vjust=1, color=bi_color) +
            geom_segment(data=rot, aes(x=0, y=0, xend=v1, yend=v2),
                arrow=arrow(length=unit(bi_arrow,"cm")), alpha=0.75, color=bi_color)
    }

    p
}
