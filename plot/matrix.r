.b = import('../base')
.ar = import('../array')

#' @param df       The data.frame containing the associations
#' @param formula  A formula of the kind value ~ axis[, axis2] FIXME: doesn't work
#' @param cols     Boolean flag indicating whether to cluster columns
#' @param rows     Boolean flag indicating whether to cluster rows
#' @param size     A size c(rows,cols) to limit the result clustering to
cluster = function(df, formula, cols=TRUE, rows=TRUE, size=NULL) {
    mat = .ar$construct(data=df, formula=formula, fun.aggregate=mean)
    indep_vars = all.vars(formula[[3]])
    rname = indep_vars[1] #FIXME: ar$construct should order by std. axis ordering
    cname = indep_vars[2] # order: 1,2 && remove t()

    if (!is.null(size))
        mat = mat[.b$top_mask(rowSums(mat), size[1]),
                  .b$top_mask(colSums(mat), size[2])]

    ord_rows = rownames(mat)
    ord_cols = colnames(mat)
    if (rows)
        ord_rows = rownames(mat)[hclust(dist(mat))$order]
    if (cols)
        ord_cols = colnames(mat)[hclust(dist(t(mat)))$order]

    df = df[df[[rname]] %in% ord_rows,]
    df = df[df[[cname]] %in% ord_cols,]
    df[[rname]] = factor(df[[rname]], ord_rows)
    df[[cname]] = factor(df[[cname]], ord_cols)
    df
}

#' Draws a matrix plot using ggplot::geom_tile or circle
#'
#' @param df       A data.frame containing the data
#' @param formula  Formula of sort value ~ row + col
#' @param color    Name of colour filed in df
#' @param label    Name of label field in df
#' @param palette  ggplot palette that should be used; default: RdYlGn
#' @param geom     ggplot geom that should be used, 'tile' or 'circle'
#' @param limits
matrix = function(df, formula, color="color", label=NULL,
                  palette="RdYlGn", geom="tile", limits=NULL,
                  na_value="#f5f5f5") {
    value = all.vars(formula[[2]])
    rows = all.vars(formula[[3]])[1]
    cols = all.vars(formula[[3]])[2]

    if ('label' %in% colnames(df))
        label = 'label'

    po.nopanel = list(theme(
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1))
    )

    p = ggplot(df, aes_string(x=cols, y=rows, fill=value))

    if (geom == "tile")
        p = p + scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, palette)),
                                     na.value=na_value, limits=limits) +
                geom_tile(colour="white") #TODO: scale size here as well
#    else if (geom == "circle")
#        p = p + scale_colour_brewer(palette) + #,
##                                       na.value=na_value, limits=limits) +
#            scale_size_area(max_size = 20)+
##            scale_size_area(max_size = max_size,
##                            labels = levels(M$value)[table(M$value) > 0]) +
#            geom_point(aes(size = 15, colour = "white"))
    else
        stop("geom needs to be either 'tile' or 'circle'")
        
    p = p + po.nopanel
    if (is.null(label))
        p
    else
        p + geom_text(data=df, aes(label=label))
}

#' @param data         Main data.frame
#' @param row          Field name for row labels
#' @param column       Field name for column labels
#' @param value        Field name for values in data
#' @param color        Field name for colour
#' @param name         Title of the legend
#' @param geom         `circle` or `tile`
#' @param max_size     maximal size of circles (have this default to tile size, or remove option?)
#' @param label_color  Color to use for labels (e.g. 'black'), or NULL if no labels
#' @param label_alpha  Whether to use alpha channel for labels
# based on: https://github.com/briatte/ggcorr
corr = function(data, method = "pairwise", palette = "RdYlGn", name = "rho",
                  geom = "tile", max_size = 6, label = FALSE, label_alpha = FALSE,
                  label_color = "black", label_round = 1, ...) {

    M = cor(data[1:ncol(data)], use = method)

    # correlation coefficients
    D = round(M, label_round)
    D = D * lower.tri(D)
    D = as.data.frame(D)
    D = data.frame(row = names(data), D)
    D = melt(D, id.vars = "row")
  
    # correlation quantiles
    M = M * lower.tri(M)
    M = as.data.frame(M)
    M = data.frame(row = names(data), M)
    M = melt(M, id.vars = "row")
    M$value[M$value == 0] = NA
    s = seq(-1, 1, by = .25)
    M$value = cut(M$value, breaks = s, include.lowest = TRUE,
                  label = cut(s, breaks = s)[-1])
    M$row = factor(M$row, levels = unique(as.character(M$variable)))
    M$num = as.numeric(M$value)
    diag  = subset(M, row == variable)

    # clean plot panel
    po.nopanel = list(theme(
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = -90))
    )
  
    p = ggplot(M, aes(row, variable))
  
    # apply main geom
    if (geom == "circle")
        p = p +
            scale_colour_brewer(name, palette = palette) +
            scale_size_area(name, max_size= max_size,
                            labels = levels(M$value)[table(M$value) > 0]) +
            geom_point(aes(size = num, colour = value))
    else
        p = p +
            scale_fill_brewer(name, palette = palette) +
            geom_tile(aes(fill = value), colour = "white")
  
    # add coefficient text
    if (label) {
        if (label_alpha)
            p = p + 
                geom_text(data = subset(D, value != 0), 
                    aes(row, variable, label = value, alpha = abs(as.numeric(value))),
                    color = label_color, show_guide = FALSE)
        else
            p = p + 
                geom_text(data = subset(D, value != 0), 
                    aes(row, variable, label = value),
                    color = label_color)
    }

    # add diagonal and options
    p  +
        geom_text(data = diag, aes(label = variable), ...) +
        scale_x_discrete(breaks = NULL) +
        scale_y_discrete(breaks = NULL) +
        labs(x = NULL, y = NULL) +
        coord_equal() +
        po.nopanel
}
