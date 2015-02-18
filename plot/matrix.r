# based on: https://github.com/briatte/ggcorr
ggcorr = function(data, method = "pairwise", palette = "RdYlGn", name = "rho",
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
