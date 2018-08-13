no_grid = function() {
    ggplot2::theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
}

no_x = function() {
    ggplot2::theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
}

no_y = function() {
    ggplot2::theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
}

no_xy = function() no_x() + no_y()
no_gx = function() no_grid() + no_x()
no_gy = function() no_grid() + no_y()
no_gxy = function() no_grid() + no_xy()
