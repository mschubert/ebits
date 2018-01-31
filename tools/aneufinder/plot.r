#' Plot AneuFinder model
#'
#' @param model  An AneuFinder model object
#' @return       A cowplot object with profiles and histograms
plot = function(model) {
    p1 = graphics::plot(model, type='profile')
    p2 = graphics::plot(model, type='histogram')
    cowplt = cowplot::plot_grid(p1, p2, nrow=2, rel_heights=c(1.2,1))
}
