import_package("dplyr", attach=TRUE)
import_package("ggplot2", attach=TRUE)
import_package("ggraph", attach=TRUE)

#' Plot OmniPath network including edge signs and directions
#'
#' @param net  ggraph-compatible network object
#' @param node_aes  aesthetics mapping for geom_node_point (use 'fill' for color)
#' @param pal  Palette to use for filling
#' @param ...  passed to `geom_node_point`
#' @return  ggplot2 object
plot = function(net, node_aes, ..., layout="stress", pal="Spectral") {
    set.seed(121979) # same layout if same nodes
    et = c(stimulation="dashed", inhibition="dotted", unclear="solid")
    add_geom = list()

    # if fill is present, add a symmetric palette
    if ("fill" %in% names(node_aes)) {
        fill_var = rlang::as_name(node_aes$fill)
        max_fill = max(net %N>% pull(!! fill_var) %>% abs())
        add_geom = c(add_geom, list(
            scale_fill_distiller(palette=pal, limit=max_fill*c(-1,1))
        ))
    }

    # if color is not present, indicate which nodes pass thresh by black outline
    thresh_var = attr(net, "var")
    thresh = attr(net, "thresh")
    if (!any(grep("colou?r", names(node_aes))) && !is.null(thresh_var) && !is.null(thresh)) {
        net = net %N>% mutate(is_hit = !! rlang::sym(thresh_var) <= thresh)
        node_aes = modifyList(node_aes, aes(color=is_hit))
        add_geom = c(add_geom, list(
            scale_color_manual(values=c("TRUE"="#000000ff", "FALSE"="#ffffff00")),
            labs(subtitle = sprintf("%s=%.2g", thresh_var, thresh))
        ))
    }

    ggraph(net, layout=layout) +
        geom_node_point(node_aes, alpha=0.7, shape=21, ...) +
        geom_edge_fan(aes(color=type, linetype=sign), alpha=0.2, strength=0.5,
                      end_cap=circle(0.01, "npc"), start_cap=circle(0.01, "npc"),
                      arrow=arrow(type="closed", length=unit(0.007, "npc"))) +
        geom_node_text(aes(label = name), size=2, repel=TRUE) +
        scale_edge_color_brewer(palette="Set1") +
        scale_edge_linetype_manual(values=et) +
        scale_size_continuous(range=c(1, 12)) +
        add_geom +
        theme_void()
}
