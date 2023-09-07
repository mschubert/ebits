import_package("ggplot2", attach=TRUE)
import_package("ggraph", attach=TRUE)

#' Plot OmniPath network including edge signs and directions
#'
#' @param net  ggraph-compatible network object
#' @param node_aes  aesthetics mapping for geom_node_point
#' @return  ggplot2 object
plot_omni = function(net, node_aes, ..., layout="stress") {
    set.seed(121979) # same layout if same nodes
    et = c(stimulation="dashed", inhibition="dotted", unclear="solid")
    ggraph(net, layout=layout) +
        geom_node_point(node_aes, alpha=0.7, ...) +
        geom_edge_fan(aes(color=type, linetype=sign), alpha=0.2, strength=0.5,
                      end_cap=circle(0.01, "npc"), start_cap=circle(0.01, "npc"),
                      arrow=arrow(type="closed", length=unit(0.007, "npc"))) +
        geom_node_text(aes(label = name), size=2, repel=TRUE) +
        viridis::scale_fill_viridis(option="magma", direction=-1) +
        scale_edge_linetype_manual(values=et) +
        theme_void()
}
