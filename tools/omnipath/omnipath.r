import_package("tidygraph", attach=TRUE)

#' Get the network as tbl_graph object
omnipath = function() {
    OmnipathR::import_all_interactions() %>%
        OmnipathR::interaction_graph() %>%
        as_tbl_graph() %>%
        activate(edges) %>%
        mutate(sign = dplyr::case_when(
            consensus_stimulation == 1 & consensus_inhibition == 0 ~ "stimulation",
            consensus_stimulation == 0 & consensus_inhibition == 1 ~ "inhibition",
            TRUE ~ "unclear"
        )) %>%
        select(from, to, sign, type) %>%
        distinct()
}
