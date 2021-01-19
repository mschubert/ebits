import_package('ggplot2', attach=TRUE)
import_package('dplyr', attach=TRUE)

#' Plot a Venn (Euler) diagram
#'
#' @param sets   List of set vectors or other input supported by 'eulerr'
#' @param ...    Parameters passed to 'eulerr::euler' fit
#' @return       ggplot2 object
venn = function(sets, ...) {
    fit = eulerr::euler(sets, ...)
#    slens = sapply(sets, length)
    df = as.data.frame(fit[c('original.values', 'fitted.values',
                             'residuals', 'regionError')]) %>%
        tibble::rownames_to_column("set") %>%
        mutate(label = case_when(
#            !grepl("\\&", set) ~ sprintf("atop(bold('%s')~~('%s'))", set, slens[set]),
            !grepl("\\&", set) ~ sprintf("atop(bold('%s'))", set),
            TRUE ~ as.character(NA)
        ))

    ellipses = do.call(eulerr:::ellipse, fit$ellipses)
    centers = lapply(ellipses, function(x) eulerr:::locate_centers(list(x))) %>%
        dplyr::bind_rows()
    polygons = ellipses %>%
        lapply(bind_rows) %>%
        setNames(rownames(fit$ellipses)) %>%
        bind_rows(.id="set")
    meta = df %>%
        filter(!is.na(label)) %>%
        cbind(centers)

    ggplot(polygons, aes(x=x, y=y)) +
        geom_polygon(aes(fill=set), color="#686868", alpha=0.2) +
        geom_text(data=na.omit(meta %>% select(-label)), aes(label=original.values)) +
        ggrepel::geom_text_repel(data=na.omit(meta), aes(label=label), parse=TRUE) +
        theme_void() +
        guides(size=FALSE, fill=FALSE) +
        coord_fixed()
}

if (is.null(module_name())) {
    sets = list(a=1:5, b=3:10, c=c(3,8:12))
    p = venn(sets)
}
