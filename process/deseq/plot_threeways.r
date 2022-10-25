`%>%` = magrittr::`%>%`
import_package('patchwork', attach=TRUE)
plt = import('../../plot')

#' Plot threeway DE comparisons for each row in results
#'
#' @param res   A DESeq2 results data.frame object
#' @param add_cols  A logical indicating whether to compare columns (only valid for 2 rows)
#' @return  A patchwork object with volcanos sample1 vs ref, 2 vs ref, cor of 1/2 ref, 2 vs 1
plot_threeways = function(res, field, max_ov=25, add_cols=FALSE) {
    do_cor = function(i) {
        both = inner_join(res[[field]][[i[1]]], res[[field]][[i[2]]], by="label") %>%
            filter(!is.na(stat.x) & !is.na(stat.y))
        m = broom::glance(lm(stat.y ~ stat.x, data=both))
        plt$denspt(both, aes(x=stat.x, y=stat.y, label=label)) +
            labs(x=res$term[i[1]], y=res$term[i[2]]) +
            ggtitle(sprintf("R^2=%.2f (p=%.2g)", m$r.squared, m$p.value))
    }

    if (add_cols && !nrow(res) %in% c(6,8))
        stop("add_cols needs exactly 6 or 8 comparisons")

    if (field != "genes") {
        res[[field]] = lapply(res[[field]], . %>% dplyr::rename(stat=statistic))
        max_ov = max_ov * 0.5
    }

    clean = function(x) gsub("_", " ", sub("^[^_]+_(.*)$", "\\1", x))
    volc = function(df, tit) plt$volcano(df, label_top=30, clamp_x=10) + ggtitle(clean(tit))
    volcs = mapply(volc, df=res[[field]], tit=res$term, SIMPLIFY=FALSE)

    vci = seq(3, nrow(res), by=3)
    cor_idx = t(matrix(setdiff(seq_len(floor(nrow(res)/3)*3), vci), nrow=2))
    cors = lapply(narray::split(cor_idx, along=1, drop=TRUE), do_cor)
    combined = tibble(a=volcs[cor_idx[,1]], b=volcs[cor_idx[,2]], c=cors, d=volcs[vci]) %>%
        rowwise() %>% mutate(comb = list(list(a,b,c,d))) %>% pull(comb) %>% do.call(c, .)
    if (add_cols) {
        combined = c(combined, list(do_cor(c(1,4)), do_cor(c(2,5)), plot_spacer(), do_cor(c(3,6))))
        if (nrow(res) == 8) # incl comparisons column 1 and 2
            combined = c(combined, volcs[c(7,8)])
    }

    (plt$text(field, size=8) / wrap_plots(combined, ncol=4)) + plot_layout(heights=c(1,30))
}
