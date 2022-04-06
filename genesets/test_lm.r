import_package("dplyr", attach=TRUE)
.idmap = import('../process/idmap')
.guess = import('../process/idmap/guess')

#' Use a linear model to test consistent gene DE in a collection of gene sets
#'
#' @param genes  A data.frame with gene separation statistics
#' @param sets   List of character vectors
#' @param label  Column name of gene names
#' @param stat   Column name of separation statistics
#' @param min_n  Minimum number of genes to consider this set (default: 2)
#' @param add_means  Column name(s) of variables to compute the mean of the set of
#' @param trim   Fraction of extremes to ignore when computing the mean
#' @return       A data.frame with association results
test_lm = function(genes, sets,
                   label=c("external_gene_name", "gene_name", "gene", "name", "label", "ensembl_gene_id"),
                   stat=c("stat", "statistic", "log2FoldChange", "estimate"),
                   min_n=2, add_means=c(), trim=0) {
    test_one = function(res, set) {
        dset = res %>% mutate(in_set = !! slab %in% set + 0)
        if (sum(dset$in_set, na.rm=TRUE) < min_n)
            return(data.frame(estimate=NA, size=length(set), size_used=NA))

        sums = dset %>% group_by(in_set) %>%
            summarize_at(vars(all_of(add_means)), function(x) mean(x, na.rm=TRUE, trim=trim)) %>%
            summarize_at(vars(all_of(add_means)), diff)

        lm(as.formula(paste(stat, "~ in_set")), data=dset) %>%
            broom::tidy() %>%
            filter(term == "in_set") %>%
            select(-term) %>%
            mutate(size = length(set),
                   size_used = sum(res[[label]] %in% set & !is.na(res[[stat]]))) %>%
            cbind(sums)
    }

    msg = c()
    if (length(label) > 1) {
        label = intersect(label, colnames(genes))[1]
        first = head(na.omit(genes[[label]]), 2) %>% sQuote() %>% paste(collapse=", ")
        id_type_df = .guess$id_type(genes[[label]])
        msg = c(msg, paste0(sQuote(label), " [", id_type_df, "] for sets (", first, ", …)"))
    }
    slab = rlang::sym(label)
    if (length(stat) > 1) {
        stat = intersect(stat, colnames(genes))[1]
        msg = c(msg, paste0(sQuote(stat), " for separation"))
    }
    if (length(msg) > 0)
        message("[geneset/test_lm] using ", paste(msg, collapse=", "))

    all_set_genes = unique(unlist(sets))
    common_genes = intersect(all_set_genes, genes[[label]])
    min_n_genes = min(length(all_set_genes), length(genes[[label]]))
    if (length(common_genes) < 0.1 * min_n_genes) {
        id_type_sets = .guess$id_type(all_set_genes)
        message("[geneset/test_lm] low identifier overlap, mapping ",
                sQuote(label), " to ", sQuote(id_type_sets))
        genes[[label]] = .idmap$gene(genes[[label]], to=id_type_sets)
    }

    lapply(sets, test_one, res=genes) %>%
        setNames(names(sets)) %>%
        dplyr::bind_rows(.id="label") %>%
        as_tibble() %>%
        na.omit() %>%
        select(label, size, size_used, !!! rlang::syms(add_means), everything()) %>%
        mutate(adj.p = p.adjust(p.value, method="fdr")) %>%
        arrange(adj.p, p.value)
}

if (is.null(module_name())) {
    library(testthat)

    genes = replicate(10, paste(c(sample(LETTERS, 5), sample(1:9, 1)), collapse=""))
    gdf = data.frame(gene = genes, stat=1:10)
    sets = list(a=head(genes, 5))
    res = test_lm(gdf, sets)

    expect_true(inherits(res, "data.frame"))
    expect_equal(res$estimate, -5)
}
