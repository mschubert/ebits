import_package('dplyr', attach=TRUE)

#' Retrieves GO categories for HGNC symbols
#'
#' @param names  Either 'id', 'name', or 'both'
#' @param dset   Dataset; e.g. '{hsapiens,mmusculus}_gene_ensembl'
#' @param genes  Identifier type for genes ('hgnc_symbol', 'entrezgene',
#'               'ensembl_gene_id', etc.)
#' @param valid  Character vector of symbols for selecting only populated categories
#' @param min_n  Minimum number of genes in category to consider it populated
#' @param ontology  Character vector of ontologies to use: BP (default), MF, CC
#' @param leaf_depth  Filter by leaf and branches up to level (default: no filter)
#' @param as_list   Whether to return a list of character vectors
go = function(dset="hsapiens_gene_ensembl", genes="hgnc_symbol", valid=NULL,
              min_n=4, ontology="BP", leaf_depth=Inf, as_list=FALSE) {
    fname = file.path(module_file("cache", mustWork=TRUE),
                      paste0(paste("go", dset, genes, sep="-"), ".rds"))
    if (file.exists(fname)) {
        sets = readRDS(fname)
    } else {
        warning("Creating cache from Biomart, this may take a while", immediate.=TRUE)
        mart = biomaRt::useMart(biomart="ensembl", dataset=dset)
        mapGO = biomaRt::getBM(attributes=c(genes, "go_id"), mart=mart)
        sets = mapGO %>% dplyr::filter(go_id != "")
        sets = tibble::as_tibble(sets[sets[[genes]] != "",])
        saveRDS(sets, file=fname)
    }

    if (is.null(valid))
        valid = unique(sets[[genes]])
    genes_per_set = sapply(unstack(sets), length)

    obj = get(paste0("GO", ontology, "PARENTS"), asNamespace("GO.db"))
    tree = AnnotationDbi::toTable(obj)

    terms = tibble::as_tibble(AnnotationDbi::toTable(GO.db::GOTERM)[,2:5])
    terms = terms[!duplicated(terms$go_id),]

    valid_sets = intersect(terms$go_id, names(genes_per_set)[genes_per_set >= min_n])
    g = igraph::graph.data.frame(tree, vertices=terms)
    gs = igraph::induced_subgraph(g, valid_sets)

    if (is.finite(leaf_depth)) {
        return_sets = c()
        for (i in seq_len(leaf_depth)) {
            leaves = igraph::V(gs)$name[igraph::degree(gs, mode="in")==0]
            return_sets = c(return_sets, leaves)
            gs = igraph::delete_vertices(gs, leaves)
        }
        terms = terms[terms$go_id %in% return_sets,]
    }

    res = dplyr::inner_join(terms, sets, by="go_id")
    if (as_list) {
        res$both = paste(res$go_id, res$Term)
        unstack(res[c(genes, "both")])
    } else
        res
}

if (is.null(module_name())) {
    cache = go()
}
