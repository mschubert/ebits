import_package("dplyr", attach=TRUE)
.idmap = import('process/idmap')
.enr = import('../tools/enrichr')
.msdb = import('../tools/msigdb')
.go = import('./go')$go

list = function() {
    dbs = c("GO_Biological_Process_2020",
      "DoRothEA",
      .enr$dbs()$name,
      .msdb$dbs())
    gtools::mixedsort(dbs)
}

get_human = function(collections, ..., drop=TRUE) {
    get_one = function(col) {
        if (col == "GO_Biological_Process_2020") {
            .go(leaf_depth=3, ontology="BP", as_list=TRUE)
        } else if (col == "GO_Cellular_Component_2020") {
            .go(leaf_depth=3, ontology="CC", as_list=TRUE)
        } else if (col == "GO_Molecular_Function_2020") {
            .go(leaf_depth=3, ontology="MF", as_list=TRUE)
        } else if (col %in% .enr$dbs()$name) {
            .enr$genes(col)
        } else if (col %in% .msdb$dbs()) {
            .msdb$genes(col)
        } else if (col == "DoRothEA") {
            dorothea::dorothea_hs %>%
                filter(mor == 1,
                       confidence %in% c("A","B","C","D")) %>%
                group_by(tf) %>%
                    filter(case_when(
                        sum(confidence %in% c("A")) >= 20 ~ confidence %in% c("A"),
                        sum(confidence %in% c("A", "B")) >= 20 ~ confidence %in% c("A", "B"),
                        sum(confidence %in% c("A", "B", "C")) >= 20 ~ confidence %in% c("A", "B", "C"),
                        sum(confidence %in% c("A", "B", "C", "D")) >= 20 ~ confidence %in% c("A", "B", "C", "D"),
                        TRUE ~ FALSE
                    )) %>%
                    mutate(tf = sprintf("%s (%s)", tf, tolower(tail(sort(confidence), 1)))) %>%
                ungroup() %>%
                select(target, tf) %>%
                unstack()
        } else
            stop("invalid gene set: ", args$geneset)
    }

    if (length(collections) == 1 & drop) {
        get_one(collections)
    } else {
        sapply(collections, get_one, simplify=FALSE)
    }
}

get_mouse = function(collections, ..., drop=TRUE) {
    map_one = function(hum) {
        mouse = stack(hum)
        mouse$values = unname(.idmap$orthologue(mouse$values, from="external_gene_name", to="mgi_symbol"))
        unstack(na.omit(mouse))
    }

    hum = get_human(collections, drop=drop)

    if (length(collections) == 1 & drop) {
        map_one(hum)
    } else {
        lapply(hum, map_one)
    }
}

if (is.null(module_name())) {
    library(testthat)

    d = get_human("DoRothEA")
    dm = get_mouse("DoRothEA")
    dd = get_mouse("DoRothEA", drop=FALSE)
    expect_true(inherits(d, "list"))
    expect_true(inherits(d[[1]], "character"))
    expect_true(inherits(dm, "list"))
    expect_true(inherits(dm[[1]], "character"))
    expect_true(inherits(dd, "list"))
    expect_equal(length(dd), 1)
    expect_true(inherits(dd$DoRothEA, "list"))
}
