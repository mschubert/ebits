if (!requireNamespace("msigdb"))
    devtools::install_github("mw201608/msigdb")

dbs = import('./dbs')$dbs
genes = import('./genes')$genes
