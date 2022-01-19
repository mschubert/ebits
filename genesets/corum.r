import_package("dplyr", attach=TRUE)
.url = "http://mips.helmholtz-muenchen.de/corum/download"

.get_cached = function(fid) {
    fname = sprintf("%s.txt.zip", fid)
    dest = file.path(module_file("data"), fname)
    if (!file.exists(dest))
        download.file(sprintf("%s/%s", .url, fname), destfile=dest)
    readr::read_tsv(dest)
}

.process = function(fid) {
    .get_cached(fid) %>%
        filter(Organism == "Human") %>%
        select(hgnc=`subunits(Gene name)`, name=ComplexName) %>%
        rowwise() %>%
            mutate(hgnc = list(tibble(strsplit(hgnc, ";")[[1]]))) %>%
        ungroup() %>%
        tidyr::unnest(hgnc) %>%
        unstack()
}

corum_core = function() {
    .process("coreComplexes")
}

corum_all = function() {
    .process("allComplexes")
}

corum_splice = function() {
    .process("spliceComplexes")
}
