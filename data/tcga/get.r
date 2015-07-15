.p = import('../path')
.io = import('../../io')

tissues = function() {
    tt = list.files(.p$path("tcga"), pattern="_voom\\.RData")
    sub("_voom.RData", "", tt)
}

clinical = function() {
    .p$load("tcga", "clinical_full.RData")
}

rna_seq = function(tissue) {
    .p$load("tcga", paste0(tissue, "_voom.RData"))
}

rppa = function() {
    .p$load("tcga", "rppa.RData")
}

mutations = function() {
    .p$load("tcga", "mutations.RData")
}
