.b = import('../../base')
.ar = import('../../array')
.file = import('./file')
cosmic = import('./cosmic')
drug = import('./drug')
MASTER_LIST = cosmic$MASTER_LIST
DRUG_PROPS = drug$DRUG_PROPS

ngs_bem = function() {
    .file$get('NGS_BEM_FATHMM_29052013v2.ro')
}

mutated_genes = function(frequency=0, intogen=F, tissue=NULL) {
    mut = t(.file$get('NGS_BEM_FATHMM_29052013v2.ro')$logical)

    if (!is.null(tissue))
        mut = mut[rownames(mut) %in% names(getTissues(tissue)),]

    if (intogen) {
        drivers = getDrivers(tissue=tissue)
        genes = intersect(unique(drivers$HGNC), colnames(mut))
        mut = mut[,genes]
    }

    if (frequency > 0)
        mut = mut[,colSums(mut)/nrow(mut) > frequency]

    mut
}

drivers = function(tissue=NULL) {
    ig = .file$get('INTOGEN_DRIVERS')
    if (!is.null(tissue))
        ig = dplyr::filter(ig, Tumor_Type %in% tissue)
    transmute(ig, HGNC=ActingDriver_Symbol, tissue=Tumor_Type)
}

basal_expression = function() {
    obj = .file$get('BASAL_EXPRESSION')
    rownames(obj$DATA) = obj$GENE_SYMBOLS
    obj$DATA
}

drug_response = function(metric='IC50s', validCosmicIds=TRUE,
            drug_names=TRUE, cell_names=FALSE) { #, min.real.IC50s=0) {
    if (grepl("IC50", metric))
        SCREENING = .file$get('DRUG_IC50')
    else if (grepl("AUC", metric))
        SCREENING = .file$get('DRUG_AUC')
    else
        stop("invalid metric")

    if (drug_names)
        colnames(SCREENING) = drug$id2name(colnames(SCREENING))

    if (cell_names)
        rownames(SCREENING) = cosmic$id2name(rownames(SCREENING))

    SCREENING
}

tissues = function(tissue=NULL, unknown=NA, dropUnknown=T, TCGA=T, minN=2) {
    stopifnot(!dropUnknown || is.na(unknown)) # if dropUnknown, unknown needs to be NA

    if (TCGA)
        tissueVec = as.character(MASTER_LIST$Study.Abbreviation) # v16 TCGA
    else
        tissueVec = as.character(MASTER_LIST$GDSC.description_1) # v16 tissues
    names(tissueVec) = MASTER_LIST$COSMIC.ID # v16 cosmic id

    tissueVec[is.na(tissueVec)] = unknown
    tissueVec[tissueVec %in% c("unknown", "", "UNABLE TO CLASSIFY")] = unknown
    tissueVec = na.omit(tissueVec)

    if (!is.null(tissue))
        tissueVec = tissueVec[tissueVec %in% tissue]

    n = sapply(tissueVec, function(t) sum(t==tissueVec, na.rm=T))
    tissueVec[n>=minN]
}
