library(modules)
.b = import('base')
.io = import('io')
.ar = import('array')
.p = import('../path')
cosmic = import('./cosmic')
drug = import('./drug')
MASTER_LIST = cosmic$MASTER_LIST
DRUG_PROPS = drug$DRUG_PROPS

getNGS_BEM = function() {
    .io$data('DATA/R_objects/Genomic/NGS_BEM_FATHMM_29052013v2')
}

getMutatedGenes = function(frequency=0, intogen=F, tissue=NULL) {
    mut = t(.io$data('DATA/R_objects/Genomic/NGS_BEM_FATHMM_29052013v2')$logical)

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

getDrivers = function(tissue=NULL) {
    ig = .io$data('DATA/R_objects/intoGen_Cancer_Drivers/cancer_drivers_5_2_2014')
    if (!is.null(tissue))
        ig = dplyr::filter(ig, Tumor_Type %in% tissue)
    transmute(ig, HGNC=ActingDriver_Symbol, tissue=Tumor_Type)
}

getEncodedMutations = function(get=c('AMPL', 'DEL', 'FUSION', 'MS', 'miRNA', 'missenseMut', 'truncMut')) {
    objs = lapply(get, function(f) .io$data(paste0('DATA/R_objects/Genomic/ML_encoding_mut_070414/', f)))
    t(do.call(rbind, objs))
}

getBASAL_EXPRESSION = function() {
    obj = .io$data('DATA/R_objects/Transcriptomic/BASAL_EXPRESSION_12062013v2')
    rownames(obj$DATA) = obj$GENE_SYMBOLS
#    library(limma)
#    limma::avereps(obj$DATA)
    obj$DATA
}

getMutationsForCellLines = function(validCosmicIds=TRUE) {
    # cell line, gene, mutation type
    .io$data("DATA/R_objects/Genomic/MUTATION_ARRAY")[validCosmicIds,,]
}

# top: top x% in sensitivity range
# abs: under x log uM
# delta: max log difference (10-folds) under most sensitive
filterDrugResponse = function(X, tissues, top=0.1, abs=0, delta=2) {
    minFunc = function(x) {
        min(c(abs,
              sort(x)[round(top*sum(!is.na(x)))],
              min(x, na.rm=T)+delta))
    }
    sensThresh = apply(X, 2, minFunc)
    drugList = .ar$split(X, along=2)

    index2filtered = function(i) {
        st = sensThresh[i]
        d = drugList[[i]]
        .ar$filter(d, along=1, function(f) sum(f<st, na.rm=T)>1, subsets=tissues) # min 2 lines
    }
    do.call(cbind, setNames(lapply(1:length(drugList), index2filtered), names(drugList)))
}

getDrugResponseForCellLines = function(metric='IC50s', validCosmicIds=T, public.only=T,
                                       use.names=T, version=17) { #, min.real.IC50s=0) {
    if (version == 15) {
        stop('invalid version')
    } else if (version == 16) {
        stop('invalid version')
    } else if (version == 17) {
        SCREENING = .io$data('DATA/R_objects/Drugs/djv17_public') # v17
    } else stop('invalid version')

    validDrugIndex = DRUG_PROPS$DRUG_ID %in% colnames(SCREENING[[metric]])
    if (public.only)
        validDrugIndex = validDrugIndex & DRUG_PROPS$WEBRELEASE == "Y"
    else
        stop("only public data available on GSK site")

    validDrugIds = as.character(DRUG_PROPS$DRUG_ID[validDrugIndex])
    Ys = SCREENING[[metric]][validCosmicIds, validDrugIds]

    if (use.names)
        colnames(Ys) = make.unique(DRUG_PROPS[colnames(Ys),'DRUG_NAME'])

#    if (min.real.IC50s > 0) {
#        tissues = getTissues(minN=3)
#        maxc = SCREENING[['maxConc']][validCosmicIds, validDrugIds]
#        .ar$intersect(tissues, Ys, maxc, along=1)
#        idx = Ys
#        idx[] = 1:length(idx)
#        Ysc = c(Ys)
#        maxc = c(maxc)
#        Ysi = .ar$map(idx, along=1, function(x) 
#                    if (sum(Ysc[idx]<maxc[idx], na.rm=T) > min.real.IC50s) T # direct->explodes
#                    else F, subsets=tissues)
#        Ys[!Ysi] = NA # yields all tissues
#    }

    Ys = as.matrix(Ys)
    rownames(Ys) = cosmic$name2id(rownames(Ys))
    Ys = Ys[!duplicated(rownames(Ys, all)),]
    Ys
}

getTissues = function(tissue=NULL, unknown=NA, dropUnknown=T, TCGA=T, minN=2) {
    stopifnot(!dropUnknown || is.na(unknown)) # if dropUnknown, unknown needs to be NA

#    tissueVec = as.character(MASTER_LIST$gdsc_desc_1) # v15 tissues #TODO: make sure not duplicates?
#    names(tissueVec) = MASTER_LIST$CosmicID # v15 cosmic id
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

