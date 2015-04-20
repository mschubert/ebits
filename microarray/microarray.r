.b = import('base', attach_operators=F)
.idmap = import('idmap')

#' Normalizes all CEL files in a given directory
#'  directory: the directory to look for .CEL files
#'  files    : file names, can be partial matches to .CEL
#'  method   : which normalisation method to use (rma, gcrma, frma)
#'  map.to   : hgnc_symbol, entrezgene, ensembl_gene_id, or NULL/FALSE
#'  NUSE.tol : tolerance of meadian NUSE score around 1, set to NA for no QC
#'  RLE.tol  : tolerance of meadian RLE score around 0, set to NA for no QC
#'  @return  : a matrix of gene-level expression values
CELsToExpression = function(directory=".", files=NA, method="rma",
                            map.to="hgnc_symbol", NUSE.tol=0.1, RLE.tol=0.1) {
    regex = "\\.(CEL|cel)(\\.gz)?$"
    lsCEL = list.files(path=directory, pattern=regex)
    if (identical(files, NA))
        files = lsCEL
    fnames = sub(regex, "", files)
    if (!grepl(regex, files[1]))
        files = sapply(files, function(f) grep(f, lsCEL, value=T))
    files = sapply(files, function(f) file.path(directory, f))

    rawData = oligo::read.celfiles(filenames=files)
    oligo::sampleNames(rawData) = fnames
    plmFit = oligo::fitProbeLevelModel(rawData, target='core')

    discard = F
    if (!is.na(NUSE.tol)) {
        med = apply(oligo::NUSE(plmFit, type="values"), 2, function(x) median(x, na.rm=T))
        discard = discard | med > 1+NUSE.tol | med < 1-NUSE.tol
    }
    if (!is.na(RLE.tol)) {
        med = apply(oligo::RLE(plmFit, type="values"), 2, function(x) median(x, na.rm=T))
        discard = discard | med > RLE.tol | med < -RLE.tol
    }
    if (any(discard)) {
        warning(paste("Discarding", sum(discard), "arrays"))
        rawData = rawData[,!discard]
    }

    if (method == "rma")
        expr = oligo::rma(rawData)
    else if (method == "gcrma") {
        rawData = affy::ReadAffy(filenames=files)
        expr = gcrma::gcrma(rawData)
    } else if (method == "frma") {
        if (class(rawData) != "GeneFeatureSet")
            rawData = affy::ReadAffy(filenames=files)
        expr = frma::frma(rawData, target="core")
        colnames(expr) = fnames

    } else
        stop("invalid method")

    if (!is.null(map.to) & map.to!=F)
        .probesTo(exprs(expr), 'probe_id', map.to)
    else
        exprs(expr)
}

#' Takes a list of GSM identifiers and returns an expression matrix
#'  GSMs     : vector or list of GSMs
#'  normalize: flag indicating whether to perform quantile norm [T/F]
#'  map.genes: whether to map probe ids to HGNC ids
#'  cachedir : directory to cache downloaded GEO files in
#'  @return  : a matrix with probe ids in rows and GSMs in columns
GSMsToExpression = function(GSMs, norm.quantile=T, map.to="hgnc_symbol", cachedir=tempdir()) {
    stopifnot(class(GSMs) == 'character')
    stopifnot(class(norm.quantile) == 'logical')

    gsms = lapply(GSMs, function(f) GEOquery::getGEO(f, destdir=cachedir))

    platform = unique(sapply(gsms, function(f) GEOquery::Meta(f)$platform_id))
    if (length(platform) != 1)
        stop("GSMs are not from the same platform")

    ids = lapply(gsms, function(g) as.character(GEOquery::Table(g)$ID_REF))
    if (length(ids)>1 && !all(do.call(all.equal, ids)))
        stop("ids not the same for different GSMs")

    vals = do.call(cbind, lapply(gsms, function(g) as.numeric(GEOquery::Table(g)$VALUE)))

    if (max(vals) > 2^5)
        vals = log2(vals)
    if (norm.quantile)
        vals = limma::normalizeQuantiles(vals)

    colnames(vals) = GSMs
    rownames(vals) = ids[[1]]

    if (!is.null(map.to) & map.to!=F) {
        library(limma)
        .idmap$gene(vals, 'probe_id', map.to, fun.aggregate=limma::avereps)
    } else
        vals
}
