.p = import('../path')
.ar = import('../../array')
.df = import('../../data_frame')
.gdsc = import('../gdsc')

index = .p$read("ccle", "CCLE_sample_info_file_2012-10-18.txt", header=TRUE)
index$COSMIC = .gdsc$cosmic$name2id(index$Cell.line.primary.name, warn=FALSE)

#' Returns a gene expression matrix (genes x COSMIC IDs)
#'
#' @param index_type  The column of `index` to be used for column names
#' @return            The expression matrix
basal_expression = function(index_type="COSMIC") {
    if (!.p$exists("ccle"), "CCLE_Expression_2012-09-29.RData") {
        warning("data file does not exist, creating from text")
        lines = readLines(.p$file("ccle", "CCLE_Expression_2012-09-29.res"))
        lines = lines[-c(2,3)]
        expr = .p$read("ccle", textConnection(lines), header=TRUE, sep="\t")
    }
}

#' Returns a drug response matrix (drugs x COSMIC IDs)
#'
#' Note that an IC50 value of 8 corresponds to the maximum assigned
#' resistance and that the real value might be higher.
#'
#' @param index_type  The column of `index` to be used for column names
#' @return            The drug response matrix
drug_response = function(index_type="COSMIC") {
    idx = select_(index, "CCLE.name", index=index_type)
    .p$read("ccle", "CCLE_NP24.2009_Drug_data_2015.02.24.csv", header=TRUE) %>%
        select(CCLE.name=CCLE.Cell.Line.Name, Compound, IC50=IC50..uM.) %>%
        .df$update(idx, match_cols="CCLE.name") %>%
        na.omit() %>%
        .ar$construct(IC50 ~ index + Compound, data=.)
}
