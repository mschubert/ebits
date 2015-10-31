library(dplyr)
.p = import('../path')
.io = import('../../io')

# see https://tcga-data.nci.nih.gov/datareports/codeTablesReport.htm
codes = list(center_code = 'code_tables/center_code.txt',
             disease_study = 'code_tables/disease_study.txt',
             platform_code = 'code_tables/platform_code.txt',
             sample_type = 'code_tables/sample_type.txt',
             tissue_source_site = 'code_tables/tissue_source_site.txt',
             portion_analyte = 'code_tables/portion_analyte.txt') %>%
   lapply(function(x) {
       fname = file.path(module_file(), x)
       .io$read_table(fname, header=TRUE, quote=NULL,
           na.strings=NULL, colClasses='character', check.names=TRUE)
   })

# eg. TCGA-02-0001-01
#  TCGA - clinical centre - participant id - sample code
# eg. TCGA-02-0001-01C-01D-0182-01
#  (same as above)[D/R]NA,.. - 
regex_barcode = paste("^TCGA",              # TCGA identifer
                      "([a-zA-Z0-9]+)",     # tissue source site (eg. GBM from MBA)
                      "([a-zA-Z0-9]+)",     # participant id (4 digit alphanumeric)
                      "([0-9]+)([A-Z])?"  , # tumor/normal id, number of vial
                      "?([0-9]+)?([A-Z])?", # portion (numbered); analyte (eg. [D/R]NA)
                      "?([a-zA-Z0-9]+)?",   # plate id (4 digit alphanumeric)
                      "?([0-9]+)?$",        # centre (eg. 01=BROAD GCC)
                      sep = "-")

# 32-bit random string
# eg. ebf3e73f-41a0-4ca5-b608-fe1c629e16de
regex_id = paste("[a-zA-Z0-9]{8}",
                 "[a-zA-Z0-9]{4}",
                 "[a-zA-Z0-9]{4}",
                 "[a-zA-Z0-9]{4}",
                 "[a-zA-Z0-9]{12}",
                 sep = "-")

# take a barcode and convert to data.frame w/ actual info
barcode2index = function(ids) {
    m = stringr::str_match(ids, regex_barcode)

    dplyr::data_frame(Bio.ID=m[,1],
                      TSS.Code = m[,2],
                      Participant.ID = m[,3],
                      Code = m[,4],
                      Vial = m[,5],
                      Portion = m[,6],
                      Analyte = m[,7],
                      Plate.ID = m[,8],
                      Analysis.Center = m[,9]) %>%
        left_join(codes$tissue_source_site, by="TSS.Code") %>%
        left_join(codes$sample_type, by="Code") %>%
        left_join(codes$disease_study, by="Study.Name") %>%
        rename(Sample.Code = Code,
               Sample.Definition = Definition)
}

barcode2study = function(ids) {
    barcode2index(ids) %>%
        select(Study.Abbreviation) %>%
        unlist() %>%
        setNames(ids)
}
