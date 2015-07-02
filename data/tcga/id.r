library(modules)
library(dplyr)
.io = import('io')

# see https://tcga-data.nci.nih.gov/datareports/codeTablesReport.htm
.codes = list(centerCode = 'code_tables/center_code.txt',
              diseaseStudy = 'code_tables/disease_study.txt',
              platformCode = 'code_tables/platform_code.txt',
              sampleType = 'code_tables/sample_type.txt',
              tissueSourceSite = 'code_tables/tissue_source_site.txt',
              portionAnalyte = 'code_tables/portion_analyte.txt') %>%
    lapply(function(x) .io$read_table(x, header=TRUE, quote=NULL,
           na.strings=NULL, colClasses='character'))

# eg. TCGA-02-0001-01
#  TCGA - clinical centre - participant id - sample code
.biospecimen = paste("TCGA",            # TCGA identifer
                    "([0-9]+)",        # tissue source site (eg. GBM from MBA)
                    "([a-zA-Z0-9]+)",  # participant id (4 digit alphanumeric)
                    "([0-9]+)",        # tumor/normal id
                    sep = "-")

# eg. TCGA-02-0001-01C-01D-0182-01
#  (same as above)[D/R]NA,.. - 
.barcode = paste("(TCGA",           # TCGA identifer
                "[0-9]+",          # tissue source site (eg. GBM from MBA)
                "[a-zA-Z0-9]+",    # participant id (4 digit alphanumeric)
                "[0-9]+)([A-Z])",  # tumor/normal id; number of vial
                "([0-9]+)([A-Z])", # portion (numbered); analyte (eg. [D/R]NA)
                "([a-zA-Z0-9]+)",  # plate id (4 digit alphanumeric)
                "([0-9]+)",        # centre (eg. 01=BROAD GCC)
                sep = "-")

# 32-bit random string
# eg. ebf3e73f-41a0-4ca5-b608-fe1c629e16de
.id = paste("[a-zA-Z0-9]{8}",
           "[a-zA-Z0-9]{4}",
           "[a-zA-Z0-9]{4}",
           "[a-zA-Z0-9]{4}",
           "[a-zA-Z0-9]{12}",
           sep = "-")

# take a biospecimen id and convert to data.frame w/ actual info
biospecimen2index = function(ids) {
    m = stringr::str_match(ids, .biospecimen)

    dplyr::data_frame(Bio.ID=m[,1],
                      TSS.Code = m[,2],
                      Participant.ID = m[,3],
                      Code = m[,4]) %>%
        inner_join(.codes$tissueSourceSite, by="TSS.Code") %>%
        inner_join(.codes$sampleType, by="Code") %>%
        inner_join(.codes$diseaseStudy, by="Study.Name") %>%
        rename(Sample.Code = Code,
               Sample.Definition = Definition)
}

# take a barcode and convert to df
barcode2index = function(ids) {
    m = stringr::str_match(ids, .barcode)

    cbind(biospec2index(m[,2]),
          Barcode = m[,1],
          Vial = m[,3],
          Portion = m[,4],
          Analyte = m[,5],
          Plate.ID = m[,6],
          Analysis.Center = m[,7]) %>%
        inner_join(.codes$portionAnalyte, by=c(Analyte='Code')) %>%
        select(-Analyte) %>%
        rename(Analyte = Definition)
}
