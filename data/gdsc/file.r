.p = import('../path')

.files = list(
    MASTER_LIST = "20140320_MASTER_LIST.ro",
    ANALYSIS_SET = "",
    BASAL_EXPRESSION = "BASAL_EXPRESSION_12062013v2.ro",
    DRUG_PROPS = "DRUG_ANALYSIS_SET_20150123.rdata",
    DRUG_IC50 = "dfIC50v17.RData",
    DRUG_AUC = "dfAUCv17.RData",
    INTOGEN_DRIVERS = "cancer_drivers_5_2_2014.ro",
    CONC = "screenConcentration.RData"
)

get = function(id) .p$load('gdsc', .files[[id]])
