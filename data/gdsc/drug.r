.b = import('../../base')
.file = import('./file')

DRUG_PROPS = .file$get('DRUG_PROPS')

name2id = function(x, fuzzy_level=1, table=FALSE) {
    .b$match(x = x,
             from = DRUG_PROPS$DRUG_NAME,
             to = DRUG_PROPS$DRUG_ID,
             fuzzy_level = fuzzy_level, table = table)
}

id2name = function(id, table=FALSE) {
    .b$match(x = as.character(id),
             from = DRUG_PROPS$DRUG_ID,
             to = DRUG_PROPS$DRUG_NAME,
             fuzzy_level = 0, table = table)
}
