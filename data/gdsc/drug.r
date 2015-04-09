.b = import('../../base')
.p = import('../path')

DRUG_PROPS = .p$load('gdsc', 'Drugs/props_public.RData') # v18

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
