.b = import('../../base')
.p = import('../path')

#MASTER_LIST = .p$load('gdsc', 'cell_lines/MASTER_LIST_03112013')
MASTER_LIST = .p$load('gdsc', 'cell_lines/20140320_MASTER_LIST')
MASTER_LIST$COSMIC.ID = as.character(MASTER_LIST$COSMIC.ID)

name2id = function(x, fuzzy_level=1, ...) {
    .b$match(x = x,
             from = MASTER_LIST$Cell.line.name,
             to = MASTER_LIST$COSMIC.ID,
             fuzzy_level = fuzzy_level, ...)
}

id2name = function(id, ...) {
    .b$match(x = as.character(id),
             from = MASTER_LIST$COSMIC.ID,
             to = MASTER_LIST$Cell.line.name,
             fuzzy_level = 0, ...)
}

#TODO: fix tissues to result or NA, no "" or "UNABLE_TO_CLASSIFY"
id2tissue = function(id, tcga=TRUE) {
    id = as.character(id)
    if (tcga)
        MASTER_LIST[id,'Study.Abbreviation']
    else
        MASTER_LIST[id,'GDSC.description_1']
}

name2tissue = . %>% name2id %>% id2tissue
