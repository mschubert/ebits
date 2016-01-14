# String helper functions

import_('./functional', attach = TRUE)

ltrim = lp(sub, '^ +', '')

rtrim = lp(sub, ' +$', '')

trim = ltrim %.% rtrim

# FIXME Vectorize
rev = p(paste, collapse = '') %.% base::rev %.% item(1) %.% p(strsplit, '')

capitalize =
    p(fapply, toupper %.% p(substring, 1, 1), p(substring, 2)) %|>%
    lp(do.call, paste0)

#' @TODO Make vectorised
readable = capitalize %.% lp(gsub, '_|-', ' ')
