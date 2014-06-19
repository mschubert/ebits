# String helper functions

import('./functional', attach = TRUE)

lstrip = lp(sub, '^ +', '')

rstrip = lp(sub, ' +$', '')

strip = lstrip %.% rstrip

# FIXME Vectorize
rev = p(paste, collapse = '') %.% base::rev %.% item(1) %.% p(strsplit, '')

capitalize =
    p(fapply, toupper %.% p(substring, 1, 1), p(substring, 2)) %|>%
    lp(do.call, paste0)

#' @TODO Make vectorised
readable = capitalize %.% lp(gsub, '_|-', ' ')
