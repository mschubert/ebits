#' Perform a HG test for all sigs vs all lists
hypergeometric_test = function(background, lists, samples, p.adjust="fdr") {
    calc = function(back, li, sam) {
        q = sum(sam %in% li) # number of positive balls in the sample
        m = sum(li %in% back) # number of positive balls in the urn
        n = length(background) - m # number of negative balls in the urn
        k = length(sam) # number of balls in the sample
        1 - phyper(q-1, m, n, k) # p-value
    }

    if (!is.list(lists))
        lists = list(lists)
    if (!is.list(samples))
        samples = list(samples)

    A = matrix(NA, nrow=length(lists), ncol=length(samples),
               dimnames=list(names(lists), names(samples)))

    for (li in seq_along(lists))
        for (si in seq_along(samples))
            A[li,si] = calc(background, lists[[li]], samples[[si]])

    A[] = p.adjust(A, method=p.adjust)
    drop(A)
}
