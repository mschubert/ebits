.ar = import('../array')

merge = function(method, X, batch, covariate=NULL) {
    if (method == "combat")
        combat(X, batch, covariate)
    else if (method == "dwd")
        dwd(X, batch)
    else if (method == "none")
        none(X)
    else
        stop("invalid method")
}

combat = function(X, batch, covariate=NULL) {
    if (is.matrix(covariate) || is.null(covariate))
        mat = covariate
    else
        mat = model.matrix(~as.factor(batch), data=covariate)
    sva::ComBat(dat=.list2mat(X), batch=batch, mod=mat, par.prior=TRUE)
}

dwd = function(X, batch) {
    ll = .mat2list(X, subsets=batch)
    esets = lapply(ll, ExpressionSet)
    eset = inSilicoMerging::merge(esets, method="DWD")
    exprs(eset)
}

none = function(X, batch=NA) {
    .list2mat(X)
}

.list2mat = function(ll) {
    if (is.list(ll))
        .ar$stack(ll, along=2)
    else
        ll
}

.mat2list = function(X, subsets) {
    if (is.matrix(X))
        .ar$split(X, along=2, subsets=subsets)
    else
        X
}
