# http://stackoverflow.com/questions/8139677
flatten = function(x, vectors=FALSE) {
    while(any(vapply(x, is.list, logical(1)))) {
        if (! vectors)
            x = lapply(x, function(x) if(is.list(x)) x else list(x))
        x = unlist(x, recursive=FALSE) 
    }
    x
}

# http://stackoverflow.com/questions/23056133
relist = function(x, like, vectors=FALSE) {
    if (! vectors)
        like = rapply(like, function(f) NA, how='replace')
    lapply(utils::relist(x, skeleton=like), function(e) unlist(e, recursive=F))
}

# apply fun preserving structure
llapply = function(x, fun, vectors=FALSE) {
    relist(lapply(flatten(x, vectors=vectors), fun), like=x, vectors=vectors)
}

transpose = function(x, simplify=TRUE) {
    re = as.list(data.frame(do.call(rbind, x)))
    if (simplify)
        lapply(re, function(r) sapply(r, function(x) x))
    else
        re
}

# subset each element of a list with subs
subset = function(x, subs) {
    mapply(function(x,s) x[s], x, subs)
}

# put the contents of the list in the namespace from which the function was called
extract = function(x) {
}

to_array = function(x, index_list, fill=NA) {
    index = do.call(expand.grid, index.list)

    if (is.null(names(x)))
        re = x
    else {
        re = setNames(rep(list(fill),nrow(index)), 1:nrow(index))
        re[names(x)] = x
    }

    array(re, dim=sapply(index.list, length), dimnames=index.list)
}
