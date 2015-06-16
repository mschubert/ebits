.jp = function(...) gsub("//", "/", paste(..., sep="/"))

.cleandf = function(df) {
    df = c(as.list(df), list(stringsAsFactors=F, row.names = rownames(df)))

    do.call(data.frame, lapply(df, function(x) {
        ulx = unlist(x)
        if (is.factor(ulx))
            as.character(ulx)
        else
            ulx
    }))
}

.names = function(X, expand.NULL=TRUE) {
    if (is.data.frame(X))
        list(rownames(X), colnames(X))
    else if (is.vector(X))
        names(X)
    else if (is.null(dimnames(X)) && expand.NULL)
        rep(list(NULL), length(dim(X)))
    else
        dimnames(X)
}

`.names<-` = function(X, value) {
    if (is.data.frame(X)) {
        rownames(X) = value[1][[1]]
        colnames(X) = value[2][[1]]
    } else if (is.vector(X))
        names(X) = value[[1]]
    else
        dimnames(X) = value
    X
}

.setNames = function(X, value) {
    .names(X) = value
    X
}

#' Returns the number of dimensions for objects in HDF5 files
#'
#' @param file   File name to load
#' @param path   Path within file to load; default: /
h5dim = function(file, path) {
    #TODO: check if value in path
    # if yes -> .jp(path, 'value')
    loc = rhdf5:::h5checktypeOrOpenLoc(file, readonly=TRUE)
    h5dataset = rhdf5::H5Dopen(loc$H5Identifier, path)
    h5spaceFile = rhdf5::H5Dget_space(h5dataset)
    dims = rhdf5::H5Sget_simple_extent_dims(h5spaceFile)$size
    rhdf5:::h5closeitLoc(loc)
    rhdf5::H5close()
    dims
}

#' Returns the names along each dimension for HDF5 files
#'
#' @param file   File name to load
#' @param path   Path within file to load; default: /
#' @param index  Subsets to load - either vector along first dimension,
#'               or a list of vectors for each dimension
#' @param return.index  Return a list of the index and the object
#' @return       The names of the object
h5names = function(file, path, index=NULL, return.index=FALSE) {
    file_ls = rhdf5::h5ls(file)
    objs = file_ls$name[file_ls$group==path]

    ndim = length(h5dim(file, .jp(path, 'value')))
    if (is.null(index))
        index = list(NULL)
    length(index) = ndim
    dim_names = rep(list(NULL), ndim)

    for (i in seq_along(index)) {
        name_i = paste0("names_", i)

        if (is.character(index[[i]])) {
            dn = rhdf5::h5read(file, .jp(path, name_i))
            index[[i]] = match(index[[i]], dn)
            dim_names[[i]] = dn[index[[i]]]

        } else {
            if (name_i %in% objs)
                dim_names[[i]] = rhdf5::h5read(file, .jp(path, name_i), index=index[i])
        }
    }

    rhdf5::H5close()

    if (return.index)
        list(dim_names=dim_names, index=index)
    else
        dim_names
}

#' Saves HDF5 files
#'
#' @param X     Object to save
#' @param file  File name to save to
h5save = function(X, file) {
    node2group = function(file, path, node) {
        if (path != "/")
            rhdf5::h5createGroup(file, path)

        if (is.list(node) && !is.data.frame(node)) {
            for (j in seq_along(node))
                node2group(file, .jp(path, names(node)[j]), node[[j]])
        } else {
            if (is.data.frame(node))
                node = .cleandf(node)

            rhdf5::h5write(node, file, .jp(path, "value"), level=0)

            dn = .names(node)
            for (j in 1:length(dn))
               if (!is.null(dn[[j]]))
                   rhdf5::h5write(dn[[j]], file, .jp(path, paste0("names_", j)))
        }
    }

    rhdf5::h5createFile(file)
    node2group(file, "/", X)
    rhdf5::H5close()
}

#' Loads HDF5 files
#'
#' @param file   File name to load
#' @param path   Path within file to load; default: /
#' @param index  Subsets to load - either vector along first dimension,
#'               or a list of vectors for each dimension
#' @return       An R object with names
h5load = function(file, path="/", index=NULL) {
    group2node = function(path, index) {
        # get all objects in the current path
        objs = file_ls$name[file_ls$group==path]

        if ('value' %in% objs) {
            # construct object with 'value' and 'names_*'
            dn = h5names(file, path, index, return.index=TRUE)
            value = rhdf5::h5read(file, .jp(path, 'value'), index=dn$index)
            .setNames(value, dn$dim_names)

        } else {
            paths = sapply(objs, function(o) .jp(path, o))

            setNames(lapply(seq_along(paths), function(i)
                            group2node(paths[i], index)), objs)
        }
    }

    if (!is.list(index))
        index = list(index)

    file_ls = rhdf5::h5ls(file)
    result = group2node(path, index)
    rhdf5::H5close()
    result
}
