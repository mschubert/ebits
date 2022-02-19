#' List all availables collections
dbs = function() {
    msigdb::listSubCollections(msigdb::getMsigdb())
}
