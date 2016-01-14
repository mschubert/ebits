# this should use data_frame infrastructure to calculate NMF clustering, optionally in cluster jobs
.b = import_('../base')
.io = import_('../io')
.df = import_('../data_frame')

#' Wrapper function that takes k, num.clusterings and performs max_iter iterations
#'
#' @param A          data matrix [genes x samples]
#' @param k          the number of clusters (matrix rows)
#' @param max_iter   number of NMF iterations to perform
#' @param seed       the random seed
#' @param nsame      number of iterations the class assignments need to stay the same
#' @param num_clusterings  The number of clusterings to perform to compute consensus
#' @return a list of the results matrices W and H, with iterations performed
.nmf = function(A, k, max_iter, seed=1234, nsame=200, num_clusterings=10) {
    one_iter = function(seed) {
        # initialise matrices
        set.seed(seed)
        m = nrow(A)
        n = ncol(A)
        W = as.double(runif(m*k, 0, 1)) # standard random init in (0,1)
        H = as.double(runif(k*n, 0 ,1)) # could use libnmf's generatematrix for it

        # calls to add: nmf_als, mu, neals, alspg, pg
        dyn.load(.io$file_path(module_file(), "nmf_mu.so"))
        re = .C("nmf_mu", a=as.double(A), w0=as.double(W), h0=as.double(H),
                 pm=as.integer(m), pn=as.integer(n), pk=as.integer(k),
                 maxiter=as.integer(max_iter), nsame=as.integer(nsame),
                 DUP=FALSE, PACKAGE='nmf_mu')

        # return decomposed matrices
        list(W = matrix(re$w0, nrow=m, ncol=k, dimnames=list(rownames(A), NULL)),
             H = matrix(re$h0, nrow=k, ncol=n, dimnames=list(NULL, colnames(A))),
             iter = re$maxiter)
    }

    # run nmf for given k and reps
    stopifnot(length(k) == 1)
    A = as.matrix(A)
    sample_names = .b$descriptive_index(A, along=2)
    seed = rep(seed, num_clusterings) + 1:num_clusterings
    result = lapply(seed, one_iter)

    # get consensus clusters and sample similarity
    clusters = lapply(result, function(x) apply(x$H, 2, order)[1,])
    connect.matrix = Reduce('+', lapply(clusters, function(l)
            outer(l,l, function(x,y) as.integer(x==y)))) / length(clusters)

    # compute cophenetic coefficient TODO: how to decide if 2 clusters are better than one?
    dist.matrix = as.dist(1 - connect.matrix)
    HC = hclust(dist.matrix, method="average")
    dist.coph = cophenetic(HC)

    # return sample membership
    data.frame(sample = sample_names[HC$order],
               cluster = cutree(HC, k = k)[HC$order],
               rho = cor(dist.matrix, dist.coph))
}

#' Wrapper function that takes k, num.clusterings and performs max_iter iterations
#'
#' @param A          data matrix [genes x samples]
#' @param k          the number of clusters (matrix rows)
#' @param max_iter   number of NMF iterations to perform
#' @param seed       the random seed
#' @param nsame      number of iterations the class assignments need to stay the same
#' @param rep        The number of clusterings to perform to compute consensus
#' @param hpc_args   Optional arguments to pass to hpc module
#' @return           A data frame with sample names, cluster membership, and score
nmf = function(A, k, max_iter=1000, seed=1234, nsame=200, rep=10, hpc_args=NULL) {
    idf = .df$create_index(k = k, args=list(num_clusterings=rep, A=A,
                  max_iter=max_iter, seed=seed, nsame=nsame))
    .df$call(idf, fun=.nmf, hpc_args=hpc_args)
}

if (is.null(module_name())) {
    #TODO: write proper tests here
    #  this is just testing that the clustering runs without error
    data = data.matrix(iris[1:4])
    result = nmf(data, 2:4)
}
