# this should use data_frame infrastructure to calculate NMF clustering, optionally in cluster jobs
.b = import('../base')
.df = import('../data_frame')

#' Wrapper function that takes k, num.clusterings and performs max_iter iterations
#'
#' @param A          data matrix [genes x samples]
#' @param k          the number of clusters (matrix rows)
#' @param max_iter   number of NMF iterations to perform
#' @param seed       the random seed
#' @param tolerance  tolerance for convergence checks in updates
#' @param num_clusterings  The number of clusterings to perform to compute consensus
#' @return a list of the results matrices W and H, with iterations performed
.nmf = function(A, k, max_iter, seed=1234, tolerance=1e-4, num_clusterings=10) {
    one_iter = function(seed) {
        # initialise matrices
        set.seed(seed)
        m = nrow(A)
        n = ncol(A)
        W = as.double(runif(m*k, 0, 1)) # standard random init in (0,1)
        H = as.double(runif(k*n, 0 ,1)) # could use libnmf's generatematrix for it

        # calls to add: nmf_als, mu, neals, alspg, pg
        dyn.load("libnmf.so")
        re = .C("nmf_mu", a=as.double(A), w0=as.double(W), h0=as.double(H),
                 pm=as.integer(m), pn=as.integer(n), pk=as.integer(k),
                 maxiter=as.integer(max_iter), pTolX=as.double(tolerance),
                 pTolFun=as.double(tolerance), DUP=FALSE, PACKAGE='libnmf')

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
                            outer(l,l, function(x,y) as.integer(x==y)))) /
                     length(clusters)

    # compute cophenetic coefficient
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
#' @param tolerance  tolerance for convergence checks in updates
#' @param rep        The number of clusterings to perform to compute consensus
#' @param hpc_args   Optional arguments to pass to hpc module
#' @return           A data frame with sample names, cluster membership, and score
nmf = function(A, k, max_iter=1000, seed=1234, tolerance=1e-4, rep=10, hpc_args=NULL) {
    .df$call(data.frame(k = k),
             .nmf,
             A=A, max_iter=max_iter, seed=seed, tolerance=tolerance,
             hpc_args=hpc_args)
}
