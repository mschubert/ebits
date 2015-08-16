/**
 * NMF_MU - calculates the nmf using a multiplicative update method
 *
 * Purpose:
 *      This routine calculates a non negative matrix factorisation of a m by n matrix A
 *
 *      A = W * H
 *
 *      where A, W and H are non-negative matrices.
 *      W is a m by k matrix
 *      H is a k by n matrix
 *      k is the approximation factor
 *
 * Description: 
 *      The multiplicative update method as described in Berry (2006) (see references [1])
 *      is based on following algorithm (in Matlab notation):
 *
 *      W = rand(m, k);
 *      H = rand(k, n);
 *      for iter = 1:maxiter
 *          H = H .* (W' * A) ./ (W' * W * H + 10E-09)
 *          W = W .* (A * H') ./ (W * H * H' + 10E-09)
 *      end
 *
 *      This routine uses the BLAS "dgemm" routine for matrix-matrix-multiplications
 *
 * Arguments:
 *
 * a        in,     pointer to matrix to factorise
 *
 * w0       in,     pointer to initialised factor-matrix w0
 *      out,    pointer to final factor-matrix w
 *
 * h0       in,     pointer to initialised factor-matrix h0
 *      out,    pointer to final factor-matrix h
 *
 * m        in,     first dimension of a and w/w0
 *
 * n        in,     second dimension of a and h/h0
 *
 * k        in,     second dimension of w/w0 and first dimension of h/h0
 *
 * maxiter  in,     maximal number of iterations to run
 *      out,    number of iterations actually run
 *
 * TolX     in,     used in check for convergence, tolerance for maxchange of matrices w and h
 *
 * TolFun   in,     used in check for convergence, tolerance for root mean square residual
 */

//#define DEBUG_LEVEL 2
//#define ERROR_CHECKING


//defines a factor added to matrix elements when used as divisor to avoid division by zero
//----------------------------------------------------------------------------------------
#define ZERO_THRESHOLD 0.0
#define DIV_BY_ZERO_AVOIDANCE 1E-09
//#define DEBUG_LEVEL 2

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>

static inline void swap(double ** a, double ** b) {
    double* temp = *a;
    *a = *b;
    *b = temp;
}


// dgemm - BLAS routine
//---------------------
// used to calculate general matrix-matrix multiplications of the form
// C = alpha * A * B + beta * C
static inline int dgemm(char transa, char transb, int m, int n, int k, double alpha, double* a, int lda, double* b, int ldb, double beta, double* c, int ldc) {
    extern int dgemm_(char* transa, char* transb, int* m, int* n, int* k, double* alpha, double* a, int* lda, double* b, int* ldb, double* beta, double* c, int* ldc);
    return dgemm_(&transa, &transb, &m, &n, &k, &alpha, a, &lda, b, &ldb, &beta, c, &ldc);
}


// nmf multiplicative updating
//---------------------
double nmf_mu(double * a, double * w0, double * h0, int * pm, int * pn, \
              int * pk, int * maxiter, const int * nsame) 
{
    int m = * pm;
    int n = * pn;
    int k = * pk;
    const int ns = * nsame;

#ifdef PROFILE_NMF_MU
    struct timeval start, end;
    gettimeofday(&start, 0);
#endif

#if DEBUG_LEVEL >= 2
    printf("Entering nmf_mu\n");
#endif


#ifdef ERROR_CHECKING
  errno = 0;
#endif

  // definition of necessary dynamic data structures
  //...for calculating matrix h
  double* numerh = (double*) malloc(sizeof(double) *k*n);
  double* work1 = (double*) malloc(sizeof(double)*k*k); // used for calculation of h & w
  double* work2 = (double*) malloc(sizeof(double)*k*n);
  double* h = (double*) malloc(sizeof(double)*k*n);

  //...for calculating matrix w
  double* numerw = (double*) malloc(sizeof(double)*m*k);
  double* work2w = (double*) malloc(sizeof(double)*m*k);
  double* w = (double*) malloc(sizeof(double)*m*k);

  int classes[n];
  int niterWOclassChange = 0;


  //-------------------

#ifdef ERROR_CHECKING
  if (errno) {
    perror("Error allocating memory in nmf_mu");
    free(numerh);
    free(numerw);
    free(work1);
    free(work2);
    free(h);
    free(w);
    free(work2w);
    return -1;
  }
#endif


#if DEBUG_LEVEL >= 2
    printf("Allocated memory\n");
#endif

//Is ZERO_THRESHOLD _not_ defined then use machine epsilon as default
#ifndef ZERO_THRESHOLD
  const double ZERO_THRESHOLD = eps;
#endif

  
  //Loop-Indices
  int iter, i;



  // factorisation step in a loop from 1 to maxiter
  for (iter = 1; iter <= *maxiter; ++iter) {

#if DEBUG_LEVEL >= 2
    printf("%i ", iter);
#endif

    // calculating matrix h
    //----------------
    // calculating numerh = w0'*a
    dgemm('T', 'N', k, n, m, 1.0, w0, m, a, m, 0., numerh, k);  
    // calculating first intermediate result work1 = w0'*w0
    dgemm('T', 'N', k, k, m, 1.0, w0, m, w0, m, 0., work1, k);
    // calculating second intermediate result work2 = work1 * h0
    dgemm('N', 'N', k, n, k, 1.0, work1, k, h0, k, 0., work2, k);

    //calculating h = h0 .* (numerh ./(work2 + eps))
    //set elements < zero_threshold to zero
    double tmp_element;
    for(i = 0; i< k*n; ++i) {
        if ( h0[i] == 0. || numerh[i]  == 0.)
            h[i] = 0.;
        else {
            tmp_element = h0[i] * (numerh[i] / (work2[i] + DIV_BY_ZERO_AVOIDANCE));
            h[i] = (tmp_element < ZERO_THRESHOLD) ? 0. : tmp_element;
        }
    }

    // calculating matrix w
    //----------------------------
    // calculating numerw = a*h'
    dgemm('N', 'T', m, k, n, 1.0, a, m, h, k, 0., numerw, m);
    // calculating first intermediate result work1 = h*h' (kxk-Matrix) => re-use of work1
    dgemm('N', 'T', k, k, n, 1.0, h, k, h, k, 0., work1, k);
    // calculating second intermediate result work2w = w0 * work1
    dgemm('N', 'N', m, k, k, 1.0, w0, m, work1, k, 0., work2w, m);

    //calculating w = w0 .* (numerw ./ (work2w + eps))
    //set elements < zero_threshold to zero
    for(i = 0; i < m*k; ++i) {
        if ( w0[i] == 0. || numerw[i] == 0.)
            w[i] = 0.;
        else {
            tmp_element = w0[i] * (numerw[i] / (work2w[i] + DIV_BY_ZERO_AVOIDANCE));
            w[i] = (tmp_element < ZERO_THRESHOLD) ? 0. : tmp_element;
        }
    }

    // storing the matrix results of the current iteration in W0 respectively H0
    swap(&w0, &w);
    swap(&h0, &h);

    // check for convergence
    if (iter > 1) {
        // check class assignment in h0
        int i,j,sameClasses=1;
        for(i=0; i<n; i++) {
            int biggestInRow = 0;
            for (j=1; j<k; j++) {
                if (h0[j*n+i] > h0[(j-1)*n+i]) // row-major: is this right?
//                if (h0[k*i+j] > h0[k*i+j-1]) // column-major
                    biggestInRow = j;
            }
            if (classes[i] != biggestInRow) {
                sameClasses = 0;
                classes[i] = biggestInRow;
            }
        }
        if (sameClasses) {
            niterWOclassChange++;
            if (niterWOclassChange >= ns && iter%2==0) {
                *maxiter = iter;
                break;
            }
        }
        else {
            niterWOclassChange = 0;
        }
    }
  } //end of loop from 1 to maxiter

//#if DEBUG_LEVEL > 2
    printf("Exiting nmf_mu after %i\n", iter);
//#endif
#ifdef PROFILE_NMF_MU
    gettimeofday(&end, 0);
    outputTiming("", start, end);
#endif

  // freeing memory if used
  free(numerh);
  free(numerw);
  free(work1);
  free(work2);
  free(h);
  free(w);
  free(work2w);

  // returning calculated norm
  return 0;//dnorm;
}
//end of nmf_mu
//-------------
