#' BiocSingularParam classes
#'
#' Find the Largest k Singular Values/Vectors of a Matrix using `RSpectra`
#' package.
#'
#' @importClassesFrom BiocSingular BiocSingularParam
#' @name SpectraParam
NULL

#' @export
#' @rdname SpectraParam
methods::setClass("SpectraParam",
    contains = "BiocSingularParam",
    slots = c(deferred = "logical", fold = "numeric")
)

#' @export
#' @rdname SpectraParam
SpectraParam <- function() {
    methods::new("SpectraParam", deferred = FALSE, fold = Inf)
}

#' @param ncv Number of Lanzcos basis vectors to use. More vectors will result
#' in faster convergence, but with greater memory use. `ncv` must be
#' satisfy \eqn{k < ncv \le p}{k < ncv <= p} where `p = min(m, n)`.
#' Default is `min(p, max(2*k+1, 20))`.
#' @param tol Precision parameter. Default is 1e-10.
#' @param maxitr Maximum number of iterations. Default is 1000.
#' @param center Either a logical value (`TRUE`/`FALSE`), or a numeric
#' vector of length \eqn{n}. If a vector \eqn{c} is supplied, then SVD is
#' computed on the matrix \eqn{A - 1c'}{A - 1 * c'}, in an implicit way without
#' actually forming this matrix.  `center = TRUE` has the same effect as
#' `center = colMeans(A)`. Default is `FALSE`. Ignored if x is a
#' `IterableMatrix` object.
#' @param scale Either a logical value (`TRUE`/`FALSE`), or a numeric
#' vector of length \eqn{n}. If a vector \eqn{s} is supplied, then SVD is
#' computed on the matrix \eqn{(A - 1c')S}{(A - 1 * c')S}, where \eqn{c} is the
#' centering vector and \eqn{S = diag(1/s)}.If `scale = TRUE`, then the
#' vector \eqn{s} is computed as the column norm of \eqn{A - 1c'}{A - 1 *
#' c'}.Default is `FALSE`. Ignored if x is a `IterableMatrix` object.
#' @inheritParams BPCells::svds
#' @inheritParams RSpectra::svds
#' @inheritParams BiocSingular::runSVD
#' @param ... Not used currently
#' @seealso [RSpectra][RSpectra::svds] and [BPCells][BPCells::svds]
#' @export
#' @importFrom BiocSingular runSVD
#' @rdname SpectraParam
methods::setMethod(
    "runSVD", "SpectraParam",
    function(x, k, nu = k, nv = k, center = FALSE, scale = FALSE, ncv = NULL,
             tol = 1e-10, maxitr = 1000, threads = 0L, ..., BSPARAM) {
        SpectraSVD(
            x = x, k = k, nu = nu, nv = nv,
            center = center, scale = scale, ncv = ncv,
            tol = tol, maxitr = maxitr,
            threads = threads
        )
    }
)

methods::setGeneric("SpectraSVD", function(x, ...) {
    standardGeneric("SpectraSVD")
})
methods::setMethod(
    "SpectraSVD", "ANY",
    function(x, k, nu, nv, center, scale, ncv, tol, maxitr, threads) {
        arg <- list(tol = tol, maxitr = maxitr, center = center, scale = scale)
        if (!is.null(ncv)) arg <- c(list(ncv = ncv), arg)
        out <- RSpectra::svds(A = x, k = k, nu = nu, nv = nv, opts = arg)
        out[c("d", "u", "v")]
    }
)

methods::setMethod("SpectraSVD", "BPCellsMatrix", function(x, ...) {
    SpectraSVD(x = entity(x), ...)
})

methods::setMethod(
    "SpectraSVD", "IterableMatrix",
    function(x, k, nu, nv, center, scale, ncv, tol, maxitr, threads) {
        arg <- list(tol = tol, maxitr = maxitr)
        if (!is.null(ncv)) arg <- c(list(ncv = ncv), arg)
        out <- BPCells::svds(
            A = x, k = k, nu = nu, nv = nv,
            opts = arg, threads = threads
        )
        out[c("d", "u", "v")]
    }
)
