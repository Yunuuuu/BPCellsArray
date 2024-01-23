#' Matrix Crossproduct
#'
#' Given matrices x and y as arguments, return a matrix cross-product.
#'
#' @inheritParams BPCells-Multiplication
#' @importFrom DelayedArray crossprod
#' @return Matrix Crossproduct, a [BPCellsMatrix] object or a dense matrix
#'   (matrix and numeric methods).
#' @seealso [%*%]
#' @aliases crossprod
#' @name BPCells-crossprod
NULL

#' @export
#' @rdname BPCells-crossprod
methods::setMethod(
    "crossprod", c(x = "BPCellsMatrix", y = "BPCellsMatrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCells-crossprod
methods::setMethod(
    "crossprod", c(x = "BPCellsMatrix", y = "dgCMatrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCells-crossprod
methods::setMethod(
    "crossprod", c(x = "dgCMatrix", y = "BPCellsMatrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCells-crossprod
methods::setMethod(
    "crossprod", c(x = "BPCellsMatrix", y = "ANY"), function(x, y) {
        t(x) %*% coerce_dgCMatrix(y)
    }
)

#' @export
#' @rdname BPCells-crossprod
methods::setMethod(
    "crossprod", c(x = "ANY", y = "BPCellsMatrix"), function(x, y) {
        t(coerce_dgCMatrix(x)) %*% y
    }
)

#' @export
#' @rdname BPCells-crossprod
methods::setMethod(
    "crossprod", c(x = "BPCellsMatrix", y = "matrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCells-crossprod
methods::setMethod(
    "crossprod", c(x = "matrix", y = "BPCellsMatrix"), function(x, y) {
        t(x) %*% y
    }
)

####################    BPCellsSeed methods    #######################
#' @importFrom DelayedArray crossprod
#' @export
#' @rdname seed-methods
methods::setMethod(
    "crossprod", c(x = "BPCellsSeed", y = "BPCellsSeed"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname seed-methods
methods::setMethod(
    "crossprod", c(x = "BPCellsSeed", y = "dgCMatrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname seed-methods
methods::setMethod(
    "crossprod", c(x = "dgCMatrix", y = "BPCellsSeed"), function(x, y) {
        t(x) %*% y
    }
)

#################### Matrix Crossproduct ########################
# following methods return a dense matrix
#' @export
#' @rdname seed-methods
methods::setMethod(
    "crossprod", c(x = "BPCellsSeed", y = "matrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname seed-methods
methods::setMethod(
    "crossprod", c(x = "matrix", y = "BPCellsSeed"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname seed-methods
methods::setMethod(
    "crossprod", c(x = "BPCellsSeed", y = "numeric"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname seed-methods
methods::setMethod(
    "crossprod", c(x = "numeric", y = "BPCellsSeed"), function(x, y) {
        t(x) %*% y
    }
)
