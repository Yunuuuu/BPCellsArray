#' Matrix Products of Transpose
#'
#' Given matrices x and y as arguments, return a matrix product of Transpose.
#'
#' @inheritParams BPCells-Multiplication
#' @importFrom Matrix tcrossprod
#' @inherit BPCells-Multiplication return
#' @seealso
#'  - [%*%][BPCells-Multiplication]
#'  - [crossprod][BPCells-crossprod]
#' @aliases tcrossprod
#' @name BPCells-tcrossprod
NULL

#' @export
#' @rdname BPCells-tcrossprod
methods::setMethod(
    "tcrossprod", c(x = "BPCellsMatrix", y = "BPCellsMatrix"), function(x, y) {
        x %*% t(y)
    }
)

#' @export
#' @rdname BPCells-tcrossprod
methods::setMethod(
    "tcrossprod", c(x = "BPCellsMatrix", y = "dgCMatrix"), function(x, y) {
        x %*% t(y)
    }
)

#' @export
#' @rdname BPCells-tcrossprod
methods::setMethod(
    "tcrossprod", c(x = "dgCMatrix", y = "BPCellsMatrix"), function(x, y) {
        x %*% t(y)
    }
)

#' @export
#' @rdname BPCells-tcrossprod
methods::setMethod(
    "tcrossprod", c(x = "BPCellsMatrix", y = "ANY"), function(x, y) {
        x %*% t(coerce_dgCMatrix(y))
    }
)

#' @export
#' @rdname BPCells-tcrossprod
methods::setMethod(
    "tcrossprod", c(x = "ANY", y = "BPCellsMatrix"), function(x, y) {
        coerce_dgCMatrix(x) %*% t(y)
    }
)

#' @export
#' @rdname BPCells-tcrossprod
methods::setMethod(
    "tcrossprod", c(x = "BPCellsMatrix", y = "matrix"), function(x, y) {
        x %*% t(y)
    }
)

#' @export
#' @rdname BPCells-tcrossprod
methods::setMethod(
    "tcrossprod", c(x = "matrix", y = "BPCellsMatrix"), function(x, y) {
        x %*% t(y)
    }
)
