#' Matrix Crossproduct
#'
#' Given matrices x and y as arguments, return a matrix cross-product.
#'
#' @inheritParams BPCells-Multiplication
#' @importFrom Matrix crossprod
#' @inherit BPCells-Multiplication return
#' @seealso
#'  - [%*%][BPCells-Multiplication]
#'  - [tcrossprod][BPCells-tcrossprod]
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
    "crossprod", c(x = "BPCellsMatrix", y = "ANY"), function(x, y) {
        t(x) %*% BPCellsSeed(y)
    }
)

#' @export
#' @rdname BPCells-crossprod
methods::setMethod(
    "crossprod", c(x = "ANY", y = "BPCellsMatrix"), function(x, y) {
        t(BPCellsSeed(x)) %*% y
    }
)

#################################################################
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

#' @export
#' @rdname BPCells-crossprod
methods::setMethod(
    "crossprod", c(x = "BPCellsMatrix", y = "numeric"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCells-crossprod
methods::setMethod(
    "crossprod", c(x = "numeric", y = "BPCellsMatrix"), function(x, y) {
        matrix(x, nrow = 1L) %*% y
    }
)
