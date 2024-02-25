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
    "tcrossprod", c(x = "BPCellsMatrix", y = "ANY"), function(x, y) {
        x %*% t(BPCellsSeed(y))
    }
)

#' @export
#' @rdname BPCells-tcrossprod
methods::setMethod(
    "tcrossprod", c(x = "ANY", y = "BPCellsMatrix"), function(x, y) {
        BPCellsSeed(x) %*% t(y)
    }
)

##############################################################
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

#' @export
#' @rdname BPCells-tcrossprod
methods::setMethod(
    "tcrossprod", c(x = "BPCellsMatrix", y = "numeric"), function(x, y) {
        x %*% matrix(x, nrow = 1L)
    }
)

#' @export
#' @rdname BPCells-tcrossprod
methods::setMethod(
    "tcrossprod", c(x = "numeric", y = "BPCellsMatrix"), function(x, y) {
        x %*% t(y)
    }
)
