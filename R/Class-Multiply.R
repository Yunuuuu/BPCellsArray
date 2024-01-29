############################################################
# MatrixMultiply
methods::setClass("BPCellsMultiplySeed",
    contains = c("BPCellsNaryOpsSeed", get_class("MatrixMultiply")),
    slots = list(left = "BPCellsSeed", right = "BPCellsSeed")
)

#' @noRd
BPCellsMultiplySeed <- function(x) {
    x@left <- BPCellsSeed(x@left)
    x@right <- BPCellsSeed(x@right)
    methods::as(x, "BPCellsMultiplySeed")
}

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixMultiply", function(x) {
    BPCellsMultiplySeed(x = x)
})

methods::setMethod("entity", "BPCellsMultiplySeed", function(x) {
    list(left = x@left, right = x@right)
})

##############################################################
#' Matrix Multiplication
#'
#' Multiplies two matrices, if they are conformable. If one argument is a
#' vector, it will be promoted to either a row or column matrix to make the two
#' arguments conformable. If both are vectors of the same length, it will return
#' the inner product (as a matrix).
#'
#' @param x,y One of `x` or `y` must be [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object, and the another must be a matrix
#' which can be coerced into [dgCMatrix][Matrix::dgCMatrix-class].
#' @return Matrix multiplication, a [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object depends on the class of `x` or
#' `y`. 
#' @importMethodsFrom BPCells %*%
#' @name BPCells-Multiplication
NULL

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "BPCellsMatrix"), function(x, y) {
        DelayedArray(x@seed %*% y@seed)
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "dgCMatrix"), function(x, y) {
        DelayedArray(x@seed %*% y)
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "dgCMatrix", y = "BPCellsMatrix"), function(x, y) {
        DelayedArray(x %*% y@seed)
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "ANY"), function(x, y) {
        DelayedArray(x@seed %*% coerce_dgCMatrix(y))
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "ANY", y = "BPCellsMatrix"), function(x, y) {
        DelayedArray(coerce_dgCMatrix(x) %*% y@seed)
    }
)

# following methods return dense matrix
#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "matrix"), function(x, y) {
        x@seed %*% y
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "matrix", y = "BPCellsMatrix"), function(x, y) {
        x %*% y@seed
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "numeric"), function(x, y) {
        x@seed %*% y
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "numeric", y = "BPCellsMatrix"), function(x, y) {
        x %*% y@seed
    }
)

#################### BPCellsSeed methods ####################################
# Following methods used by internal
#' @importMethodsFrom BPCells %*%
#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsSeed", y = "BPCellsSeed"), function(x, y) {
        if (x@transpose != y@transpose) {
            if (x@transpose) {
                cli::cli_warn(
                    "{.arg x} is transposed but {.arg y} not, transposing the storage axis for {.arg x}" # nolint
                )
                x <- BPCells::transpose_storage_order(x)
            } else {
                cli::cli_warn(
                    "{.arg y} is transposed but {.arg x} not, transposing the storage axis for {.arg y}" # nolint
                )
                y <- BPCells::transpose_storage_order(y)
            }
        }
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importClassesFrom Matrix dgCMatrix
#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsSeed", y = "dgCMatrix"), function(x, y) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "dgCMatrix", y = "BPCellsSeed"), function(x, y) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#################### Matrix multiplication ########################
# following methods return a dense matrix
#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsSeed", y = "matrix"), function(x, y) {
        storage.mode(y) <- "double"
        methods::callNextMethod()
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "matrix", y = "BPCellsSeed"), function(x, y) {
        storage.mode(x) <- "double"
        methods::callNextMethod()
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsSeed", y = "numeric"), function(x, y) {
        storage.mode(y) <- "double"
        methods::callNextMethod()
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "numeric", y = "BPCellsSeed"), function(x, y) {
        storage.mode(x) <- "double"
        methods::callNextMethod()
    }
)
