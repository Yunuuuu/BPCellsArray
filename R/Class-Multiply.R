############################################################
# MatrixMultiply
mould_BPCells("BPCellsDelayedMultiply", "MatrixMultiply",
    remove = c("left", "right"),
    # BPCellsDelayedNaryOp: `seeds` slot
    contains = "BPCellsDelayedNaryOp"
)

#####################################################
methods::setMethod("to_DelayedArray", "MatrixMultiply", function(object) {
    slots <- setdiff(methods::slotNames(object), c("left", "right"))
    names(slots) <- slots
    slots <- lapply(slots, methods::slot, object = object)
    slots$seeds <- list(
        left = to_DelayedArray(object@left),
        right = to_DelayedArray(object@right)
    )
    rlang::inject(S4Vectors::new2(
        Class = "BPCellsDelayedMultiply", !!!slots, check = FALSE
    ))
})

methods::setMethod("to_BPCells", "BPCellsDelayedMultiply", function(object) {
    slots <- setdiff(methods::slotNames(object), "seeds")
    names(slots) <- slots
    slots <- lapply(slots, methods::slot, object = object)
    slots <- c(slots, lapply(object@seeds, to_BPCells))
    rlang::inject(
        S4Vectors::new2(Class = "MatrixMultiply", !!!slots, check = FALSE)
    )
})

summary.BPCellsDelayedMultiply <- function(object) {
    "Multiply sparse matrices"
}
methods::setMethod(
    "summary", "BPCellsDelayedMultiply",
    summary.BPCellsDelayedMultiply
)

##############################################################
#' Matrix Multiplication
#'
#' Multiplies two matrices, if they are conformable. If one argument is a
#' vector, it will be promoted to either a row or column matrix to make the two
#' arguments conformable. If both are vectors of the same length, it will return
#' the inner product (as a matrix).
#'
#' @param x,y One of `x` or `y` must be [BPCellsMatrix][BPCellsMatrix-class]
#' object, and the another must be a [BPCellsMatrix][BPCellsMatrix-class] object
#' or  any objects can be converted into [BPCellsSeed] object.
#' @return A dense matrix if one of `x` or `y` is a regular matrix or atomic
#' vector. Otherwise, a [BPCellsMatrix][BPCellsMatrix-class] object.
#' @importMethodsFrom BPCells %*%
#' @seealso
#'  - [crossprod][BPCells-crossprod]
#'  - [tcrossprod][BPCells-tcrossprod]
#' @name BPCells-Multiplication
NULL

#' @include Class-BPCellsMatrix.R
#' @importMethodsFrom BPCells %*%
#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "BPCellsMatrix"), function(x, y) {
        delayed <- x@delayed
        x <- to_BPCells(x@seed)
        y <- to_BPCells(y@seed)
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
        with_delayed(delayed, DelayedArray(methods::callGeneric()))
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "ANY"), function(x, y) {
        y <- DelayedArray(BPCellsSeed(y))
        methods::callGeneric()
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "ANY", y = "BPCellsMatrix"), function(x, y) {
        x <- DelayedArray(BPCellsSeed(x))
        methods::callGeneric()
    }
)

#################### Matrix multiplication ########################
# following methods return a dense matrix
#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "matrix"), function(x, y) {
        x <- to_BPCells(x@seed)
        y <- matrix_to_double(y)
        methods::callGeneric()
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "matrix", y = "BPCellsMatrix"), function(x, y) {
        y <- to_BPCells(y@seed)
        x <- matrix_to_double(x)
        methods::callGeneric()
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "numeric"), function(x, y) {
        x <- to_BPCells(x@seed)
        y <- matrix_to_double(y)
        methods::callGeneric()
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "numeric", y = "BPCellsMatrix"), function(x, y) {
        y <- to_BPCells(y@seed)
        x <- matrix_to_double(x)
        methods::callGeneric()
    }
)
