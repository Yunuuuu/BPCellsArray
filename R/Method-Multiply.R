############################################################
# MatrixMultiply
#' @importClassesFrom DelayedArray DelayedNaryOp
mould_BPCells("BPCellsDelayedMultiply", "MatrixMultiply",
    remove = c("left", "right"),
    # BPCellsDelayedNaryOp: `seeds` slot
    contains = c("BPCellsDelayedOp", "DelayedNaryOp")
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

#' @exportS3Method base::summary
summary.BPCellsDelayedMultiply <- function(object) {
    "Multiply sparse matrices"
}
methods::setMethod(
    "summary", "BPCellsDelayedMultiply",
    summary.BPCellsDelayedMultiply
)

#' @exportS3Method base::summary
summary.MatrixMultiply <- summary.BPCellsDelayedMultiply
methods::setMethod("summary", "MatrixMultiply", summary.BPCellsDelayedMultiply)

##############################################################
#' Matrix Multiplication
#'
#' Multiplies two matrices, if they are conformable. If one argument is a
#' vector, it will be promoted to either a row or column matrix to make the two
#' arguments conformable. If both are vectors of the same length, it will return
#' the inner product (as a matrix).
#'
#' @param x,y One of `x` or `y` must be a `r rd_matrix()`, and the another must
#' be a `r rd_matrix()` or a `r rd_seed()`.
#' @return A dense matrix if one of `x` or `y` is a regular matrix or atomic
#' vector. Otherwise, a `r rd_matrix()`.
#' @importMethodsFrom BPCells %*%
#' @seealso
#'  - [crossprod][BPCells-crossprod]
#'  - [tcrossprod][BPCells-tcrossprod]
#' @name BPCells-Multiplication
NULL

.multiply_BPCells <- function(x, y, call = rlang::caller_env()) {
    if (x@transpose != y@transpose) {
        if (x@transpose) {
            cli::cli_warn(c(
                "!" = paste(
                    "{.arg x} is transposed but {.arg y} is not",
                    "transposing the storage axis for {.arg x}",
                    sep = ", "
                ),
                INCOMPATIBLE_STORAGE_AXIS_MSG
            ), call = call)
            x <- BPCells::transpose_storage_order(x)
        } else {
            cli::cli_warn(c(
                "!" = paste(
                    "{.arg y} is transposed but {.arg x} is not",
                    "transposing the storage axis for {.arg y}",
                    sep = ", "
                ),
                INCOMPATIBLE_STORAGE_AXIS_MSG
            ), call = call)
            y <- BPCells::transpose_storage_order(y)
        }
    }
    x %*% y
}

#' @include Class-BPCellsMatrix.R
#' @importMethodsFrom BPCells %*%
#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "BPCellsMatrix"),
    function(x, y) {
        x <- to_BPCells(x@seed)
        y <- to_BPCells(y@seed)
        ans <- .multiply_BPCells(x, y)
        DelayedArray(ans)
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "ANY"),
    function(x, y) {
        y <- BPCellsSeed(y)
        x <- to_BPCells(x@seed)
        ans <- .multiply_BPCells(x, y)
        DelayedArray(ans)
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "ANY", y = "BPCellsMatrix"),
    function(x, y) {
        x <- BPCellsSeed(x)
        y <- to_BPCells(y@seed)
        ans <- .multiply_BPCells(x, y)
        DelayedArray(ans)
    }
)

#################### Matrix multiplication ########################
# following methods return a dense matrix
#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "matrix"),
    function(x, y) {
        y <- matrix_to_double(y)
        x <- to_BPCells(x@seed)
        methods::callGeneric()
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "matrix", y = "BPCellsMatrix"),
    function(x, y) {
        x <- matrix_to_double(x)
        y <- to_BPCells(y@seed)
        methods::callGeneric()
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "numeric"),
    function(x, y) {
        y <- matrix_to_double(y)
        x <- to_BPCells(x@seed)
        methods::callGeneric()
    }
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "numeric", y = "BPCellsMatrix"),
    function(x, y) {
        x <- matrix_to_double(x)
        y <- to_BPCells(y@seed)
        methods::callGeneric()
    }
)
