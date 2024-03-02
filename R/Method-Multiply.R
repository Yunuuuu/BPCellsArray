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

.multiply_BPCells <- function(x, y, call = rlang::caller_env()) {
    if (x@transpose != y@transpose) {
        if (x@transpose) {
            cli::cli_warn(c(
                "!" = c_msg(
                    "{.arg x} is transposed but {.arg y} is not",
                    "transposing the storage axis for {.arg x}",
                    sep = ", "
                ),
                INCOMPATIBLE_STORAGE_AXIS_MSG
            ), call = call)
            x <- BPCells::transpose_storage_order(x)
        } else {
            cli::cli_warn(c(
                "!" = c_msg(
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
    array_call_BPCells_method(
        x = , y = ,
        method = quote(.multiply_BPCells(x, y)),
        Arrays = c("x", "y")
    )
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "ANY"),
    array_call_BPCells_method(
        x = , y = ,
        before = expression(y <- BPCellsSeed(y)),
        method = quote(.multiply_BPCells(x, y))
    )
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "ANY", y = "BPCellsMatrix"),
    array_call_BPCells_method(
        x = , y = ,
        before = expression(x <- BPCellsSeed(x)),
        method = quote(.multiply_BPCells(x, y)),
        Arrays = "y"
    )
)

#################### Matrix multiplication ########################
# following methods return a dense matrix
#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "matrix"),
    array_call_BPCells_method(
        x = , y = ,
        before = expression(y <- matrix_to_double(y)),
        Arrays = "x", convert = FALSE
    )
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "matrix", y = "BPCellsMatrix"),
    array_call_BPCells_method(
        x = , y = ,
        before = expression(x <- matrix_to_double(x)),
        Arrays = "y", convert = FALSE
    )
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "numeric"),
    array_call_BPCells_method(
        x = , y = ,
        before = expression(y <- matrix_to_double(y)),
        Arrays = "x", convert = FALSE
    )
)

#' @export
#' @rdname BPCells-Multiplication
methods::setMethod(
    "%*%", c(x = "numeric", y = "BPCellsMatrix"),
    array_call_BPCells_method(
        x = , y = ,
        before = expression(x <- matrix_to_double(x)),
        Arrays = "y", convert = FALSE
    )
)
