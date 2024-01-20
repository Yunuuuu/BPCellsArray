#' Delayed MatrixDir arrays
#'
#' The `BPCellsDirArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `MatrixDir` object in
#' BPCells.
#'
#' @importClassesFrom BPCells MatrixDir
#' @name BPCellsDir
#' @seealso [writeBPCellsDirArray]
methods::setClass("BPCellsDirSeed", contains = c("BPCellsSeed", "MatrixDir"))

#' @param x For `BPCellsDirSeed` and `BPCellsDirArray`, a path of the
#' `MatrixDir` data, or a `MatrixDir` object. For other function, a
#' `BPCellsDirSeed` or `BPCellsDirArray` object.
#' @inheritParams BPCells::open_matrix_dir
#' @export
#' @rdname BPCellsDir
BPCellsDirSeed <- function(x, buffer_size = 8192L) {
    if (rlang::is_string(x)) {
        matrix_dir <- BPCells::open_matrix_dir(
            dir = x, buffer_size = buffer_size
        )
    } else if (methods::is(x, "MatrixDir")) {
        matrix_dir <- x
    } else {
        cli::cli_abort(
            "{.arg x} must be a {.cls string} or {.cls MatrixDir} object"
        )
    }
    methods::as(matrix_dir, "BPCellsDirSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @rdname BPCellsDir
methods::setClass("BPCellsDirArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsDirSeed"),
    prototype = list(seed = methods::new("BPCellsDirSeed"))
)

#' @param seed A `BPCellsDirSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname BPCellsDir
methods::setMethod(
    "DelayedArray", "BPCellsDirSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsDirArray")
)

#' @param ... Additional parameters passed into `BPCellsDirSeed`.
#' @export
#' @rdname BPCellsDir
BPCellsDirArray <- function(x, ...) {
    DelayedArray(BPCellsDirSeed(x, ...))
}

#' @export
#' @rdname BPCellsDir
#' @include Class-BPCellsMatrix.R
methods::setClass("BPCellsDirMatrix",
    contains = c("BPCellsMatrix"),
    slots = c(seed = "BPCellsDirSeed")
)

#' @export
#' @importMethodsFrom DelayedArray matrixClass
#' @rdname BPCellsDir
methods::setMethod("matrixClass", "BPCellsDirArray", function(x) {
    "BPCellsDirMatrix"
})


###################################################################
###########################  Methods  #############################
###################################################################


#' Write a sparce matrices into a BPCells Directory of files format
#'
#' @param x Input matrix, any matrix can be coerced into
#' [dgCMatrix][Matrix::dgCMatrix-class] object.
#' @param path Directory to save the data into, if `NULL`, will use a temporary
#' directory.
#' @param ... Additional parameters passed to specific methods.
#' @inheritParams BPCells::write_matrix_dir
#' @return A [BPCellsDir] object.
#' @export
#' @name writeBPCellsDirArray
methods::setGeneric(
    "writeBPCellsDirArray",
    function(x, ...) standardGeneric("writeBPCellsDirArray")
)

.writeBPCellsDirArray <- function(
    x, path = NULL, compress = TRUE,
    buffer_size = 8192L,
    overwrite = FALSE) {
    path <- path %||% tempfile("BPCellsDirArray")
    obj <- BPCells::write_matrix_dir(
        mat = x, dir = path, compress = compress,
        buffer_size = buffer_size, overwrite = overwrite
    )
    BPCellsDirArray(obj)
}

#' @export
#' @rdname writeBPCellsDirArray
methods::setMethod("writeBPCellsDirArray", "ANY", function(x, ...) {
    .writeBPCellsDirArray(x = coerce_dgCMatrix(x), ...)
})

#' @export
#' @rdname writeBPCellsDirArray
methods::setMethod(
    "writeBPCellsDirArray", "IterableMatrix", .writeBPCellsDirArray
)

#' @export
#' @rdname writeBPCellsDirArray
methods::setMethod("writeBPCellsDirArray", "BPCellsSeed", .writeBPCellsDirArray)

#' @export
#' @rdname writeBPCellsDirArray
methods::setMethod("writeBPCellsDirArray", "dgCMatrix", .writeBPCellsDirArray)

.as_BPCellsDirArray <- function(from) writeBPCellsDirArray(from)

#' @export
methods::setAs("ANY", "BPCellsDirArray", .as_BPCellsDirArray)

#' @export
methods::setAs("ANY", "BPCellsDirMatrix", .as_BPCellsDirArray)

#' @export
#' @importMethodsFrom DelayedArray path
#' @rdname BPCellsDir
methods::setMethod("path", "BPCellsDirSeed", function(object) object@dir)

#' @param i,j Row and Column index.
#' @param drop Not used, always be `FALSE`.
#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsDir
methods::setMethod(
    "[", "BPCellsDirSeed",
    function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)
