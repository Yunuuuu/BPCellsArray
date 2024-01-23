#' Delayed MatrixDir arrays
#'
#' The `BPCellsDirArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `MatrixDir` object in
#' BPCells.
#'
#' @param x For Specific functions:
#' - `BPCellsDirArray`: A `MatrixDir` object.
#' - `matrixClass`: A `BPCellsDirArray` object.
#' @name BPCellsDir
#' @seealso
#' - [BPCellsSeed]
#' - [writeBPCellsDirArray]
NULL

methods::setClass("BPCellsDirSeed",
    contains = c("BPCellsSeed", get_class("MatrixDir"))
)

#' @param x A `BPCellsDirSeed` or `BPCellsDirArray` object. For following
#' functions:
#' - `BPCellsDirSeed` and `BPCellsDirArray`: A string for the path of the
#'   BPCells matrix dir.
#' @rdname BPCellsDir
#' @noRd
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
#' @export
#' @rdname BPCellsDir
methods::setClass("BPCellsDirArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsDirSeed")
)

#' @param seed A `BPCellsDirSeed` object.
#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsDir
methods::setMethod(
    "DelayedArray", "BPCellsDirSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsDirArray")
)

#' @inheritDotParams BPCells::open_matrix_dir buffer_size
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

#' @importFrom DelayedArray matrixClass
#' @export
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
#' @param ...
#' - For `BPCellsMatrix` method: additional parameters passed to `BPCellsSeed`
#'   methods.
#' - For `ANY` method: additional parameters passed to `dgCMatrix` methods.
#' @inheritParams BPCells::write_matrix_dir
#' @return A [BPCellsDirMatrix][BPCellsDir] object.
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
methods::setMethod(
    "writeBPCellsDirArray", "IterableMatrix", .writeBPCellsDirArray
)

#' @export
#' @rdname writeBPCellsDirArray
methods::setMethod("writeBPCellsDirArray", "BPCellsSeed", .writeBPCellsDirArray)

#' @export
#' @rdname writeBPCellsDirArray
methods::setMethod("writeBPCellsDirArray", "BPCellsMatrix", function(x, ...) {
    .writeBPCellsDirArray(x = x@seed, ...)
})

#' @export
#' @rdname writeBPCellsDirArray
methods::setMethod("writeBPCellsDirArray", "dgCMatrix", .writeBPCellsDirArray)

#' @export
#' @rdname writeBPCellsDirArray
methods::setMethod("writeBPCellsDirArray", "ANY", function(x, ...) {
    .writeBPCellsDirArray(x = coerce_dgCMatrix(x), ...)
})

.as_BPCellsDirArray <- function(from) writeBPCellsDirArray(from)

#' @export
methods::setAs("ANY", "BPCellsDirArray", .as_BPCellsDirArray)

#' @export
methods::setAs("ANY", "BPCellsDirMatrix", .as_BPCellsDirArray)

#' @export
methods::setAs("BPCellsMatrix", "dgCMatrix", function(from) {
    methods::as(from@seed, "dgCMatrix")
})

#' @export
methods::setAs("BPCellsMatrix", "matrix", function(from) {
    as.matrix(from@seed)
})
