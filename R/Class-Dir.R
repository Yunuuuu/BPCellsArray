methods::setClass("BPCellsDirSeed",
    contains = c("BPCellsSeed", get_class("MatrixDir"))
)

BPCellsDirSeed <- function(x) methods::as(x, "BPCellsDirSeed")

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixDir", function(x) {
    BPCellsDirSeed(x = x)
})

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsSeed
#' @include Class-BPCellsMatrix.R
methods::setClass("BPCellsDirArray",
    contains = "BPCellsArray",
    slots = c(seed = "BPCellsDirSeed")
)

#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "DelayedArray", "BPCellsDirSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsDirArray")
)

#' @export
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsDirMatrix",
    contains = c("BPCellsMatrix"),
    slots = c(seed = "BPCellsDirSeed")
)

#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("matrixClass", "BPCellsDirArray", function(x) {
    "BPCellsDirMatrix"
})

#' Read/write sparse matrices from (or into) directory on disk
#' 
#' @description
#' - `readBPCellsDirMatrix`: read a sparce matrices from a directory on disk
#' - `writeBPCellsDirArray`: Write a sparce matrices into a directory on disk
#' @param path A string path of Directory to read or save the data into. For
#' `writeBPCellsDirArray`, if `NULL`, will use a temporary directory.
#' @inheritParams BPCells::open_matrix_dir
#' @export
#' @name BPCellsDir-IO
readBPCellsDirMatrix <- function(path, buffer_size = 8192L) {
    assert_string(path, empty_ok = FALSE)
    obj <- BPCells::open_matrix_dir(
        dir = path,
        buffer_size = as.integer(buffer_size)
    )
    DelayedArray(BPCellsDirSeed(obj))
}

#' Write a sparce matrices into a directory on disk
#'
#' @inherit BPCells::write_matrix_dir details
#' @param x Input matrix, any matrix can be coerced into
#' [dgCMatrix][Matrix::dgCMatrix-class] object.
#' @param ... Additional arguments passed into specific methods.
#' @param bitpacking A bool, whether or not to compress the data using
#' Bitpacking Compression.
#' @param overwrite A bool, If `TRUE`, write to a temp dir then overwrite
#' existing data.
#' @inheritParams BPCells::write_matrix_dir
#' @return A [BPCellsMatrix][BPCellsMatrix-class] object.
#' @export
#' @aliases writeBPCellsDirArray
#' @rdname BPCellsDir-IO
methods::setGeneric(
    "writeBPCellsDirArray",
    function(x, ...) standardGeneric("writeBPCellsDirArray")
)

.writeBPCellsDirArray <- function(
    x, path = NULL, bitpacking = TRUE,
    buffer_size = 8192L,
    overwrite = FALSE) {
    assert_bool(bitpacking)
    assert_bool(overwrite)
    path <- path %||% tempfile("BPCellsDirArray")
    obj <- BPCells::write_matrix_dir(
        mat = x, dir = path, compress = bitpacking,
        buffer_size = as.integer(buffer_size),
        overwrite = overwrite
    )
    DelayedArray(BPCellsDirSeed(obj))
}

#' @export
#' @rdname BPCellsDir-IO
methods::setMethod(
    "writeBPCellsDirArray", "IterableMatrix", .writeBPCellsDirArray
)

#' @export
#' @rdname BPCellsDir-IO
methods::setMethod("writeBPCellsDirArray", "BPCellsSeed", .writeBPCellsDirArray)

#' @export
#' @rdname BPCellsDir-IO
methods::setMethod("writeBPCellsDirArray", "BPCellsMatrix", function(x, ...) {
    .writeBPCellsDirArray(x = x@seed, ...)
})

#' @export
#' @rdname BPCellsDir-IO
methods::setMethod("writeBPCellsDirArray", "dgCMatrix", .writeBPCellsDirArray)

#' @export
#' @rdname BPCellsDir-IO
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

#' @export
methods::setAs("ANY", "BPCellsArray", .as_BPCellsDirArray)

#' @export
methods::setAs("ANY", "BPCellsMatrix", .as_BPCellsDirArray)
