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

#' Read a BPCells Dir matrix from the path
#' @param path A string for the path of the BPCells matrix dir.
#' @inheritDotParams BPCells::open_matrix_dir -dir
#' @export
readDirMatrix <- function(path, ...) {
    assert_string(path, empty_ok = FALSE)
    obj <- BPCells::open_matrix_dir(dir = path, ...)
    DelayedArray(BPCellsDirSeed(obj))
}

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
#' @return A [BPCellsMatrix][BPCellsMatrix-class] object.
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
    DelayedArray(BPCellsDirSeed(obj))
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

#' @export
methods::setAs("ANY", "BPCellsArray", .as_BPCellsDirArray)

#' @export
methods::setAs("ANY", "BPCellsMatrix", .as_BPCellsDirArray)
