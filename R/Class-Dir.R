summary.MatrixDir <- function(object) {
    sprintf(
        "Load %s matrix from directory",
        if (object@compressed) "compressed" else "uncompressed"
    )
}
methods::setMethod("summary", "MatrixDir", summary.MatrixDir)

#' @importFrom DelayedArray path
#' @export
#' @rdname internal-methods
methods::setMethod("path", "MatrixDir", function(object, ...) object@dir)

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
    DelayedArray(obj)
}

#' Write a sparce matrices into a directory on disk
#'
#' @inherit BPCells::write_matrix_dir details
#' @inheritParams BPCellsSeed
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
        mat = BPCellsSeed(x),
        dir = path, compress = bitpacking,
        buffer_size = as.integer(buffer_size),
        overwrite = overwrite
    )
    DelayedArray(obj)
}

#' @export
#' @rdname BPCellsDir-IO
methods::setMethod("writeBPCellsDirArray", "ANY", .writeBPCellsDirArray)

#' @export
#' @rdname BPCellsDir-IO
methods::setMethod(
    "writeBPCellsDirArray", "BPCellsMatrix",
    set_BPCellsArray_method(x = , ... = )
)
