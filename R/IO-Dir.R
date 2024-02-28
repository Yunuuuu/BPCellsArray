summary.MatrixDir <- function(object) {
    sprintf(
        "Load %s matrix from directory",
        if (object@compressed) "compressed" else "uncompressed"
    )
}
methods::setMethod("summary", "MatrixDir", summary.MatrixDir)

#' Read/write sparse matrices from (or into) directory on disk
#'
#' @description
#' - `readBPCellsDirMatrix`: read a sparce matrices from a directory on disk
#' - `writeBPCellsDirArray`: Write a sparce matrices into a directory on disk
#' @param path A string path of directory to read or save the data into. For
#' `writeBPCellsDirArray`, this can be `NULL` means using a temporary directory.
#' @inheritParams BPCells::open_matrix_dir
#' @export
#' @name BPCellsDir-IO
readBPCellsDirMatrix <- function(path, buffer_size = 8192L, seedform = NULL) {
    assert_string(path, empty_ok = FALSE)
    seedform <- match_seedform(seedform)
    obj <- BPCells::open_matrix_dir(
        dir = path,
        buffer_size = as.integer(buffer_size)
    )
    with_seedform(seedform, DelayedArray(obj))
}

#' Write a sparce matrices into a directory on disk
#'
#' @inherit BPCells::write_matrix_dir details
#' @param x A [BPCellsMatrix][BPCellsMatrix-class] object or any objects can be
#'    converted into [BPCellsSeed] object.
#' @param ... Additional arguments passed into specific methods.
#' @param bitpacking A bool, whether or not to compress the data using
#' Bitpacking Compression.
#' @param overwrite A bool, If `TRUE`, write to a temp dir then overwrite
#' existing data.
#' @param seedform A string, `"BPCells"` or `"DelayedArray"`, if `NULL`, will
#'  use the default value.
#'  - For `readBPCells*`: (use `seedform()` to check) to check the default
#'    value.
#'  - For `writeBPCells*`:
#'     - For `BPCellsMatrix` object: the default value will be extracted from
#'       `x` directly (use `seedform(x)` to check).
#'     - For other object: the default value will be extracted from global
#'       option (use `seedform()` to check).
#' @inheritParams BPCells::write_matrix_dir
#' @inheritParams BPCellsMatrix-class
#' @return A [BPCellsMatrix][BPCellsMatrix-class] object.
#' @inherit BPCellsSeed seealso
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
    overwrite = FALSE,
    seedform = NULL) {
    assert_bool(bitpacking)
    assert_bool(overwrite)
    lst <- extract_IterableMatrix_and_seedform(x, seedform)
    path <- path %||% tempfile("BPCellsDirArray")
    obj <- BPCells::write_matrix_dir(
        mat = lst$seed,
        dir = path, compress = bitpacking,
        buffer_size = as.integer(buffer_size),
        overwrite = overwrite
    )
    with_seedform(lst$seedform, DelayedArray(obj))
}

#' @export
#' @rdname BPCellsDir-IO
methods::setMethod("writeBPCellsDirArray", "ANY", .writeBPCellsDirArray)
