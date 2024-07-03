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
#' - `writeBPCellsDirMatrix`: Write a sparce matrices into a directory on disk
#' @param path A string path of directory to read or save the data into. For
#' `writeBPCellsDirMatrix`, this can be `NULL` means using a temporary
#' directory.
#' @inheritParams BPCells::open_matrix_dir
#' @export
#' @name BPCellsDir-IO
readBPCellsDirMatrix <- function(path, buffer_size = 8192L) {
    assert_string(path, empty_ok = FALSE)
    ans <- BPCells::open_matrix_dir(
        dir = path,
        buffer_size = as.integer(buffer_size)
    )
    DelayedArray(ans)
}

#' @inherit BPCells::write_matrix_dir details
#' @param x A `r rd_matrix()` or a `r rd_seed()`.
#' @param ... Additional arguments passed into specific methods.
#' @param bitpacking A bool, whether or not to compress the data using
#' Bitpacking Compression.
#' @param overwrite A bool, If `TRUE`, write to a temp dir then overwrite
#' existing data.
#' @inheritParams BPCells::write_matrix_dir
#' @inheritParams BPCellsMatrix-class
#' @return A `r rd_matrix()`.
#' @inherit BPCellsSeed seealso
#' @export
#' @aliases writeBPCellsDirMatrix
#' @rdname BPCellsDir-IO
methods::setGeneric(
    "writeBPCellsDirMatrix",
    function(x, ...) standardGeneric("writeBPCellsDirMatrix")
)

.writeBPCellsDirMatrix <- function(
    x, path = NULL, bitpacking = TRUE,
    buffer_size = 8192L,
    overwrite = FALSE) {
    assert_bool(bitpacking)
    assert_bool(overwrite)
    seed <- extract_IterableMatrix(x)
    path <- path %||% tempfile("BPCellsDirMatrix")
    ans <- BPCells::write_matrix_dir(
        mat = seed,
        dir = path, compress = bitpacking,
        buffer_size = as.integer(buffer_size),
        overwrite = overwrite
    )
    DelayedArray(ans)
}

#' @export
#' @rdname BPCellsDir-IO
methods::setMethod("writeBPCellsDirMatrix", "ANY", .writeBPCellsDirMatrix)
