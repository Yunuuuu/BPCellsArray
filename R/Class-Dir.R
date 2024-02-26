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
#' `writeBPCellsDirArray`, this can be `NULL`, and the internal will use a
#' temporary directory.
#' @inheritParams BPCells::open_matrix_dir
#' @export
#' @name BPCellsDir-IO
readBPCellsDirMatrix <- function(path, buffer_size = 8192L, seed_form = NULL) {
    assert_string(path, empty_ok = FALSE)
    seed_form <- match_seed_form(seed_form)
    obj <- BPCells::open_matrix_dir(
        dir = path,
        buffer_size = as.integer(buffer_size)
    )
    with_seed_form(seed_form, DelayedArray(obj))
}

#' Write a sparce matrices into a directory on disk
#'
#' @inherit BPCells::write_matrix_dir details
#' @param x A [BPCellsMatrix][BPCellsMatrix-class] object or any objects can be
#' converted into [BPCellsSeed] object.
#' @param ...
#'  - Generic function: Additional arguments passed into specific methods.
#'  - BPCellsMatrix Method: Additional arguments passed into `ANY` method.
#' @param bitpacking A bool, whether or not to compress the data using
#' Bitpacking Compression.
#' @param overwrite A bool, If `TRUE`, write to a temp dir then overwrite
#' existing data.
#' @param seed_form A string, `"BPCells"` or `"DelayedArray"`, if `NULL`, will
#' use the default value. 
#'  - For `readBPCells*`: use `set_seed_form()` with missing argument to check
#'    the default value.
#'  - For `writeBPCells*`: 
#'      * For `BPCellsMatrix` method: the default value will be extracted from
#'        `x` directly. 
#'      * For `ANY` method: use `set_seed_form()` with missing argument to check
#'        the default value.
#' @inheritParams BPCells::write_matrix_dir
#' @inheritParams BPCellsMatrix-class
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
    overwrite = FALSE,
    seed_form = NULL) {
    assert_bool(bitpacking)
    assert_bool(overwrite)
    seed_form <- match_seed_form(seed_form)
    path <- path %||% tempfile("BPCellsDirArray")
    obj <- BPCells::write_matrix_dir(
        mat = BPCellsSeed(x),
        dir = path, compress = bitpacking,
        buffer_size = as.integer(buffer_size),
        overwrite = overwrite
    )
    with_seed_form(seed_form, DelayedArray(obj))
}

#' @export
#' @rdname BPCellsDir-IO
methods::setMethod(
    "writeBPCellsDirArray", "BPCellsMatrix",
    function(x, ..., seed_form = NULL) {
        seed_form <- seed_form %||% x@SeedForm
        .writeBPCellsDirArray(x = x, ..., seed_form = seed_form)
    }
)

#' @export
#' @rdname BPCellsDir-IO
methods::setMethod("writeBPCellsDirArray", "ANY", .writeBPCellsDirArray)
