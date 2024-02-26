summary.MatrixH5 <- function(object) {
    sprintf(
        "Load %s matrix in HDF5 file (group: %s)",
        if (object@compressed) "compressed" else "uncompressed",
        object@group
    )
}
methods::setMethod("summary", "MatrixH5", summary.MatrixH5)

#' Read/write sparse matrices from (or into) HDF5 file
#'
#' @description
#' - `readBPCellsHDF5Matrix`: read a sparce matrices from a HDF5 file on disk
#' - `writeBPCellsHDF5Array`: Write a sparce matrices into a HDF5 file on disk
#' @param path A string path of the `HDF5` file to read or save data into.
#' @export
#' @name BPCellsHDF5-IO
readBPCellsHDF5Matrix <- function(path, group, buffer_size = 8192L, delayed = NULL) {
    assert_string(path, empty_ok = FALSE)
    assert_bool(delayed, null_ok = TRUE)
    delayed <- delayed %||% GlobalOptions$DelayedBPCells
    obj <- BPCells::open_matrix_hdf5(
        path = path, group = group,
        buffer_size = as.integer(buffer_size)
    )
    with_delayed(delayed, DelayedArray(obj))
}

#' @inherit BPCells::write_matrix_hdf5 details
#' @inheritParams BPCellsDir-IO
#' @inheritParams BPCells::open_matrix_hdf5
#' @param gzip Gzip compression level. Default is 0 (no gzip compression). This
#' is recommended when both compression and compatibility with outside programs
#' is required. Using `compress=TRUE` is recommended as it is >10x faster with
#' often similar compression levels. So `gzip` will always be zero when
#' `compress` is `TRUE`.
#' @inherit BPCellsDir-IO return
#' @export
#' @aliases writeBPCellsHDF5Array
#' @rdname BPCellsHDF5-IO
methods::setGeneric(
    "writeBPCellsHDF5Array",
    function(x, ...) standardGeneric("writeBPCellsHDF5Array")
)

.writeBPCellsHDF5Array <- function(x, path, group, bitpacking = TRUE, buffer_size = 8192L, chunk_size = 1024L, overwrite = FALSE, gzip = 0L, delayed = NULL) {
    assert_bool(bitpacking)
    assert_bool(overwrite)
    assert_bool(delayed, null_ok = TRUE)
    delayed <- delayed %||% GlobalOptions$DelayedBPCells
    if (bitpacking) {
        gzip <- 0L
    } else {
        gzip <- as.integer(gzip)
    }
    obj <- BPCells::write_matrix_hdf5(
        mat = BPCellsSeed(x),
        path = path, group = group,
        compress = bitpacking,
        buffer_size = as.integer(buffer_size),
        chunk_size = as.integer(chunk_size),
        overwrite = overwrite, gzip_level = gzip
    )
    with_delayed(delayed, DelayedArray(obj))
}

#' @export
#' @rdname BPCellsHDF5-IO
methods::setMethod("writeBPCellsHDF5Array", "BPCellsMatrix", function(x, ...) {
    delayed <- x@delayed
    .writeBPCellsHDF5Array(x = x, ..., delayed = delayed)
})

#' @export
#' @rdname BPCellsHDF5-IO
methods::setMethod("writeBPCellsHDF5Array", "ANY", .writeBPCellsHDF5Array)
