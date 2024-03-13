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
#' - `writeBPCellsHDF5Matrix`: Write a sparce matrices into a HDF5 file on disk
#' @param path A string path of the `HDF5` file to read or save data into.
#' @export
#' @name BPCellsHDF5-IO
readBPCellsHDF5Matrix <- function(path, group, buffer_size = 8192L, seedform = NULL) {
    assert_string(path, empty_ok = FALSE)
    seedform <- match_seedform(seedform)
    obj <- BPCells::open_matrix_hdf5(
        path = path, group = group,
        buffer_size = as.integer(buffer_size)
    )
    with_seedform(seedform, DelayedArray(obj))
}

#' @export
#' @aliases writeBPCellsHDF5Matrix
#' @rdname BPCellsHDF5-IO
methods::setGeneric(
    "writeBPCellsHDF5Matrix",
    function(x, ...) standardGeneric("writeBPCellsHDF5Matrix")
)

.writeBPCellsHDF5Matrix <- function(x, path, group, bitpacking = TRUE, buffer_size = 8192L, chunk_size = 1024L, overwrite = FALSE, gzip = 0L, seedform = NULL) {
    assert_bool(bitpacking)
    assert_bool(overwrite)
    lst <- extract_IterableMatrix_and_seedform(x, seedform)
    if (bitpacking) {
        gzip <- 0L
    } else {
        gzip <- as.integer(gzip)
    }
    obj <- BPCells::write_matrix_hdf5(
        mat = lst$seed,
        path = path, group = group,
        compress = bitpacking,
        buffer_size = as.integer(buffer_size),
        chunk_size = as.integer(chunk_size),
        overwrite = overwrite, gzip_level = gzip
    )
    with_seedform(lst$seedform, DelayedArray(obj))
}


#' @inheritParams BPCellsDir-IO
#' @inheritParams BPCells::open_matrix_hdf5
#' @param gzip `r rd_gzip()`. This is recommended when both compression and
#' compatibility with outside programs is required. Using `bitpacking=TRUE` is
#' recommended as it is >10x faster with often similar compression levels. So
#' `gzip` will always be zero when `bitpacking` is `TRUE`.
#' @inherit BPCells::write_matrix_hdf5 details
#' @inherit BPCellsDir-IO return
#' @inherit BPCellsSeed seealso
#' @export
#' @rdname BPCellsHDF5-IO
methods::setMethod("writeBPCellsHDF5Matrix", "ANY", .writeBPCellsHDF5Matrix)
