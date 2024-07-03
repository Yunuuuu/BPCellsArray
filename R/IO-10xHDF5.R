summary.10xMatrixH5 <- function(object) {
    "Load matrix from 10x HDF5 file"
}
methods::setMethod("summary", "10xMatrixH5", summary.10xMatrixH5)

#' Read/write sparse matrices from (or into) 10x feature matrix
#'
#' @description
#' - `readBPCells10xHDF5Matrix`: read a sparce matrices from a HDF5 file on disk
#' - `writeBPCells10xHDF5Matrix`: Write a sparce matrices into a HDF5 file on
#'   disk
#' @inheritParams BPCells::open_matrix_10x_hdf5
#' @param path A string path of the `10x HDF5` file to read or save data into.
#' @export
#' @name BPCells10xHDF5-IO
readBPCells10xHDF5Matrix <- function(path, feature_type = NULL,
                                     buffer_size = 16384L) {
    assert_string(path, empty_ok = FALSE)
    ans <- BPCells::open_matrix_10x_hdf5(
        path = path, feature_type = feature_type,
        buffer_size = as.integer(buffer_size)
    )
    DelayedArray(ans)
}

#' @export
#' @aliases writeBPCells10xHDF5Matrix
#' @rdname BPCells10xHDF5-IO
methods::setGeneric(
    "writeBPCells10xHDF5Matrix",
    function(x, ...) standardGeneric("writeBPCells10xHDF5Matrix")
)

.writeBPCells10xHDF5Matrix <- function(
    x, path,
    barcodes = colnames(x),
    feature_ids = rownames(x),
    feature_names = rownames(x),
    feature_types = "Gene Expression",
    feature_metadata = list(),
    buffer_size = 16384L,
    chunk_size = 1024L,
    gzip = 0L) {
    assert_string(path, empty_ok = FALSE)
    if (file.exists(path)) cli::cli_abort("{.arg path} requested is exist")
    seed <- extract_IterableMatrix(x)
    mode <- storage_mode(seed)
    if (mode != "uint32_t") {
        cli::cli_warn(paste(
            "10x HDF5 file should be a {.field uint32_t} matrix,",
            "but you provided a {.field {mode}}"
        ))
    }
    ans <- BPCells::write_matrix_10x_hdf5(
        mat = seed,
        path = path,
        barcodes = barcodes,
        feature_ids = feature_ids,
        feature_names = feature_names,
        feature_types = feature_types,
        feature_metadata = feature_metadata,
        buffer_size = as.integer(buffer_size),
        chunk_size = as.integer(chunk_size),
        gzip_level = as.integer(gzip),
        type = mode
    )
    DelayedArray(ans)
}

#' @inherit BPCells::write_matrix_10x_hdf5 details
#' @inherit BPCellsDir-IO return
#' @inheritParams BPCellsDir-IO
#' @param gzip `r rd_gzip()`.
#' @inherit BPCellsSeed seealso
#' @export
#' @rdname BPCells10xHDF5-IO
methods::setMethod(
    "writeBPCells10xHDF5Matrix", "ANY",
    .writeBPCells10xHDF5Matrix
)
