#' @exportS3Method base::summary `10xMatrixH5`
summary.AnnDataMatrixH5 <- function(object) {
    "Load matrix from AnnData HDF5 file"
}
methods::setMethod("summary", "AnnDataMatrixH5", summary.AnnDataMatrixH5)

#' Read/write sparse matrices from (or into) AnnData HDF5 file
#'
#' @description
#' - `readBPCellsAnnHDF5Matrix`: read a sparce matrices from a AnnData HDF5
#'   file on disk.
#' - `writeBPCellsAnnHDF5Matrix`: Write a sparce matrices into a AnnData HDF5
#'   file on disk.
#' @param path A string path of the AnnData HDF5 file to read or save data
#' into.
#' @export
#' @name BPCellsAnnHDF5-IO
readBPCellsAnnHDF5Matrix <- function(path, group = "X", buffer_size = 16384L) {
    assert_string(path, empty_ok = FALSE)
    ans <- BPCells::open_matrix_anndata_hdf5(
        path = path, group = group,
        buffer_size = as.integer(buffer_size)
    )
    DelayedArray(ans)
}

#' @export
#' @aliases writeBPCellsAnnHDF5Matrix
#' @rdname BPCellsAnnHDF5-IO
methods::setGeneric(
    "writeBPCellsAnnHDF5Matrix",
    function(x, ...) standardGeneric("writeBPCellsAnnHDF5Matrix")
)

.writeBPCellsAnnHDF5Matrix <- function(x, path, group = "X",
                                       buffer_size = 16384L, chunk_size = 1024L,
                                       gzip = 0L) {
    seed <- extract_IterableMatrix(x)
    ans <- BPCells::write_matrix_anndata_hdf5(
        mat = seed,
        path = path, group = group,
        buffer_size = as.integer(buffer_size),
        chunk_size = as.integer(chunk_size),
        gzip_level = as.integer(gzip)
    )
    DelayedArray(ans)
}


#' @inheritParams BPCellsDir-IO
#' @inheritParams BPCells::open_matrix_hdf5
#' @param gzip `r rd_gzip()`.
#' @inherit BPCells::write_matrix_anndata_hdf5 details
#' @inherit BPCellsDir-IO return
#' @inherit BPCellsSeed seealso
#' @export
#' @rdname BPCellsAnnHDF5-IO
methods::setMethod(
    "writeBPCellsAnnHDF5Matrix", "ANY",
    .writeBPCellsAnnHDF5Matrix
)
