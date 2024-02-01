# For every new class
# - add method of subset: Method-subset.R
# - add method of showtree: showtree.R
methods::setClass("BPCellsHDF5Seed",
    contains = c("BPCellsBasicSeed", get_class("MatrixH5"))
)

BPCellsHDF5Seed <- function(x) methods::as(x, "BPCellsHDF5Seed")

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixH5", function(x) {
    BPCellsHDF5Seed(x = x)
})

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsSeed
#' @include Class-BPCellsMatrix.R
methods::setClass("BPCellsHDF5Array",
    contains = "BPCellsArray",
    slots = c(seed = "BPCellsHDF5Seed")
)

#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "DelayedArray", "BPCellsHDF5Seed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsHDF5Array")
)

#' @export
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsHDF5Matrix",
    contains = c("BPCellsMatrix"),
    slots = c(seed = "BPCellsHDF5Seed")
)

#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("matrixClass", "BPCellsHDF5Array", function(x) {
    "BPCellsHDF5Matrix"
})

methods::setMethod("summary", "BPCellsHDF5Seed", function(object) {
    sprintf(
        "Load %s matrix in HDF5 file (group: %s)",
        if (object@compressed) "compressed" else "uncompressed",
        object@group
    )
})

#' Read/write sparse matrices from (or into) HDF5 file
#'
#' @description
#' - `readBPCellsHDF5Matrix`: read a sparce matrices from a HDF5 file on disk
#' - `writeBPCellsHDF5Array`: Write a sparce matrices into a HDF5 file on disk
#' @param path A string path of the `HDF5` file to read or save data into.
#' @export
#' @name BPCellsHDF5-IO
readBPCellsHDF5Matrix <- function(path, group, buffer_size = 8192L) {
    assert_string(path, empty_ok = FALSE)
    obj <- BPCells::open_matrix_hdf5(
        path = path, group = group,
        buffer_size = as.integer(buffer_size)
    )
    DelayedArray(BPCellsHDF5Seed(obj))
}

#' @inherit BPCells::write_matrix_hdf5 details
#' @inheritParams writeBPCellsDirArray
#' @inheritParams BPCells::open_matrix_hdf5
#' @param gzip Gzip compression level. Default is 0 (no gzip compression). This
#' is recommended when both compression and compatibility with outside programs
#' is required. Using `compress=TRUE` is recommended as it is >10x faster with
#' often similar compression levels. So `gzip` will always be zero when
#' `compress` is `TRUE`.
#' @return A [BPCellsMatrix][BPCellsMatrix-class] object.
#' @export
#' @aliases writeBPCellsHDF5Array
#' @rdname BPCellsHDF5-IO
methods::setGeneric(
    "writeBPCellsHDF5Array",
    function(x, ...) standardGeneric("writeBPCellsHDF5Array")
)

.writeBPCellsHDF5Array <- function(x, path, group, bitpacking = TRUE, buffer_size = 8192L, chunk_size = 1024L, overwrite = FALSE, gzip = 0L) {
    assert_bool(bitpacking)
    assert_bool(overwrite)
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
    DelayedArray(BPCellsHDF5Seed(obj))
}

#' @export
#' @rdname BPCellsHDF5-IO
methods::setMethod("writeBPCellsHDF5Array", "ANY", .writeBPCellsHDF5Array)

#' @export
#' @rdname BPCellsHDF5-IO
methods::setMethod("writeBPCellsHDF5Array", "BPCellsMatrix", function(x, ...) {
    .writeBPCellsHDF5Array(x = x@seed, ...)
})
