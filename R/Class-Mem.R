summary.PackedMatrixMemBase <- function(object) {
    "Load compressed matrix from memory"
}
methods::setMethod(
    "summary", "PackedMatrixMemBase",
    summary.PackedMatrixMemBase
)
summary.UnpackedMatrixMemBase <- function(object) {
    "Load uncompressed matrix from memory"
}
methods::setMethod(
    "summary", "UnpackedMatrixMemBase",
    summary.UnpackedMatrixMemBase
)

###################################################################
###########################  Methods  #############################
###################################################################

#' Write a sparce matrices into memory with BPCells format
#'
#' @inheritParams writeBPCellsDirArray
#' @inheritParams BPCells::write_matrix_memory
#' @param ... For `BPCellsMatrix` method: additional parameters passed to `ANY`
#'   methods.
#' @return A [BPCellsMatrix][BPCellsMatrix-class] object.
#' @export
#' @name writeBPCellsMemArray
methods::setGeneric(
    "writeBPCellsMemArray",
    function(x, ...) standardGeneric("writeBPCellsMemArray")
)

.writeBPCellsMemArray <- function(x, compress = TRUE) {
    obj <- BPCells::write_matrix_memory(
        mat = BPCellsSeed(x),
        compress = compress
    )
    DelayedArray(obj)
}

#' @export
#' @rdname writeBPCellsMemArray
methods::setMethod("writeBPCellsMemArray", "ANY", .writeBPCellsMemArray)

#' @export
#' @rdname writeBPCellsMemArray
methods::setMethod(
    "writeBPCellsMemArray", "BPCellsMatrix",
    set_BPCellsArray_method(x = , ... = )
)
