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
#' @name BPCellsMem-IO
NULL

#' @export
#' @rdname BPCellsMem-IO
methods::setGeneric(
    "writeBPCellsMemArray",
    function(x, ...) standardGeneric("writeBPCellsMemArray")
)

#' @inherit BPCells::write_matrix_memory details
#' @inheritParams writeBPCellsDirArray
#' @inheritParams BPCells::write_matrix_memory
#' @inherit BPCellsDir-IO return
#' @export
#' @rdname BPCellsMem-IO
methods::setMethod(
    "writeBPCellsMemArray", "BPCellsMatrix",
    set_BPCellsArray_method(x = , ... = )
)

.writeBPCellsMemArray <- function(x, compress = TRUE, delayed = NULL) {
    assert_bool(delayed, null_ok = TRUE)
    delayed <- delayed %||% GlobalOptions$DelayedBPCells
    obj <- BPCells::write_matrix_memory(
        mat = BPCellsSeed(x),
        compress = compress
    )
    with_delayed(delayed = delayed, DelayedArray(obj))
}

#' @export
#' @rdname BPCellsMem-IO
methods::setMethod("writeBPCellsMemArray", "ANY", .writeBPCellsMemArray)
