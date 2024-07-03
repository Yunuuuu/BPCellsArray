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
    "writeBPCellsMemMatrix",
    function(x, ...) standardGeneric("writeBPCellsMemMatrix")
)

.writeBPCellsMemMatrix <- function(x, compress = TRUE) {
    seed <- extract_IterableMatrix(x)
    ans <- BPCells::write_matrix_memory(mat = seed, compress = compress)
    DelayedArray(ans)
}

#' @inheritParams BPCellsDir-IO
#' @inherit BPCells::write_matrix_memory details
#' @inheritParams BPCells::write_matrix_memory
#' @inherit BPCellsDir-IO return
#' @inherit BPCellsSeed seealso
#' @export
#' @rdname BPCellsMem-IO
methods::setMethod("writeBPCellsMemMatrix", "ANY", .writeBPCellsMemMatrix)
