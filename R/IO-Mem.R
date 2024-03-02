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

.writeBPCellsMemMatrix <- function(x, compress = TRUE, seedform = NULL) {
    lst <- extract_IterableMatrix_and_seedform(x, seedform)
    obj <- BPCells::write_matrix_memory(mat = lst$seed, compress = compress)
    with_seedform(seedform = lst$seedform, DelayedArray(obj))
}

#' @inheritParams BPCellsDir-IO
#' @inherit BPCells::write_matrix_memory details
#' @inheritParams BPCells::write_matrix_memory
#' @param seedform A string, `"BPCells"` or `"DelayedArray"`, if `NULL`, will
#'  use the default value.
#'  - For `BPCellsMatrix` object: the default value will be extracted from
#'    `x` directly (use `seedform(x)` to check).
#'  - For other object: the default value will be extracted from global
#'    option (use `seedform()` to check).
#' @inherit BPCellsDir-IO return
#' @inherit BPCellsSeed seealso
#' @export
#' @rdname BPCellsMem-IO
methods::setMethod("writeBPCellsMemMatrix", "ANY", .writeBPCellsMemMatrix)
