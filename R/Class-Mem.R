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

.writeBPCellsMemArray <- function(x, compress = TRUE, seed_form = NULL) {
    seed_form <- match_seed_form(seed_form)
    obj <- BPCells::write_matrix_memory(
        mat = BPCellsSeed(x),
        compress = compress
    )
    with_seed_form(seed_form = seed_form, DelayedArray(obj))
}

#' @inherit BPCells::write_matrix_memory details
#' @inheritParams writeBPCellsDirArray
#' @inheritParams BPCells::write_matrix_memory
#' @inherit BPCellsDir-IO return
#' @export
#' @rdname BPCellsMem-IO
methods::setMethod("writeBPCellsMemArray", "BPCellsMatrix", function(x, ...) {
    seed_form <- x@SeedForm
    .writeBPCellsMemArray(x = x, ..., seed_form = seed_form)
})

#' @export
#' @rdname BPCellsMem-IO
methods::setMethod("writeBPCellsMemArray", "ANY", .writeBPCellsMemArray)
