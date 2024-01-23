#' Delayed PackedMatrixMemBase and UnpackedMatrixMemBase of BPCells
#'
#' The `BPCellsMemArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `PackedMatrixMemBase`
#' and `UnpackedMatrixMemBase` object.
#'
#' @param x For Specific functions:
#' - `BPCellsMemArray`: A `PackedMatrixMemBase` or `UnpackedMatrixMemBase`
#'   object.
#' - `matrixClass`: A `BPCellsMemArray` object.
#' @name BPCellsMem
#' @seealso
#' - [BPCellsSeed]
#' - [writeBPCellsMemArray]
NULL

methods::setClass("BPCellsMemSeed", contains = "BPCellsSeed")
methods::setClass("BPCellsPackedMemSeed",
    contains = c("BPCellsMemSeed", get_class("PackedMatrixMemBase"))
)
methods::setClass("BPCellsUnpackedMemSeed",
    contains = c("BPCellsMemSeed", get_class("UnpackedMatrixMemBase"))
)

#' @param x A `PackedMatrixMemBase` or `UnpackedMatrixMemBase` object.
#' @rdname BPCellsMem
#' @noRd
methods::setGeneric("BPCellsMemSeed", function(x, ...) {
    class <- standardGeneric("BPCellsMemSeed")
    methods::as(x, Class = class)
})

################### Packed class
methods::setClass("BPCellsPackedMem_uint32_tSeed",
    contains = c(
        "BPCellsPackedMemSeed",
        get_class("PackedMatrixMem_uint32_t")
    )
)
methods::setMethod(
    "BPCellsMemSeed", "PackedMatrixMem_uint32_t",
    function(x) "BPCellsPackedMem_uint32_tSeed"
)
methods::setClass("BPCellsPackedMem_floatSeed",
    contains = c(
        "BPCellsPackedMemSeed",
        get_class("PackedMatrixMem_float")
    )
)
methods::setMethod(
    "BPCellsMemSeed", "PackedMatrixMem_float",
    function(x) "BPCellsPackedMem_floatSeed"
)
methods::setClass("BPCellsPackedMem_doubleSeed",
    contains = c(
        "BPCellsPackedMemSeed",
        get_class("PackedMatrixMem_double")
    )
)
methods::setMethod(
    "BPCellsMemSeed", "PackedMatrixMem_double",
    function(x) "BPCellsPackedMem_doubleSeed"
)

################### unPacked class
methods::setClass("BPCellsunPackedMem_uint32_tSeed",
    contains = c(
        "BPCellsUnpackedMemSeed",
        get_class("UnpackedMatrixMem_uint32_t")
    )
)
methods::setMethod(
    "BPCellsMemSeed", "UnpackedMatrixMem_uint32_t",
    function(x) "BPCellsunPackedMem_uint32_tSeed"
)

methods::setClass("BPCellsunPackedMem_floatSeed",
    contains = c(
        "BPCellsUnpackedMemSeed",
        get_class("UnpackedMatrixMem_float")
    )
)
methods::setMethod(
    "BPCellsMemSeed", "UnpackedMatrixMem_float",
    function(x) "BPCellsunPackedMem_floatSeed"
)

methods::setClass("BPCellsunPackedMem_doubleSeed",
    contains = c(
        "BPCellsUnpackedMemSeed",
        get_class("UnpackedMatrixMem_double")
    )
)
methods::setMethod(
    "BPCellsMemSeed", "UnpackedMatrixMem_double",
    function(x) "BPCellsunPackedMem_doubleSeed"
)

methods::setMethod("BPCellsMemSeed", "ANY", function(x) {
    cli::cli_abort("{.arg x} must be a {.cls PackedMatrixMemBase} or {.cls UnpackedMatrixMemBase} object.")
})

###################################################

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsMem
methods::setClass("BPCellsMemArray",
    contains = "DelayedArray", slots = c(seed = "BPCellsMemSeed")
)

#' @param seed A `BPCellsMemSeed` object.
#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsMem
methods::setMethod(
    "DelayedArray", "BPCellsMemSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsMemArray")
)

#' @export
#' @rdname BPCellsMem
BPCellsMemArray <- function(x) {
    DelayedArray(BPCellsMemSeed(x))
}

#' @export
#' @rdname BPCellsMem
#' @include Class-BPCellsMatrix.R
methods::setClass("BPCellsMemMatrix",
    contains = c("BPCellsMatrix"),
    slots = c(seed = "BPCellsMemSeed")
)

#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsMem
methods::setMethod("matrixClass", "BPCellsMemArray", function(x) {
    "BPCellsMemMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################

#' Write a sparce matrices into memory with BPCells format
#'
#' @param x Input matrix, any matrix can be coerced to a
#' [dgCMatrix][Matrix::dgCMatrix-class] object.
#' @inheritParams BPCells::write_matrix_memory
#' @param ...
#' - For `BPCellsMatrix` method: additional parameters passed to `BPCellsSeed`
#'   methods.
#' - For `ANY` method: additional parameters passed to `dgCMatrix` methods.
#' @return A [BPCellsMemMatrix][BPCellsMem] object.
#' @export
#' @name writeBPCellsMemArray
methods::setGeneric(
    "writeBPCellsMemArray",
    function(x, ...) standardGeneric("writeBPCellsMemArray")
)

.writeBPCellsMemArray <- function(x, compress = TRUE) {
    obj <- BPCells::write_matrix_memory(mat = x, compress = compress)
    BPCellsMemArray(obj)
}

#' @export
#' @rdname writeBPCellsMemArray
methods::setMethod(
    "writeBPCellsMemArray", "IterableMatrix", .writeBPCellsMemArray
)

#' @export
#' @rdname writeBPCellsMemArray
methods::setMethod("writeBPCellsMemArray", "BPCellsSeed", .writeBPCellsMemArray)

#' @export
#' @rdname writeBPCellsMemArray
methods::setMethod("writeBPCellsMemArray", "BPCellsMatrix", function(x, ...) {
    .writeBPCellsMemArray(x = x@seed, ...)
})

#' @export
#' @rdname writeBPCellsMemArray
methods::setMethod("writeBPCellsMemArray", "dgCMatrix", .writeBPCellsMemArray)

#' @export
#' @rdname writeBPCellsMemArray
methods::setMethod("writeBPCellsMemArray", "ANY", function(x, ...) {
    .writeBPCellsMemArray(x = coerce_dgCMatrix(x), ...)
})

.as_BPCellsMemArray <- function(from) writeBPCellsMemArray(from)

#' @export
methods::setAs("ANY", "BPCellsMemArray", .as_BPCellsMemArray)

#' @export
methods::setAs("ANY", "BPCellsMemMatrix", .as_BPCellsMemArray)

#' @export
methods::setAs("BPCellsMatrix", "dgCMatrix", function(from) {
    methods::as(from@seed, "dgCMatrix")
})

#' @export
methods::setAs("BPCellsMatrix", "matrix", function(from) {
    as.matrix(from@seed)
})
