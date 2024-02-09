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
#' @noRd
NULL

methods::setClass("BPCellsMemSeed", contains = c("BPCellsBasicSeed", "VIRTUAL"))
methods::setClass("BPCellsPackedMemSeed",
    contains = c("BPCellsMemSeed", BPCells_class("PackedMatrixMemBase"))
)
methods::setClass("BPCellsUnpackedMemSeed",
    contains = c("BPCellsMemSeed", BPCells_class("UnpackedMatrixMemBase"))
)

################################################################
methods::setMethod("summary", "BPCellsPackedMemSeed", function(object) {
    "Load compressed matrix from memory"
})

methods::setMethod("summary", "BPCellsUnpackedMemSeed", function(object) {
    "Load uncompressed matrix from memory"
})

#' @param x A `PackedMatrixMemBase` or `UnpackedMatrixMemBase` object.
#' @rdname BPCellsMem
#' @noRd
methods::setGeneric("BPCellsMemSeed", function(x) {
    class <- standardGeneric("BPCellsMemSeed")
    methods::as(x, Class = class)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "PackedMatrixMemBase", function(x) {
    BPCellsMemSeed(x = x)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "UnpackedMatrixMemBase", function(x) {
    BPCellsMemSeed(x = x)
})

################### Packed class
methods::setClass("BPCellsPackedMem_uint32_tSeed",
    contains = c(
        "BPCellsPackedMemSeed",
        BPCells_class("PackedMatrixMem_uint32_t")
    )
)
methods::setMethod(
    "BPCellsMemSeed", "PackedMatrixMem_uint32_t",
    function(x) "BPCellsPackedMem_uint32_tSeed"
)
methods::setClass("BPCellsPackedMem_floatSeed",
    contains = c(
        "BPCellsPackedMemSeed",
        BPCells_class("PackedMatrixMem_float")
    )
)
methods::setMethod(
    "BPCellsMemSeed", "PackedMatrixMem_float",
    function(x) "BPCellsPackedMem_floatSeed"
)
methods::setClass("BPCellsPackedMem_doubleSeed",
    contains = c(
        "BPCellsPackedMemSeed",
        BPCells_class("PackedMatrixMem_double")
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
        BPCells_class("UnpackedMatrixMem_uint32_t")
    )
)
methods::setMethod(
    "BPCellsMemSeed", "UnpackedMatrixMem_uint32_t",
    function(x) "BPCellsunPackedMem_uint32_tSeed"
)

methods::setClass("BPCellsunPackedMem_floatSeed",
    contains = c(
        "BPCellsUnpackedMemSeed",
        BPCells_class("UnpackedMatrixMem_float")
    )
)
methods::setMethod(
    "BPCellsMemSeed", "UnpackedMatrixMem_float",
    function(x) "BPCellsunPackedMem_floatSeed"
)

methods::setClass("BPCellsunPackedMem_doubleSeed",
    contains = c(
        "BPCellsUnpackedMemSeed",
        BPCells_class("UnpackedMatrixMem_double")
    )
)
methods::setMethod(
    "BPCellsMemSeed", "UnpackedMatrixMem_double",
    function(x) "BPCellsunPackedMem_doubleSeed"
)

###################################################

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsMemArray",
    contains = "BPCellsArray", slots = c(seed = "BPCellsMemSeed")
)

#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "DelayedArray", "BPCellsMemSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsMemArray")
)

#' @export
#' @rdname BPCellsMatrix-class
#' @include Class-BPCellsMatrix.R
methods::setClass("BPCellsMemMatrix",
    contains = c("BPCellsMatrix"),
    slots = c(seed = "BPCellsMemSeed")
)

#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("matrixClass", "BPCellsMemArray", function(x) {
    "BPCellsMemMatrix"
})

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
    DelayedArray(BPCellsSeed(obj))
}

#' @export
#' @rdname writeBPCellsMemArray
methods::setMethod("writeBPCellsMemArray", "ANY", .writeBPCellsMemArray)

.as_BPCellsMemArray <- function(from) writeBPCellsMemArray(from)

#' @export
methods::setAs("ANY", "BPCellsMemArray", .as_BPCellsMemArray)

#' @export
methods::setAs("ANY", "BPCellsMemMatrix", .as_BPCellsMemArray)
