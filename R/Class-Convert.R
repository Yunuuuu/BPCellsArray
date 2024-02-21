methods::setClass("BPCellsConvertSeed",
    contains = c(
        "BPCellsUnaryOpsSeed",
        BPCells_class("ConvertMatrixType")
    ),
    slots = list(matrix = "BPCellsSeed")
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "ConvertMatrixType", function(x) {
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsConvertSeed")
})

methods::setMethod("summary", "BPCellsConvertSeed", function(object) {
    sprintf(
        "Convert type from %s to %s",
        storage_mode(entity(object)),
        storage_mode(object)
    )
})

###################################################################
###########################  Methods  #############################
###################################################################

#####################   BPCellsConvertMatrix   #######################
#' Convert the storage type of a BPCellsArray object
#'
#' @param object A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object.
#' @param ... Additional parameters passed into specific methods.
#' @name convert_mode
NULL

#' @return
#'  - `convert_mode`: A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object with storage mode converted into
#' the specified.
#' @export
#' @rdname convert_mode
methods::setGeneric(
    "convert_mode",
    function(object, ...) standardGeneric("convert_mode")
)

#' @param mode Storage mode of BPCells matrix, one of `uint32_t` (unsigned
#' 32-bit integer), `float` (32-bit real number), or `double` (64-bit real
#' number). R cannot differentiate 32-bit and 64-bit real number, so
#' [type][BPCellsSeed-class] method always return "double" for both `float`
#' and `double` mode.
#' @seealso [convert_matrix_type][BPCells::convert_matrix_type]
#' @export
#' @rdname convert_mode
methods::setMethod("convert_mode", "BPCellsSeed", function(object, mode) {
    mode <- match.arg(mode, BPCells_MODE)
    obj <- BPCells::convert_matrix_type(object, type = mode)
    BPCellsSeed(obj)
})

#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname convert_mode
methods::setMethod("convert_mode", "BPCellsMatrix", function(object, mode) {
    object <- object@seed
    DelayedArray(methods::callGeneric())
})

#' @inheritParams convert_mode
#' @export
#' @rdname internal-methods
methods::setMethod("convert_mode", "ANY", function(object, mode) {
    cli::cli_abort(
        "{.arg object} must be a {.cls BPCellsSeed} or {.cls BPCellsMatrix} object"
    )
})

#' @return
#'  - `storage_mode`: A string indicates the storage mode.
#' @export
#' @rdname convert_mode
methods::setGeneric(
    "storage_mode", function(object) standardGeneric("storage_mode")
)

storage_mode_internal <- function(object) {
    BPCells_get("matrix_type")(object)
}

#' @export
#' @rdname convert_mode
methods::setMethod("storage_mode", "BPCellsSeed", storage_mode_internal)

#' @export
#' @rdname convert_mode
methods::setMethod("storage_mode", "IterableMatrix", storage_mode_internal)

#' @export
#' @rdname convert_mode
methods::setMethod("storage_mode", "BPCellsMatrix", function(object) {
    object <- object@seed
    methods::callGeneric()
})
