#' @importFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsColBindMatrixSeed", function(object) {
    unlist(lapply(object@matrix_list, path),
        recursive = FALSE, use.names = FALSE
    )
})

#' @importFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsRowBindMatrixSeed", function(object) {
    unlist(lapply(object@matrix_list, path),
        recursive = FALSE, use.names = FALSE
    )
})

#' @importFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsConvertSeed", function(object) {
    path(object@matrix)
})

#' @importFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsdgCMatrixSeed", function(object) {
    character()
})

#' @importFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsDirSeed", function(object) object@dir)

#' @importFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsMaskSeed", function(object) {
    c(path(object@matrix), path(object@mask))
})

#' @importFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsMultiplySeed", function(object) {
    c(path(object@left), path(object@right))
})

#' @importFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsRankTransformSeed", function(object) {
    path(object@matrix)
})

#' @importFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsRenameDimsSeed", function(object) {
    path(object@matrix)
})

#' @importFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsSubsetSeed", function(object) {
    path(object@matrix)
})

#' @importFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsTransformedSeed", function(object) {
    path(object@matrix)
})

#' @importFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsMemSeed", function(object) {
    character()
})
