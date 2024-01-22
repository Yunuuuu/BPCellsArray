#' House of BPCellsSeed methods
#'
#' Following methods are used by [BPCellsSeed-class] objects, you should always
#' use the methods of [BPCellsMatrix-class]
#'
#' @param x,y,object A [BPCellsSeed][BPCellsSeed-class] object.
#' @param ...
#'  - `rbind2` and `cbind2`: Not used currently.
#'  - `rbind`, `arbind`, `cbind`, and `acbind`: A list of
#'    [BPCellsSeed][BPCellsSeed-class] objects.
#'  - `[` and `[<-`: Not used currently.
#'  - `matrix_stats`: additional arguments to [BPCells::matrix_stats].
#' @name seed-methods
NULL

#' House of internal methods
#'
#' Following methods are used by package internal, for messages purpose, usually
#' the method for `ANY` object.
#'
#' @param x,y A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object.
#' @param value A matrix which can be coerced into
#'     [dgCMatrix][Matrix::dgCMatrix-class].
#' @param seed A [BPCellsSeed][BPCellsSeed-class] object.
#' @name internal-methods
NULL
