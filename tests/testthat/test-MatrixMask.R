mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
mask <- matrix(
    sample(c(0, 1),
        size = nrow(obj) * ncol(obj), replace = TRUE,
        prob = c(0.7, 0.3)
    ),
    nrow = nrow(obj)
)
mat[mask > 0L] <- 0L
mask <- methods::as(methods::as(mask, "dgCMatrix"), "IterableMatrix")

# UnaryMask --------------------------------------------------
obj <- BPCells:::mask_matrix(obj, mask)
testthat::test_that("`to_DelayedArray()` and `to_BPCells()` works well for `UnaryMatrixMask` object", {
    # to_DelayedArray
    delayedop_obj <- to_DelayedArray(obj)
    testthat::expect_s4_class(delayedop_obj, "BPCellsDelayedMaskUnaryIsoOp")
    testthat::expect_identical(obj@matrix, delayedop_obj@seed)
    # to_BPCells
    bpcells_obj <- to_BPCells(delayedop_obj)
    testthat::expect_s4_class(bpcells_obj, "MatrixMask")
    testthat::expect_identical(bpcells_obj, obj)
})
test_methods(obj, mat = mat, name = "UnaryMatrixMask")

# NaryMask --------------------------------------------------
obj <- BPCells:::mask_matrix(
    obj,
    BPCells::write_matrix_dir(mask, dir = tempfile(tmpdir = tmpdir))
)
testthat::test_that("`to_DelayedArray()` and `to_BPCells()` works well for `NaryMatrixMask` object", {
    # to_DelayedArray
    delayedop_obj <- to_DelayedArray(obj)
    testthat::expect_s4_class(delayedop_obj, "BPCellsDelayedMaskNaryIsoOp")
    testthat::expect_identical(names(delayedop_obj@seeds), c("matrix", "mask"))
    # to_BPCells
    bpcells_obj <- to_BPCells(delayedop_obj)
    testthat::expect_s4_class(bpcells_obj, "MatrixMask")
    testthat::expect_identical(bpcells_obj, obj)
})
test_methods(obj, mat = mat, name = "NaryMatrixMask")
