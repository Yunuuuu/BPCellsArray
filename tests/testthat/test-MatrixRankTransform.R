mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
obj <- BPCells:::rank_transform(obj, "col")
testthat::test_that("`to_DelayedArray()` and `to_BPCells()` works well for `MatrixRankTransform` object", {
    # to_DelayedArray
    delayedop_obj <- to_DelayedArray(obj)
    testthat::expect_s4_class(delayedop_obj, "BPCellsDelayedRankTransform")
    testthat::expect_identical(obj@matrix, delayedop_obj@seed)
    # to_BPCells
    bpcells_obj <- to_BPCells(delayedop_obj)
    testthat::expect_s4_class(bpcells_obj, "MatrixRankTransform")
    testthat::expect_identical(bpcells_obj, obj)
})

test_methods(obj, name = "MatrixRankTransform")
