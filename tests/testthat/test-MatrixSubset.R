mat <- mock_matrix(200, 40)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
obj <- obj[1:100L, 1:20]
mat <- mat[1:100L, 1:20]

testthat::test_that("`to_DelayedArray()` and `to_BPCells()` works well for `MatrixSubset` object", {
    # to_DelayedArray
    delayedop_obj <- to_DelayedArray(obj)
    testthat::expect_s4_class(delayedop_obj, "BPCellsDelayedSubset")
    testthat::expect_identical(obj@matrix, delayedop_obj@seed)
    # to_BPCells
    bpcells_obj <- to_BPCells(delayedop_obj)
    testthat::expect_s4_class(bpcells_obj, "MatrixSubset")
    testthat::expect_identical(bpcells_obj, obj)
})

test_methods(obj, mat = mat, name = "MatrixSubset")
