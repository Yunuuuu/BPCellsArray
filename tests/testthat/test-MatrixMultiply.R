mat1 <- mock_matrix(30, 20)
mat2 <- mock_matrix(ncol(mat1), 100)
path <- normalizePath(
    c(tempfile(tmpdir = tmpdir), tempfile(tmpdir = tmpdir)),
    mustWork = FALSE
)
obj <- BPCells::write_matrix_dir(mat = as(mat1, "dgCMatrix"), dir = path[1L])
obj2 <- BPCells::write_matrix_dir(mat = as(mat2, "dgCMatrix"), dir = path[2L])
obj <- obj %*% obj2
mat <- mat1 %*% mat2
testthat::test_that("`to_DelayedArray()` and `to_BPCells()` works well for `MatrixMultiply` object", {
    # to_DelayedArray
    delayedop_obj <- to_DelayedArray(obj)
    testthat::expect_s4_class(delayedop_obj, "BPCellsDelayedMultiply")
    testthat::expect_identical(names(delayedop_obj@seeds), c("left", "right"))
    # to_BPCells
    bpcells_obj <- to_BPCells(delayedop_obj)
    testthat::expect_s4_class(bpcells_obj, "MatrixMultiply")
    testthat::expect_identical(bpcells_obj, obj)
})
test_methods(obj, mat = mat, name = "MatrixMultiply")
