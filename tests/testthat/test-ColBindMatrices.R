mat1 <- mock_matrix(30, 20)
mat2 <- mock_matrix(30, 20)
path <- normalizePath(c(tempfile(tmpdir = tmpdir), tempfile(tmpdir = tmpdir)), mustWork = FALSE)
obj1 <- BPCells::write_matrix_dir(mat = as(mat1, "dgCMatrix"), dir = path[1L])
obj2 <- BPCells::write_matrix_dir(mat = as(mat2, "dgCMatrix"), dir = path[2L])
obj <- cbind(obj1, obj2)
mat <- cbind(mat1, mat2)

testthat::test_that("`to_DelayedArray()` and `to_BPCells()` works well for `ColBindMatrices` object", {
    delayedop_obj <- to_DelayedArray(obj)
    testthat::expect_s4_class(delayedop_obj, "BPCellsDelayedAbind")
    testthat::expect_identical(delayedop_obj@seeds, obj@matrix_list)
    testthat::expect_identical(delayedop_obj@along, 2L)
    bpcells_obj <- to_BPCells(delayedop_obj)
    testthat::expect_s4_class(bpcells_obj, "ColBindMatrices")
    testthat::expect_identical(bpcells_obj, obj)
})

test_methods(obj, mat = mat, name = "ColBindMatrices")
