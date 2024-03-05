mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)

testthat::test_that("`writeBPCellsDirMatrix()` works as expected", {
    testthat::expect_no_error(
        obj <- writeBPCellsDirMatrix(mat, path = path, overwrite = TRUE)
    )
    testthat::expect_identical(path(obj), path)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
})

testthat::test_that("`to_DelayedArray()` and `to_BPCells()` works well for `MatrixDir` object", {
    delayedop_obj <- to_DelayedArray(obj)
    testthat::expect_s4_class(delayedop_obj, "MatrixDir")
    testthat::expect_identical(delayedop_obj, obj)
    bpcells_obj <- to_BPCells(delayedop_obj)
    testthat::expect_s4_class(bpcells_obj, "MatrixDir")
    testthat::expect_identical(bpcells_obj, obj)
})

test_methods(obj, mat = mat, name = "MatrixDir")
