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

test_methods(obj, mat = mat, name = "MatrixDir")
