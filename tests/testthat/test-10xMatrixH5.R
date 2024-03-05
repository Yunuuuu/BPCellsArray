mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
suppressWarnings(obj <- BPCells::write_matrix_10x_hdf5(
    mat = as(as(mat, "dgCMatrix"), "IterableMatrix"), path = path
))

testthat::test_that("`writeBPCells10xHDF5Matrix()` works as expected", {
    testthat::expect_error(writeBPCells10xHDF5Matrix(mat, path = path))
    testthat::expect_warning(
        obj <- writeBPCells10xHDF5Matrix(mat, path = path, overwrite = TRUE)
    )
    testthat::expect_identical(path(obj), path)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
})

test_methods(obj, mat = mat, name = "10xMatrixH5")
