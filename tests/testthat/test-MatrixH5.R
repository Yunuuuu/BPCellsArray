mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_hdf5(
    mat = as(mat, "dgCMatrix"),
    path = path, group = "BPCells"
)

testthat::test_that("`writeBPCellsHDF5Matrix()` works as expected", {
    testthat::expect_error(
        writeBPCellsHDF5Matrix(mat, "BPCellsArray", path = path)
    )
    testthat::expect_no_error(
        obj <- writeBPCellsHDF5Matrix(mat,
            path = path,
            "BPCellsArray", overwrite = TRUE
        )
    )
    testthat::expect_identical(path(obj), path)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
})

testthat::test_that("`to_DelayedArray()` and `to_BPCells()` works well for `MatrixH5` object", {
    delayedop_obj <- to_DelayedArray(obj)
    testthat::expect_s4_class(delayedop_obj, "MatrixH5")
    testthat::expect_identical(delayedop_obj, obj)
    bpcells_obj <- to_BPCells(delayedop_obj)
    testthat::expect_s4_class(bpcells_obj, "MatrixH5")
    testthat::expect_identical(bpcells_obj, obj)
})

test_methods(obj, mat = mat, name = "MatrixH5")
