mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
iterable_dgc <- methods::as(methods::as(mat, "dgCMatrix"), "IterableMatrix")
obj <- BPCells::write_matrix_anndata_hdf5(
    mat = BPCells::convert_matrix_type(iterable_dgc, "uint32_t"),
    path = path
)

testthat::test_that("`writeBPCellsAnnHDF5Matrix()` works as expected", {
    testthat::expect_error(writeBPCellsAnnHDF5Matrix(mat, path = path))
    path <- tempfile(tmpdir = tmpdir)
    testthat::expect_no_error(
        obj <- writeBPCellsAnnHDF5Matrix(mat, path = path)
    )
    testthat::expect_identical(path(obj), path)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
})

testthat::test_that("`to_DelayedArray()` and `to_BPCells()` works well for `AnnDataMatrixH5` object", {
    delayedop_obj <- to_DelayedArray(obj)
    testthat::expect_s4_class(delayedop_obj, "AnnDataMatrixH5")
    testthat::expect_identical(delayedop_obj, obj)
    bpcells_obj <- to_BPCells(delayedop_obj)
    testthat::expect_s4_class(bpcells_obj, "AnnDataMatrixH5")
    testthat::expect_identical(bpcells_obj, obj)
})

test_methods(obj, mat = mat, name = "AnnDataMatrixH5")
