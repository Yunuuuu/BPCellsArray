mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_hdf5(
    mat = as(mat, "dgCMatrix"),
    path = path, group = "BPCells"
)

common_test(
    obj, path,
    mat = mat, 
    name = "HDF5"
)
testthat::test_that("`subset()` BPCellsHDF5Seed object works as expected", {
    seed <- BPCellsSeed(obj)
    testthat::expect_s4_class(seed[1:10, ], "BPCellsSubsetSeed")
    testthat::expect_s4_class(seed[, 1:10], "BPCellsSubsetSeed")
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsSubsetSeed")
})

testthat::test_that("`writeBPCellsHDF5Array()` works as expected", {
    testthat::expect_no_error(
        obj <- writeBPCellsHDF5Array(mat,
            path = path,
            "BPCellsArray", overwrite = TRUE
        )
    )
    testthat::expect_identical(path(obj), path)
    testthat::expect_s4_class(obj, "BPCellsHDF5Matrix")
})
