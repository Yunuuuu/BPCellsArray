mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)

test_BPCellsArray(
    obj, path,
    mat = mat,
    name = "Dir"
)

testthat::test_that("`writeBPCellsDirArray()` works as expected", {
    testthat::expect_no_error(
        obj <- writeBPCellsDirArray(mat, path = path, overwrite = TRUE)
    )
    testthat::expect_identical(path(obj), path)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
})
