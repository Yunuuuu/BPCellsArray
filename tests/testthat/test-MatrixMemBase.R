mat <- mock_matrix(30, 20)
sparse_mat <- as(mat, "dgCMatrix")
path <- character()

# Packed Memory object
obj <- BPCells::write_matrix_memory(mat = sparse_mat)
test_methods(obj, mat = mat, name = "PackedMatrixMemBase")

# Unpacked Memory object
obj <- BPCells::write_matrix_memory(mat = sparse_mat, FALSE)
test_methods(obj, mat = mat, name = "UnpackedMatrixMemBase")

testthat::test_that("`writeBPCellsMemMatrix()` works as expected", {
    testthat::expect_no_error(
        obj <- writeBPCellsMemMatrix(sparse_mat, compress = TRUE)
    )
    testthat::expect_error(path(obj))
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_no_error(
        obj <- writeBPCellsMemMatrix(sparse_mat, compress = FALSE)
    )
    testthat::expect_error(path(obj))
    testthat::expect_s4_class(obj, "BPCellsMatrix")
})
