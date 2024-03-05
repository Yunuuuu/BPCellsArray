mat <- mock_matrix(30, 20)
sparse_mat <- methods::as(mat, "dgCMatrix")

# Packed Memory object
obj <- BPCells::write_matrix_memory(mat = sparse_mat)

testthat::test_that("`to_DelayedArray()` and `to_BPCells()` works well for `PackedMatrixMemBase` object", {
    delayedop_obj <- to_DelayedArray(obj)
    testthat::expect_s4_class(delayedop_obj, "PackedMatrixMemBase")
    testthat::expect_identical(delayedop_obj, obj)
    bpcells_obj <- to_BPCells(delayedop_obj)
    testthat::expect_s4_class(bpcells_obj, "PackedMatrixMemBase")
    testthat::expect_identical(bpcells_obj, obj)
})

test_methods(obj, mat = mat, name = "PackedMatrixMemBase")

# Unpacked Memory object
obj <- BPCells::write_matrix_memory(mat = sparse_mat, FALSE)

testthat::test_that("`to_DelayedArray()` and `to_BPCells()` works well for `UnpackedMatrixMemBase` object", {
    delayedop_obj <- to_DelayedArray(obj)
    testthat::expect_s4_class(delayedop_obj, "UnpackedMatrixMemBase")
    testthat::expect_identical(delayedop_obj, obj)
    bpcells_obj <- to_BPCells(delayedop_obj)
    testthat::expect_s4_class(bpcells_obj, "UnpackedMatrixMemBase")
    testthat::expect_identical(bpcells_obj, obj)
})

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
