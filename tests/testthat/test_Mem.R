mat <- mock_matrix(30, 20)
sparse_mat <- as(mat, "dgCMatrix")
path <- character()

# Packed Memory object
obj <- BPCells::write_matrix_memory(mat = sparse_mat)
common_test(
    obj, path,
    mat = mat, 
    seed_fn = BPCellsMemSeed, 
    name = "Mem"
)

# Unpacked Memory object
obj <- BPCells::write_matrix_memory(mat = sparse_mat, FALSE)
common_test(
    obj, path,
    mat = mat, 
    seed_fn = BPCellsMemSeed, 
    name = "Mem"
)

testthat::test_that("`subset()` BPCellsMemSeed object works as expected", {
    seed <- BPCellsMemSeed(obj)
    testthat::expect_s4_class(seed[1:10, ], "BPCellsSubsetSeed")
    testthat::expect_s4_class(seed[, 1:10], "BPCellsSubsetSeed")
})

testthat::test_that("`writeBPCellsMemArray()` works as expected", {
    testthat::expect_no_error(
        obj <- writeBPCellsMemArray(sparse_mat, compress = TRUE)
    )
    testthat::expect_identical(path(obj), character())
    testthat::expect_s4_class(obj, "BPCellsMemMatrix")
    testthat::expect_no_error(
        obj <- writeBPCellsMemArray(sparse_mat, compress = FALSE)
    )
    testthat::expect_identical(path(obj), character())
    testthat::expect_s4_class(obj, "BPCellsMemMatrix")
})

testthat::test_that("`as()` methods works as expected", {
    testthat::expect_s4_class(as(mat, "BPCellsMemMatrix"), "BPCellsMemMatrix")
    testthat::expect_s4_class(as(mat, "BPCellsMemArray"), "BPCellsMemMatrix")
})
