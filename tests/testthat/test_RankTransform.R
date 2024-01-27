mat <- mock_matrix(2000, 200)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
obj <- BPCells:::rank_transform(obj, "col")
# mat <- matrixStats::colRanks(mat,
#     ties.method = "average",
#     preserveShape = TRUE
# )
# common_test(mat, obj, path, BPCellsRankTransformSeed, "RankTransform")

testthat::test_that(
    "`subset()` BPCellsRankTransformSeed object works as expected",
    {
        seed <- BPCellsRankTransformSeed(obj)
        testthat::expect_s4_class(seed[1:10, ], "BPCellsSubsetSeed")
        testthat::expect_s4_class(seed[, 1:10], "BPCellsSubsetSeed")
        testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsSubsetSeed")
    }
)
