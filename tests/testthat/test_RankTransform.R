mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
obj <- BPCells:::rank_transform(obj, "col")
# mat <- matrixStats::colRanks(mat,
#     ties.method = "average",
#     preserveShape = TRUE
# )
common_test(
    obj, path,
    name = "RankTransform"
)
