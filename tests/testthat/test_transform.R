mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)

test_BPCellsArray(
    BPCells::expm1_slow(obj), path,
    name = "TransformExpm1Slow",
    skip_multiplication = TRUE # often return Inf value
)

test_BPCellsArray(
    BPCells::log1p_slow(obj), path,
    name = "TransformLog1p"
)

# test_BPCellsArray(
#     expm1(obj), path,
#     name = "TransformExpm1",
#     skip_multiplication = TRUE # often return Inf value
# )

# test_BPCellsArray(
#     log1p(obj), path,
#     name = "TransformLog1pSingle"
# )

test_BPCellsArray(
    obj + 1, path,
    mat = mat + 1,
    name = "TransformScaleShift"
)

test_BPCellsArray(
    obj * 2, path,
    mat = mat * 2,
    name = "TransformScaleShift"
)
