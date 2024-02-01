mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)

common_test(
    BPCells::expm1_slow(obj), path,
    seed_fn = BPCellsTransformedSeed,
    name = "TransformExpm1Slow",
    skip_multiplication = TRUE # often return Inf value
)

common_test(
    BPCells::log1p_slow(obj), path,
    seed_fn = BPCellsTransformedSeed,
    name = "TransformLog1pSlow"
)

common_test(
    expm1(obj), path,
    seed_fn = BPCellsTransformedSeed,
    name = "TransformExpm1",
    skip_multiplication = TRUE # often return Inf value
)

common_test(
    log1p(obj), path,
    seed_fn = BPCellsTransformedSeed,
    name = "TransformLog1p"
)

# common_test(
#     obj + 1, path,
#     mat = mat + 1,
#     seed_fn = BPCellsTransformedSeed,
#     name = "TransformScaleShift"
# )

# common_test(
#     obj * 2, path,
#     mat = mat * 2,
#     seed_fn = BPCellsTransformedSeed,
#     name = "TransformScaleShift"
# )
