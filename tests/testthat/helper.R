mock_matrix <- function(ngenes, ncells) {
    cell.means <- 2^stats::runif(ngenes, 2, 10)
    cell.disp <- 100 / cell.means + 0.5
    cell.data <- matrix(stats::rnbinom(ngenes * ncells,
        mu = cell.means,
        size = 1 / cell.disp
    ), ncol = ncells)
    rownames(cell.data) <- sprintf("Gene_%s", formatC(seq_len(ngenes),
        width = 4, flag = 0
    ))
    colnames(cell.data) <- sprintf("Cell_%s", formatC(seq_len(ncells),
        width = 3, flag = 0
    ))
    cell.data
}
