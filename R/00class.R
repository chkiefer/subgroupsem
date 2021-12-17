#' @export
plot.subgroupsem <- function(x, ...) {
    obj <- x
    n_sg <- nrow(obj$summary_statistics)
    x <- 1:n_sg
    y <- obj$summary_statistics$quality
    max_qf <- max(y)
    plot(
        x, y,
        xlab = "Subgroup index",
        ylab = "Interestingness measure",
        col = "black",
        type = "b",
        pch = 21,
        ylim = c(min(y), max(y)),
        main = "Interestingness of subgroups"
    )
}

#' @export
summary.subgroupsem <- function(object, ...) {
    obj <- object
    cat("General information:")
    cat("\n")

    cat("Elapsed time: ")
    cat(obj$time_elapsed)
    cat("\n\n")

    cat("Summary of subgroup search:\n")
    print(obj$summary_statistics)
}