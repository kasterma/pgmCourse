library(testthat)
library(igraph)

phi_1 <- create_factor(rep(1, 4), list(c(1,2), c(2,2)))
phi_2 <- create_factor(rep(1, 4), list(c(2,2), c(3,2)))
phi_3 <- create_factor(rep(1, 4), list(c(1,2), c(4,2)))

Phi <- list(phi_1, phi_2, phi_3)

#' Create Bethe cluster graph
#'
#' @param factors list of factors to create the graph for
#'
#' @return the Bethe cluster graph
#' @export
#'
#' @examples
bethe <- function(factors) {
  big_clusters <- Map(function(factor) sapply(factor$vars, function(x) x[1]), factors)
  little_clusters <- as.list(unique(c(big_clusters, recursive = TRUE)))
  clusters <- c(big_clusters, little_clusters)
  clusters_ch <- Map(as.character, clusters)
  g <- make_empty_graph(n = length(clusters))
  V(g)$name <- clusters_ch
  bc_len <- length(big_clusters)
  for (idx_big in seq_along(big_clusters)) {
    for (idx_small in seq_along(little_clusters)) {
      if (little_clusters[[idx_small]] %in% big_clusters[[idx_big]])
        g <- g + edge(idx_big, idx_small + bc_len)
    }
  }
  g
}

result_df <- data.frame(from = I(list(c("1", "2"), c("1", "2"), c("2", "3"), c("2", "3"),
                                      c("1", "4"), c("1", "4"))),
                        to = I(list("1", "2", "2", "3", "1", "4")))
expect_equal(as_data_frame(bethe(Phi)), result_df, check.attributes = FALSE)

expect_equal(as_data_frame(bethe(Phi)),
             structure(list(from = list(c("1", "2"), c("1", "2"),
                                        c("2", "3"), c("2", "3"),
                                        c("1", "4"), c("1", "4")),
                            to = list("1", "2", "2", "3", "1", "4")),
                       .Names = c("from", "to"),
                       class = "data.frame", row.names = c(NA,
                                                           6L)))
xx <- list(from = list(c("1", "2"), c("1", "2"),
                       c("2", "3"), c("2", "3"),
                       c("1", "4"), c("1", "4")),
           to = list("1", "2", "2", "3", "1", "4"))

G <- as.undirected(bethe(Phi))

