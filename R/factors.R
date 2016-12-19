#' Convert indices to assignments
#'
#' Note: this function accepts a sequence of indices as well, in this case it
#' will return a matrix of assignments (one assignment per row).
#'
#' @param idx input index (assumed to be in correct range)
#' @param vars a list of pairs, var index, var scope size
#'
#' @return the assignment associated to this index
#'
#' @export
#'
#' @examples
#' vars <- list(c(1,3),c(3,2))
#' index_to_assignment(3, vars)
#' index_to_assignment(c(2,4,5), vars)
index_to_assignment <- function(idx, vars) {
  scope_sizes <- sapply(vars, pryr::f(x, x[2]))
  strides <- c(1, cumprod(scope_sizes))
  i2a <- pryr::f(i, ((idx - 1) %/% strides[i]) %% scope_sizes[i] + 1)
  sapply(seq_along(scope_sizes), i2a)
}


#' Convert assignent to incies
#'
#' Note: this function accepts a matrix of assignemnts (one per row), in this
#' case the return will be a vector of respective indices.
#'
#' @param assign an assignment to the variables
#' @param vars a list of paris, var index, var scope size
#'
#' @return the index associated to this assignment
#' @export
#'
#' @examples
#' vars <- list(c(1,3),c(3,2))
#' assignment_to_index(c(1,1), vars)
#' assignment_to_index(matrix(c(1,1, 3,2), nrow = 2, byrow = TRUE), vars)
assignment_to_index <- function(assign, vars) {
  scope_sizes <- sapply(vars, pryr::f(x, x[2]))
  strides <- c(1, cumprod(head(scope_sizes, n = -1)))
  as.vector((assign - 1) %*% strides + 1)
}


#' Create a factor
#'
#' @param vals the vector of values for this factor
#' @param vars the list of variables and scopes for this factor.
#'   list(c(1,4), (2,3)) denotes that this factor has two variables with ids
#'   1 and 2, and the scope of factor 1 is of size 4, the scope of factor 2
#'   is of size 3.
#'
#' @return
#' @export
#'
#' @examples
create_factor <- function(vals, vars) {
  stopifnot(!anyDuplicated(sapply(vars, pryr::f(x, x[1]))),
            prod(sapply(vars, pryr::f(x, x[2])), length(vals)))
  list(vals = vals, vars = vars)
}
