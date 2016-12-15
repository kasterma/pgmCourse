#' Title
#'
#' @param idx
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
index_to_assignment <- function(idx, vars) {

}

#' Title
#'
#' @param assign
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
assignment_to_index <- function(assign, vars) {

}


#' create_factor
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
