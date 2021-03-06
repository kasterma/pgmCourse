var_ids <- function(vars) {
  sapply(vars, function(x) x[1])
}

var_scopes <- function(vars) {
  sapply(vars, function(x) x[2])
}

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
  scope_sizes <- var_scopes(vars)
  strides <- c(1, cumprod(scope_sizes))
  i2a <- function(i) ((idx - 1) %/% strides[i]) %% scope_sizes[i] + 1
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
  if (length(assign) == 0 && length(vars) == 0)
    1
  else {
    scope_sizes <- var_scopes(vars)
    strides <- c(1, cumprod(head(scope_sizes, n = -1)))
    as.vector((assign - 1) %*% strides + 1)
  }
}


#' Create a factor
#'
#' @param vals the vector of values for this factor
#' @param vars the list of variables and scopes for this factor.
#'   list(c(1,4), (2,3)) denotes that this factor has two variables with ids
#'   1 and 2, and the scope of factor 1 is of size 4, the scope of factor 2
#'   is of size 3.
#'
#' @return the created factor
#' @export
#'
#' @examples
#' create_factor(c(1,2,3,4,5,6), list(c(1,2), c(2, 3)))
create_factor <- function(vals, vars) {
  if (length(vals) == 1 & length(vars) == 0)
    list(vals = vals, vars = vars)
  else {
    stopifnot(!anyDuplicated(var_ids(vars)),
              prod(var_scopes(vars)) == length(vals))
    list(vals = vals, vars = vars)
  }
}

#' Normalize factor
#'
#' Normalize a factor to be a probability distribution; can pass variables
#' on which this distribution is to be conditioned.
#'
#' @param fact factor to normalize
#' @param ... variables the probability is conditioned over
#'
#' @return normalized factor
#' @export
#'
#' @examples
#' x <- create_factor(c(1,1,2,2), list(c(1,2), c(2,2)))
#' normalize_factor(x)      # the joint distribution
#' normalize_factor(x, 1)   # the conditional distribution on 2 conditioned on 1
#' normalize_factor(x, 2)   # the ocnditional distribution on 1 conditioned on 2
normalize_factor <- function(fact, ...) {
  stopifnot(all(fact$vals >= 0))

  conditional_vars <- c(...)
  cv_locs <- match(conditional_vars, var_ids(fact$vars))
  stopifnot(all(!is.na(cv_locs)))

  if (length(conditional_vars) == 0) {
    fact$vals <- fact$vals/sum(fact$vals)
  } else {
    names(cv_locs) <- paste0("V", conditional_vars)
    scope_sizes <- var_scopes(fact$vars)
    strides <- c(1, cumprod(scope_sizes))
    df <- data.frame(vals = fact$vals)
    # add in the columns for the variables to condition over
    for (var in names(cv_locs)) {
      var_idx <- cv_locs[var]
      df[[var]] <- rep(x = seq(1, scope_sizes[var_idx]), each = strides[var_idx], length.out = prod(scope_sizes))
    }
    df %<>% dplyr::group_by_(.dots = names(cv_locs)) %>% dplyr::mutate(vals = vals / sum(vals))
    fact$vals <- df$vals
  }
  fact
}

#' Expand factor to data.frame
#'
#' Generate a data frame with the information in a factor; format is such
#' that it allows for easy printing.
#'
#' @param fact factor to be converted
#' @param var_names vector of variable names; the names used are extracted using
#'     the variable indices from this.  If missing generate some variable names.
#'
#' @return data.frame with all info from the factor
#' @export
#'
#' @examples
factor2df <- function(fact, var_names) {
  var_ids <- var_ids(fact$vars)
  if (missing(var_names))
    var_names <- paste0("variable-", var_ids)
  else
    var_names <- var_names[var_ids]
    stopifnot(all(!is.na(var_names)))
  scope_sizes <- var_scopes(fact$vars)
  names(scope_sizes) <- var_names
  strides <- c(1, cumprod(scope_sizes))
  names(strides) <- var_names

  df <- data.frame(vals = fact$vals)
  for (var in var_names) {
    df[[var]] <- rep(x = seq(1, scope_sizes[var]),
                     each = strides[var],
                     length.out = length(fact$vals))
  }
  df
}

#' Product of factors
#'
#' @param f1 first factor
#' @param f2 second factor
#'
#' @return product of the input factors
#' @export
#'
#' @examples
#' f1 <- create_factor(c(1,2,3,4,5,6), list(c(1, 2), c(2,3)))
#' f2 <- create_factor(c(1,2,3,4,5,6), list(c(2, 3), c(3,2)))
#' factor_product(f1, f2)
factor_product <- function(f1, f2) {
  f1.var_ids <- var_ids(f1$vars)
  f1.var_scopes <- var_scopes(f1$vars)
  f2.var_ids <- var_ids(f2$vars)
  f2.var_scopes <- var_scopes(f2$vars)
  var_ids <- union(f1.var_ids, f2.var_ids)
  f1.map <- match(var_ids, f1.var_ids)
  f1.imap <- match(f1.var_ids, var_ids)
  f2.map <- match(var_ids, f2.var_ids)
  f2.imap <- match(f2.var_ids, var_ids)
  var_scopes <- ifelse(!is.na(f1.map), f1.var_scopes[f1.map], f2.var_scopes[f2.map])

  vars <- mapply(c, var_ids, var_scopes, SIMPLIFY = FALSE)

  vals <- numeric(prod(var_scopes))
  for (idx in seq_len(prod(var_scopes))) {
    assignment <- index_to_assignment(idx, vars)
    ass1 <- assignment[f1.imap]
    ass2 <- assignment[f2.imap]

    vals[idx] <-
      f1$vals[assignment_to_index(ass1, f1$vars)] *
      f2$vals[assignment_to_index(ass2, f2$vars)]
  }

  create_factor(vals, mapply(c, var_ids, var_scopes, SIMPLIFY = FALSE))
}

#' Marginalize a factor
#'
#' @param fact input factor
#' @param idx variable to marginalize out
#'
#' @return marginalized factor
#' @export
#'
#' @examples
#' fact <- create_factor(c(1,2,3,4,5,6), list(c(1,2), c(2,3)))
#' fm1 <- factor_marginaliztion(fact,3)
#' fm2 <- factor_marginaliztion(fact,2)
#' fm3 <- factor_marginaliztion(fact,1)
factor_marginaliztion <- function(fact, idx) {
  var_ids <- var_ids(fact$vars)
  if (!idx %in% var_ids)
    fact
  else {
    i <- match(idx, var_ids)
    var_scopes <- var_scopes(fact$vars)
    vals <- numeric(prod(var_scopes[-i]))
    for (j in seq_len(prod(var_scopes))) {
      assignment <- index_to_assignment(j, fact$vars)
      vals[assignment_to_index(assignment[-i], fact$vars[-i])] <-
        vals[assignment_to_index(assignment[-i], fact$vars[-i])] + fact$vals[j]
    }
    create_factor(vals, mapply(c, var_ids[-i], var_scopes[-i], SIMPLIFY = FALSE))
  }
}

#' Reduce factor
#'
#' @param fact factor to be reduced
#' @param idx index of variable on which to reduce
#' @param val value of variable on which to reduce
#'
#' @return reduced factor
#' @export
#'
#' @examples
#' fact <- create_factor(c(1,2,3,4,5,6), list(c(1,2), c(2,3)))
#' factor_reduction(fact, 1, 1)
factor_reduction <- function(fact, idx, val) {
  var_ids <- var_ids(fact$vars)
  i <- match(idx, var_ids)
  if (is.na(i)) {
    fact
  } else {
    scopes <- var_scopes(fact$vars)
    strides <- c(1,cumprod(scopes))
    idxs <- rep(seq(1, scopes[i]), each = strides[i], length.out = prod(scopes))
    create_factor(fact$vals[idxs == val], fact$vars[-i])
  }
}
