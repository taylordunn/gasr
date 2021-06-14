#' Calculate a GAS T-score
#'
#' Computes a single GAS T-score for a subject at a point in time.
#'
#' @param scores Discrete goal scores.
#' @param weights Optional goal weights. If not provided, all weights are
#'   set to 1 (i.e. unweighted goals).
#' @param rho Average degree of inter-correlation between goal scores.
#'   By default, `rho` = 0.3.
#' @param na.rm Logical. If TRUE (default), then removes missing values.
#'
#' @return A single score aggregating a subject's set of a goal scores.
#' @export
#'
#' @examples
#' d <- list(scores = c(-2, 0, -1, 2, 0, 1),
#'           weights = c(3, 5, 1, 4, 2, 6))
#' calc_tscore(d$scores, d$weights)
calc_tscore <- function(scores, weights = NULL, rho = 0.3, na.rm = TRUE) {
  # If weights are not provided, impute weight = 1 for each goal
  if (is.null(weights)) {
    weights <- rep(1, length(scores))
  }

  if (na.rm) {
    non_na <- !is.na(scores) & !is.na(weights)
    scores <- scores[non_na]
    weights <- weights[non_na]
  }

  if (all(weights == 0)) {
    weights <- rep(1, length(weights))
  }

  numerator <- 10 * sum(scores * weights)
  denominator <- (1 - rho) * sum(weights^2) + rho * (sum(weights))^2
  gas_tscore <- 50 + numerator / denominator^0.5
  return(gas_tscore)
}
