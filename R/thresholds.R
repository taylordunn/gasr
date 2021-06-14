#' Create score thresholds
#'
#' Creates a set of thresholds which can be used to discretize
#' latent continuous goal scores into "observed" discrete scores.
#'
#' This function follows the approach introduced by
#' Urach et al. 2019 to get equally spaced attainment levels from
#' a cumulative standard normal distribution.
#' The distribution `centre` can be shifted to adjust the "difficulty" of
#' goals.
#'
#' @references
#' \insertRef{Urach2019}{gasr}
#'
#' @param n_levels The number of levels to use. Defaults to the traditional
#'   5 levels.
#' @param centre The centre of the normal distribution from which the thresholds
#'   are taken. See 'Details'.
#'
#' @return A numeric vector of thresholds with `n_levels + 1` values. The first
#'   value will be `-Inf`, and the last `Inf`, so that extremely small or large
#'   values can still be discretized.
#' @export
#'
#' @examples
#' create_thresholds()
#' create_thresholds(n_levels = 3)
#'
#' # Make goals slightly easier by shifting the thresholds left
#' create_thresholds(centre = -0.2)
#' # Or harder by shifting right
#' create_thresholds(centre = 0.2)
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats qnorm
create_thresholds <- function(n_levels = 5, centre = 0) {
  if (n_levels %% 2 == 0) {
    warning("It is recommended to use an odd number of attainment levels in ",
            "goal attainment scaling.")
  }

  stats::qnorm(
    seq(0, 1, length.out = n_levels + 1),
    centre, 1
  )
}

#' Discretize from thresholds
#'
#' Discretizes continuous latent goal score values into discrete levels
#' centred at 0, using a set of thresholds.
#'
#' @param score_continuous Numeric vector of continuous goal scores.
#' @param thresholds Numeric vector of thresholds separating the goal
#'   attainment levels. Must be an even number of thresholds (which corresponds
#'   to an odd number of attainment levels)
#'
#' @return Numeric vector, the same length as `score_continuous`, of integers
#'   corresponding to discrete attainment levels. For a typical 5-point
#'   attainment scale, the returned values will be of the set
#'   \{-2, -1, 0, 1, 2\}.
#' @export
#'
#' @examples
#' thresh <- create_thresholds()
#' y <- rnorm(10)
#' y_discrete <- discretize_from_thresholds(y, thresh)
discretize_from_thresholds <- function(score_continuous, thresholds) {
  if (length(thresholds) %% 2 == 1) {
    stop("An odd number of attainment levels ",
         "(and thus an even number of elements in thresholds) ",
         "is required.")
  }

  # Cuts scores into levels ranging from 1 to the number of levels (e.g. 1 to 5)
  score_discrete <- cut(score_continuous, breaks = thresholds, labels = FALSE)
  # Shift the values so that they are centred at 0
  score_discrete <- score_discrete - length(thresholds) / 2

  return(score_discrete)
}
