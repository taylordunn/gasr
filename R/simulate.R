#' Simulate subjects
#'
#' Simulate a number of subjects with subject-level random effects, and randomly
#' assign them to groups.
#'
#' @param n_subjects Number of subjects.
#' @param sigma_u Standard deviation of the subject-level random effect.
#' @param group_allocation A named list of groups and their probability of
#'   assignment. By default, will give equal probability to "control" and
#'   "treatment" groups. If NULL, will returns subjects without groups.
#' @param random_allocation Logical. If TRUE (default), randomly assigns to
#'   groups based on probabilities in `group_allocation`. If FALSE, assigns with
#'   equal probability. Note that this option will return an error if the
#'   subjects cannot be neatly divided. For example, `n_subjects` = 21 with
#'   two groups of probabilities 0.5 and 0.5, will return an error if
#'   `random_allocation = FALSE` because 21 cannot be equally divided in two.
#'
#' @return A data frame with a row for each subject.
#' @export
#'
#' @examples
#' sim_subjects(n_subjects = 20)
#' sim_subjects(
#'   group_allocation = list("placebo" = 1/2,
#'                           "1x dose" = 1/4, "2x dose" = 1/4)
#' )
#'
#' # If using non-random group allocation, `n_subjects` must be able to be
#' #  divided using the given `group_allocation`
#' sim_subjects(
#'   n_subjects = 99,
#'   group_allocation = list("placebo" = 1/3,
#'                           "1x dose" = 1/3, "2x dose" = 1/3),
#'   random_allocation = FALSE
#' )
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom stats rnorm
#' @importFrom rlang .data
#' @importFrom purrr map_int
sim_subjects <- function(n_subjects = 100, sigma_u = 0.5,
                         group_allocation = list("control" = 0.5,
                                                 "treatment" = 0.5),
                         random_allocation = TRUE) {
  subjects <- tibble::tibble(
    subject_id = generate_subject_ids(n_subjects),
    subject_re = stats::rnorm(n_subjects, mean = 0, sd = sigma_u),
  )

  if (!is.null(group_allocation)) {
    if (random_allocation) {
      subjects <- subjects %>%
        mutate(
          group = sample(names(group_allocation), size = n_subjects,
                         replace = TRUE, prob = as.numeric(group_allocation))
        )
    } else {
      subjects <- subjects %>%
        mutate(
          group =  rep(names(group_allocation),
                       purrr::map_int(group_allocation,
                                      ~as.integer(. * n_subjects)))
        )
    }

    subjects <- subjects %>%
      dplyr::mutate(
        group = factor(.data$group, levels = names(group_allocation))
      )
  }

  return(subjects)
}

#' Simulate goals
#'
#' For each subject, simulate a specified number of goals. There are three
#' options for defining the number of goals: `n_goals` sets the same number
#' for each subject, `n_goals_range` sets the minimum and maximum number,
#' and `n_goals_prob` sets specific probabilities for each number.
#'
#' @param subjects A data frame with a row per subject.
#' @param n_goals The exact number of goals for each subject.
#' @param n_goals_range A vector defining the minimum and maximum number of
#'   goals per subjects. Randomly samples the range of goals with uniform
#'   probability.
#' @param n_goals_prob A named list with element `n_goals` defining the possible
#'   numbers of goals, and element `prob` defining the probabilities of getting
#'   each number. For example:
#'   `n_goals_prob = list(n_goals = 1:3, probs = c(0.3, 0.3, 0.4)`.
#' @param sigma_e Standard deviation of the goal-level random effect, i.e.
#'   the random error of each latent goal score.
#'
#' @return The same `subjects` data frame with a `n_goals` column, and
#'   `goals` list column. The `goals` list column contains tibbles with two
#'   variables: a subject-specific goal index `goal_num`, and a normally
#'   distributed random effect `goal_re`.
#' @export
#'
#' @examples
#' sim_subjects() %>%
#'   sim_goals(n_goals = 3, sigma_e = 0.8)
#'
#' sim_subjects() %>%
#'   sim_goals(n_goals_range = c(2, 7))
#'
#' sim_subjects() %>%
#'   sim_goals(n_goals_prob = list(n_goals = c(6, 3, 4),
#'                                 prob = c(0.3, 0.5, 0.2)))
#'
#' @importFrom dplyr mutate n
#' @importFrom purrr map
sim_goals <- function(subjects, n_goals = NULL, n_goals_range = NULL,
                      n_goals_prob = NULL, sigma_e = 0.5) {
  if (is.null(n_goals) & is.null(n_goals_range) & is.null(n_goals_prob)) {
    warning("The number of goals was not specified ",
            "(via arguments `n_goals`, `n_goals_range`, or `n_goals_prob`).",
            "\nSetting `n_goals` = 3.")
    n_goals <- 3
  }

  subjects %>%
    dplyr::mutate(
      # Have to use an ugly if/else chain because case_when evaluates
      #  n_goals_range[] which returns an error if NULL
      n_goals = if (!is.null(n_goals)) {
        n_goals
      } else if (!is.null(n_goals_range)) {
        sample(
          seq(n_goals_range[1], n_goals_range[2]),
          size = dplyr::n(), replace = TRUE
        )
      } else if (!is.null(n_goals_prob)) {
        sample(
          n_goals_prob$n_goals,
          size = dplyr::n(), replace = TRUE,
          prob = n_goals_prob$prob
        )
      },
      goals = purrr::map(
        n_goals,
        ~tibble::tibble(goal_num = 1:.x,
                        goal_re = rnorm(.x, 0, sigma_e))
      )
    )
}

#' Simulate a treatment effect
#'
#' Currently only simulates a uniform distribution of treatment effects,
#' regardless of group allocation.
#'
#' @param goals A data frame with a row for each goal.
#' @param delta The size of the treatment effect.
#'
#' @return The same `goals` data frame with the added `treatment_fe` column.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#'
#' # Generate some subjects and goals
#' d <- sim_subjects(n_subjects = 40) %>%
#'   sim_goals(n_goals_range = c(1, 3))
#'
#' # Apply treatment effects in one of two ways
#' # Using purrr::map()
#' d %>%
#'   mutate(
#'     goals = map(
#'       goals,
#'       ~sim_treatment_effect(., delta = 0.4)
#'     )
#'   )
#'
#' # Or using tidyr::unnest() on the goals column
#' d %>%
#'   unnest(goals) %>%
#'   group_by(subject_id) %>%
#'   sim_treatment_effect(delta = 0.4)
#'
#' @importFrom dplyr mutate n
#' @importFrom stats runif
sim_treatment_effect <- function(goals, delta = 0.3) {
  goals %>%
    dplyr::mutate(
      treatment_fe = runif(dplyr::n(), 0, 2 * delta)
    )
}

#' Simulate goal weights
#'
#' Adds numeric weights to a set of goals (usually a single subject's set of
#' goals).
#'
#' There are a few valid options to the `weight_type` argument, including
#' "unweighted", "preference", and "treatment".
#' The "unweighted" type is the default and is the simplest:
#' it sets all goal weights to 1.
#' The "preference" type randomly randomly samples (without replacement)
#' integer weights from 1 to `n`. For instance, a subject with `n` = 3 goals
#' will have weights 1, 2, and 3 randomly assigned to their goals.
#' The "treatment" type computes a weight biased by the treatment effect, and
#' so requires a numeric `treatment_fe` column. These weights are calculated
#' as `n * treatment_fe / sum(treatment_fe)`.
#'
#' @param goals A data frame with a row for each goal.
#' @param weight_type The type of weights to apply. See 'Details'.
#'
#' @return The same `goals` data frame with the added `goal_weight` column.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#'
#' # Generate some subjects and goals
#' d <- sim_subjects(n_subjects = 40) %>%
#'   sim_goals(n_goals_range = c(1, 3))
#'
#' # Apply weights in one of two ways
#' # Using purrr::map()
#' d %>%
#'   mutate(
#'     goals = map(
#'       goals, sim_goal_weights
#'     )
#'   )
#' # Or using tidyr::unnest() on the goals column
#' d %>%
#'   unnest(goals) %>%
#'   group_by(subject_id) %>%
#'   sim_goal_weights(weight_type = "preference")
#'
#' # The "treatment" goal weights require a treatment_fe column
#' d %>%
#'   unnest(goals) %>%
#'   group_by(subject_id) %>%
#'   sim_treatment_effect(delta = 0.4) %>%
#'   sim_goal_weights(weight_type = "treatment")
#' @importFrom dplyr mutate n
#' @importFrom rlang .data
sim_goal_weights <- function(
  goals,
  weight_type = c("unweighted", "preference", "treatment")
) {
  weight_type <- match.arg(weight_type)

  goals %>%
    dplyr::mutate(
      goal_weight = if (weight_type == "unweighted") {
        1.0
      } else if (weight_type == "preference") {
        sample(1:dplyr::n(), dplyr::n(), replace = FALSE)
      } else if (weight_type == "treatment") {
        dplyr::n() * .data$treatment_fe / sum(.data$treatment_fe)
      }
    )

}

#' Simulate a single cross-sectional trial
#'
#' @param mean_control_response The mean response in the control group.
#'
#' @return A data frame with a row per trial (numbered 1 to `n_sim`) with
#'   list column` data` containing the simulated data.
#' @export
#'
#' @inheritParams sim_subjects
#' @inheritParams sim_goals
#' @inheritParams sim_treatment_effect
#' @inheritParams sim_goal_weights
#' @inheritParams create_thresholds
#' @importFrom dplyr mutate group_by ungroup
#' @importFrom tidyr unnest
#' @importFrom rlang .data
sim_trial <- function(
  n_subjects = 40, sigma_u = 0.5,
  group_allocation =  list("control" = 0.5, "treatment" = 0.5),
  random_allocation = TRUE,
  n_goals = NULL, n_goals_range = c(3, 6), n_goals_prob = NULL,
  sigma_e = 0.5, delta = 0.3, mean_control_response = -0.3,
  weight_type = "unweighted",
  n_levels = 5, score_dist = "norm", centre = 0
) {
  if (!is.null(n_goals) | !is.null(n_goals_prob)) {
    n_goals_range <- NULL
  }

  thresh <- create_thresholds(n_levels, score_dist, centre)

  sim_subjects(n_subjects, sigma_u, group_allocation, random_allocation) %>%
    sim_goals(n_goals, n_goals_range, n_goals_prob, sigma_e) %>%
    tidyr::unnest(.data$goals) %>%
    dplyr::group_by(.data$subject_id) %>%
    sim_treatment_effect(delta) %>%
    sim_goal_weights(weight_type) %>%
    dplyr::mutate(
      score_continuous = ifelse(.data$group == "treatment",
                                .data$treatment_fe, mean_control_response) +
        .data$subject_re + .data$goal_re,
      score_discrete = discretize_from_thresholds(.data$score_continuous,
                                                  thresh),
      tscore = calc_tscore(.data$score_discrete, .data$goal_weight)
    ) %>%
    dplyr::ungroup()
}
