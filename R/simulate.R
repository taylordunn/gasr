#' Simulate subjects
#'
#' @param n_subjects Number of subjects.
#' @param sigma_u Standard deviation of the subject-level random effect.
#' @param group_allocation A named list of groups and their probability of
#'   assignment. By default, will equally allocate subjects to "control" and
#'   "treatment"
#'
#' @return A dataframe with a row for each subject.
#' @export
#'
#' @examples
#' sim_subjects(n_subjects = 20)
#' sim_subjects(
#'   group_allocation = list("placebo" = 1/2,
#'                           "1x dose" = 1/4, "2x dose" = 1/4)
#' )
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom stats rnorm
#' @importFrom rlang .data
sim_subjects <- function(n_subjects = 100, sigma_u = 0.5,
                         group_allocation = list("control" = 0.5,
                                                 "treatment" = 0.5)) {
  tibble::tibble(
    subject_id = generate_subject_ids(n_subjects),
    group = sample(names(group_allocation), size = n_subjects,
                   replace = TRUE, prob = as.numeric(group_allocation)),
    subject_re = stats::rnorm(n_subjects, mean = 0, sd = sigma_u),
  ) %>%
    dplyr::mutate(group = factor(.data$group, levels = names(group_allocation)))
}

#' Simulate goals
#'
#' @param subjects A dataframe with a row per subject.
#' @param n_goals The exact number of goals for each subject.
#' @param n_goals_range A vector defining the minimum and maximum number of
#'   goals per subjects. Randomly samples the range of goals with uniform
#'   probbility.
#' @param n_goals_prob A named list with element `n_goals` defining the possible
#'   numbers of goals, and element `prob` defining the probabilities of getting
#'   each number. For example:
#'   `n_goals_prob = list(n_goals = 1:3, probs = c(0.3, 0.3, 0.4`.
#' @param sigma_e Standard deviation of the goal-level random effect, i.e.
#'   the random error of each latent goal score.
#'
#' @return The same `subjects` dataframe with a `n_goals` column, and
#'   `goals` list column.
#' @export
#'
#' @examples
#'
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
#' regardless of group allocation
#'
#' @param goals A dataframe with a row for each goal.
#' @param delta The size of the treatment effect
#'
#' @return The same `goals` dataframe with the added `treatment_fe` column.
#' @export
#'
#' @importFrom dplyr mutate n
#' @importFrom stats runif
sim_treatment_effect <- function(goals, delta = 0.3) {
  goals %>%
    dplyr::mutate(
      treatment_fe = runif(dplyr::n(), 0, 2 * delta)
    )
}
