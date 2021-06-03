#' Simulate subjects
#'
#' @param n_subjects Number of subjects.
#' @param sigma_u Standard deviation of the subject-level random effect.
#' @param group_allocation A named list of groups and their probability of
#'   assignment. By default, will equally allocate subjects to "control" and
#'   "treatment"
#'
#' @return
#' @export
#'
#' @examples
#' sim_subjects(n_subjects = 20)
#' sim_subjects(
#'   group_allocation = list("placebo" = 1/2,
#'                           "1x dose" = 1/4, "2x dose" = 1/4)
#' )
sim_subjects <- function(n_subjects = 100, sigma_u = 0.5,
                         group_allocation = list("control" = 0.5,
                                                 "treatment" = 0.5)) {
  tibble::tibble(
    subject_id = generate_subject_ids(n_subjects),
    group = sample(names(group_allocation), size = n_subjects,
                   replace = TRUE, prob = as.numeric(group_allocation)),
    subject_re = rnorm(n_subjects, mean = 0, sd = sigma_u),
  ) %>%
    dplyr::mutate(group = factor(group, levels = names(group_allocation)))
}

sim_goals <- function(subjects, n_goals = NULL, n_goals_range = NULL,
                      n_goals_prob = NULL) {
  if (is.null(n_goals) & is.null(n_goals_range) & is.null(n_goals_prob)) {
    warning("The number of goals was not specified (via the arguments ",
            "`n_goals`, `n_goals_range`, or `n_goals_prob`).\n",
            "Setting `n_goals` = 3.")
    n_goals <- 3
  }

  subjects %>%
    mutate(
      n_goals = if (!is.null(n_goals)) {
        n_goals
      } else if (!is.null(n_goals_range)) {
        sample(
          seq(n_goals_range[1], n_goals_range[2]),
          size = n(), replace = TRUE
        )
      } else if (!is.null(n_goals_prob)) {
        sample(
          n_goals_prob$n_goals,
          size = n(), replace = TRUE,
          prob = n_goals_prob$prob
        )
      }
    )
  subjects_goals %>% rowwise()%>%
    mutate(goal_num = str_c(1:n_goals, collapse = ",")) %>%
    separate_rows(goal_num, sep = ",")
  # Of purrr?
}

subjects %>%
  sim_goals()

n_goals_prob <- data.frame(n_goals = 3:5, prob = c(2/3, 1/6, 1/6))
# n_goals_prob$n_goals
# n_goals_prob$prob
