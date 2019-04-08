
#' Cumulative Binomial Probability
#'
#' @param n_success cumulative number of successes to calculate
#' @param prob_success probability of a single success
#' @param sample_size size of the sample (number of trials)
#' @importFrom dplyr %>%
#' @examples
#' cumul_binom_prob(n_success = 10, prob_success = 0.5, sample_size = 20)
#' @export
cumul_binom_prob <- function(n_success, prob_success, sample_size) {
  lower_tails <- c(lower_tail = TRUE,
                   upper_tail = FALSE)

  results <- purrr::map(lower_tails, function(x) {

    pbinom(q = n_success, size = sample_size,  prob = prob_success,
           lower.tail = x)
  }) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(n_success = n_success,
                  prob_success = prob_success,
                  sample_size = sample_size)

  explanation <- c(
    paste("The probability of", n_success, "or fewer successes is", round(results$lower_tail, 4)),
    paste("The probability of more than", n_success, "successes is", round(results$upper_tail, 4))
  )

  list(results = results,
       explanation = explanation)
}



#' Binomial Probability
#'
#' @param n_success number of successes to calculate
#' @param prob_success probability of a single success
#' @param sample_size size of the sample (number of trials)
#' @importFrom dplyr %>%
#' @examples
#' binom_prob(n_success = 10, prob_success = 0.5, sample_size = 20)
#' @export
binom_prob <- function(n_success, prob_success, sample_size) {

  results <- dbinom(x = n_success, size = sample_size,  prob = prob_success) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(prob = value) %>%
    dplyr::mutate(n_success = n_success,
           prob_success = prob_success,
           sample_size = sample_size)

  explanation <- paste("The probability of exactly", n_success, "successes is", round(results$prob, 4))

  list(results = results,
       explanation = explanation)
}

