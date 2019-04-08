
trials <- 100

p <- 0.05
q <- 0.95

dbinom(0, size=100, prob=0.00125)

pbinom(1, size=100, prob=0.00125, lower.tail = F)


cumul_binom_prob <- function(n_success, prob_success, sample_size) {
  lower_tails <- c(lower_tail = TRUE,
                   upper_tail = FALSE)

  results <- purrr::map(lower_tails, function(x) {
    pbinom(q = n_success, size = sample_size,  prob = prob_success,
           lower.tail = x)
  }) %>%
    tibble::as_tibble() %>%
    mutate(n_success = n_success,
           prob_success = prob_success,
           sample_size = sample_size)

  explanation <- c(
    paste("The probability of", n_success, "or fewer successes is", round(results$lower_tail, 4)),
    paste("The probability of more than", n_success, "successes is", round(results$upper_tail, 4))
  )

  list(results = results,
       explanation = explanation)
}


binom_prob <- function(n_success, prob_success, sample_size) {

  results <- dbinom(x = n_success, size = sample_size,  prob = prob_success) %>%
    tibble::as_tibble() %>%
    rename(prob = value) %>%
    mutate(n_success = n_success,
           prob_success = prob_success,
           sample_size = sample_size)

  explanation <- paste("The probability of exactly", n_success, "successes is", round(results$prob, 4))

  list(results = results,
       explanation = explanation)
}

