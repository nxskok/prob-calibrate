# functions for calibrate

loglik <- function(obs, p) {
  # obs is vector in {1, 2, 3} of results observed / simulate
  # p is df with columns x1, x2, x3 of result probs
  p %>% mutate(obs = obs) %>%
    mutate(opp = case_when(obs == 1 ~ x1,
                           obs == 2 ~ x2,
                           obs == 3 ~ x3,
                           TRUE     ~ 1)) %>%
    summarize(loglik = -sum(log(opp))) %>% pull(loglik)
}

random_sample <- function(p) {
  p %>%
    mutate(u = runif(nrow(.))) %>%
    mutate(x = case_when(u < x1      ~ 1,
                         u < x1 + x2 ~ 2,
                         TRUE        ~ 3)) %>%
    pull(x)
}

sim_dist <- function(p, n_sim = 1000) {
  tibble(sim = 1:n_sim) %>%
    rowwise() %>%
    mutate(my_sample = list(random_sample(p))) %>%
    mutate(ll = loglik(my_sample, p)) %>%
    # unnest_wider(my_sample, names_sep = "_") %>%
    pull(ll)
}

count_sim <- function(oll, v) {
  # oll is observed loglik
  # v is simulated logliks
  tibble(v) %>%
    mutate(ge = (v >= oll), le = (v <= oll)) %>%
    summarize(count_greater = sum(ge), count_lesser = sum(le))
}

run_sim <- function(p, obs, n_sim = 1000) {
  # p is fitted probs from model
  # obs is observed results
  oll <- loglik(obs, p)
  v <- sim_dist(p, n_sim)
  count_sim(oll, v)
}
