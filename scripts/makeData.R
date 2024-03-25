library(tidyverse)
library(MASS)
set.seed(123)
data <- tribble(~x,~y,
        "A",as_tibble(mvrnorm(n = 100, mu = c(1,4), Sigma = matrix(c(1,0.9,
                                                           0.9,1), nrow = 2))),
        "B",as_tibble(mvrnorm(n = 100, mu = c(2,4), Sigma = matrix(c(1,0.9,
                                                           0.9,1), nrow = 2))),
        "C",as_tibble(mvrnorm(n = 100, mu = c(2,4), Sigma = matrix(c(4,0.9,
                                                                     0.9,4), nrow = 2)))) %>%
  unnest(y)

write_csv(data, "data/example_data.csv")

data %>%
  dplyr::select(x, V1) %>%
  pivot_wider(names_from = x, values_from = V1) %>%
  unnest(cols = c(A, B, C)) %>%
  write_csv(., "data/example_2.csv")


