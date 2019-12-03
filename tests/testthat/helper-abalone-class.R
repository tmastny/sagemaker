library(recipes)
library(dplyr)

abalone_class <- function() {
  rec <- recipe(rings ~ ., data = sagemaker::abalone) %>%
    step_filter(rings %in% top_rings$rings) %>%
    step_integer(all_outcomes(), zero_based = TRUE) %>%
    prep(training = sagemaker::abalone, retain = TRUE)

  juice(rec) %>%
    select(rings, everything())
}
