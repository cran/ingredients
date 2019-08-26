## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  fig.width = 7,
  fig.height = 3.5,
  warning = FALSE,
  message = FALSE
)

## ----message=FALSE, warning=FALSE----------------------------------------
library("DALEX")
library("ingredients")
library("randomForest")
titanic <- na.omit(titanic)

model_titanic_rf <- randomForest(survived == "yes" ~ .,
                                 data = titanic)

explain_titanic_rf <- explain(model_titanic_rf,
                            data = titanic[,-9],
                            y = titanic$survived == "yes",
                            label = "Random Forest")

passanger <- titanic[sample(nrow(titanic), 1) ,-9]
passanger

## ------------------------------------------------------------------------
importance_rf <- feature_importance(explain_titanic_rf)
plot(importance_rf)

## ------------------------------------------------------------------------
describe(importance_rf)

## ------------------------------------------------------------------------
perturbed_variable <- "class"
cp_rf <- ceteris_paribus(explain_titanic_rf,
                         passanger,
                         variables = perturbed_variable)
plot(cp_rf, variable_type = "categorical")

## ------------------------------------------------------------------------
describe(cp_rf)

## ------------------------------------------------------------------------
describe(cp_rf,
         display_numbers = TRUE,
         label = "the probability that the passanger will survive")

## ------------------------------------------------------------------------
describe(cp_rf,
         display_numbers = TRUE,
         label = "the probability that the passanger will survive",
         variables = perturbed_variable)

## ------------------------------------------------------------------------
perturbed_variable_continuous <- "age"
cp_rf <- ceteris_paribus(explain_titanic_rf,
                         passanger)
plot(cp_rf, variables = perturbed_variable_continuous)
describe(cp_rf, variables = perturbed_variable_continuous)

## ------------------------------------------------------------------------
pdp <- aggregate_profiles(cp_rf, type = "partial")
plot(pdp, variables = "fare")
describe(pdp, variables = "fare")

## ------------------------------------------------------------------------
pdp <- aggregate_profiles(cp_rf, type = "partial", variable_type = "categorical")
plot(pdp, variables = perturbed_variable)
describe(pdp, variables = perturbed_variable)

