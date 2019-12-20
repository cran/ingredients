## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
library("DALEX")
head(titanic)

## -----------------------------------------------------------------------------
# prepare model
library("randomForest")
titanic <- na.omit(titanic)
model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
                                   fare + sibsp + parch,  data = titanic)
model_titanic_rf

## -----------------------------------------------------------------------------
library("DALEX")
explain_titanic_rf <- explain(model_titanic_rf,
                      data = titanic[,-9],
                      y = titanic$survived == "yes",
                      label = "Random Forest v7")

## -----------------------------------------------------------------------------
library("ingredients")

fi_rf <- feature_importance(explain_titanic_rf)
head(fi_rf)
plot(fi_rf)

## -----------------------------------------------------------------------------
pp_age  <- partial_dependency(explain_titanic_rf, variables =  c("age", "fare"))
head(pp_age)
plot(pp_age)

## -----------------------------------------------------------------------------
cp_age  <- conditional_dependency(explain_titanic_rf, variables =  c("age", "fare"))
plot(cp_age)

## -----------------------------------------------------------------------------
ap_age  <- accumulated_dependency(explain_titanic_rf, variables =  c("age", "fare"))
plot(ap_age)

## -----------------------------------------------------------------------------
new_passanger <- data.frame(
  class = factor("1st", levels = c("1st", "2nd", "3rd", "deck crew", "engineering crew", "restaurant staff", "victualling crew")),
  gender = factor("male", levels = c("female", "male")),
  age = 8,
  sibsp = 0,
  parch = 0,
  fare = 72,
  embarked = factor("Southampton", levels = c("Belfast", "Cherbourg", "Queenstown", "Southampton"))
)

sp_rf <- ceteris_paribus(explain_titanic_rf, new_passanger)
plot(sp_rf) +
  show_observations(sp_rf)

## -----------------------------------------------------------------------------
plot(sp_rf,
     variables = c("class", "embarked", "gender", "sibsp"),
     variable_type = "categorical")

## -----------------------------------------------------------------------------
passangers <- select_sample(titanic, n = 100)

sp_rf <- ceteris_paribus(explain_titanic_rf, passangers)
clust_rf <- cluster_profiles(sp_rf, k = 3)
head(clust_rf)
plot(sp_rf, alpha = 0.1) +
  show_aggregated_profiles(clust_rf, color = "_label_", size = 2)

## -----------------------------------------------------------------------------
sessionInfo()

