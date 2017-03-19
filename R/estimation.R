require(lme4)

estimate.glmm <- function(design, symptom) {
  
  setorder(design, user_id, day_in_cycle)
  results <- glmer(formula = get(symptom) ~ days_from_ovulation + activity_day + activity_user | user_id,
                   data = design, family = binomial)
  
  return(results)
}



