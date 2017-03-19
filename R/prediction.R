get.preds <- function(model, cycle0, symptom, data) {
  
  # get validation skeleton
  skeleton <- cycle0[rep(seq_len(nrow(cycle0)), cycle0[, expected_cycle_length]), .(user_id, cycle_id)]
  skeleton[, `:=`(symptom = symptom, 
                  day_in_cycle = 1:.N,
                  cycle_id = NULL), 
           by = user_id]
  
  # add validation features:
  validation <- merge(merge(skeleton, data[, .(activity_user)], by = "user_id"), 
                      data[, .(activity_day)], by = "days_from_ovulation")
  validation[, days_from_ovulation := day_in_cycle - (expected_cycle_length - 14)]
  
  setorder(validation, user_id, day_in_cycle)
  
  # assign probabilities
  probs <- predict(model, validation, type = "response")
  skeleton[, probability := probs]
  
  return(skeleton)
}