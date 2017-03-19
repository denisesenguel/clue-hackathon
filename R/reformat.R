require(data.table)
require(reshape2)

reformat <- function(cycle, tracking, activity,
                     target_symptoms = c("happy", "pms", "sad", "sensitive_emotion", "energized", "exhausted",
                                         "high_energy", "low_energy", "cramps", "headache", "ovulation_pain",
                                         "tender_breasts", "acne_skin", "good_skin", "oily_skin", "dry_skin")) {
  
  symptoms_long <- tracking[symptom %in% target_symptoms, 
                            .(user_id, cycle_id, day_in_cycle, symptom)]
  symptoms_wide <- dcast(symptoms_long, user_id + cycle_id + day_in_cycle ~ symptom, length)
  
  cycle[, no_of_cycles := .N, by = user_id]
  
  # build data skeleton: every user needs complete cycle record
  skeleton <- cycle[rep(seq_len(nrow(cycle)), cycle[, cycle_length]), .(user_id, cycle_id)]
  skeleton[, day_in_cycle := 1:.N, by = list(user_id, cycle_id)]  
  
  design_matrix <- merge(merge(merge(skeleton, symptoms_wide, 
                                     by = c("user_id", "cycle_id", "day_in_cycle"),
                                     all.x = TRUE), cycle, 
                               by = c("user_id", "cycle_id"),
                               all.x = TRUE), activity,
                         by = c("user_id", "cycle_id", "day_in_cycle"),
                         all.x = TRUE)
  
  design_matrix[, days_from_ovulation := day_in_cycle - (cycle_length - 14)]
  
  design_matrix[is.na(design_matrix)] <- 0
  
  design_matrix[, is_active := as.numeric(date != 0)]
  design_matrix[, date := NULL]
  
  setorder(design_matrix, user_id, cycle_id, day_in_cycle)
  
  return(design_matrix)
}


get.features <- function(data, symptom) {
  
  # ideally this should be some smoothed curve over several days 
  # group by neighboring two as well
  data[, paste0("no_of_", symptom) := sum(get(symptom))/.N, 
       by = list(user_id, days_from_ovulation)]
  
  # get mean activity by days_from_ovulation (overall and by symptom)
  data[, `:=`(activity_day = sum(is_active)/no_of_cycles,
              activity_symptom_day = sum(is_active * get(symptom))), by = days_from_ovulation]
  
  # get mean activity of user 
  data[, activity_user := sum(is_active)/.N, by = user_id]
  data[, symptom := symptom]
}
 # some descriptives?
