source("R/reformat.R")
source("R/estimation.R")
source("R/prediction.R")

require(parallel)

data_cycle <- fread("data/cycles.csv")
data_tracking <- fread("data/tracking.csv")
data_activity <- fread("data/active_days.csv")
data_cycle0 <- fread("data/cycles0.csv")

target_symptoms = c("happy", "pms", "sad", "sensitive_emotion", "energized", "exhausted",
                    "high_energy", "low_energy", "cramps", "headache", "ovulation_pain",
                    "tender_breasts", "acne_skin", "good_skin", "oily_skin", "dry_skin")
target_symptoms <- c("cramps")

data_all <- reformat(cycle = data_cycle,
                     tracking = data_tracking,
                     activity = data_activity, 
                     target_symptoms = target_symptoms)

list.data_design <- lapply(target_symptoms, function(s) get.features(data_all, symptom = s))

noOfCores <- detectCores()

list.model <- mclapply(list.data_design, function(d) estimate.glmm(design = d, 
                                                                   symptom = unique(d[, symptom])),
                       mc.cores = noOfCores)

list.preds <- lapply(list.model, function(m) get.predictions(m))

pred_probs <- do.call("rbind", list.preds)

write.table(pred_probs, file = "result.txt", 
            sep = ",", quote = FALSE, row.names = FALSE)