library(data.table)

df <- read.table("./data/tracking.csv", sep=",", header=TRUE)

symptoms <- c("happy", "pms", "sad", "sensitive_emotion", "energized", "exhausted",
              "high_energy", "low_energy", "cramps", "headache", "ovulation_pain",
              "tender_breasts", "acne_skin", "good_skin", "oily_skin", "dry_skin")

df <- subset(df, symptom %in% symptoms)

df <- df[1:100,]

selected <- df[, c("user_id","cycle_id")]
user_num_cycles <- aggregate(cycle_id ~ user_id, selected, max)

user_symptoms_total <- aggregate(cycle_id ~ user_id+symptom+day_in_cycle, df, length)

process <- function(row) {
    user_id <- row["user_id"]
    total <- user_num_cycles[user_num_cycles == user_id,]$cycle_id
    as.numeric(row["cycle_id"]) / total
}

user_symptoms_total$probability <- apply(user_symptoms_total, 1, process)

drops <- c("cycle_id")
user_symptoms_total <- user_symptoms_total[ , !(names(user_symptoms_total) %in% drops)]

write.table(user_symptoms_total, file = "result.txt", sep = ",", quote = FALSE, row.names = FALSE)
