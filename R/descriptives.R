require(data.table)
require(ggplot2)

# user level descriptives
users <- fread("data/users.csv")
users[, bmi := weight / (height/100)^2]
# too many NAs

# cycle level descriptives
cycle <- fread("data/cycles.csv")

# no of cycles per user
cycle[, no_of_cycles := .N, by = user_id]
min(cycle[, no_of_cycles])

# cycle length 

# mean overall and by user
mean(cycle[, cycle_length])
var(cycle[, cycle_length])
mean_cycle_lengths <- unique(cycle[, `:=`(mean = mean(cycle_length),
                                          var = var(cycle_length)), by = user_id][, .(user_id, mean, var)])

# menstruation length overall and by user
mean(cycle[, period_length])
var(cycle[, period_length])
mean_period_lengths <- unique(cycle[, `:=`(mean = mean(period_length),
                                           var = var(period_length)), by = user_id][, .(user_id, mean, var)])

# correlation between period length and cycle length?? between-users and within users?
# maybe there's a correlation between period length in previous cycle and period length in current cycle?
# this in turn could correlate with symptoms?

# based on the history of a user (all cycles aligned around estimated or observed ovulation)

tracking <- fread("data/tracking.csv")

# how often do users track ovulation pain?
length(unique(tracking[symptom == 'ovulation_pain', user_id]))
nrow(tracking[symptom == 'ovulation_pain', ])

# which symptoms are tracked how often?
tracked_symptoms <- unique(tracking[, count_symptom := .N/nrow(tracking), by = list(category, symptom)][, .(category, symptom, count_symptom)])
setorder(tracked_symptoms, category, symptom)



# aligning user cycles around ovulation - steps:
# ovulation: if user did a test and it came out positive -> is_ovulation = TRUE
# ovulation likely: ovulation pain -> is_ovulation = TRUE
# for every remaining cycle of users that have at least one of the above indications
# -> take the length of the luteal phase of that cycle and calculate 
