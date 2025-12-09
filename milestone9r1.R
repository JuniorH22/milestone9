library(readr)
fars <- read_csv("C:/Users/user/Downloads/milestone_scripts_only_latest/fars_clean.csv", show_col_types = FALSE)

fars$persons <- as.numeric(fars$persons)
fars$fatals  <- as.numeric(fars$fatals)
fars$hour    <- as.numeric(fars$hour)

results <- data.frame(
  test = character(),
  group1_mean = numeric(),
  group2_mean = numeric(),
  p_value = numeric(),
  n1 = integer(),
  n2 = integer(),
  stringsAsFactors = FALSE
)

# Test 1: fatals day (6-17) vs night (else) 
day_mask <- fars$hour >= 6 & fars$hour <= 17
fatals_day <- fars$fatals[day_mask & !is.na(fars$fatals) & !is.na(fars$hour)]
fatals_night <- fars$fatals[(!day_mask) & !is.na(fars$fatals) & !is.na(fars$hour)]
if (length(fatals_day) > 1 && length(fatals_night) > 1) {
  t1 <- t.test(fatals_day, fatals_night)
  results <- rbind(results, data.frame(
    test = "Fatals day vs night",
    group1_mean = mean(fatals_day),
    group2_mean = mean(fatals_night),
    p_value = t1$p.value,
    n1 = length(fatals_day),
    n2 = length(fatals_night)
  ))
}

# Test 2: persons clear weather (code 1) vs other 
clear_mask <- fars$weather == 1
persons_clear <- fars$persons[clear_mask & !is.na(fars$persons) & !is.na(fars$weather)]
persons_other <- fars$persons[(!clear_mask) & !is.na(fars$persons) & !is.na(fars$weather)]
if (length(persons_clear) > 1 && length(persons_other) > 1) {
  t2 <- t.test(persons_clear, persons_other)
  results <- rbind(results, data.frame(
    test = "Persons clear vs other",
    group1_mean = mean(persons_clear),
    group2_mean = mean(persons_other),
    p_value = t2$p.value,
    n1 = length(persons_clear),
    n2 = length(persons_other)
  ))
}

print(results)

if (nrow(results) >= 1) print(paste("Test 1 p-value:", signif(results$p_value[1], 4))) else print("Test 1 not run")
if (nrow(results) >= 2) print(paste("Test 2 p-value:", signif(results$p_value[2], 4))) else print("Test 2 not run")

