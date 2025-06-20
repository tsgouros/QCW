#Create a data frame of future read dates

june.reads <- data.frame(
  juneDate= as.Date(c("2025-06-04", "2025-06-11", "2025-06-18", "2025-06-18", "2025-06-18", "2025-06-25")),
  cycle= c(3,4,1,5,6,2)
  )

# Set cycle as an integer

june.reads$cycle <- as.integer(june.reads$cycle)

# Add future date to water.means data frame

merge.water.means <- merge(water.means,june.reads, by = "cycle")

# Project future use

projections <- merge.water.means %>%
  mutate(ndate=(convertDate(juneDate) * (pi/6))) %>%
  mutate(juneUsage = amp * (cos(ndate + tst) + 1) + off + (slp * ndate));
  
system("date")

                               