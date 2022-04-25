# Simulation of a plan for the usable dataset

# NOTE: OUR DATASET IS ONLY FEMALES AND KIDS

library(ggplot2)

mat <- matrix(runif(12, 0, 100), nrow=4, byrow=TRUE)

sim_df <- data.frame(mat)

colnames(sim_df) <- c("states", "illiterates", "attending_school") # we only simulate a few columns, wlog
sim_df$states <- c("Delhi", "Haryana", "Punjab", "Goa") # we only choose a few states and omit the total (India)

ggplot(sim_df, aes(states, illiterates)) +
  geom_point()

# so from the simulated data, it looks like 86% of women and children in Haryana are illiterate, such as my partner 
# and 45% of them attend school