# Simulation of a plan for the usable dataset

library(ggplot2)

mat <- matrix(runif(24, 0, 100), nrow=6, byrow=TRUE)

sim_df <- data.frame(mat)

colnames(sim_df) <- c("state", "att_sch", "decision_health", "infant_mortality_rate") 
sim_df$state <- c("Delhi", "Haryana", "Himachal Pradesh", "Jammu & Kashmir", 
                  "Punjab", "Rajasthan") 

ggplot(sim_df, aes(state, infant_mortality_rate)) +
  geom_point()

# In the simulation, we observe that Delhi has the highest infant mortality rate followed by 
# Punjab and Himachal Pradesh.