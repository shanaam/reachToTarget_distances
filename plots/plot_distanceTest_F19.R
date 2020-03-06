library(tidyverse)
library(ggbeeswarm)

vector_confint <- function(vector, interval = 0.95) {
  # Standard deviation of sample
  vec_sd <- sd(vector, na.rm = TRUE)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector, na.rm = TRUE)
  # Error according to t distribution
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  # result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(error)
}


allDataPath <- "data/complete/all_reaches.csv"

allReaches <- read.csv(allDataPath)

means <- allReaches[ , 6:13] %>%
  apply( 1, mean, na.rm = TRUE)

ci_95 <- allReaches[ , 6:13] %>%
  apply( 1, vector_confint)

allReaches <- cbind(allReaches, means, ci_95)

# flip the data
allReaches$means <- allReaches$means * -1

allReaches$trial_num <- 1:332

#plot
lc <- ggplot(allReaches, aes(x = trial_num, y = means)) +
  theme_minimal() +
  geom_line(aes(y = cursor_rotation), stat = "identity", colour = "black", size = 2, alpha = 0.7) +
  geom_point(stat = "identity", colour = "#e51636", fill = "#e51636", size = 4) +
  geom_smooth(aes(ymin = means - ci_95, ymax = means + ci_95), 
              stat = "identity", colour = "none", fill = "#e51636", size = 1) +
  scale_x_continuous(name = "trial") +
  scale_y_continuous(limits = c(-30, 60), breaks = c(-30, -15, 0, 15, 30, 45), name = "hand deviation (°)") +
  theme(text = element_text(size=40), axis.text = element_text(size=40), legend.text = element_text(size=48), panel.grid.major.y = element_line(colour = "#ABABAB"))

lc
ggsave(lc, height = 14, width = 24, device = "svg", filename = "plots/testingReaches.svg")
