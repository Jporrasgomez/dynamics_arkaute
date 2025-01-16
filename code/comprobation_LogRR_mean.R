



# control and treatment values
control <- c(2, 4, 5, 3)
warming <- c(1, 8, 100, 7)



# all combinations between them
data <- expand.grid(control, warming)
colnames(data) <- c("control", "warming")
colnames(data1) <- c("control", "warming")


# scenario 2 (comparing to the mean of control)
data$scenario2 <- log(data$warming / mean(control))
scenario2 <- c(unique(data$scenario2)[1],
               unique(data$scenario2)[2],
               unique(data$scenario2)[3],
               unique(data$scenario2)[4])

# scenario 3 (mean of all combinations)
data1$scenario3 <- log(data$warming / data1$control)
scenario3 <- c(mean(data$scenario3[1:4]),
               mean(data$scenario3[5:8]),
               mean(data$scenario3[9:12]),
               mean(data$scenario3[13:16]))

# direct difference between scenarios
round(scenario3 - scenario2, 2)
scenario3
scenario2
