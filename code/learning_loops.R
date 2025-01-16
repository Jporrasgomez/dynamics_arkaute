





#### APRENDIENDO A HACER LOOPS with Chat GPT ######

# Example 1: Sum of numbers from 1 to 10 using a for loop
sum <- 0
for (i in 1:10) {
  sum <- sum + i
}
print(sum)


# Example: Multiply each element in a vector by 2 using a for loop
numbers <- c(1, 2, 3, 4, 5)
for (i in 1:length(numbers)) {
  numbers[i] <- numbers[i] * 2
}
print(numbers)


#Now, could you write a "for" loop that calculates the square of each element in a given vector? 

myvector <- seq(1, 20, 0.5)
for (i in 1:length(myvector)) {
  myvector[i] <- sqrt(myvector[i])
}
print(myvector)

#Create a data.frame
working_data <- data.frame(
 person = c("Ana", "Clau", "Javi", "Natalie"), 
 monday = c(1, 2, 2, 6),
 tuesday = c(0.2, 1, 4, 0),
 wednesday = c(2, 4, 1, 8),
 thursday = c(4, 2, 1, 4),
 friday = c(1, 1, 1, 1)
)

# Write a loop to calculate the average number of hours worked by each person over 
# the week using the working_data data frame you've created. Then, add a new column named average_hours
# to the working_data data frame to store the calculated averages.

# Without a loop
library(tidyr)
working_data_avg <- pivot_longer(working_data, cols = c("monday", "tuesday", "wednesday", "thursday", "friday"), names_to = "day", values_to = "work_h")
library(dplyr)
working_data_avg <- summarise(group_by(working_data_avg, person),
                              avrg_wh = mean(work_h))
working_data <- merge(working_data, working_data_avg)
print(working_data)

#With a loop ()
working_data$average <- c(NA)
for (i in 1:nrow(working_data)){
  working_data$average[i] <- mean(as.numeric(working_data[i, 2:6]))
  
}

print(working_data)

# Write a loop to determine the person(s) who worked the most hours during the week using
# the working_data data frame. Then, print out the name(s) of the person(s) with the
# maximum number of hours worked.

working_data$total_h <- NA
for (i in 1:nrow(working_data)){
  working_data$total_h[i] <- sum(working_data[i, 2:6])
}

# Find the row(s) where total hours worked equals the maximum
max_hours_indices <- which(working_data$total_h == max(working_data$total_h))

# Print out the name(s) of the person(s) with the maximum total hours worked
cat("Person(s) who worked the most hours: ", working_data$person[max_hours_indices], "\n")

# Other
# Create a data frame representing student scores
scores_data <- data.frame(
  Student = c("Alice", "Bob", "Charlie", "David"),
  Math = c(85, 90, 88, 92),
  Science = c(75, 80, 85, 90),
  History = c(70, 75, 80, 85)
)
 
# Calculate the average score for each student and add it as
# a new column named "Average" to the scores_data data frame.

scores_data$avg <- NA
for (i in 1:nrow(scores_data)){
  scores_data$avg[i] <- mean(as.numeric(scores_data[i, 2:4]))
}
print(scores_data) 


#### para probar un loop, dar valores a i o a i y a j. Por ejemplo: i = 1 j = 1
 