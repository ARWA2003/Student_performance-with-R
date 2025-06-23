
students <- read.csv("StudentsPerformance.csv")

#to check Duplicates
any_duplicates <- any(duplicated(students))
if (any_duplicates) {
  print("There are duplicate rows.")
} else {
  print("No duplicate rows.")
}

#function to get mode
getMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#replace the miss value in internet column "Internet"
students$internet[is.na(students$internet)] <- getMode(students$internet)
null_values <- sum(is.na(students))

#replace incosistent data in column "sex"
students$sex[students$sex=="Female"]<-"F"
students$sex[students$sex=="Male"]<-"M"

#########################################################################################

#boxplot(students$G1)      #have no outliers
boxplot(students$G1,
        main = "Box Plot for grade of G1", 
        ylab = "Grades 1")

#boxplot(students$G2)
boxplot(students$G2,
        main = "Box Plot for grade of G2", 
        ylab = "Grades 2")

#boxplot(students$G3)
boxplot(students$G3,
        main = "Box Plot for grade of G3", 
        ylab = "Grades 3")

################################################################################################

#outliers G1
Q1_1 <- quantile(students$G1, 0.25)
Q3_1 <- quantile(students$G1, 0.75)

# Calculate the IQR (Interquartile Range)
IQR_value_1 <- IQR(students$G1)

# Define the lower and upper bounds to identify potential outliers
lower_bound_1 <- Q1_1 - 1.5 * IQR_value_1
upper_bound_1 <- Q3_1 + 1.5 * IQR_value_1

# Identify and remove outliers
outliers_1 <- students$G1 < lower_bound_1 | students$G1 > upper_bound_1
sum_outliers_1 <- sum(outliers_1)

#outliers G2
Q1_2 <- quantile(students$G2, 0.25)
Q3_2 <- quantile(students$G2, 0.75)

# Calculate the IQR (Interquartile Range)
IQR_value_2 <- IQR(students$G2)

# Define the lower and upper bounds to identify potential outliers
lower_bound_2 <- Q1_2 - 1.5 * IQR_value_2
upper_bound_2 <- Q3_2 + 1.5 * IQR_value_2

# Identify and remove outliers
outliers_2 <- students$G2 < lower_bound_2 | students$G2 > upper_bound_2
sum_outliers_2 <- sum(outliers_2)
students <- students[!outliers_2, ]


#outliers G3
Q1_3 <- quantile(students$G3, 0.33)
Q3_3 <- quantile(students$G3, 0.83)

# Calculate the IQR (Interquartile Range)
IQR_value_3 <- IQR(students$G3)

# Define the lower and upper bounds to identify potential outliers
lower_bound_3 <- Q1_3 - 1.5 * IQR_value_3
upper_bound_3 <- Q3_3 + 1.5 * IQR_value_3

# Identify and remove outliers
outliers_3 <- students$G3 < lower_bound_3 | students$G3 > upper_bound_3
sum_outliers_3 <- sum(outliers_3)
students <- students[!outliers_3, ]


#re boxplot(students$G2)
boxplot(students$G2,
        main = "Box Plot for grade of G2 without outliers", 
        ylab = "Grades 2")

#re boxplot(students$G3)
boxplot(students$G3,
        main = "Box Plot for grade of G3 without outliers", 
        ylab = "Grades 3")



###########################################################################################
#scatter plot
plot(students$studytime,students$G1,
     xlab = "Study Time", ylab = "G1",
     main = "Scatter Plot of G1")

plot(students$studytime,students$G2,
     xlab = "Study Time", ylab = "G2",
     main = "Scatter Plot of G2")

plot(students$studytime,students$G3,
     ylab = "Study Time" ,xlab = "G3" ,
     main = "Scatter Plot of G3")

hist(students$failures,col="blue",xlab = "Fail" ,main = "Frequent")

plot(students$studytime,students$failures,main = "analyse failures and study time ",xlab = "study time", ylab = "failures")
students$avr_grades <- rowMeans(students[, c("G1", "G2", "G3")], na.rm = TRUE)

##################################################################################################
interval_width <- 17  # or any other threshold you desire
students$age <- cut(students$age, breaks = c(-Inf, 17, Inf), labels = c("17 or below", "Greater than 17"), include.lowest = TRUE)

interval_width <- 17  # or any other threshold you desire
students$age<- cut(students$age, breaks = c(-Inf, interval_width, Inf), labels = c("17 and below", "Above 17"), include.lowest = TRUE)

# Print the result
print(students)

#install.packages("dbscan")
#library("dbscan")
#cluster <-students[, c("studytime","failures","absences","G1",    "G2", "G3")]
#kmeans_result <- kmeans(cluster, centers =7)
#print(kmeans_result)
#dbscan_result <- dbscan(cluster,eps = 3, MinPts =  4)
#print(dbscan_result)
target <- students$failures
print(target)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
index <- sample(1:nrow(students), 0.8 * nrow(students))
train_data <-students[index, ]
test_data <- students[-index, ]
fit<-rpart(target ~students$sex+students$age+students$Fjob+students$Mjob+students$internet+students$romantic+students$studytime+students$school+students$health+students$absences+students$avr_grades,data=train_data,method = "class",parms = list(split= "information"),control = rpart.control(minsplit=20))
summary(fit)
predictions <- predict(fit,  test_data, type = "class")

conf_matrix <- table(target, predictions)
print(conf_matrix)

acc <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", acc))


rpart.plot(fit)



