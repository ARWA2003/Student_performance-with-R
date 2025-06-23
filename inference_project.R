
#أروى حسام صابر


library(readr)
students <- read_csv("StudentsPerformance.csv")

install.packages("rpart")
install.packages("rpart.plot")
install.packages("dbscan")

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

#replace inconsistent data in column "sex"
students$sex[students$sex=="Female"]<-"F"
students$sex[students$sex=="Male"]<-"M"

#########################################################################################

#boxplot(students$G1)      #have NO Outliers
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

#Identify Q1,Q2 
Q1_2 <- quantile(students$G2, 0.25)
Q3_2 <- quantile(students$G2, 0.75)

#Calculate the IQR (Interquartile Range)
IQR_value_2 <- IQR(students$G2)

# Define the lower and upper bounds
lower_bound_2 <- Q1_2 - 1.5 * IQR_value_2
upper_bound_2 <- Q3_2 + 1.5 * IQR_value_2

# Replace higher outliers with upper bound and lower outliers with lower bound
students$G2[students$G2 < lower_bound_2] <- lower_bound_2
students$G2[students$G2 > upper_bound_2] <- upper_bound_2


#Identify Q1,Q2 
Q1_3 <- quantile(students$G3, 0.33)
Q3_3 <- quantile(students$G3, 0.83)

# Calculate the IQR (Interquartile Range) 
IQR_value_3 <- IQR(students$G3)

# Define the lower and upper bounds 
lower_bound_3 <- Q1_3 - 1.5 * IQR_value_3
upper_bound_3 <- Q3_3 + 1.5 * IQR_value_3

# Replace higher outliers with upper bound and lower outliers with lower bound
students$G3[students$G3 < lower_bound_3] <- lower_bound_3
students$G3[students$G3 > upper_bound_3] <- upper_bound_3



#re boxplot(students$G2)
boxplot(students$G2,
        main = "Box Plot for grade of G2 without outliers", 
        ylab = "Grades 2")

#re boxplot(students$G3)
boxplot(students$G3,
        main = "Box Plot for grade of G3 without outliers", 
        ylab = "Grades 3")


summary(students)
######################################################################################################

students$avr_grades <- rowMeans(students[, c("G1", "G2", "G3")], na.rm = TRUE)
students$sum <- rowSums(students[, c("G1", "G2", "G3")])
students$GPA <- students$sum
students$GPA<- cut(students$GPA, breaks = c(-Inf, 30, 40, 50, 60, Inf), labels =  c("F", "D", "C", "B", "A" ), include.lowest = TRUE)

hist(students$absences, 
     main = "Histogram of Absences", 
     xlab = "Number of Absences",
     ylab = "Frequency",
     col = "skyblue",
     border = "black",
     breaks = 20)

plot(students$failures, students$avr_grades,
     main = "Scatterplot of Failures and Average Grade",
     xlab = "Number of Failures",
     ylab = "Average Grade",
     col = "skyblue",
     pch = 16)

##################################################################################################
#clustring

# the data set doesn't need normalization.
#normalized_data <- scale(students$age)
#print(normalized_data)

cluster <-students[, c("studytime","failures","absences","avr_grades")]

#To determine the center of k-means
wss=numeric(16)   
for(i in 1:16)
{
  wss[i]=sum(kmeans(cluster,i)$withinss)
}
plot(1:16,wss,type='b',xlab="number of cluster",ylab="within grp sum square")
#K-means
kmeans_result <- kmeans(cluster, centers = 5)
print(kmeans_result)

#DBSCAN
library("dbscan")
dbscan_result <- dbscan(cluster,eps =2, minPts =  3)
print(dbscan_result)

print(students$avr_grades)
#######################################################################################################
#Discretization

interval_width <- 17  # or any other threshold you desire
students$age <- cut(students$age, breaks = c(-Inf,interval_width , Inf), labels = c("17 or below", "Greater than 17"), include.lowest = TRUE)
x <- 3 # or any other threshold you desire
students$goout<- cut(students$goout, breaks = c(-Inf, x, Inf), labels = c("3 or below", "Greater than 3"), include.lowest = TRUE)
y<- 2 # or any other threshold you desire
students$studytime<- cut(students$studytime, breaks = c(-Inf, y, Inf), labels = c("2 or below", "Greater than 2"), include.lowest = TRUE)
students$health<- cut(students$health, breaks = c(-Inf, x, Inf), labels =c("3 or below", "Greater than 3"), include.lowest = TRUE)
z <- 15 # or any other threshold you desire
students$absences<- cut(students$absences, breaks = c(-Inf, z, Inf), labels = c("15 or below", "Greater than 15"), include.lowest = TRUE)
students$avr_grades <- cut(
  students$avr_grades,
  breaks = c(4.333, 10, 15, 20),
  labels = c("[4.333: 10]", "[10 : 15]", "[15 : 20]"),
  include.lowest = TRUE
)
print(students)


#Classification
#Tree

library(rpart)
library(rpart.plot)

target <- students$GPA
print(target)

index <- sample(1:nrow(students), 0.6 * nrow(students))
train_data <-students[index, ]
test_data <- students[-index, ]

fit<-rpart(target ~students$sex+students$age+students$internet+students$romantic+students$studytime+students$school+students$health+students$absences+students$avr_grades,data=train_data,method = "class",parms = list(split= "information"),control = rpart.control(minsplit=3))
predictions <- predict(fit,  test_data, type = "class")

conf_matrix <- table(target, predictions)
print(conf_matrix)

acc <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", acc))


rpart.plot(fit)
###################################################################