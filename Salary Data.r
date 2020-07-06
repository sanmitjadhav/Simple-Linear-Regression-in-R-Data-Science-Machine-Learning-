getwd()

dl.tm <- read.csv("Salary_Data.csv")
View(Salary_Data)
Experience_Years <- (Salary_Data$YearsExperience)
Salary <- (Salary_Data$Salary)

# Boxplot Generation #
boxplot(Experience_Years, horizontal = TRUE)
boxplot(Salary,horizontal = TRUE)
boxplot(Experience_Years,Salary)

# Histogram Generation #
attach(Salary_Data)
hist(Experience_Years)
hist(Salary)
summary(Salary_Data)

# scatter plot Genration #
plot(Experience_Years,Salary)
qqnorm(Experience_Years)
qqline(Experience_Years)
qqnorm(Salary)
qqline(Salary)

# To apply Shapiro Test to check given Dataset's p-value #
shapiro.test(Experience_Years) # P-value = 0.1034 > 0.05, so it is Normally Distributed.#
shapiro.test(Salary) # p-Value = 0.01516 < 0.05, so it is NOT normally distributed. #

# Now to Generate Kurtosis and Skewness #
# Standardisation Applications #
install.packages("e1071")
library("e1071")
kurtosis(Experience_Years)
skewness(Salary)
kurtosis(Experience_Years)
skewness(Experience_Years)

# Correlation Coefficient for Delivery Time & Sorting Time #
cor(Experience_Years,Salary) # r=0.9782416 #

# Building of Model1 #
Model1 <- lm(Experience_Years~Salary)
View(Model1)
summary(Model1) # R-square= 0.9554 , p-value= 2.2e-16 #
confint(Model1,level = 0.95) # R-Square = 0.9554 #
predict(Model1,interval="predict")

# Applying Logarthmic Transformation for Accurate R-Square Value #
model2 <- lm(Experience_Years~log(Salary),data = Salary_Data)
View(model2)
summary(model2) # R-square=0.9295, p-value= 2.2e-06 #
confint(model2,level = 0.95)
predict(model2,interval = "predict")

# Exponential Transformation for Accurate R-Square value #
model3 <- lm(log(Experience_Years)~Salary)
View(model3)
summary(model3) # R-square=0.8487, p-value= 3.25e-13 #
confint(model3,level = 0.95)
predict(model3,interval = "predict")
Salary_Hike_Prediction <- exp(model3$fit)
View(Salary_Hike_Prediction)
Salary_Hike_Prediction

# Final Prediction #
Salary_Hike <- cbind(Experience_Years=Salary_Data$Experience_Years,Salary=Salary_Data$Salary,predicted_Value=Salary_Hike_Prediction)
Salary_Hike_Prediction

write.csv(Salary_Hike_Prediction,file = "Salary_Hike_Prediction.csv")
getwd() 
