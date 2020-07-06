getwd()

dl.tm <- read.csv("emp_data.csv")
View(em.dt)
Salary_hike <- (em.dt$Salary_hike)
Churn_out_rate <- (em.dt$Churn_out_rate)

# Boxplot Generation #
boxplot(Salary_hike,horizontal = TRUE)
boxplot(Churn_out_rate,horizontal = TRUE)
boxplot(Salary_hike,Churn_out_rate)

# Histogram Generation #
attach(em.dt)
hist(Salary_hike)
hist(Churn_out_rate)
summary(em.dt)

# scatter plot Genration #
plot(Salary_hike,Churn_out_rate)
qqnorm(Salary_hike)
qqline(Salary_hike)
qqnorm(Churn_out_rate)
qqline(Churn_out_rate)

# To apply Shapiro Test to check given Dataset's p-value #
shapiro.test(Salary_hike) # P-value = 0.5018 > 0.05, so it is Normally Distributed.#
shapiro.test(Churn_out_rate) # p-Value = 0.7348 > 0.05, so it is also normally distributed. #

# Now to Generate Kurtosis and Skewness #
# Standardisation Applications #
install.packages("e1071")
library("e1071")
kurtosis(Salary_hike)
skewness(Salary_hike)
kurtosis(Churn_out_rate)
skewness(Churn_out_rate)

# Correlation Coefficient for Salary Hike & Churn Out Rate #
cor(Salary_hike,Churn_out_rate) # r= -0.9117216 < 0.85, so correlation is moderate.

# Building of Model1 #
Model1 <- lm(Salary_hike~Churn_out_rate)
View(Model1)
summary(Model1) # R-square= 0.8101, p-value= 0.0002386 #
confint(Model1,level = 0.95)
predict(Model1,interval="predict")

# Applying Logarthmic Transformation for Accurate R-Square Value #
model2 <- lm(Salary_hike~log(Churn_out_rate),data = em.dt)
View(model2)
summary(model2) # R-square=0.8577 , p-value= 7.377e-05 #
confint(model2,level = 0.95)
predict(model2,interval = "predict")

# Exponential Transformation for Accurate R-Square value #
model3 <- lm(log(Salary_hike)~Churn_out_rate)
View(model3)
summary(model3) # R-square=0.8297, p-value= 0.0001532 #
confint(model3,level = 0.95)
predict(model3,interval = "predict")
Churn_out_rate_prediction <- exp(model3$fit)
View(Churn_out_rate_prediction)

# Final Prediction #
Final_Churn_out_Rate_Prediction <- cbind(Salary_hike=em.dt$Salary_hike,Churn_out_rate=em.dt$Churn_out_rate,predicted_Churn_Out_Rate=Churn_out_rate_prediction)
Final_Churn_out_Rate_Prediction

write.csv(Final_Churn_out_Rate_Prediction,file = "Churn_Out_Rate_Prediction.csv")
getwd() 
