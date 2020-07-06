getwd()

dl.tm <- read.csv("delivery_time.csv")
View(dl.tm)
dt <- (dl.tm$Delivery.Time)
st <- (dl.tm$Sorting.Time)

# Boxplot Generation #
boxplot(dt,horizontal = TRUE)
boxplot(st,horizontal = TRUE)
boxplot(dl.tm)

# Histogram Generation #
attach(dl.tm)
hist(dt)
hist(st)
summary(dl.tm)

# scatter plot Genration #
plot(dt,st)
qqnorm(dt)
qqline(dt)
qqnorm(st)
qqline(st)

# To apply Shapiro Test to check given Dataset's p-value #
shapiro.test(dt) # P-value = 0.8963>0.05, so it is Normally Distributed.#
shapiro.test(st) # p-Value = 0.1881>0.05, so it is normally distributed. #

# Now to Generate Kurtosis and Skewness #
# Standardisation Applications #
install.packages("e1071")
library("e1071")
kurtosis(dt)
skewness(dt)
kurtosis(st)
skewness(st)

# Correlation Coefficient for Delivery Time & Sorting Time #
cor(dt,st) # r=0.8259973 < 0.85, so correlation is moderate.

# Building of Model1 #
Model1 <- lm(dt~st)
View(Model1)
summary(Model1) # R-square= 0.6655, p-value= 3.983e-06
confint(Model1,level = 0.95)
predict(Model1,interval="predict")

# Applying Logarthmic Transformation for Accurate R-Square Value #
model2 <- lm(dt~log(st),data = dl.tm)
View(model2)
summary(model2) # R-square=0.6794, p-value= 2.642e-06 #
confint(model2,level = 0.95)
predict(model2,interval = "predict")

# Exponential Transformation for Accurate R-Square value #
model3 <- lm(log(dt)~st)
View(model3)
summary(model3) # R-square=0.6957, p-value= 1.593e-06 #
confint(model3,level = 0.95)
predict(model3,interval = "predict")
Delivery_time_prediction <- exp(model3$fit)
Delivery_time_prediction

# Final Prediction #
Final_Prediction <- cbind(Sorting.Time=dl.tm$Sorting.Time,Delivery.Time=dl.tm$Delivery.Time,predicted_Value=Delivery_time_prediction)
Final_Prediction

write.csv(Final_Prediction,file = "Delivery_Time_Prediction.csv")
getwd() 
