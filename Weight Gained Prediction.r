getwd()
setwd("C:\users\sanmi\Desktop\Assesments")
install.packages("readxl")
library(readxl)
wt.gn <- read.csv("calories_consumed.csv")
View(wt.gn)
attach(wt.gn)
summary(wt.gn)
colnames(wt.gn) <- c("WeightGained","CaloriesConsumed")
colnames(wt.gn)
plot(WeightGained,CaloriesConsumed)

# Boxplot Generation #
boxplot(WeightGained, horizontal = TRUE)
boxplot(CaloriesConsumed,horizontal = TRUE)

# Histogram Generation #
hist(Weight.gained..grams.) # Right Skewed ie mean > median so this weight.gained data is not symmentric.#
hist(Calories.Consumed) # Right Skewed ie mean > median so this Calories.Consumed data is not symmentric. #

# Plot qqline,qqnorm - test whether data is Normally distributed normally or not. #
qqnorm(Weight.gained..grams.)
qqline(Weight.gained..grams.)
qqnorm(Calories.Consumed)
qqline(Calories.Consumed)

# To apply Shapiro Test to check weather given Dataset's p-value #
shapiro.test(Weight.gained..grams.) # p-value = 0.006646 < 0.05. #
shapiro.test(Calories.Consumed) # p-value = 0.4887 > 0.05 #
# from above analysis of shapiro Test i.e. p-value. #
# weight.gained..grams. is not Normally distributed #
# Calories.Consumed is Normally Distributed. #

# Now to see Kurtosis and snkewness of Dataset #
# Standardization Application #
scale_wt.gn<- scale(wt.gn)
View(scale_wt.gn)
summary(scale_wt.gn)
scale_wt.gn <- as.data.frame(scale_wt.gn)
View(scale_wt.gn)
shapiro.test(scale_wt.gn$WeightGained)
shapiro.test(scale_wt.gn$CaloriesConsumed)
kurtosis(scale_wt.gn$CaloriesConsumed)
skewness(scale_wt.gn$CaloriesConsumed)

 # Correlation Coefficient for Calories consumed & Weight Gained # 
install.packages("e1071")
library("e1071")
kurtosis(Calories.Consumed)
skewness(Calories.Consumed)
attach(wt.gn)
plot(Calories.Consumed,Weight.gained..grams., col="Blue")
plot(scale_wt.gn$CaloriesConsumed,scale_wt.gn$WeightGained)
cor(CaloriesConsumed,WeightGained)    

# Model Building; Model 1 #
wt.gn_model1 <- lm(CaloriesConsumed~WeightGained)
summary(wt.gn_model1) # Multiple R-squared:  0.8882, i.e. 0.8882 > 0.80
                      # so our model is Build on accurately #
?confint
summary(wt.gn_model1)
confint(wt.gn_model1, level = 0.95)
predict(wt.gn_model1,interval = "predict")

# Applying Logarthmic Transformation for Accurate R-Square Value #
wt.gn_model2 <- lm(CaloriesConsumed~log(WeightGained),data = wt.gn)
View(wt.gn_model2)
confint(wt.gn_model2,level = 0.95) # R-Square is 0.8674 > 0.80
predict(wt.gn_model3,interval = "predict")
summary(wt.gn_model2)

# Exponential Transformation for Accurate R-Square value #
wt.gn_model3 <- lm(log(CaloriesConsumed)~WeightGained,data = wt.gn)
View(wt.gn_model3)
confint(wt.gn_model3,level = 0.95)
predict(wt.gn_model3,interval = "predict")
summary(wt.gn_model3) # R-Square is 0.7917 which is Accurate #
prediction <- exp(wt.gn_model3$fit)
prediction

# Final Prediction #
Final_Prediction <- cbind(CaloriesConsumed=wt.gn$CaloriesConsumed,WeightGained=wt.gn$WeightGained,Predicted_Weight_Gained=prediction)
Final_Prediction
write.csv(Final_Prediction, file = "Weight.Gained.Prediction.csv")
getwd()
