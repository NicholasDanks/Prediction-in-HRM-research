library(caret)
library(MASS)
data <- read.csv(file = "2015 ISSP data.csv")

# Reproducing Drabe et al, USA
usa_data <- na.omit(data[data$country ==36, ])
jp_data <- na.omit(data[data$country ==19, ])
de_data <- na.omit(data[data$country ==14, ])

# Total data
all_data <- rbind(usa_data, jp_data, de_data)

 # Estimate USA model
lm_usa <- lm(scale(job_sat) ~ factor(sex) + factor(Age_cat) + scale(educyrs) + scale(income) + scale(advancement) + scale(security) + scale(interesting) + scale(independent) + scale(rel_mgmt) + scale(rel_clgs), data = usa_data, )
summary(lm_usa)

# Estimate Japan model
lm_jp <- lm(scale(job_sat) ~ factor(sex) + factor(Age_cat) + scale(educyrs) + scale(income) + scale(advancement) + scale(security) + scale(interesting) + scale(independent) + scale(rel_mgmt) + scale(rel_clgs), data = jp_data)
summary(lm_jp)

# Estimate German model
lm_de <- lm(scale(job_sat) ~ factor(sex) + factor(Age_cat) + scale(educyrs) + scale(income) + scale(advancement) + scale(security) + scale(interesting) + scale(independent) + scale(rel_mgmt) + scale(rel_clgs), data = de_data)
summary(lm_de)

# Estimate All model
lm_all <- lm(scale(job_sat) ~ factor(sex) + factor(Age_cat) + scale(educyrs) + scale(income) + scale(advancement) + scale(security) + scale(interesting) + scale(independent) + scale(rel_mgmt) + scale(rel_clgs), data = all_data)
summary(lm_all)

# CV predictive metrics for each country ----
# Obtain CV predictive metrics for the model
data_ctrl <- trainControl(method = "cv", number = 10)
usa_model_caret <- train(job_sat ~ factor(sex) + factor(Age_cat) + educyrs + income + advancement + security + interesting + independent + rel_mgmt + rel_clgs,
                         data = usa_data,                        
                         trControl = data_ctrl,              # folds
                         method = "lm",                      # specifying regression model
                         na.action = na.pass)    

# Inspect the USA model
summary(usa_model_caret)

# Output the cross-validated predictive metrics 
usa_model_caret$results

# Obtain CV predictive metrics for the German model
data_ctrl_de <- trainControl(method = "cv", number = 10)
model_caret_de <- train(job_sat ~ factor(sex) + factor(Age_cat) + educyrs + income + advancement + security + interesting + independent + rel_mgmt + rel_clgs,
                        data = de_data,                        
                        trControl = data_ctrl,              # folds
                        method = "lm",                      # specifying regression model
                        na.action = na.pass)    

# Inspect the German model
summary(model_caret_de)

# Output the cross-validated predictive metrics 
model_caret_de$results

# Obtain CV predictive metrics for the japanese model
data_ctrl_jp <- trainControl(method = "cv", number = 10)
model_caret_jp <- train(job_sat ~ factor(sex) + factor(Age_cat) + educyrs + income + advancement + security + interesting + independent + rel_mgmt + rel_clgs,
                        data = jp_data,                        
                        trControl = data_ctrl_jp,              # folds
                        method = "lm",                      # specifying regression model
                        na.action = na.pass)    

# Inspect the Japanese model
summary(model_caret_jp)

# Output the cross-validated predictive metrics 
model_caret_jp$results

# Obtain CV predictive metrics for the all model
data_ctrl_all <- trainControl(method = "cv", number = 10)
model_caret_all <- train(job_sat ~ factor(sex) + factor(Age_cat) + educyrs + income + advancement + security + interesting + independent + rel_mgmt + rel_clgs,
                         data = all_data,                        
                         trControl = data_ctrl_all,              # folds
                         method = "lm",                      # specifying regression model
                         na.action = na.pass)    

# Inspect the all model
summary(model_caret_all)

# Output the cross-validated predictive metrics 
model_caret_all$results


# Countries predicting each other ----
# USA Predicting Japan
prediction <- predict(lm_usa, newdata = jp_data)
unscaled_predictions <- (prediction * attr(lm_usa$fitted.values, "scaled:scale"))+attr(lm_usa$fitted.values, "scaled:center")
USA_japan_RMSE <- sqrt(mean((jp_data[, "job_sat"] - unscaled_predictions)^2))
USA_japan_MAE <- mean(abs(jp_data[, "job_sat"] - unscaled_predictions))

# USA Predicted Germany
prediction <- predict(lm_usa, newdata = de_data)
unscaled_predictions <- (prediction * attr(lm_usa$fitted.values, "scaled:scale"))+attr(lm_usa$fitted.values, "scaled:center")
USA_germany_RMSE <- sqrt(mean((de_data[, "job_sat"] - unscaled_predictions)^2))
USA_germany_MAE<- mean(abs(de_data[, "job_sat"] - unscaled_predictions))

# Germany Predicting Japan
prediction <- predict(lm_de, newdata = jp_data)
unscaled_predictions <- (prediction * attr(lm_de$fitted.values, "scaled:scale"))+attr(lm_de$fitted.values, "scaled:center")
DE_japan_RMSE <- sqrt(mean((jp_data[, "job_sat"] - unscaled_predictions)^2))
DE_japan_MAE <- mean(abs(jp_data[, "job_sat"] - unscaled_predictions))

# Germany Predicting USA
prediction <- predict(lm_de, newdata = usa_data)
unscaled_predictions <- (prediction * attr(lm_de$fitted.values, "scaled:scale"))+attr(lm_de$fitted.values, "scaled:center")
DE_USA_RMSE <- sqrt(mean((usa_data[, "job_sat"] - unscaled_predictions)^2))
DE_USA_MAE<- mean(abs(usa_data[, "job_sat"] - unscaled_predictions))

# Japan Predicting Germany
prediction <- predict(lm_jp, newdata = de_data)
unscaled_predictions <- (prediction * attr(lm_jp$fitted.values, "scaled:scale"))+attr(lm_jp$fitted.values, "scaled:center")
JP_DE_RMSE <- sqrt(mean((de_data[, "job_sat"] - unscaled_predictions)^2))
JP_DE_MAE <- mean(abs(de_data[, "job_sat"] - unscaled_predictions))

# Japan Predicting USA
prediction <- predict(lm_jp, newdata = usa_data)
unscaled_predictions <- (prediction * attr(lm_jp$fitted.values, "scaled:scale"))+attr(lm_jp$fitted.values, "scaled:center")
JP_USA_RMSE <- sqrt(mean((usa_data[, "job_sat"] - unscaled_predictions)^2))
JP_USA_MAE<- mean(abs(usa_data[, "job_sat"] - unscaled_predictions))

# What would happen if our researcher only had a sample of 250?
results <- matrix(0, nrow = 1000, ncol = 3)
for (i in 1:1000) {
  set.seed(123+i)
  index <- sample(rownames(usa_data), size = 500, replace = FALSE,)
  train <- index[1:((length(index)*0.5))]
  test <- index[((length(index)*0.5)+1):length(index)]
  train_lm <- lm(as.formula(lm_usa$call), data = usa_data[train, ])
  unscaled_fitted_values <- (train_lm$fitted.values * attr(train_lm$fitted.values, "scaled:scale"))+attr(train_lm$fitted.values, "scaled:center")
  train_rmse <- sqrt(mean((usa_data[train, "job_sat"] - unscaled_fitted_values)^2))
  prediction <- predict(train_lm, newdata = usa_data[test,])
  unscaled_predictions <- (prediction * attr(train_lm$fitted.values, "scaled:scale"))+attr(train_lm$fitted.values, "scaled:center")
  test_rmse <- sqrt(mean((usa_data[test, "job_sat"] - unscaled_predictions)^2))
  results[i,1] <- train_rmse
  results[i,2] <- test_rmse
  results[i,3] <- summary(train_lm)$r.squared
}
colnames(results) <- c("IS-RMSE", "OOS-RMSE", "R2")
summary(results)

# Calculate the range of RMSE for models with 0.343 <= R2 <= 0.454
range(results[results[,"R2"] >= 0.434 & results[,"R2"] <= 0.454,"OOS-RMSE"])

# Calculate the number of models in this range
sum(results[,"R2"] >= 0.434 & results[,"R2"] <= 0.454)

par(mfrow=c(1,2))
# view the relationship between OOS-MSE and R2
plot(results[,"R2"], results[,"OOS-RMSE"], main = "1.a", xlab = "R-Squared", ylab = "Out-of-sample RMSE")
abline(a = lm(`OOS-RMSE` ~ R2, data = as.data.frame(results))$coefficients[1], b = lm(`OOS-RMSE` ~ R2, data = as.data.frame(results))$coefficients[2])
abline(v = c(0.434, 0.454), lty = 2)

# view the relationship between OOS-MSE and IS-MSE
plot(results[,"IS-RMSE"], results[,"OOS-RMSE"], main = "1.b", xlab = "In-sample RMSE", ylab = "Out-of-sample RMSE")
abline(a = lm(`OOS-RMSE` ~ `IS-RMSE`, data = as.data.frame(results))$coefficients[1], b = lm(`OOS-RMSE` ~ `IS-RMSE`, data = as.data.frame(results))$coefficients[2])

par(mfrow=c(1,1))
# distribution of the RMSE for R2 >0.44 < 0.45
plot(density(results[results[,"R2"] > 0.434 & results[,"R2"] < 0.454, "OOS-RMSE"]), xlab = "Out-of-sample RMSE", main='')
