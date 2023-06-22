library(dplyr)
library(lubridate)
library(forecast)
library(randomForest)
library(xgboost)
library(ggplot2)

f_df <- read.csv("c:/test_input/archivo.csv", header = TRUE, sep = ",")
new_data<- read.csv("c:/test_input/data_predicciones.csv", header = TRUE, sep = ",")

filtered_df <- f_df %>%
  filter(item %in% c(179, 186, 309, 385, 408, 416, 465, 526, 528, 589, 596))

filtered_df <- filtered_df[, c("date", "item", "qty", "TMAX", "PRCP", "holiday_uk", "store")]
filtered_df$date <- as.Date(filtered_df$date)
filtered_df$date <- as.numeric(filtered_df$date)
filtered_df$item <- as.numeric(filtered_df$item)
filtered_df$store <- as.numeric(filtered_df$store)

filtered_df <- na.omit(filtered_df)
filtered_df <- filtered_df[complete.cases(filtered_df), ]

set.seed(123)
train_indices <- sample(nrow(filtered_df), 0.7 * nrow(filtered_df))
train_data <- filtered_df[train_indices, ]
test_data <- filtered_df[-train_indices, ]

# 1. REGREX multiple variables Regresión de múltiples variables
lm_model <- lm(qty ~ TMAX + PRCP + holiday_uk + store+ item, data = train_data)

lm_predictions <- predict(lm_model, newdata = test_data)
lm_mae <- mean(abs(lm_predictions - test_data$qty), na.rm = TRUE)
lm_rmse <- sqrt(mean((lm_predictions - test_data$qty)^2, na.rm = TRUE))
lm_r2 <- 1 - sum((test_data$qty - lm_predictions)^2) / sum((test_data$qty - mean(test_data$qty))^2)

cat("Regresión de Múltiples Variables:\n")
cat("MAE:", lm_mae, "\n")
cat("RMSE:", lm_rmse, "\n")
cat("R2:", lm_r2, "\n")


#Time series model
ts_data <- ts(train_data$qty, frequency = 1)
arima_model <- auto.arima(ts_data)
ts_predictions <- forecast(arima_model, h = nrow(test_data))$mean

ts_mae <- mean(abs(ts_predictions - test_data$qty))
ts_rmse <- sqrt(mean((ts_predictions - test_data$qty)^2))
ts_r2 <- 1 - sum((test_data$qty - ts_predictions)^2) / sum((test_data$qty - mean(test_data$qty))^2)

cat("Regresión de ARIMA:\n")
cat("MAE:", ts_mae, "\n")
cat("RMSE:", ts_rmse, "\n")
cat("R2:", ts_r2, "\n")


#RandomForest model
test_data <- na.omit(test_data)
test_data <- test_data[complete.cases(test_data), ]

rf_model <- randomForest(qty ~ store+TMAX + PRCP + holiday_uk+item, data = train_data)

rf_predictions <- predict(rf_model, newdata = test_data)

rf_mae <- mean(abs(rf_predictions - test_data$qty))
rf_rmse <- sqrt(mean((rf_predictions - test_data$qty)^2))
rf_r2 <- 1 - sum((test_data$qty - rf_predictions)^2) / sum((test_data$qty - mean(test_data$qty))^2)

cat("Random Forest:\n")
cat("MAE:", rf_mae, "\n")
cat("RMSE:", rf_rmse, "\n")
cat("R2:", rf_r2, "\n")

# 4. XGBoost
xgb_data <- xgb.DMatrix(as.matrix(train_data[, c("store","TMAX", "PRCP", "holiday_uk", "date","item")]), label = train_data$qty)

xgb_params <- list(objective = "reg:squarederror", eval_metric = "rmse")
xgb_model <- xgboost(data = xgb_data, params = xgb_params, nrounds = 100)
xgb_predictions <- predict(xgb_model, as.matrix(test_data[, c("store","TMAX", "PRCP", "holiday_uk", "date","item")]))

xgb_mae <- mean(abs(xgb_predictions - test_data$qty))
xgb_rmse <- sqrt(mean((xgb_predictions - test_data$qty)^2))
xgb_r2 <- 1 - sum((test_data$qty - xgb_predictions)^2) / sum((test_data$qty - mean(test_data$qty))^2)

cat("XGBoost:\n")
cat("MAE:", xgb_mae, "\n")
cat("RMSE:", xgb_rmse, "\n")
cat("R2:", xgb_r2, "\n")


#Make predictions


new_data_matrix <- as.matrix(new_data[, c("store","TMAX", "PRCP", "holiday_uk", "date","item")])
new_data_matrix[, "item"] <- as.factor(new_data_matrix[, "item"])
new_data$date <- as.Date(new_data$date)
new_data$date <- as.numeric(new_data$date)

xgb_new_data <- xgb.DMatrix(as.matrix(new_data[, c("store","TMAX", "PRCP", "holiday_uk", "date","item")]))


predictions <- predict(xgb_model, xgb_new_data)
new_data$predictions <- round(predictions,0)

item_sum$item <- factor(item_sum$item)
item_colors <- rainbow(length(item_sum$item))

ggplot(item_sum, aes(x = reorder(item, -total_predictions), y = total_predictions, fill = item)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_predictions), vjust = -0.5) +
  scale_fill_manual(values = item_colors) +
  xlab("Item") +
  ylab("Total Predictions") +
  ggtitle("Total Predictions by Item")

write.csv(new_data, file = "predictions.csv", row.names = FALSE)