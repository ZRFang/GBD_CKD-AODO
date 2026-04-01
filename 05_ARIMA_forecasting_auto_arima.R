library(tidyverse)
library(forecast)
library(tseries)
library(ggpubr)
data_raw <- read_csv("gbd_data.csv")
age_weights <- tibble(
  age_name = c("40-44 years", "45-49 years", "50-54 years", "55-59 years", 
               "60-64 years", "65-69 years", "70-74 years", "75-79 years", 
               "80-84 years", "85-89 years", "90-94 years", "95+ years"),
  weight = c(0.0604, 0.0567, 0.0505, 0.0433, 
             0.0355, 0.0274, 0.0194, 0.0123, 
             0.0069, 0.0031, 0.0010, 0.0003) 
)
age_weights <- age_weights %>%
  mutate(std_weight = weight / sum(weight))
data_asr <- data_raw %>%
  filter(age_name %in% age_weights$age_name) %>% 
  left_join(age_weights, by = "age_name") %>%
  group_by(location_name, cause_name, year) %>%
  summarise(
    asdr_40plus = sum(val * std_weight),
    .groups = "drop"
  )
print("数据清洗完成，前几行如下：")
print(head(data_asr))
fit_arima_forecast <- function(df, region_name, disease_name) {
  ts_data <- ts(df$asdr_40plus, start = 1990, end = 2021, frequency = 1)
  fit <- auto.arima(ts_data, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  forecast_val <- forecast(fit, h = 19)
  hist_df <- data.frame(
    year = 1990:2021,
    val = as.numeric(ts_data),
    type = "Historical",
    lower = NA,
    upper = NA
  )
  pred_df <- data.frame(
    year = 2022:2040,
    val = as.numeric(forecast_val$mean),
    type = "Projected",
    lower = as.numeric(forecast_val$lower[,2]), # 95% CI
    upper = as.numeric(forecast_val$upper[,2])  # 95% CI
  )
  bind_rows(hist_df, pred_df) %>%
    mutate(location = region_name, cause = disease_name)
}
params_grid <- data_asr %>%
  distinct(location_name, cause_name)
results_list <- list()
for(i in 1:nrow(params_grid)) {
  loc <- params_grid$location_name[i]
  cau <- params_grid$cause_name[i]
  
  subset_data <- data_asr %>%
    filter(location_name == loc, cause_name == cau) %>%
    arrange(year)
  results_list[[i]] <- fit_arima_forecast(subset_data, loc, cau)
}
final_forecast_data <- bind_rows(results_list)
print("预测计算完成！")
sdi_levels <- c("High SDI", "High-middle SDI", "Global", "Middle SDI", "Low-middle SDI", "Low SDI")
plot_data <- final_forecast_data %>%
  filter(location %in% sdi_levels)
plot_data$location <- factor(plot_data$location, levels = sdi_levels)
custom_colors <- c(
  "Global"          = "black",
  "High SDI"        = "#313695",
  "High-middle SDI" = "#74ADD1",
  "Middle SDI"      = "#F46D43",
  "Low-middle SDI"  = "#D73027",
  "Low SDI"         = "#A50026"
)
plot_forecast <- function(data, disease_title) {
  ggplot(data, aes(x = year, y = val, color = location, fill = location)) +
    geom_line(data = filter(data, type == "Historical"), linewidth = 0.8) +
    geom_line(data = filter(data, type == "Projected"), linetype = "dashed", linewidth = 0.8) +
    geom_ribbon(data = filter(data, type == "Projected"), 
                aes(ymin = lower, ymax = upper), 
                alpha = 0.15, color = NA) + 
    scale_color_manual(values = custom_colors) + 
    scale_fill_manual(values = custom_colors) +
    # -----------------------
  labs(title = disease_title,
       x = "Year", 
       y = "ASDR (per 100,000)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 9)
    ) +
    geom_vline(xintercept = 2021, linetype = "dotted", color = "gray50")
}
plot_ckd <- plot_data %>%
  filter(cause == "Chronic kidney disease") %>%
  plot_forecast("(A)Chronic Kidney Disease (40+):\nASDR Forecast 2022-2040")
plot_adod <- plot_data %>%
  filter(cause == "Alzheimer's disease and other dementias") %>%
  plot_forecast("(B)Alzheimer's & Other Dementias (40+):\nASDR Forecast 2022-2040")
final_plot <- ggarrange(plot_ckd, plot_adod, 
                        ncol = 2, 
                        common.legend = TRUE, 
                        legend = "bottom")
ggsave("Figure_Forecast_BlackGlobal.pdf", plot = final_plot, width = 12, height = 6, dpi = 300)
ggsave("Figure_Forecast_BlackGlobal.png", plot = final_plot, width = 12, height = 6, dpi = 300)

print()
dir.create("Forecast_Tables", showWarnings = FALSE)
unique_combinations <- unique(final_forecast_data[, c("cause", "location")])
for(i in 1:nrow(unique_combinations)) {

  current_cause <- unique_combinations$cause[i]
  current_loc <- unique_combinations$location[i]
  subset_data <- final_forecast_data %>%
    filter(type == "Projected",
           cause == current_cause,
           location == current_loc) %>%
    select(year, val, lower, upper)
  cause_short <- ifelse(current_cause == "Chronic kidney disease", "CKD", "ADOD")
  loc_short <- gsub(" ", "", current_loc) 
  file_name <- paste0("Forecast_Tables/Forecast_", cause_short, "_", loc_short, ".csv")
  write_csv(subset_data, file_name)
  cat("已保存:", file_name, "\n")
}
print()
model_metrics_list <- list()
output_file <- "ARIMA_Model_Details_Full_Report.txt"
file.create(output_file)
unique_combinations <- unique(data_asr[, c("location_name", "cause_name")])
sink(output_file, append = TRUE)
cat("=======================================================\n")
cat("   ARIMA Model Detailed Diagnostics Report (40+ years) \n")
cat("=======================================================\n\n")
for(i in 1:nrow(unique_combinations)) {
  loc <- unique_combinations$location_name[i]
  cau <- unique_combinations$cause_name[i]
  subset_data <- data_asr %>%
    filter(location_name == loc, cause_name == cau) %>%
    arrange(year)
  ts_data <- ts(subset_data$asdr_40plus, start = 1990, end = 2021, frequency = 1)
  fit <- auto.arima(ts_data, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  cat(paste0("-------------------------------------------------------\n"))
  cat(paste0("Location: ", loc, "  |  Disease: ", cau, "\n"))
  cat(paste0("-------------------------------------------------------\n"))
  print(summary(fit)) 
  cat("\n\n")
  acc <- accuracy(fit)[1, ] 
  arima_order <- arimaorder(fit)
  model_name <- paste0("ARIMA(", arima_order[1], ",", arima_order[2], ",", arima_order[3], ")")
  lb_test <- Box.test(residuals(fit), type = "Ljung-Box")
  model_metrics_list[[i]] <- data.frame(
    Location = loc,
    Cause = cau,
    Model = model_name,
    AIC = fit$aic,
    AICc = fit$aicc,
    BIC = fit$bic,
    LogLikelihood = fit$loglik,
    RMSE = acc["RMSE"],
    MAPE = acc["MAPE"], 
    MAE = acc["MAE"],
    Ljung_Box_p_value = lb_test$p.value # p值
  )
}

sink()

final_metrics_table <- bind_rows(model_metrics_list)

write_csv(final_metrics_table, "ARIMA_Model_Summary_Table.csv")

closeAllConnections()

print("任务完成！请查看生成的文件：")
print("1. [ARIMA_Model_Details_Full_Report.txt] -> 包含你要的 Coefficients 和详细矩阵")
print("2. [ARIMA_Model_Summary_Table.csv] -> 包含 AIC, RMSE, ARIMA(p,d,q) 的整理表格")

library(dplyr)
library(tidyr)
library(readr)
table_s1 <- final_forecast_data %>%
  filter(year %in% c(2021, 2040)) %>%
  select(location, cause, year, val, lower, upper) %>%
  pivot_wider(
    names_from = year,
    values_from = c(val, lower, upper)
  ) %>%
  mutate(
    pct_change_num = (val_2040 - val_2021) / val_2021 * 100,
    Value_2021 = sprintf("%.1f", val_2021),
    Value_2040 = sprintf("%.1f [%.1f–%.1f]", val_2040, lower_2040, upper_2040),
    Change_Str = sprintf("%+.1f%%", pct_change_num)
  ) %>%
  select(location, cause, Value_2021, Value_2040, Change_Str) %>%
  rename(
    `Region` = location,
    `Disease` = cause,
    `ASDR 2021 (Observed)` = Value_2021,
    `ASDR 2040 (Projected) [95% CI]` = Value_2040,
    `% Change (2021-2040)` = Change_Str
  ) %>%
  arrange(Disease, factor(Region, levels = c("Global", "High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI")))
print(table_s1)
write_csv(table_s1, "Supplementary_Table_S1_Forecast_Change.csv")
