# --- 0. 加载所有需要的包 ---
library(tidyverse)
library(forecast)
library(tseries)
library(ggpubr)

# --- 1. 读取数据 ---
# 请确保你的文件名是 'gbd_data.csv'，或者在这里修改为你的实际文件名
# 如果报错 "找不到文件"，请用 setwd("你的文件路径") 设置工作目录
data_raw <- read_csv("gbd_data.csv")

# --- 2. 数据预处理：重新计算 40+ Age-Standardized Rate (ASR) ---

# 定义 GBD 2019 全球标准人口权重 (仅针对 40岁及以上)
# 来源：GBD 2019 Reference Standard Population
age_weights <- tibble(
  age_name = c("40-44 years", "45-49 years", "50-54 years", "55-59 years", 
               "60-64 years", "65-69 years", "70-74 years", "75-79 years", 
               "80-84 years", "85-89 years", "90-94 years", "95+ years"),
  weight = c(0.0604, 0.0567, 0.0505, 0.0433, 
             0.0355, 0.0274, 0.0194, 0.0123, 
             0.0069, 0.0031, 0.0010, 0.0003) 
)

# 归一化权重 (因为我们只取了40+部分，总和不为1，需重新分配)
age_weights <- age_weights %>%
  mutate(std_weight = weight / sum(weight))

# 计算 ASR
data_asr <- data_raw %>%
  filter(age_name %in% age_weights$age_name) %>% # 筛选40岁以上
  left_join(age_weights, by = "age_name") %>%    # 匹配权重
  group_by(location_name, cause_name, year) %>%
  summarise(
    asdr_40plus = sum(val * std_weight), # 加权求和
    .groups = "drop"
  )

print("数据清洗完成，前几行如下：")
print(head(data_asr))

# --- 3. ARIMA 建模与预测函数 ---

fit_arima_forecast <- function(df, region_name, disease_name) {
  
  # 转换为时间序列 (1990-2021)
  ts_data <- ts(df$asdr_40plus, start = 1990, end = 2021, frequency = 1)
  
  # 自动 ARIMA 建模 (Box-Jenkins 方法)
  fit <- auto.arima(ts_data, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  
  # 预测未来 19 年 (2022-2040)
  forecast_val <- forecast(fit, h = 19)
  
  # 整理历史数据
  hist_df <- data.frame(
    year = 1990:2021,
    val = as.numeric(ts_data),
    type = "Historical",
    lower = NA,
    upper = NA
  )
  
  # 整理预测数据
  pred_df <- data.frame(
    year = 2022:2040,
    val = as.numeric(forecast_val$mean),
    type = "Projected",
    lower = as.numeric(forecast_val$lower[,2]), # 95% CI
    upper = as.numeric(forecast_val$upper[,2])  # 95% CI
  )
  
  # 合并
  bind_rows(hist_df, pred_df) %>%
    mutate(location = region_name, cause = disease_name)
}

# --- 4. 批量运行所有地区 ---

params_grid <- data_asr %>%
  distinct(location_name, cause_name)

results_list <- list()

# 循环计算
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

# --- 5. 绘图 (复刻 Figure 10) ---

# --- 5. 绘图 (Global 为黑色版) ---

# 1. 设置图例顺序 (保持不变)
sdi_levels <- c("High SDI", "High-middle SDI", "Global", "Middle SDI", "Low-middle SDI", "Low SDI")
plot_data <- final_forecast_data %>%
  filter(location %in% sdi_levels)
plot_data$location <- factor(plot_data$location, levels = sdi_levels)

# 2. 定义自定义颜色 (Key Step)
# 这一步手动指定每个地区的颜色
custom_colors <- c(
  "Global"          = "black",    # <--- 全球设为黑色
  "High SDI"        = "#313695",  # 深蓝
  "High-middle SDI" = "#74ADD1",  # 浅蓝
  "Middle SDI"      = "#F46D43",  # 橙红 (或者用黄色 #FDAE61，看你喜好)
  "Low-middle SDI"  = "#D73027",  # 红
  "Low SDI"         = "#A50026"   # 深红
)

# 或者如果你想保留之前的 RdYlBu 风格，除了 Global 黑之外：
# Middle SDI 用米黄色可能看不清，建议用橙色系。

# 3. 绘图函数 (应用自定义颜色)
plot_forecast <- function(data, disease_title) {
  ggplot(data, aes(x = year, y = val, color = location, fill = location)) +
    # 历史实线
    geom_line(data = filter(data, type == "Historical"), linewidth = 0.8) +
    # 预测虚线
    geom_line(data = filter(data, type == "Projected"), linetype = "dashed", linewidth = 0.8) +
    # 预测区间 (Global 的阴影也会变黑，透明度 alpha=0.1 会显示为灰色)
    geom_ribbon(data = filter(data, type == "Projected"), 
                aes(ymin = lower, ymax = upper), 
                alpha = 0.15, color = NA) + 
    # --- 这里的 scale 变了 ---
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

# 4. 生成两张图
plot_ckd <- plot_data %>%
  filter(cause == "Chronic kidney disease") %>%
  plot_forecast("(A)Chronic Kidney Disease (40+):\nASDR Forecast 2022-2040")

plot_adod <- plot_data %>%
  filter(cause == "Alzheimer's disease and other dementias") %>%
  plot_forecast("(B)Alzheimer's & Other Dementias (40+):\nASDR Forecast 2022-2040")

# 5. 拼图
final_plot <- ggarrange(plot_ckd, plot_adod, 
                        ncol = 2, 
                        common.legend = TRUE, 
                        legend = "bottom")

# 6. 保存 (保持宽屏尺寸)
ggsave("Figure_Forecast_BlackGlobal.pdf", plot = final_plot, width = 12, height = 6, dpi = 300)
ggsave("Figure_Forecast_BlackGlobal.png", plot = final_plot, width = 12, height = 6, dpi = 300)

print("图片已保存！Global 线条现在是黑色的了。")
# --- 6. 导出预测数据为附表 (CSV) ---

# 确保文件夹存在 (生成的表格会保存在 'Forecast_Tables' 文件夹下，保持整洁)
dir.create("Forecast_Tables", showWarnings = FALSE)

# 获取所有疾病和地区的组合
unique_combinations <- unique(final_forecast_data[, c("cause", "location")])

# 循环导出每一个文件
for(i in 1:nrow(unique_combinations)) {
  
  # 1. 提取当前组合的信息
  current_cause <- unique_combinations$cause[i]
  current_loc <- unique_combinations$location[i]
  
  # 2. 筛选数据:
  #    - 只要预测部分 (type == "Projected")
  #    - 只要当前疾病和地区
  subset_data <- final_forecast_data %>%
    filter(type == "Projected",
           cause == current_cause,
           location == current_loc) %>%
    # 3. 整理列 (符合您上传的附表格式)
    select(year, val, lower, upper)
  
  # 4. 生成文件名 (例如: Forecast_CKD_Global.csv)
  #    将长名字简化一下，方便阅读
  cause_short <- ifelse(current_cause == "Chronic kidney disease", "CKD", "ADOD")
  # 去掉空格，把 "High SDI" 变成 "HighSDI"
  loc_short <- gsub(" ", "", current_loc) 
  
  file_name <- paste0("Forecast_Tables/Forecast_", cause_short, "_", loc_short, ".csv")
  
  # 5. 写入 CSV
  write_csv(subset_data, file_name)
  
  cat("已保存:", file_name, "\n")
}

print("所有附表已生成！请在 R 工作目录下的 'Forecast_Tables' 文件夹中查看。")
# --- 7. 提取 ARIMA 模型详细参数与诊断指标 ---

# 1. 准备存储容器
model_metrics_list <- list() # 用于存储干净的汇总表格
output_file <- "ARIMA_Model_Details_Full_Report.txt" # 用于存储你想要的详细原始输出

# 清空/创建文本文件 (避免追加写入时混淆)
file.create(output_file)

# 获取所有组合
unique_combinations <- unique(data_asr[, c("location_name", "cause_name")])

# 打开文本连接，准备写入详细报告
sink(output_file, append = TRUE)

cat("=======================================================\n")
cat("   ARIMA Model Detailed Diagnostics Report (40+ years) \n")
cat("=======================================================\n\n")

# --- 循环开始 ---
for(i in 1:nrow(unique_combinations)) {
  
  # A. 获取当前地区和疾病
  loc <- unique_combinations$location_name[i]
  cau <- unique_combinations$cause_name[i]
  
  # 筛选数据
  subset_data <- data_asr %>%
    filter(location_name == loc, cause_name == cau) %>%
    arrange(year)
  
  ts_data <- ts(subset_data$asdr_40plus, start = 1990, end = 2021, frequency = 1)
  
  # B. 重新拟合模型 (必须与预测时一致)
  fit <- auto.arima(ts_data, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  
  # --- 输出 1: 写入详细文本报告 (这就是你要的那个格式) ---
  cat(paste0("-------------------------------------------------------\n"))
  cat(paste0("Location: ", loc, "  |  Disease: ", cau, "\n"))
  cat(paste0("-------------------------------------------------------\n"))
  
  # 这里会打印出 Coefficients, sigma^2, AIC, Error measures 等所有内容
  print(summary(fit)) 
  cat("\n\n")
  
  # --- 输出 2: 提取关键指标到表格 (方便做表) ---
  
  # 获取精度指标 (RMSE, MAPE, etc.)
  acc <- accuracy(fit)[1, ] # 取训练集误差
  
  # 获取模型参数 (p, d, q)
  arima_order <- arimaorder(fit)
  model_name <- paste0("ARIMA(", arima_order[1], ",", arima_order[2], ",", arima_order[3], ")")
  
  # Ljung-Box 检验 (残差白噪声检验，通常也要汇报，p>0.05表示模型良好)
  lb_test <- Box.test(residuals(fit), type = "Ljung-Box")
  
  # 存入列表
  model_metrics_list[[i]] <- data.frame(
    Location = loc,
    Cause = cau,
    Model = model_name,
    AIC = fit$aic,
    AICc = fit$aicc,
    BIC = fit$bic,
    LogLikelihood = fit$loglik,
    RMSE = acc["RMSE"],
    MAPE = acc["MAPE"],    # 平均绝对百分比误差
    MAE = acc["MAE"],
    Ljung_Box_p_value = lb_test$p.value # p值
  )
}

# 关闭文本记录
sink()

# --- 3. 整合并保存汇总表格 ---
final_metrics_table <- bind_rows(model_metrics_list)

# 导出汇总表 CSV
write_csv(final_metrics_table, "ARIMA_Model_Summary_Table.csv")

# 恢复控制台输出
closeAllConnections()

print("任务完成！请查看生成的文件：")
print("1. [ARIMA_Model_Details_Full_Report.txt] -> 包含你要的 Coefficients 和详细矩阵")
print("2. [ARIMA_Model_Summary_Table.csv] -> 包含 AIC, RMSE, ARIMA(p,d,q) 的整理表格")
# ==============================================================================
# --- 8. 最终大合集：计算 % Change (2021-2040) 并生成论文用表 ---
# ==============================================================================

# 确保已经有了 final_forecast_data (来自步骤 4)
# 如果没有，请先运行前面的步骤 4

library(dplyr)
library(tidyr)
library(readr)

# 1. 数据转换与计算
table_s1 <- final_forecast_data %>%
  # 筛选关键年份：2021 (观测值) 和 2040 (预测值)
  filter(year %in% c(2021, 2040)) %>%
  
  # 选择需要的列：地区、疾病、年份、数值、置信区间下限、上限
  select(location, cause, year, val, lower, upper) %>%
  
  # 将长表格转换为宽表格：让 2021 和 2040 的数据排在同一行，方便相减
  pivot_wider(
    names_from = year,
    values_from = c(val, lower, upper)
  ) %>%
  
  # 开始计算和格式化
  mutate(
    # --- 核心计算公式：(2040值 - 2021值) / 2021值 * 100 ---
    pct_change_num = (val_2040 - val_2021) / val_2021 * 100,
    
    # --- 格式化第1列：2021年数值 (保留1位小数) ---
    # 2021是历史数据，没有置信区间
    Value_2021 = sprintf("%.1f", val_2021),
    
    # --- 格式化第2列：2040年预测值 [95% CI] ---
    # 格式示例：1234.5 [1100.2 - 1300.8]
    Value_2040 = sprintf("%.1f [%.1f–%.1f]", val_2040, lower_2040, upper_2040),
    
    # --- 格式化第3列：百分比变化 (带加号) ---
    # 如果大于0加正号，保留1位小数
    Change_Str = sprintf("%+.1f%%", pct_change_num)
  ) %>%
  
  # 只保留最后展示需要的列
  select(location, cause, Value_2021, Value_2040, Change_Str) %>%
  
  # 重命名列名 (符合论文格式)
  rename(
    `Region` = location,
    `Disease` = cause,
    `ASDR 2021 (Observed)` = Value_2021,
    `ASDR 2040 (Projected) [95% CI]` = Value_2040,
    `% Change (2021-2040)` = Change_Str
  ) %>%
  
  # 排序：让 Global 排在前面，然后是 SDI
  arrange(Disease, factor(Region, levels = c("Global", "High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI")))

# 2. 打印查看结果
print("生成的表格预览：")
print(table_s1)

# 3. 保存为 CSV 文件 (可以直接复制到 Word 或 Excel)
write_csv(table_s1, "Supplementary_Table_S1_Forecast_Change.csv")

print("恭喜！最终表格已保存为 'Supplementary_Table_S1_Forecast_Change.csv'")
