library(tidyverse)

# 1. 读取数据
df_rate <- read_csv("input_rate.csv")
df_number <- read_csv("input_number.csv")

# 2. 定义权重
gbd_weights_40plus <- tibble(
  age_name = c("40-44 years", "45-49 years", "50-54 years", "55-59 years",
               "60-64 years", "65-69 years", "70-74 years", "75-79 years",
               "80-84 years", "85-89 years", "90-94 years", "95+ years"),
  standard_weight = c(0.0659, 0.0597, 0.0536, 0.0463,
                      0.0381, 0.0299, 0.0217, 0.0145,
                      0.0086, 0.0041, 0.0014, 0.0004)
) %>%
  mutate(rescaled_weight = standard_weight / sum(standard_weight))

# 3. 计算 ASR（注意这里的 year 替换了 year_id）
df_asr_recalculated <- df_rate %>%
  filter(age_name %in% gbd_weights_40plus$age_name) %>%
  left_join(gbd_weights_40plus, by = "age_name") %>%
  # ⚠️ 修改处：将 year_id 改为了 year
  group_by(location_name, year, sex_name, cause_name, measure_name) %>%
  summarise(
    asr_val = sum(val * rescaled_weight, na.rm = TRUE),
    asr_lower = sum(lower * rescaled_weight, na.rm = TRUE),
    asr_upper = sum(upper * rescaled_weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(metric_name = "Rate")

# 4. 计算 Number（注意这里的 year 替换了 year_id）
df_number_total <- df_number %>%
  filter(age_name %in% gbd_weights_40plus$age_name) %>%
  # ⚠️ 修改处：将 year_id 改为了 year
  group_by(location_name, year, sex_name, cause_name, measure_name) %>%
  summarise(
    number_val = sum(val, na.rm = TRUE),
    number_lower = sum(lower, na.rm = TRUE), 
    number_upper = sum(upper, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(metric_name = "Number")

# 5. 合并与导出
final_cleaned_data <- bind_rows(
  df_asr_recalculated %>% rename(val = asr_val, lower = asr_lower, upper = asr_upper),
  df_number_total %>% rename(val = number_val, lower = number_lower, upper = number_upper)
) %>%
  # ⚠️ 修改处：合并导出时也使用 year
  select(location_name, year, sex_name, cause_name, measure_name, metric_name, val, lower, upper)

write_csv(final_cleaned_data, "cleaned_GBD_data_40plus.csv")
