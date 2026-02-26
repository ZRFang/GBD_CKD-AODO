# 加载绘图包
library(ggplot2)
library(ggrepel) # 用于防止文字重叠

# 1. 录入数据 (来自文中的 Table s1 和 Table s2)
# -----------------------------------------------------------
data <- data.frame(
  Name = c(
    # --- 21个地理区域 ---
    'Andean Latin America', 'Australasia', 'Caribbean', 'Central Asia',
    'Central Europe', 'Central Latin America', 'Central Sub-Saharan Africa',
    'East Asia', 'Eastern Europe', 'Eastern Sub-Saharan Africa',
    'High-income Asia Pacific', 'High-income North America',
    'North Africa and Middle East', 'Oceania', 'South Asia',
    'Southeast Asia', 'Southern Latin America', 'Southern Sub-Saharan Africa',
    'Tropical Latin America', 'Western Europe', 'Western Sub-Saharan Africa',
    # --- 5个 SDI 分层 ---
    'High SDI', 'High-middle SDI', 'Middle SDI', 'Low-middle SDI', 'Low SDI',
    # --- 全球 ---
    'Global'
  ),
  CKD_ASR = c(
    872.44, 216.61, 735.85, 493.20, 266.92, 1171.14, 1124.70,
    322.36, 204.68, 948.36, 235.57, 508.81, 846.64, 699.04,
    540.57, 846.26, 515.22, 895.96, 516.97, 241.72, 930.73,
    358.51, 324.64, 596.45, 686.98, 791.80, # SDI 数据
    529.62 # Global 数据
  ),
  ADOD_ASR = c(
    272.02, 405.09, 313.56, 379.25, 386.04, 335.60, 591.41,
    555.11, 396.72, 460.68, 461.33, 499.20, 476.29, 397.82,
    308.27, 418.63, 368.23, 408.97, 503.17, 443.24, 320.74,
    460.75, 481.70, 455.35, 360.40, 383.04, # SDI 数据
    450.98 # Global 数据
  ),
  Type = c(
    rep("Region", 21), # 前21个是区域
    rep("SDI", 5),     # 中间5个是SDI
    "Global"           # 最后一个是Global
  )
)

# 2. 绘制散点图
# -----------------------------------------------------------
p <- ggplot(data, aes(x = log(CKD_ASR), y = log(ADOD_ASR))) +
  
  # 添加散点：不同类型用不同颜色和形状
  geom_point(aes(color = Type, size = Type, shape = Type), alpha = 0.8) +
  
  # 自定义颜色：区域灰色，Global黑色，SDI红色
  scale_color_manual(values = c("Global" = "black", "Region" = "grey70", "SDI" = "#E41A1C")) +
  
  # 自定义大小：Global最大，SDI次之，Region最小
  scale_size_manual(values = c("Global" = 5, "Region" = 3, "SDI" = 4)) +
  
  # 自定义形状：Global菱形，SDI圆形
  scale_shape_manual(values = c("Global" = 18, "Region" = 16, "SDI" = 17)) +
  
  # 添加趋势线 (仅基于21个区域，不包含聚合数据，以防偏差)
  geom_smooth(data = subset(data, Type == "Region"), method = "lm", 
              color = "grey50", linetype = "dashed", se = TRUE, alpha=0.1) +
  
  # 添加文字标签 (自动避让重叠)
  geom_text_repel(aes(label = Name), size = 3, max.overlaps = 20) +
  
  # 各种美化
  theme_bw() +
  labs(
    title = "Ecological Dissociation: CKD vs. ADOD Burdens (2021)",
    subtitle = "Log-transformed Age-Standardized DALY Rates across 21 GBD Regions and SDI Quintiles",
    x = "Log Age-Standardized DALY Rate of CKD",
    y = "Log Age-Standardized DALY Rate of ADOD",
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

# 显示图表
print(p)

# 如果想保存图片，取消下面这行的注释
ggsave("Correlation_Plot.pdf", width = 10, height = 7)
# ===========================================================
# 3. 计算 R 值和 P 值
# ===========================================================

# 第一步：提取只包含 21 个区域的数据
region_data <- subset(data, Type == "Region")

# 第二步：进行皮尔逊相关性检验 (注意要取 log，因为图也是 log 坐标)
res <- cor.test(log(region_data$CKD_ASR), log(region_data$ADOD_ASR), method = "pearson")

# 第三步：打印详细结果
print(res)

# 第四步：如果你只想提取 r 值和 p 值
r_value <- res$estimate
p_value <- res$p.value

cat("Correlation coefficient (r):", r_value, "\n")
cat("P-value:", p_value, "\n")
# 创建标签文本
label_text <- paste0("r = ", round(r_value, 3), "\n", 
                     "p = ", round(p_value, 3))

# 把文字加到图上 (x和y的坐标根据 log 值调整，例如 x=5.5, y=6.0 左右)
p + annotate("text", x = 5.5, y = 5.5, label = label_text, size = 3, fontface = "italic")

