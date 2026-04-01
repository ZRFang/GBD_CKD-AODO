library(ggplot2)
library(ggrepel)
data <- data.frame(
  Name = c(
    'Andean Latin America', 'Australasia', 'Caribbean', 'Central Asia',
    'Central Europe', 'Central Latin America', 'Central Sub-Saharan Africa',
    'East Asia', 'Eastern Europe', 'Eastern Sub-Saharan Africa',
    'High-income Asia Pacific', 'High-income North America',
    'North Africa and Middle East', 'Oceania', 'South Asia',
    'Southeast Asia', 'Southern Latin America', 'Southern Sub-Saharan Africa',
    'Tropical Latin America', 'Western Europe', 'Western Sub-Saharan Africa',
    'High SDI', 'High-middle SDI', 'Middle SDI', 'Low-middle SDI', 'Low SDI',
    'Global'
  ),
  CKD_ASR = c(
    872.44, 216.61, 735.85, 493.20, 266.92, 1171.14, 1124.70,
    322.36, 204.68, 948.36, 235.57, 508.81, 846.64, 699.04,
    540.57, 846.26, 515.22, 895.96, 516.97, 241.72, 930.73,
    358.51, 324.64, 596.45, 686.98, 791.80, 
    529.62
  ),
  ADOD_ASR = c(
    272.02, 405.09, 313.56, 379.25, 386.04, 335.60, 591.41,
    555.11, 396.72, 460.68, 461.33, 499.20, 476.29, 397.82,
    308.27, 418.63, 368.23, 408.97, 503.17, 443.24, 320.74,
    460.75, 481.70, 455.35, 360.40, 383.04,
    450.98
  ),
  Type = c(
    rep("Region", 21),
    rep("SDI", 5),
    "Global"
  )
)
p <- ggplot(data, aes(x = log(CKD_ASR), y = log(ADOD_ASR))) +
  geom_point(aes(color = Type, size = Type, shape = Type), alpha = 0.8) +
  scale_color_manual(values = c("Global" = "black", "Region" = "grey70", "SDI" = "#E41A1C")) +
  scale_size_manual(values = c("Global" = 5, "Region" = 3, "SDI" = 4)) +
  scale_shape_manual(values = c("Global" = 18, "Region" = 16, "SDI" = 17)) 
  geom_smooth(data = subset(data, Type == "Region"), method = "lm", 
              color = "grey50", linetype = "dashed", se = TRUE, alpha=0.1) +
  geom_text_repel(aes(label = Name), size = 3, max.overlaps = 20) +
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
print(p)
ggsave("Correlation_Plot.pdf", width = 10, height = 7)
region_data <- subset(data, Type == "Region")
res <- cor.test(log(region_data$CKD_ASR), log(region_data$ADOD_ASR), method = "pearson")
print(res)
r_value <- res$estimate
p_value <- res$p.value
cat("Correlation coefficient (r):", r_value, "\n")
cat("P-value:", p_value, "\n")
label_text <- paste0("r = ", round(r_value, 3), "\n", 
                     "p = ", round(p_value, 3))
p + annotate("text", x = 5.5, y = 5.5, label = label_text, size = 3, fontface = "italic")
