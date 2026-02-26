# ==============================================================================
# 1. 环境准备与数据加载
# ==============================================================================
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, sf, rnaturalearth, rnaturalearthdata, patchwork, ggsci, cowplot, countrycode, classInt, RColorBrewer)

# 读取数据
ckd_raw <- read_csv("CKD_2021.csv")
adod_raw <- read_csv("ADOD_2021.csv")
raw_data <- bind_rows(ckd_raw, adod_raw)

# 计算 40+ ASR
std_pop_weights <- tibble(
  age_name = c("40-44 years", "45-49 years", "50-54 years", "55-59 years", 
               "60-64 years", "65-69 years", "70-74 years", "75-79 years", 
               "80-84 years", "85-89 years", "90-94 years", "95+ years"),
  std_pop = c(6.04, 5.43, 4.70, 3.96, 3.20, 2.46, 1.79, 1.21, 0.75, 0.41, 0.18, 0.06)
)

plot_data <- raw_data %>%
  left_join(std_pop_weights, by = "age_name") %>%
  group_by(location_name, cause_name) %>%
  summarise(ASDR = sum(val * std_pop, na.rm = TRUE) / sum(std_pop, na.rm = TRUE), .groups = "drop") %>%
  mutate(iso3 = countrycode(location_name, "country.name", "iso3c")) %>%
  filter(!is.na(iso3))

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica") %>%
  select(iso_a3, geometry)
map_ready <- world %>% left_join(plot_data, by = c("iso_a3" = "iso3")) %>% filter(!is.na(cause_name))

# ==============================================================================
# 2. 核心绘图函数
# ==============================================================================
create_flexible_panel <- function(data, disease_name, panel_label) {
  
  sub_data <- data %>% filter(cause_name == disease_name)
  
  # --- 1. 范围图例 (Ranges) ---
  breaks <- classIntervals(sub_data$ASDR, n = 10, style = "quantile")$brks
  breaks <- unique(round(breaks))
  breaks[1] <- floor(min(sub_data$ASDR, na.rm=T))
  breaks[length(breaks)] <- ceiling(max(sub_data$ASDR, na.rm=T))
  labels <- paste0(head(breaks, -1), "–", tail(breaks, -1))
  sub_data$ASDR_cat <- cut(sub_data$ASDR, breaks = breaks, labels = labels, include.lowest = TRUE)
  
  n_colors <- length(labels)
  fill_scale <- scale_fill_manual(
    values = rev(colorRampPalette(brewer.pal(11, "Spectral"))(n_colors)),
    name = "ASDR in 2021\nper 100,000 persons",
    guide = guide_legend(
      ncol = 2, byrow = TRUE, title.position = "top",
      keyheight = unit(0.35, "cm"), keywidth = unit(0.35, "cm"),
      label.theme = element_text(size = 6),
      title.theme = element_text(size = 7, face = "bold")
    ),
    na.value = "grey90"
  )
  
  # --- 2. 主地图 ---
  p_main <- ggplot(sub_data) +
    geom_sf(aes(fill = ASDR_cat), color = "black", size = 0.05) +
    fill_scale +
    coord_sf(crs = "+proj=robin") +
    labs(title = panel_label) + 
    theme_void() +
    theme(
      legend.position = c(0.12, 0.25), 
      legend.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b=10)),
      panel.border = element_blank()
    )
  
  # --- 3. 小图制作 (自然宽度) ---
  inset_theme <- theme_void() +
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "black", fill = NA, size = 0.3),
      plot.title = element_text(size = 4.5, hjust = 0.5, face = "bold", margin = margin(b = 1.5, t = 1.5)),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  make_inset <- function(coords, title) {
    ggplot(sub_data) +
      geom_sf(aes(fill = ASDR_cat), color = "black", size = 0.05) +
      fill_scale +
      # expand = FALSE 保证严格按照给定的经纬度裁剪
      coord_sf(xlim = coords[1:2], ylim = coords[3:4], expand = FALSE) +
      labs(title = title) + inset_theme
  }
  
  # 定义区域 (计算经度跨度用于后续排版)
  # 1. Caribbean: -95 to -55 -> Span = 40
  i1 <- make_inset(c(-95, -55, 5, 28), "Caribbean &\nCentral America")
  
  # 2. Persian Gulf: 38 to 62 -> Span = 24
  i2 <- make_inset(c(38, 62, 12, 32), "Persian Gulf")
  
  # 3. Balkan: 12 to 32 -> Span = 20
  i3 <- make_inset(c(12, 32, 34, 48), "Balkan\nPeninsula")
  
  # 4. SE Asia: 90 to 145 -> Span = 55
  i4 <- make_inset(c(90, 145, -12, 25), "Southeast Asia")
  
  # 模块内部图
  # West Africa: -18 to 10 -> Span = 28
  i_wafr <- make_inset(c(-18, 10, 0, 20), "West Africa")
  
  # E Medit: 32 to 45 -> Span = 13
  i_emed <- make_inset(c(32, 45, 28, 38), "Eastern\nMediterranean")
  
  # N Europe: -5 to 35 -> Span = 40
  i_neur <- make_inset(c(-5, 35, 48, 62), "Northern Europe")
  
  # --- 4. 智能排版 (Smart Layout) ---
  
  # 定义统一间距权重
  # 在经度比例尺下，2度左右的间距比较合适
  SPACER_W <- 2 
  
  # (A) 构建矩形模块 (The Block)
  # 上层宽度 = W_Africa(28) + Space(2) + E_Medit(13) = 43
  # 下层宽度 = N_Europe(40)
  # 它们非常接近！我们按比例分配宽度即可让它们对齐。
  
  # 上层布局
  block_top <- i_wafr + plot_spacer() + i_emed + 
    plot_layout(widths = c(28, SPACER_W, 13))
  
  # 组合模块 (上/下)
  block_combined <- block_top / i_neur + 
    plot_layout(heights = c(1, 1)) # 上下高度一致
  
  # (B) 组合整个底部条带
  # 总宽度比例分配：
  # i1(40), i2(24), i3(20), i4(55), Block(43)
  # 间距均为 SPACER_W (2)
  
  inset_strip <- i1 + plot_spacer() + 
    i2 + plot_spacer() + 
    i3 + plot_spacer() + 
    i4 + plot_spacer() + 
    block_combined + 
    plot_layout(
      nrow = 1,
      widths = c(40, SPACER_W, 24, SPACER_W, 20, SPACER_W, 55, SPACER_W, 43)
    )
  
  # --- 5. 最终拼接 ---
  final_panel <- p_main / inset_strip + plot_layout(heights = c(10, 3))
  
  return(final_panel)
}

# ==============================================================================
# 3. 生成并保存
# ==============================================================================

panel_a <- create_flexible_panel(map_ready, "Chronic kidney disease", "A. Chronic Kidney Disease")
panel_b <- create_flexible_panel(map_ready, "Alzheimer's disease and other dementias", "B. Alzheimer's & Dementia")

final_plot <- panel_a | panel_b

# 保存 (使用宽幅以容纳所有细节)
ggsave("Figure1_Final_Proportional.pdf", final_plot, width = 22, height = 9, dpi = 300)

print(final_plot)
