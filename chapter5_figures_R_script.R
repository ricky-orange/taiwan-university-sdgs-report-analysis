# chapter5_figures_R_script_v5.R
# Purpose: Generate thesis-ready COLOR figures for Chapter 5.
# Input: SDGs_R_ready_tables_v3.xlsx
# Output folder: output_figures_v5
# Notes: This version fixes school_sdg_percentage being stored as wide format.

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(showtext)

# -----------------------------
# 0. Paths
# -----------------------------
input_file <- "SDGs_R_ready_tables_v3.xlsx"
output_dir <- "output_figures_v5"
if (!dir.exists(output_dir)) dir.create(output_dir)

# -----------------------------
# 1. Font setting
# -----------------------------

font_file <- "C:/Windows/Fonts/kaiu.ttf"

if (file.exists(font_file)) {
  
  font_add(
    family = "TW",
    regular = font_file
  )
  
  showtext_auto()
  
  base_family_use <- "TW"
  
} else {
  
  message("kaiu.ttf not found. Using default font.")
  
  base_family_use <- "sans"
}

# -----------------------------
# 2. Global theme
# -----------------------------
base_theme <- theme_minimal(base_family = base_family_use, base_size = 128) +
  theme(
    plot.title = element_text(size = 128, face = "bold", hjust = 0.5, margin = margin(b = 28)), # 圖表主標題
    axis.title = element_text(size = 112, face = "bold"), # X/Y 軸標題
    axis.text = element_text(size = 96), # X/Y 軸刻度文字
    legend.title = element_text(size = 96, face = "bold"), # 圖例標題
    legend.text = element_text(size = 96), # 圖例內容文字
    legend.key.height = unit(3, "cm"), # 增加圖例高度
    legend.key.width = unit(1.2, "cm"), # 增加圖例寬度
    plot.caption = element_text(size = 96, hjust = 0), # 圖表註解文字
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(35, 35, 35, 35)
  )

theme_set(base_theme)

# -----------------------------
# 3. Read tables
# -----------------------------
overall_sdg_count <- read_excel(input_file, sheet = "overall_sdg_count")
school_sdg_percentage <- read_excel(input_file, sheet = "school_sdg_percentage")
disclosure_by_school <- read_excel(input_file, sheet = "disclosure_by_school")
deviation_by_school <- read_excel(input_file, sheet = "deviation_by_school")
deviation_by_quality <- read_excel(input_file, sheet = "deviation_by_quality")

# Normalize column names
names(overall_sdg_count) <- tolower(names(overall_sdg_count))
names(school_sdg_percentage) <- tolower(names(school_sdg_percentage))
names(disclosure_by_school) <- tolower(names(disclosure_by_school))
names(deviation_by_school) <- tolower(names(deviation_by_school))
names(deviation_by_quality) <- tolower(names(deviation_by_quality))

# -----------------------------
# 4. Prepare SDG labels and factor order
# -----------------------------
sdg_levels <- paste0("SDG", 1:17)

# overall_sdg_count should contain column: sdg, count, percentage
if ("sdg" %in% names(overall_sdg_count)) {
  overall_sdg_count$sdg <- toupper(as.character(overall_sdg_count$sdg))
  overall_sdg_count$sdg <- factor(overall_sdg_count$sdg, levels = sdg_levels, ordered = TRUE)
}

# school_sdg_percentage in this workbook is WIDE:
# school, SDG1, SDG2, ..., SDG17
# Convert to LONG: school, sdg, percentage
if (!("sdg" %in% names(school_sdg_percentage))) {
  school_sdg_percentage <- school_sdg_percentage %>%
    pivot_longer(
      cols = starts_with("sdg"),
      names_to = "sdg",
      values_to = "percentage"
    )
}

school_sdg_percentage <- school_sdg_percentage %>%
  mutate(
    sdg = toupper(as.character(sdg)),
    sdg = factor(sdg, levels = sdg_levels, ordered = TRUE)
  )

# disclosure quality order
if ("disclosure_level" %in% names(disclosure_by_school)) {
  disclosure_by_school$disclosure_level <- factor(disclosure_by_school$disclosure_level, levels = c("低", "中", "高"), ordered = TRUE)
}
if ("disclosure_level" %in% names(deviation_by_quality)) {
  deviation_by_quality$disclosure_level <- factor(deviation_by_quality$disclosure_level, levels = c("低", "中", "高"), ordered = TRUE)
}

# -----------------------------
# 5. Figure 5-1: Overall SDGs count
# -----------------------------
p1_data <- overall_sdg_count %>%
  arrange(count) %>%
  mutate(sdg_ordered = factor(sdg, levels = sdg))

p1 <- ggplot(p1_data, aes(x = sdg_ordered, y = count)) +
  geom_col(fill = "#2C7FB8") +
  geom_text(
    aes(label = count),
    hjust = -0.15,
    size = 32,
    family = base_family_use
  ) +
  coord_flip() +
  labs(
    title = "整體 SDGs 揭露次數",
    x = "SDGs",
    y = "揭露次數"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  )

ggsave(
  filename = file.path(output_dir, "圖5-1_整體SDGs揭露次數.png"),
  plot = p1,
  width = 24,
  height = 18,
  dpi = 300
)

# -----------------------------
# 6. Figure 5-2: School x SDGs heatmap
# Deep color = high percentage
# -----------------------------
p2 <- ggplot(school_sdg_percentage, aes(x = sdg, y = school, fill = percentage)) +
  geom_tile(color = "white", linewidth = 0.9) +
  scale_fill_gradientn(
    colors = c("#66BD63", "#D9EF8B", "#FEE08B", "#FDAE61", "#F46D43", "#D73027"),
    values = scales::rescale(c(0, 0.04, 0.08, 0.12, 0.16, 0.20)),
    labels = percent_format(accuracy = 1),
    na.value = "grey98"
  ) +
  labs(
    title = "六校 SDGs 揭露比例熱度圖",
    x = "SDGs",
    y = "學校",
    fill = "比例"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank()
  )

ggsave(
  filename = file.path(output_dir, "圖5-2_六校SDGs揭露比例熱度圖.png"),
  plot = p2,
  width = 28,
  height = 16,
  dpi = 300
)

# -----------------------------
# 7. Figure 5-3: Disclosure quality by school, color
# -----------------------------
p3 <- ggplot(disclosure_by_school, aes(x = school, y = percentage, fill = disclosure_level)) +
  geom_col(position = "fill", color = "white", linewidth = 0.9) +
  scale_fill_manual(
    values = c("低" = "#D95F02", "中" = "#E6AB02", "高" = "#1B9E77"),
    drop = FALSE
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "各校揭露品質比例",
    x = "學校",
    y = "比例",
    fill = "揭露品質"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

ggsave(
  filename = file.path(output_dir, "圖5-3_各校揭露品質比例.png"),
  plot = p3,
  width = 26,
  height = 18,
  dpi = 300
)

# -----------------------------
# 8. Figure 5-4: Deviation rate by school
# -----------------------------
p4_data <- deviation_by_school %>%
  arrange(deviation_rate) %>%
  mutate(school_ordered = factor(school, levels = school))

p4 <- ggplot(p4_data, aes(x = school_ordered, y = deviation_rate)) +
  geom_col(fill = "#D95F02") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "各校 SDGs 偏離率",
    x = "學校",
    y = "偏離率"
  )

ggsave(
  filename = file.path(output_dir, "圖5-4_各校SDGs偏離率.png"),
  plot = p4,
  width = 24,
  height = 16,
  dpi = 300
)

# -----------------------------
# 9. Figure 5-5: Deviation rate by disclosure quality
# -----------------------------
p5_data <- deviation_by_quality %>%
  mutate(
    deviation_label = percent(deviation_rate, accuracy = 0.01)
  )

p5 <- ggplot(p5_data, aes(x = disclosure_level, y = deviation_rate, fill = disclosure_level)) +
  geom_col() +
  geom_text(
    aes(label = deviation_label),
    vjust = -0.35,
    size = 28,
    family = base_family_use
  ) +
  scale_fill_manual(
    values = c("低" = "#D95F02", "中" = "#E6AB02", "高" = "#1B9E77"),
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.01),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "揭露品質與 SDGs 偏離率",
    x = "揭露品質",
    y = "偏離率",
    fill = "揭露品質"
  )

ggsave(
  filename = file.path(output_dir, "圖5-5_揭露品質與SDGs偏離率.png"),
  plot = p5,
  width = 20,
  height = 16,
  dpi = 300
)



# -----------------------------
# 10. Figure 6-1: HHI concentration ranking by school
# Source sheet: concentration_hhi
# Columns used: school, sdg_coverage, top3_share, hhi
# HHI = sum of squared SDG disclosure shares; higher value means more concentrated disclosure.
# -----------------------------

# Optional: if the workbook does not contain concentration_hhi, compute it from school_sdg_percentage.
if (!exists("concentration_hhi") || nrow(concentration_hhi) == 0) {
  concentration_hhi <- school_sdg_percentage %>%
    group_by(school) %>%
    summarise(
      sdg_coverage = sum(!is.na(percentage) & percentage > 0),
      top3_share = sum(sort(percentage, decreasing = TRUE)[1:3], na.rm = TRUE),
      hhi = sum(percentage^2, na.rm = TRUE),
      .groups = "drop"
    )
}

# If percentages were accidentally stored as 0-100, convert top3_share to 0-1.
concentration_hhi <- concentration_hhi %>%
  mutate(
    top3_share = ifelse(top3_share > 1, top3_share / 100, top3_share),
    hhi_label = sprintf("%.3f", hhi),
    top3_label = percent(top3_share, accuracy = 0.1),
    school_ordered = factor(school, levels = school[order(hhi)])
  )

p6_1 <- ggplot(concentration_hhi, aes(x = school_ordered, y = hhi)) +
  geom_col(fill = "#2C7FB8") +
  geom_text(
    aes(label = hhi_label),
    hjust = -0.15,
    size = 28,
    family = base_family_use
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "各校 SDGs 揭露集中度 HHI 排名",
    x = "學校",
    y = "HHI 集中度"
  ) +
  theme(
    plot.margin = margin(35, 120, 35, 35)
  )

ggsave(
  filename = file.path(output_dir, "圖6-1_各校SDGs揭露集中度HHI排名.png"),
  plot = p6_1,
  width = 24,
  height = 16,
  dpi = 300
)

# -----------------------------
# 11. Figure 6-2: Top-3 SDGs share ranking by school
# Source sheet: concentration_hhi
# Columns used: school, top3_share
# This figure directly shows whether disclosures are concentrated in the top three SDGs.
# -----------------------------

p6_2_data <- concentration_hhi %>%
  mutate(
    school_ordered = factor(school, levels = school[order(top3_share)]),
    top3_label = percent(top3_share, accuracy = 0.1)
  )

p6_2 <- ggplot(p6_2_data, aes(x = school_ordered, y = top3_share)) +
  geom_col(fill = "#D95F02") +
  geom_text(
    aes(label = top3_label),
    hjust = -0.15,
    size = 28,
    family = base_family_use
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.20))
  ) +
  labs(
    title = "各校前三大 SDGs 揭露占比排名",
    x = "學校",
    y = "前三大 SDGs 占比"
  ) +
  theme(
    plot.margin = margin(35, 140, 35, 35)
  )

ggsave(
  filename = file.path(output_dir, "圖6-2_各校前三大SDGs揭露占比排名.png"),
  plot = p6_2,
  width = 24,
  height = 16,
  dpi = 300
)


message("Done. Figures were saved to: ", normalizePath(output_dir))
