# ============================================================
# Create Thesis-ready Figures for Keyword Analysis
# Input:
#   keyword_analysis_outputs.xlsx
#
# Output folder:
#   keyword_analysis_figures
#
# Output figures:
#   圖5-X_整體高頻關鍵字分布.png
#   圖5-X_各SDGs主要關鍵字共現分布_三欄加大間距版.png
#   圖5-X_SDGs與高頻關鍵字共現熱圖.png
#   圖5-X_高頻關鍵字與揭露品質之關係.png
#   圖5-X_高頻關鍵字之SDGs偏離比例.png
# ============================================================


# ============================================================
# 0. Package setup
# ============================================================

packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "ggplot2",
  "forcats",
  "scales",
  "showtext",
  "grid"
)

installed <- rownames(installed.packages())

for (p in packages) {
  if (!(p %in% installed)) {
    install.packages(p)
  }
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(scales)
library(showtext)
library(grid)


# ============================================================
# 1. Paths
# ============================================================

input_file <- "keyword_analysis_outputs.xlsx"

output_dir <- "keyword_analysis_figures"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# ============================================================
# 2. Font setting
# ============================================================

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


# ============================================================
# 3. Global theme
# ============================================================

base_theme <- theme_minimal(base_family = base_family_use, base_size = 128) +
  theme(
    plot.title = element_text(
      size = 128,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 28)
    ),
    plot.subtitle = element_text(
      size = 96,
      hjust = 0.5,
      margin = margin(b = 24)
    ),
    axis.title = element_text(
      size = 112,
      face = "bold"
    ),
    axis.text = element_text(
      size = 96
    ),
    legend.title = element_text(
      size = 96,
      face = "bold"
    ),
    legend.text = element_text(
      size = 96
    ),
    legend.key.height = unit(3, "cm"),
    legend.key.width = unit(1.2, "cm"),
    plot.caption = element_text(
      size = 96,
      hjust = 0
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(35, 35, 35, 35)
  )

theme_set(base_theme)


# ============================================================
# 4. Helper functions
# ============================================================

save_png <- function(plot, filename, width = 24, height = 18, dpi = 300) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white",
    limitsize = FALSE
  )
}

sort_sdg <- function(x) {
  x <- toupper(as.character(x))
  factor(x, levels = paste0("SDG", 1:17), ordered = TRUE)
}

# For reordering labels within facets
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}

clean_deviation_label <- function(x) {
  case_when(
    str_to_upper(as.character(x)) %in% c("YES", "Y", "TRUE", "T", "是", "有", "偏離") ~ "有偏離",
    str_to_upper(as.character(x)) %in% c("NO", "N", "FALSE", "F", "否", "無", "未偏離") ~ "無偏離",
    TRUE ~ as.character(x)
  )
}


# ============================================================
# 5. Read tables
# ============================================================

keyword_frequency <- read_excel(input_file, sheet = "04_keyword_frequency")
top_keywords_by_sdg <- read_excel(input_file, sheet = "05_top_keywords_by_sdg")
sdg_keyword_matrix <- read_excel(input_file, sheet = "06_sdg_keyword_matrix")
keyword_disclosure <- read_excel(input_file, sheet = "07_keyword_disclosure")
keyword_deviation <- read_excel(input_file, sheet = "08_keyword_deviation")

names(keyword_frequency) <- tolower(names(keyword_frequency))
names(top_keywords_by_sdg) <- tolower(names(top_keywords_by_sdg))
names(sdg_keyword_matrix) <- tolower(names(sdg_keyword_matrix))
names(keyword_disclosure) <- tolower(names(keyword_disclosure))
names(keyword_deviation) <- tolower(names(keyword_deviation))


# ============================================================
# 6. Normalize variables
# ============================================================

if ("sdg" %in% names(top_keywords_by_sdg)) {
  top_keywords_by_sdg <- top_keywords_by_sdg %>%
    mutate(sdg = sort_sdg(sdg))
}

if ("sdg" %in% names(sdg_keyword_matrix)) {
  sdg_keyword_matrix <- sdg_keyword_matrix %>%
    mutate(sdg = sort_sdg(sdg))
}

if ("disclosure_level" %in% names(keyword_disclosure)) {
  keyword_disclosure <- keyword_disclosure %>%
    mutate(
      disclosure_level = factor(
        disclosure_level,
        levels = c("低", "中", "高"),
        ordered = TRUE
      )
    )
}


# ============================================================
# Figure 1. Overall top keywords bar chart
# 顯示前 20 名高頻關鍵字，並於長條末端標示次數
# ============================================================

top_n_keywords <- 20

p1_data <- keyword_frequency %>%
  slice_head(n = top_n_keywords) %>%
  arrange(n) %>%
  mutate(
    keyword_ordered = factor(keyword, levels = keyword)
  )

p1 <- ggplot(p1_data, aes(x = keyword_ordered, y = n)) +
  geom_col(fill = "#2C7FB8") +
  geom_text(
    aes(label = n),
    hjust = -0.15,
    size = 28,
    family = base_family_use
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "整體高頻關鍵字分布",
    x = "關鍵字",
    y = "出現次數"
  )

save_png(
  p1,
  "圖5-X_整體高頻關鍵字分布.png",
  width = 26,
  height = 18
)


# ============================================================
# Figure 2. Top keywords by SDG co-occurrence
# 各 SDGs 主要關鍵字共現分布
#
# 修改重點：
# 1. 改成 ncol = 3，讓每列三個 SDG 小圖
# 2. 不使用 coord_flip()
# 3. 直接把 keyword 放到 y 軸，讓 Y 軸文字間距比較容易控制
# ============================================================

p2_data <- top_keywords_by_sdg %>%
  mutate(
    sdg = sort_sdg(sdg),
    keyword_reordered = reorder_within(keyword, n, sdg)
  )

p2 <- ggplot(p2_data, aes(y = keyword_reordered, x = n)) +
  geom_col(fill = "#2C7FB8", width = 0.55) +
  facet_wrap(~ sdg, scales = "free_y", ncol = 3) +
  scale_y_reordered(expand = expansion(add = 1.8)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "各 SDGs 主要關鍵字共現分布",
    x = "共現次數",
    y = "關鍵字"
  ) +
  theme(
    strip.text = element_text(size = 168, face = "bold"),
    axis.text.y = element_text(
      size = 168,
      lineheight = 1.1,
      margin = margin(r = 18)
    ),
    axis.text.x = element_text(size = 150),
    axis.title = element_text(size = 220, face = "bold"),
    plot.title = element_text(size = 240, face = "bold", hjust = 0.5),
    panel.spacing.x = unit(2.0, "lines"),
    panel.spacing.y = unit(4.5, "lines"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

save_png(
  p2,
  "圖5-X_各SDGs主要關鍵字共現分布_三欄加大間距版.png",
  width = 48,
  height = 58,
  dpi = 300
)


# ============================================================
# Figure 3. SDG × Keyword co-occurrence heatmap
# 本圖呈現 SDGs 與高頻關鍵字於段落層次之共現關係，
# 並非表示關鍵字對單一 SDG 的唯一歸屬。
# ============================================================

p3_data <- sdg_keyword_matrix %>%
  pivot_longer(
    cols = -sdg,
    names_to = "keyword",
    values_to = "n"
  ) %>%
  mutate(
    sdg = sort_sdg(sdg)
  ) %>%
  group_by(keyword) %>%
  mutate(keyword_total = sum(n, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    keyword = fct_reorder(keyword, keyword_total)
  )

p3 <- ggplot(p3_data, aes(x = keyword, y = sdg, fill = n)) +
  geom_tile(color = "white", linewidth = 0.9) +
  scale_fill_gradientn(
    colors = c("#66BD63", "#D9EF8B", "#FEE08B", "#FDAE61", "#F46D43", "#D73027"),
    values = scales::rescale(c(0, 5, 10, 15, 20, max(p3_data$n, na.rm = TRUE))),
    labels = comma,
    na.value = "grey98"
  ) +
  labs(
    title = "SDGs 與高頻關鍵字共現熱圖",
    x = "關鍵字",
    y = "SDGs",
    fill = "共現次數"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank()
  )

save_png(
  p3,
  "圖5-X_SDGs與高頻關鍵字共現熱圖.png",
  width = 34,
  height = 18
)


# ============================================================
# Figure 4. Keyword × disclosure level
# Top 20 keywords only
# ============================================================

top_keywords_20 <- keyword_frequency %>%
  slice_head(n = 20) %>%
  pull(keyword)

p4_data <- keyword_disclosure %>%
  filter(keyword %in% top_keywords_20) %>%
  group_by(keyword) %>%
  mutate(
    total_keyword = sum(n, na.rm = TRUE),
    proportion = n / total_keyword
  ) %>%
  ungroup() %>%
  arrange(total_keyword) %>%
  mutate(
    keyword_ordered = factor(keyword, levels = unique(keyword))
  )

p4 <- ggplot(
  p4_data,
  aes(x = keyword_ordered, y = proportion, fill = disclosure_level)
) +
  geom_col(position = "fill", color = "white", linewidth = 0.9) +
  coord_flip() +
  scale_fill_manual(
    values = c("低" = "#D95F02", "中" = "#E6AB02", "高" = "#1B9E77"),
    drop = FALSE
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "高頻關鍵字與揭露品質之關係",
    x = "關鍵字",
    y = "比例",
    fill = "揭露品質"
  )

save_png(
  p4,
  "圖5-X_高頻關鍵字與揭露品質之關係.png",
  width = 28,
  height = 20
)


p5_data <- keyword_deviation %>%
  filter(keyword %in% top_keywords_20) %>%
  mutate(
    has_deviation_clean = clean_deviation_label(has_deviation)
  ) %>%
  group_by(keyword) %>%
  summarise(
    total_keyword = sum(n, na.rm = TRUE),
    deviation_n = sum(n[has_deviation_clean == "有偏離"], na.rm = TRUE),
    deviation_rate = ifelse(total_keyword > 0, deviation_n / total_keyword, 0),
    .groups = "drop"
  ) %>%
  filter(total_keyword > 0) %>%
  arrange(deviation_rate) %>%
  mutate(
    keyword_ordered = factor(keyword, levels = keyword),
    deviation_label = percent(deviation_rate, accuracy = 0.1)
  )

p5_max <- max(p5_data$deviation_rate, na.rm = TRUE)

p5 <- ggplot(p5_data, aes(x = keyword_ordered, y = deviation_rate)) +
  geom_col(fill = "#D95F02") +
  geom_text(
    aes(
      y = deviation_rate + p5_max * 0.03,
      label = deviation_label
    ),
    hjust = 0,
    size = 28,
    family = base_family_use
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1),
    limits = c(0, p5_max * 1.25),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "高頻關鍵字之 SDGs 偏離比例",
    x = "關鍵字",
    y = "偏離比例"
  ) +
  theme(
    plot.margin = margin(35, 120, 35, 35)
  )

save_png(
  p5,
  "圖5-X_高頻關鍵字之SDGs偏離比例.png",
  width = 24,
  height = 18
)


# ============================================================
# Completion message
# ============================================================

message("Done. Figures were saved to: ", normalizePath(output_dir))