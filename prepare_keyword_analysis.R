# ============================================================
# Keyword Analysis Data Preparation
# Source file:
#   SDGs_analysis_ALL_deviation_checked_v3.xlsx
#
# Purpose:
#   1. Read ChatGPT-assisted SDG analysis results
#   2. Split multiple keywords into long format
#   3. Split multiple SDGs into SDG × Keyword long format
#   4. Generate frequency tables and cross-tabulation tables
#   5. Export Excel and UTF-8 BOM CSV files
#
# Suggested script name:
#   prepare_keyword_analysis_data.R
# ============================================================


# ============================================================
# 0. Package setup
# ============================================================

packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "openxlsx",
  "janitor",
  "readr"
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
library(openxlsx)
library(janitor)
library(readr)


# ============================================================
# 1. File settings
# ============================================================

input_file <- "SDGs_analysis_ALL_deviation_checked_v3.xlsx"

output_xlsx <- "keyword_analysis_outputs.xlsx"

output_dir <- "keyword_analysis_csv"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# ============================================================
# 2. Read Excel file
# ============================================================

raw_data <- read_excel(input_file, sheet = 1)

raw_data <- raw_data %>%
  janitor::clean_names()

cat("Detected column names:\n")
print(names(raw_data))


# ============================================================
# 3. Specify actual column names
# ============================================================

col_school <- "school_name"
col_sdg <- "sdgs_list"
col_text <- "sdgs_content"
col_keywords <- "keywords"
col_disclosure <- "disclosure_level"
col_disclosure_reason <- "disclosure_reason"
col_deviation <- "has_deviation"
col_deviation_reason <- "deviation_reason"
col_page <- "page"

required_cols <- c(
  col_school,
  col_sdg,
  col_text,
  col_keywords,
  col_disclosure,
  col_deviation
)

missing_cols <- required_cols[!(required_cols %in% names(raw_data))]

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "\nPlease check the Excel column names."
    )
  )
}


# ============================================================
# 4. Create row_id
# ============================================================

if (!("row_id" %in% names(raw_data))) {
  raw_data <- raw_data %>%
    mutate(row_id = row_number())
}


# ============================================================
# 5. Cleaning functions
# ============================================================

clean_keyword <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\s+", "") %>%
    str_replace_all("^[-–—]+", "") %>%
    str_replace_all("[-–—]+$", "") %>%
    str_trim()
}

clean_sdg <- function(x) {
  x %>%
    as.character() %>%
    str_to_upper() %>%
    str_replace_all("ＳＤＧ", "SDG") %>%
    str_replace_all("SDGS", "SDG") %>%
    str_replace_all("SDG\\s*", "SDG") %>%
    str_replace_all("GOAL\\s*", "SDG") %>%
    str_trim()
}


# ============================================================
# 6. Preserve wide-format data
# ============================================================

wide_data <- raw_data %>%
  mutate(
    keywords_original = .data[[col_keywords]],
    sdgs_original = .data[[col_sdg]]
  )


# ============================================================
# 7. Keyword long format
# Each row = one paragraph × one keyword
# ============================================================

keyword_long <- wide_data %>%
  mutate(
    keyword = as.character(.data[[col_keywords]])
  ) %>%
  separate_rows(
    keyword,
    sep = "；|;|、|，|,|\\n|\\r\\n|/"
  ) %>%
  mutate(
    keyword = clean_keyword(keyword)
  ) %>%
  filter(
    !is.na(keyword),
    keyword != "",
    keyword != "無",
    keyword != "NA",
    keyword != "N/A",
    keyword != "不適用"
  ) %>%
  distinct(row_id, keyword, .keep_all = TRUE)


# ============================================================
# 8. SDG × Keyword long format
# Each row = one paragraph × one SDG × one keyword
# ============================================================

keyword_sdg_long <- keyword_long %>%
  mutate(
    sdg = as.character(.data[[col_sdg]])
  ) %>%
  separate_rows(
    sdg,
    sep = "；|;|、|，|,|\\n|\\r\\n|/"
  ) %>%
  mutate(
    sdg = clean_sdg(sdg)
  ) %>%
  filter(
    !is.na(sdg),
    sdg != "",
    sdg != "無",
    sdg != "NA",
    sdg != "N/A",
    sdg != "不適用"
  ) %>%
  distinct(row_id, keyword, sdg, .keep_all = TRUE)


# ============================================================
# 9. Overall keyword frequency
# ============================================================

keyword_frequency <- keyword_long %>%
  count(keyword, name = "n") %>%
  mutate(
    percent = round(n / sum(n) * 100, 2)
  ) %>%
  arrange(desc(n), keyword) %>%
  mutate(rank = row_number()) %>%
  select(rank, keyword, n, percent)


# ============================================================
# 10. Main SDGs for each keyword
# ============================================================

keyword_main_sdg <- keyword_sdg_long %>%
  count(keyword, sdg, name = "n") %>%
  group_by(keyword) %>%
  arrange(desc(n), sdg, .by_group = TRUE) %>%
  summarise(
    main_sdgs = paste(head(sdg, 3), collapse = "；"),
    .groups = "drop"
  )

keyword_frequency_with_sdg <- keyword_frequency %>%
  left_join(keyword_main_sdg, by = "keyword") %>%
  select(rank, keyword, n, percent, main_sdgs)


# ============================================================
# 11. Top keywords by SDG
# Default: top 5 keywords for each SDG
# ============================================================

keyword_by_sdg <- keyword_sdg_long %>%
  count(sdg, keyword, name = "n") %>%
  group_by(sdg) %>%
  arrange(desc(n), keyword, .by_group = TRUE) %>%
  mutate(
    percent_within_sdg = round(n / sum(n) * 100, 2),
    rank_within_sdg = row_number()
  ) %>%
  ungroup()

keyword_by_sdg_top5 <- keyword_by_sdg %>%
  filter(rank_within_sdg <= 5)


# ============================================================
# 12. SDG × Keyword matrix
# Default: top 30 overall keywords
# ============================================================

top_n_keyword <- 30

top_keywords <- keyword_frequency %>%
  slice_head(n = top_n_keyword) %>%
  pull(keyword)

sdg_keyword_matrix <- keyword_sdg_long %>%
  filter(keyword %in% top_keywords) %>%
  count(sdg, keyword, name = "n") %>%
  pivot_wider(
    names_from = keyword,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(sdg)


# ============================================================
# 13. Keyword × disclosure level
# ============================================================

keyword_disclosure <- keyword_long %>%
  mutate(
    disclosure_level = as.character(.data[[col_disclosure]])
  ) %>%
  count(keyword, disclosure_level, name = "n") %>%
  group_by(keyword) %>%
  mutate(
    total = sum(n),
    percent = round(n / total * 100, 2)
  ) %>%
  ungroup() %>%
  arrange(desc(total), keyword, disclosure_level)


# ============================================================
# 14. Keyword × deviation status
# ============================================================

keyword_deviation <- keyword_long %>%
  mutate(
    has_deviation = as.character(.data[[col_deviation]])
  ) %>%
  count(keyword, has_deviation, name = "n") %>%
  group_by(keyword) %>%
  mutate(
    total = sum(n),
    percent = round(n / total * 100, 2)
  ) %>%
  ungroup() %>%
  arrange(desc(total), keyword, has_deviation)


# ============================================================
# 15. SDG × deviation status
# ============================================================

sdg_deviation <- keyword_sdg_long %>%
  mutate(
    has_deviation = as.character(.data[[col_deviation]])
  ) %>%
  count(sdg, has_deviation, name = "n") %>%
  group_by(sdg) %>%
  mutate(
    total = sum(n),
    percent = round(n / total * 100, 2)
  ) %>%
  ungroup() %>%
  arrange(sdg, has_deviation)


# ============================================================
# 16. School × keyword
# ============================================================

school_keyword <- keyword_long %>%
  count(
    school_name = .data[[col_school]],
    keyword,
    name = "n"
  ) %>%
  group_by(school_name) %>%
  arrange(desc(n), keyword, .by_group = TRUE) %>%
  mutate(
    percent_within_school = round(n / sum(n) * 100, 2),
    rank_within_school = row_number()
  ) %>%
  ungroup()

school_keyword_top5 <- school_keyword %>%
  filter(rank_within_school <= 5)


# ============================================================
# 17. School × SDG
# ============================================================

school_sdg <- keyword_sdg_long %>%
  count(
    school_name = .data[[col_school]],
    sdg,
    name = "n"
  ) %>%
  group_by(school_name) %>%
  arrange(desc(n), sdg, .by_group = TRUE) %>%
  mutate(
    percent_within_school = round(n / sum(n) * 100, 2)
  ) %>%
  ungroup()


# ============================================================
# 18. Deviation case summary
# ============================================================

deviation_cases <- keyword_sdg_long %>%
  filter(str_to_upper(as.character(.data[[col_deviation]])) == "YES") %>%
  select(
    row_id,
    school_name = all_of(col_school),
    page = all_of(col_page),
    sdg,
    keyword,
    text = all_of(col_text),
    disclosure_level = all_of(col_disclosure),
    has_deviation = all_of(col_deviation),
    deviation_reason = all_of(col_deviation_reason)
  ) %>%
  arrange(school_name, sdg, keyword)


# ============================================================
# 19. Export Excel workbook
# ============================================================

wb <- createWorkbook()

addWorksheet(wb, "01_wide_data")
writeData(wb, "01_wide_data", wide_data)

addWorksheet(wb, "02_keyword_long")
writeData(wb, "02_keyword_long", keyword_long)

addWorksheet(wb, "03_sdg_keyword_long")
writeData(wb, "03_sdg_keyword_long", keyword_sdg_long)

addWorksheet(wb, "04_keyword_frequency")
writeData(wb, "04_keyword_frequency", keyword_frequency_with_sdg)

addWorksheet(wb, "05_top_keywords_by_sdg")
writeData(wb, "05_top_keywords_by_sdg", keyword_by_sdg_top5)

addWorksheet(wb, "06_sdg_keyword_matrix")
writeData(wb, "06_sdg_keyword_matrix", sdg_keyword_matrix)

addWorksheet(wb, "07_keyword_disclosure")
writeData(wb, "07_keyword_disclosure", keyword_disclosure)

addWorksheet(wb, "08_keyword_deviation")
writeData(wb, "08_keyword_deviation", keyword_deviation)

addWorksheet(wb, "09_sdg_deviation")
writeData(wb, "09_sdg_deviation", sdg_deviation)

addWorksheet(wb, "10_top_keywords_by_school")
writeData(wb, "10_top_keywords_by_school", school_keyword_top5)

addWorksheet(wb, "11_school_sdg")
writeData(wb, "11_school_sdg", school_sdg)

addWorksheet(wb, "12_deviation_cases")
writeData(wb, "12_deviation_cases", deviation_cases)


# Basic formatting
header_style <- createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#1F4E79",
  halign = "center",
  valign = "center",
  textDecoration = "bold",
  border = "Bottom"
)

for (s in names(wb)) {
  addStyle(
    wb,
    sheet = s,
    style = header_style,
    rows = 1,
    cols = 1:50,
    gridExpand = TRUE
  )
  freezePane(wb, sheet = s, firstRow = TRUE)
  setColWidths(wb, sheet = s, cols = 1:50, widths = "auto")
}

saveWorkbook(wb, output_xlsx, overwrite = TRUE)


# ============================================================
# 20. Export UTF-8 BOM CSV files
# ============================================================

write_excel_csv(
  keyword_long,
  file.path(output_dir, "keyword_long_format_UTF8BOM.csv"),
  na = ""
)

write_excel_csv(
  keyword_sdg_long,
  file.path(output_dir, "sdg_keyword_long_format_UTF8BOM.csv"),
  na = ""
)

write_excel_csv(
  keyword_frequency_with_sdg,
  file.path(output_dir, "keyword_frequency_overall_UTF8BOM.csv"),
  na = ""
)

write_excel_csv(
  keyword_by_sdg_top5,
  file.path(output_dir, "top_keywords_by_sdg_UTF8BOM.csv"),
  na = ""
)

write_excel_csv(
  sdg_keyword_matrix,
  file.path(output_dir, "sdg_keyword_matrix_UTF8BOM.csv"),
  na = ""
)

write_excel_csv(
  keyword_disclosure,
  file.path(output_dir, "keyword_disclosure_level_UTF8BOM.csv"),
  na = ""
)

write_excel_csv(
  keyword_deviation,
  file.path(output_dir, "keyword_deviation_status_UTF8BOM.csv"),
  na = ""
)

write_excel_csv(
  sdg_deviation,
  file.path(output_dir, "sdg_deviation_status_UTF8BOM.csv"),
  na = ""
)

write_excel_csv(
  school_keyword_top5,
  file.path(output_dir, "top_keywords_by_school_UTF8BOM.csv"),
  na = ""
)

write_excel_csv(
  school_sdg,
  file.path(output_dir, "school_sdg_distribution_UTF8BOM.csv"),
  na = ""
)

write_excel_csv(
  deviation_cases,
  file.path(output_dir, "deviation_cases_summary_UTF8BOM.csv"),
  na = ""
)


# ============================================================
# 21. Completion message
# ============================================================

cat("\nData preparation completed.\n")
cat("Excel output: ", output_xlsx, "\n")
cat("CSV output folder: ", output_dir, "\n")
cat("Number of original rows: ", nrow(wide_data), "\n")
cat("Number of keyword-long rows: ", nrow(keyword_long), "\n")
cat("Number of SDG-keyword-long rows: ", nrow(keyword_sdg_long), "\n")
cat("Number of unique keywords: ", n_distinct(keyword_long$keyword), "\n")
cat("Number of unique SDGs: ", n_distinct(keyword_sdg_long$sdg), "\n")
