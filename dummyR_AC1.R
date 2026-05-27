# ==========================================
# 設定區：載入套件
# ==========================================

# 需要使用的套件
# readxl：讀取 Excel
# caret：計算 Kappa、Accuracy、Sensitivity、Specificity
# openxlsx：輸出 Excel
# irrCAC：計算 Gwet's AC1

required_packages <- c("readxl", "caret", "e1071", "openxlsx", "irrCAC")

# 檢查尚未安裝的套件，若沒有安裝則自動安裝
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) install.packages(new_packages)

# 載入套件
library(readxl)
library(caret)
library(openxlsx)
library(irrCAC)


# ==========================================
# 1. 讀取檔案
# ==========================================

print("請選取原始資料檔案：SDGs_DV2.xlsx ...")

# 手動選擇 Excel 檔案
file_path <- file.choose()

# 讀取 Excel
df <- read_excel(file_path)

# 檢查欄位名稱
print("資料欄位如下：")
print(colnames(df))


# ==========================================
# 2. 定義函數：計算 Kappa、AC1 與其他指標
# ==========================================

calculate_metrics <- function(ai_vec, my_vec, label_name) {
  
  # 轉成數值，避免 Excel 讀進來變成文字
  ai_vec <- as.numeric(ai_vec)
  my_vec <- as.numeric(my_vec)
  
  # 計算 AI 與人工判讀為 1 的次數
  ai_count <- sum(ai_vec == 1, na.rm = TRUE)
  my_count <- sum(my_vec == 1, na.rm = TRUE)
  
  # 將資料轉成 factor
  # levels = c(0, 1) 是為了確保即使某個 SDG 全部都是 0，也仍保留 0 和 1 兩類
  ai_factor <- factor(ai_vec, levels = c(0, 1))
  my_factor <- factor(my_vec, levels = c(0, 1))
  
  # ==========================================
  # 2-1. 計算 Kappa 與分類指標
  # ==========================================
  
  cm <- confusionMatrix(
    ai_factor,
    my_factor,
    mode = "prec_recall",
    positive = "1"
  )
  
  # ==========================================
  # 2-2. 計算 Gwet's AC1
  # ==========================================
  
  # irrCAC 套件需要資料格式為：
  # 每一欄是一位評分者
  # 每一列是一筆被評分資料
  ratings <- data.frame(
    AI = ai_vec,
    Human = my_vec
  )
  
  ac1_result <- gwet.ac1.raw(ratings)
  
  # ==========================================
  # 2-3. 整理結果
  # ==========================================
  
  data.frame(
    SDG = label_name,
    
    # 次數
    AI_Count = ai_count,
    My_Count = my_count,
    
    # 原本 Kappa 指標：保留
    Kappa = round(cm$overall["Kappa"], 4),
    
    # 新增 Gwet's AC1：主要建議使用指標
    AC1 = round(ac1_result$est$coeff.val[1], 4),
    
    # AC1 的輔助資訊
    Agreement = round(ac1_result$est$pa[1], 4),
    Chance_Agreement = round(ac1_result$est$pe[1], 4),
    
    # 原本 confusionMatrix 的其他指標
    Accuracy = round(cm$overall["Accuracy"], 4),
    Sensitivity = round(cm$byClass["Sensitivity"], 4),
    Specificity = round(cm$byClass["Specificity"], 4),
    Accuracy_Lower = round(cm$overall["AccuracyLower"], 4),
    Accuracy_Upper = round(cm$overall["AccuracyUpper"], 4),
    P_Value = round(cm$overall["AccuracyPValue"], 5),
    
    stringsAsFactors = FALSE
  )
}


# ==========================================
# 3. 開始計算 SDG1 ~ SDG17
# ==========================================

results <- data.frame()

# 用來累積所有 SDG 的 AI 與人工判讀結果
# 最後用於計算 Overall
all_ai_values <- c()
all_my_values <- c()

for (i in 1:17) {
  
  # 欄位名稱，例如 AI_SDG1、My_SDG1
  ai_col <- paste0("AI_SDG", i)
  my_col <- paste0("My_SDG", i)
  
  # 檢查欄位是否存在
  if (ai_col %in% colnames(df) && my_col %in% colnames(df)) {
    
    print(paste("正在計算：SDG", i))
    
    # 取出目前 SDG 的 AI 與人工判讀資料
    current_ai <- df[[ai_col]]
    current_my <- df[[my_col]]
    
    # 累積到 Overall 使用
    all_ai_values <- c(all_ai_values, current_ai)
    all_my_values <- c(all_my_values, current_my)
    
    # 計算目前 SDG 的指標
    row_data <- calculate_metrics(
      current_ai,
      current_my,
      paste0("SDG", i)
    )
    
    # 加入結果表
    results <- rbind(results, row_data)
    
  } else {
    
    print(paste("警告：找不到欄位", ai_col, "或", my_col))
  }
}


# ==========================================
# 4. 計算 Overall 整體指標
# ==========================================

print("正在計算 Overall 整體指標...")

overall_row <- calculate_metrics(
  all_ai_values,
  all_my_values,
  "Overall"
)

# 將 Overall 接在 SDG1 ~ SDG17 後面
final_results <- rbind(results, overall_row)


# ==========================================
# 5. 顯示結果
# ==========================================

print("--- 分析完成，結果預覽 ---")
print(final_results)


# ==========================================
# 6. 輸出 Excel
# ==========================================

output_filename <- "SDGs_Kappa_AC1_Result_Human_AI.xlsx"

write.xlsx(final_results, output_filename)

print(paste("檔案已儲存為：", output_filename))