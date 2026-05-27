# ==========================================
# 設定區：載入套件
# ==========================================
required_packages <- c("readxl", "caret", "e1071", "openxlsx")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(readxl)
library(caret)
library(openxlsx)

# ==========================================
# 1. 讀取檔案
# ==========================================
print("請選取 Python 產出的 'SDGs_DV.xlsx' ...")
file_path <- file.choose() 
df <- read_excel(file_path)

# ==========================================
# 2. 定義一個函數來計算各項指標
# (這樣我們可以在迴圈用，也可以在最後算總和時重複用，不用寫兩次)
# ==========================================
calculate_metrics <- function(ai_vec, my_vec, label_name) {
  
  # 計算次數
  ai_count <- sum(ai_vec == 1, na.rm = TRUE)
  my_count <- sum(my_vec == 1, na.rm = TRUE)
  
  # 轉為因子 (確保 0 和 1 都存在)
  ai_factor <- factor(ai_vec, levels = c(0, 1))
  my_factor <- factor(my_vec, levels = c(0, 1))
  
  # 計算混淆矩陣
  cm <- confusionMatrix(ai_factor, my_factor, mode = "prec_recall", positive = "1")
  
  # 提取數值
  data.frame(
    SDG = label_name,
    AI_Count = ai_count,
    My_Count = my_count,
    Kappa = round(cm$overall["Kappa"], 4),
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
# 3. 開始迴圈：計算 SDG1 ~ SDG17
# ==========================================
results <- data.frame() # 建立空表格

# 為了確保計算總和時欄位對齊，我們先抓出所有相關欄位
all_ai_values <- c()
all_my_values <- c()

for (i in 1:17) {
  ai_col <- paste0("AI_SDG", i)
  my_col <- paste0("My_SDG", i)
  
  if (ai_col %in% colnames(df) && my_col %in% colnames(df)) {
    
    # 取出單一 SDG 的資料
    current_ai <- df[[ai_col]]
    current_my <- df[[my_col]]
    
    # 存入暫存區 (為了最後算 Overall)
    all_ai_values <- c(all_ai_values, current_ai)
    all_my_values <- c(all_my_values, current_my)
    
    # 計算該 SDG 指標並加入結果表
    row_data <- calculate_metrics(current_ai, current_my, paste0("SDG", i))
    results <- rbind(results, row_data)
  }
}

# ==========================================
# 4. 計算「Overall (整體總和)」
# ==========================================
print("正在計算 Overall 整體指標...")
overall_row <- calculate_metrics(all_ai_values, all_my_values, "Overall")

# 將 Overall 接在原本的結果下面
final_results <- rbind(results, overall_row)

# ==========================================
# 5. 輸出結果
# ==========================================
print("--- 分析完成，結果預覽 (最後一行為 Overall) ---")
print(tail(final_results)) # 印出最後幾行給你看

output_filename <- "SDGs_Kappa_Result_v4_Total.xlsx"
write.xlsx(final_results, output_filename)
print(paste("檔案已儲存為：", output_filename))