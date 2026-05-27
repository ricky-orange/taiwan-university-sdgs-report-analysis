import pandas as pd
import re
from sklearn.preprocessing import MultiLabelBinarizer

def main():
    # 1. 讀取 Excel 檔案
    input_file = "SDGs_List.xlsx"
    output_file = "SDGs_DV.xlsx"
    
    print(f"正在讀取檔案: {input_file} ...")
    try:
        df = pd.read_excel(input_file)
    except FileNotFoundError:
        print(f"錯誤：找不到檔案 '{input_file}'，請確認檔案位置。")
        return

    # 2. 定義標準 SDG 列表 (SDG1 ~ SDG17)
    # 這樣做的目的是確保不管資料有沒有出現 SDG17，輸出的表格都會有這個欄位
    sdg_standard_cols = [f'SDG{i}' for i in range(1, 18)]

    # 定義處理單一欄位的函數
    def process_sdg_column(source_df, col_name, prefix):
        """
        參數:
        source_df: 原始 DataFrame
        col_name: 要處理的欄位名稱 (如 'AI_list')
        prefix: 輸出欄位的前綴 (如 'AI')
        """
        
        # 資料清洗：處理 NaN 並進行切割
        # re.split(r'[;；]', x) 可以同時切割英文分號(;)與中文全形分號(；)
        # strip() 去除可能存在的空格
        cleaned_data = source_df[col_name].fillna('').astype(str).apply(
            lambda x: [item.strip() for item in re.split(r'[;；]', x) if item.strip()]
        )

        # 使用 sklearn 的 MultiLabelBinarizer 進行 One-Hot Encoding
        mlb = MultiLabelBinarizer()
        matrix = mlb.fit_transform(cleaned_data)
        
        # 建立暫時的 DataFrame
        temp_df = pd.DataFrame(matrix, columns=mlb.classes_)

        # 強制對齊標準 SDG1-17
        # 如果資料中有出現標準外的標籤(如 typos)，會被捨棄
        # 如果資料中缺少的標籤(如沒人選 SDG17)，會補 0
        temp_df = temp_df.reindex(columns=sdg_standard_cols, fill_value=0)

        # 重新命名欄位，加上前綴 (例如 AI_SDG1)
        temp_df.columns = [f"{prefix}_{col}" for col in temp_df.columns]
        
        return temp_df

    # 3. 執行轉換
    print("正在轉換 AI 判讀欄位...")
    df_ai = process_sdg_column(df, 'AI_list', 'AI')

    print("正在轉換人工判讀欄位...")
    df_my = process_sdg_column(df, 'My_list', 'My')

    # 4. 合併結果
    # 保留原始 ID，並接上處理後的兩個寬表
    df_final = pd.concat([df[['ID']], df_ai, df_my], axis=1)

    # 5. 輸出 Excel
    print(f"正在儲存檔案至: {output_file} ...")
    df_final.to_excel(output_file, index=False)
    
    print("完成！")
    print(f"總欄位數: {len(df_final.columns)} (預期應為 1 + 17 + 17 = 35 欄)")

if __name__ == "__main__":
    main()