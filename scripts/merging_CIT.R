library(readxl)
library(dplyr)

safe_df_merge <- function(data_list) {
  # Step 1: Get all column names
  all_columns <- unique(unlist(lapply(data_list, names)))
  
  # Step 2: Detect conflicting column types
  col_classes <- lapply(all_columns, function(col) {
    sapply(data_list, function(df) {
      if (col %in% names(df)) class(df[[col]])[1] else NA
    })
  })
  names(col_classes) <- all_columns
  
  conflicted_cols <- names(Filter(function(classes) {
    length(unique(na.omit(classes))) > 1
  }, col_classes))
  
  # Step 3: Convert only conflicting columns to character
  data_list <- lapply(data_list, function(df) {
    for (col in conflicted_cols) {
      if (col %in% names(df)) {
        df[[col]] <- as.character(df[[col]])
      }
    }
    return(df)
  })
  
  # Step 4: Combine them safely
  dplyr::bind_rows(data_list)
}

#Reading files
Ibadan1 <- read_excel("./CIT 2023/CIT-Ibadan 2023/CIT 1 - 2023 _table.xlsx")
Bagauda1 <- read_excel("./CIT 2023/CIT-Bagauda 2023/2024-01-23-11-39-41_CIT 1_2023 trial-PLOTDATA_2_KN_dishaya_20230718_103453_table.xlsx")
Bauchi1 <- read_excel("./CIT 2023/CIT-Bauchi 2023/2023-10-10-01-02-32_CIT 1_2023 trial-PLOTDATA_4_NI06_dishaya_20230718_103453_table.xlsx")
Shika1 <- read_excel("./CIT 2023/CIT-Shika 2023/2024-01-29-10-52-38_CIT 1_2023 trial-PLOTDATA_1_IARC132_dishaya_20230718_103453_table.xlsx")
Wudil1 <- read_excel("./CIT 2023/CIT-Wudil 2023/2024-01-26-02-44-46_CIT 1_2023 trial-PLOTDATA_3_IARC71_dishaya_20230718_103453_table.xlsx")

#combines the dataframe into a list
list_1<- list(Ibadan1,Bagauda1,Bauchi1,Shika1,Wudil1)
CIT1 <- safe_df_merge(list_1) #calls the function that merges 

#Reading files
Ibadan2 <- read_excel("./CIT 2023/CIT-Ibadan 2023/CIT 2 - 2023 _table.xlsx")
Bagauda2 <- read_excel("./CIT 2023/CIT-Bagauda 2023/2024-01-23-11-40-53_CIT2_2023 trial-PLOTDATA_2_KN_dishaya_20230718_093541_table.xlsx")
Bauchi2 <- read_excel("./CIT 2023/CIT-Bauchi 2023/2023-10-10-01-04-55_CIT2_2023 trial-PLOTDATA_4_NI06_dishaya_20230718_093541_table.xlsx")
Shika2 <- read_excel("./CIT 2023/CIT-Shika 2023/2024-01-29-10-54-51_CIT2_2023 trial-PLOTDATA_1_IARC132_dishaya_20230718_093541_table.xlsx")
Wudil2 <- read_excel("./CIT 2023/CIT-Wudil 2023/2024-01-26-02-46-08_CIT2_2023 trial-PLOTDATA_3_IARC71_dishaya_20230718_093541_table.xlsx")

#combines the dataframe into a list
list_2<- list(Ibadan2,Bagauda2,Bauchi2,Shika2,Wudil2)
CIT2 <- safe_df_merge(list_2) #calls the function that merges

#Reading files
Ibadan3 <- read_excel("./CIT 2023/CIT-Ibadan 2023/CIT 2 - 2023 _table.xlsx")
Bagauda3 <- read_excel("./CIT 2023/CIT-Bagauda 2023/2024-01-23-11-39-58_CIT 3_2023 trial-PLOTDATA_2_KN_dishaya_20230718_094519_table.xlsx")
Bauchi3 <- read_excel("./CIT 2023/CIT-Bauchi 2023/2023-10-10-01-02-53_CIT 3_2023 trial-PLOTDATA_4_NI06_dishaya_20230718_094519_table.xlsx")
Shika3 <- read_excel("./CIT 2023/CIT-Shika 2023/2024-01-29-10-53-02_CIT 3_2023 trial-PLOTDATA_1_IARC132_dishaya_20230718_094519_table.xlsx")
Wudil3 <- read_excel("./CIT 2023/CIT-Wudil 2023/2024-01-26-02-45-10_CIT 3_2023 trial-PLOTDATA_3_IARC71_dishaya_20230718_094519_table.xlsx")

#combines the dataframe into a list
list_3<- list(Ibadan3,Bagauda3,Bauchi3,Shika3,Wudil3)
CIT3 <- safe_df_merge(list_3) #calls the function that merges

#Reading files
Ibadan4 <- read_excel("./CIT 2023/CIT-Ibadan 2023/CIT 4 - 2023 _table.xlsx")
Bagauda4 <- read_excel("./CIT 2023/CIT-Bagauda 2023/2024-01-23-11-40-14_CIT 4_2023 trial-PLOTDATA_2_KN_dishaya_20230718_095419_table.xlsx")
Bauchi4 <- read_excel("./CIT 2023/CIT-Bauchi 2023/2023-10-10-01-03-22_CIT 4_2023 trial-PLOTDATA_4_NI06_dishaya_20230718_095419_table.xlsx")
Shika4 <- read_excel("./CIT 2023/CIT-Shika 2023/2024-01-29-10-53-29_CIT 4_2023 trial-PLOTDATA_1_IARC132_dishaya_20230718_095419_table.xlsx")
Wudil4 <- read_excel("./CIT 2023/CIT-Wudil 2023/2024-01-26-02-45-28_CIT 4_2023 trial-PLOTDATA_3_IARC71_dishaya_20230718_095419_table.xlsx")

#combines the dataframe into a list
list_4<- list(Ibadan4,Bagauda4,Bauchi4,Shika4,Wudil4)
CIT4 <- safe_df_merge(list_4) #calls the function that merges

#Reading files
Ibadan5 <- read_excel("./CIT 2023/CIT-Ibadan 2023/CIT 5 - 2023 _table.xlsx")
Bagauda5 <- read_excel("./CIT 2023/CIT-Bagauda 2023/2024-01-23-11-40-31_CIT 5_2023 trial-PLOTDATA_2_KN_dishaya_20230718_095903_table.xlsx")
Bauchi5 <- read_excel("./CIT 2023/CIT-Bauchi 2023/2023-10-10-01-04-04_CIT 5_2023 trial-PLOTDATA_4_NI06_dishaya_20230718_095903_table.xlsx")
Shika5 <- read_excel("./CIT 2023/CIT-Shika 2023/2024-01-29-10-54-10_CIT 5_2023 trial-PLOTDATA_1_IARC132_dishaya_20230718_095903_table.xlsx")
Wudil5 <- read_excel("./CIT 2023/CIT-Wudil 2023/2024-01-26-02-45-48_CIT 5_2023 trial-PLOTDATA_3_IARC71_dishaya_20230718_095903_table.xlsx")

#combines the dataframe into a list
list_5<- list(Ibadan5,Bagauda5,Bauchi5,Shika5,Wudil5)
CIT5 <- safe_df_merge(list_5) #calls the function that merges


#Writing to files
library(writexl)

writing_list <- list(CIT1, CIT2, CIT3, CIT4, CIT5)

file_names <- c("CIT1_2023_merged.csv", "CIT2_2023_merged.csv", "CIT3_2023_merged.csv", "CIT4_2023_merged.csv", "CIT5_2023_merged.csv")
for (i in seq_along(writing_list)) {
  write.csv(writing_list[[i]], file = file_names[i], row.names = FALSE)
}
