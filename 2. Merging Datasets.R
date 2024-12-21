library(data.table) 
library(word2vec)
library(quanteda)
library(utils)
library(stringr)
library(dplyr)
library(tidyr)
library(readxl)

#TCPD DATASET
# Specify the path to the gzipped file
gzipped_file_path <- "path/TCPD_QH.tsv.gz"

# Read the decompressed file into a data frame
qh_dataframe <- read.table(gzipped_file_path, header = TRUE, sep = "\t")

#2019-2024
path_2019_24 <- "path/2019-2024.xlsx"
new_df<-read_excel(path_2019_24)
new_df<-new_df[,-1]

#MERGED FINAL DATA
final_df <- rbind(qh_dataframe,new_df)
colnames(final_df)
final_df <- final_df%>%
  mutate(full_text = paste(question_text, answer_text, subject, sep = " "))
#write.csv(final_df,"final_dataset.csv")