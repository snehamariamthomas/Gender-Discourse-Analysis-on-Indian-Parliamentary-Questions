#PDF ONLINE LINK
library(XML)
library(RCurl)
library(pdftools)
library(stringr)
library(tesseract)
library(httr)
library(png)

# CREATE INDIVIDUAL LIST OF STARRED-UNSTARRED PER SESSION IN 17th LS:
# Session No:Total Questions
#  15:      
#  14:
#  13:
#  12:
#  11:
#  10:
#   9:
#   8:
#   7:
#   6:
#   5:
#   4:
#   3:
#   2:
#   1:

#This is done as links of accessing PDF is separate for unstarred and starred questions
# Remember to change the code at following lines based on what you are updating:
# 1. Initialisation of List: Name
# 2. Iterable based on no of questions
# 3. URL link "...annex/17../A(S|U)
# 4. Creation of Variable 'Field ID' when list is created

# Initialize list to store the extracted information
questions_17_1_AS <- list()

for (i in 1:500){
  url <- paste0("https://sansad.in/getFile/loksabhaquestions/annex/171/AS", i, ".pdf?source=pqals")
  
  # Use tryCatch to handle errors
  result <- tryCatch({
    text <- pdf_text(url)
    full_text <- paste(text, collapse = "\n")
    
    # Remove Hindi characters
    pattern <- "[\u0900-\u097F]+"
    full_text <- str_replace_all(full_text, pattern, "")
    
    # Extract Ministry
    pattern <- "\\bWill\\s+the\\s+Minister\\s+of\\s+[^:]+:"
    extracted_line <- regmatches(full_text, regexpr(pattern, full_text, perl = TRUE))
    if (length(extracted_line) > 0) {
      pattern <- "(?i)\\bMinister\\s*of\\s*[^:]+\\b.*?(?=\\bbe\\b)"
      extracted_substring <- regmatches(extracted_line, regexpr(pattern, extracted_line, perl = TRUE))
      ministry <- if (length(extracted_substring) > 0) extracted_substring else NA
    } else {
      ministry <- NA
    }
    
    # Extract Question Number
    pattern <- "(?i)QUESTION NO\\.?\\s?\\*?\\-?(\\d+)"
    question_number_match <- regmatches(full_text, regexec(pattern, full_text))
    question_number <- if (length(question_number_match[[1]]) > 1) question_number_match[[1]][2] else NA
    
    # Extract Member who asked the question
    if (!is.na(question_number)) {
      pattern_member <- paste0("\\*?\\s*", question_number, "\\.?\\s*(?i:s|d.*?)\\s*.*?(?i:will)")
      extracted_text <- regmatches(full_text, regexec(pattern_member, full_text, ignore.case = TRUE))
      if (length(extracted_text[[1]]) > 0) {
        matched_substring <- extracted_text[[1]][1]
        cleaned_text <- gsub("\\*|\\d+|\\.|\\n|Will|\\:", "", matched_substring)
        cleaned_text <- gsub("(SHRI|SHRIMATI|KUMARI|DR)\\s*", ", \\1 ", cleaned_text, ignore.case = FALSE)
        cleaned_text <- gsub("^\\s*,\\s*", "", cleaned_text)
        cleaned_text <- gsub("\\s*,", ",", cleaned_text)
        cleaned_text <- gsub("\\s+$", "", cleaned_text)
        member_details <- cleaned_text
      } else {
        member_details <- NA
      }
    } else {
      member_details <- NA
    }
    
    # Extract Question
    pattern <- "Will\\s+the\\s+Minister\\s+of\\s+[^:]+:.*?(\\R|.)*?ANSWER"
    matches <- regmatches(full_text, regexpr(pattern, full_text, perl = TRUE))
    if (length(matches) > 0) {
      matches <- gsub("\n", "", matches)
      matches <- gsub("ANSWER$", "", matches)
      matches <- trimws(matches)
      question <- matches
    } else {
      question <- NA
    }
    
    # Extract Answer
    pattern <- "(?s)\\bANSWER\\b.+"
    start_index <- grep("\\bANSWER\\b", full_text)
    if (length(start_index) > 0) {
      combined_text <- paste(full_text[start_index:length(full_text)], collapse = "")
      matches <- regmatches(combined_text, regexpr(pattern, combined_text, perl = TRUE))
      if (length(matches) > 0) {
        cleaned_matches <- gsub("\\n", "", matches)
        cleaned_matches <- gsub("ANSWER.*?\\)", "", cleaned_matches)
        answer <- cleaned_matches
      } else {
        answer <- NA
      }
    } else {
      answer <- NA
    }
    
    list(
      question_id = paste0("171AS:", i),
      quest_num = question_number,
      ministry = ministry,
      member = member_details,
      question = question,
      answer = answer
    )
  }, error = function(e) {
    # On error, return NAs for all fields except question_id
    list(
      question_id = paste0("171AS:", i),
      quest_num = NA,
      ministry = NA,
      member = NA,
      question = NA,
      answer = NA
    )
  })
  
  # Store the result in the list
  questions_17_1_AS[[i]] <- result
}


questions_list_17_1_AS <- do.call(rbind, lapply(questions_17_1_AS, as.data.frame)) 
  
#MERGING ALL DATA

final_data_frames <- list(questions_list_17_15_AS, questions_list_17_15_AU,
                    questions_list_17_14_AS, questions_list_17_14_AU,
                    questions_list_17_12_AS, questions_list_17_12_AU,
                    questions_list_17_11_AS, questions_list_17_11_AU,
                    questions_list_17_10_AS, questions_list_17_10_AU,
                    questions_list_17_9_AS, questions_list_17_9_AU,
                    questions_list_17_8_AS, questions_list_17_8_AU,
                    questions_list_17_7_AS, questions_list_17_7_AU,
                    questions_list_17_6_AS, questions_list_17_6_AU,
                    questions_list_17_5_AS, questions_list_17_5_AU,
                    questions_list_17_4_AU,
                    questions_list_17_3_AS, questions_list_17_3_AU,
                    questions_list_17_2_AS, questions_list_17_2_AU,
                    questions_list_17_1_AS, questions_list_17_1_AU)

# Combine all data frames into one
combined_df <- do.call(rbind, final_data_frames)
#write.csv(combined_df, "17LS_data.csv")


## HANDLING: Missing Vales, Text extraction from Image

####ALL COLUMNS MISSING: RECHECKING######
# Extract the question_id values for all columns NA

na_rows <- which(rowSums(is.na(combined_df[, -which(names(combined_df) == "question_id")])) == ncol(combined_df) - 1)
question_ids <- combined_df$question_id[na_rows]
#CREATING LINK AGAIN
create_link <- function(question_id) {
  # Split the question_id at the colon
  parts <- strsplit(question_id, ":")[[1]]
  
  # Extract the base and the number part
  base <- parts[1]
  number <- parts[2]
  
  # Determine the AS/AU part
  if (grepl("AS", base)) {
    type <- "AS"
  } else if (grepl("AU", base)) {
    type <- "AU"
  } else {
    stop("Unexpected format in question_id")
  }
  
  # Extract the part before AS or AU
  prefix <- sub(type, "", base)
  
  # Construct the link with '/' before AS or AU
  link <- paste0("https://sansad.in/getFile/loksabhaquestions/annex/", prefix, "/", type, number, ".pdf?source=pqals")
  
  return(link)
}

nan_rows_links <- list()
for (question_id in question_ids){
  link <- create_link(question_id)
  nan_rows_links <- c(nan_rows_links, list(link))
}
print(nan_rows_links)

# EXTRACTING TEXT
extract_text_from_pdf <- function(pdf_url) {
  pdf_text <- tryCatch(
    {
      text <- pdf_text(pdf_url)
      text
    },
    error = function(e) {
      print(paste("Error reading PDF from URL:", pdf_url))
      return(NULL)
    }
  )
  return(pdf_text)
}

extracted_text <- list()

# Iterate over each link in nan_rows_links
for (link in nan_rows_links) {
  # Extract text from the PDF
  text <- extract_text_from_pdf(link)
  
  # Append extracted text to the list
  extracted_text <- c(extracted_text, list(text))
}

extracted_text_nan <- data.frame(
  link = unlist(nan_rows_links),
  text = sapply(extracted_text, function(text) {
    if (is.null(text)) {
      return(NA)
    } else {
      return(paste(unlist(text), collapse = " "))
    }
  }),
  stringsAsFactors = FALSE
)

View(extracted_text_nan)

# Iterate over the rows of the data frame
question_ids_with_na <- c()

# Iterate over the rows of the data frame
for (i in seq_len(nrow(combined_df))) {
  # Check if the member column value is NA
  if (is.na(combined_df$member[i])) {
    # Append the corresponding question_id to the vector
    question_ids_with_na <- c(question_ids_with_na, combined_df$question_id[i])
  }
}

#Removing Those questions which have not been found/ Error in connecting
extracted_text_df <- extracted_text_nan[!is.na(extracted_text_nan$text), ]
extracted_text_df$text <- trimws(extracted_text_df$text)
devanagari_pattern <- "[\u0900-\u097F]"

# Filter out rows containing Devanagari script characters
corrected_text_df <- extracted_text_df[!str_detect(extracted_text_df$text, devanagari_pattern), ]
corrected_text_df$text <- trimws(corrected_text_df$text)



#PDF AS IMAGES

empty_text_rows <- corrected_text_df[corrected_text_df$text == "", ]
# Function to perform OCR on image
perform_ocr <- function(image_path) {
  # Create a tesseract object
  tess <- tesseract()
  
  # Use OCR on the image
  ocr_text <- ocr(image_path, engine = tess)
  
  return(ocr_text)
  Sys.sleep(2)
}

# Function to download PDF, convert to images, and perform OCR
perform_ocr_from_pdf <- function(pdf_url) {
  # Download PDF file
  temp_pdf <- tempfile(fileext = ".pdf")
  download.file(pdf_url, temp_pdf, mode = "wb")
  
  # Convert PDF to images
  pdf_images <- pdf_convert(temp_pdf, format = "png")
  
  # Perform OCR on each image and concatenate the text
  ocr_text <- sapply(pdf_images, perform_ocr)
  
  return(paste(ocr_text, collapse = "\n"))
  
}

# Iterate through rows of empty_text_rows
for (i in seq(from = 35, to = nrow(empty_text_rows))) {
  link <- empty_text_rows[i, "link"]
  retries <- 3  # Number of retry attempts
  attempt <- 0
  
  while (attempt < retries) {
    attempt <- attempt + 1
    tryCatch({
      extracted_text <- perform_ocr_from_pdf(link)
      empty_text_rows[i, "extracted_text"] <- extracted_text
      Sys.sleep(5)
      break  # Break out of the retry loop if successful
    }, error = function(e) {
      message(paste("Error occurred for link:", link, "- Attempt", attempt))
      Sys.sleep(10)  # Wait before retrying
      if (attempt == retries) {
        empty_text_rows[i, "extracted_text"] <- NA
        message(paste("Maximum retry attempts reached for link:", link))
      }
    })
  }
}


empty_text_rows<- read.csv("C:/Users/sneha/OneDrive/Desktop/Dissertation/imagepdf.csv")
if (!"question_id" %in% colnames(empty_text_rows)) {
  empty_text_rows$question_id <- NA
}
if (!"quest_num" %in% colnames(empty_text_rows)) {
  empty_text_rows$quest_num <- NA
}
if (!"ministry" %in% colnames(empty_text_rows)) {
  empty_text_rows$ministry <- NA
}
if (!"member" %in% colnames(empty_text_rows)) {
  empty_text_rows$member <- NA
}
if (!"question" %in% colnames(empty_text_rows)) {
  empty_text_rows$question <- NA
}
if (!"answer" %in% colnames(empty_text_rows)) {
  empty_text_rows$answer <- NA
}

# Define the function to process each row
process_row <- function(extracted_text) {
  if (is.na(extracted_text)) {
    return(list(
      quest_num = NA,
      ministry = NA,
      member = NA,
      question = NA,
      answer = NA
    ))
  }
  
  # Remove Hindi characters
  pattern <- "[\u0900-\u097F]+"
  extracted_text <- gsub(pattern, "", extracted_text)
  
  # Extract Ministry
  pattern <- "Will\\s+the\\s+Minister\\s+of\\s+[^:]+:"
  extracted_line <- regmatches(extracted_text, regexpr(pattern, extracted_text, perl = TRUE))
  if (length(extracted_line) > 0) {
    pattern <- "(?i)\\bMinister\\s*of\\s*[^:]+\\b.*?(?=\\bbe\\b)"
    extracted_substring <- regmatches(extracted_line, regexpr(pattern, extracted_line, perl = TRUE))
    ministry <- if (length(extracted_substring) > 0) extracted_substring else NA
  } else {
    ministry <- NA
  }
  
  # Extract Question Number
  pattern <- "(?i)QUESTION NO\\.?\\s?\\*?\\-?(\\d+)"
  question_number_match <- regmatches(extracted_text, regexec(pattern, extracted_text))
  question_number <- if (length(question_number_match[[1]]) > 1) question_number_match[[1]][2] else NA
  
  # Extract Member who asked the question
  if (!is.na(question_number)) {
    pattern_member <- paste0("\\*?\\s*", question_number, "\\.?\\s*(?i:s|d.*?)\\s*.*?(?i:will)")
    extracted_text_member <- regmatches(extracted_text, regexec(pattern_member, extracted_text, ignore.case = TRUE))
    if (length(extracted_text_member[[1]]) > 0) {
      matched_substring <- extracted_text_member[[1]][1]
      cleaned_text <- gsub("\\*|\\d+|\\.|\\n|Will|\\:", "", matched_substring)
      cleaned_text <- gsub("(SHRI|SHRIMATI|KUMARI|DR)\\s*", ", \\1 ", cleaned_text, ignore.case = FALSE)
      cleaned_text <- gsub("^\\s*,\\s*", "", cleaned_text)
      cleaned_text <- gsub("\\s*,", ",", cleaned_text)
      cleaned_text <- gsub("\\s+$", "", cleaned_text)
      member_details <- cleaned_text
    } else {
      member_details <- NA
    }
  } else {
    member_details <- NA
  }
  
  # Extract Question
  pattern <- "Will\\s+the\\s+Minister\\s+of\\s+[^:]+:.*?(\\R|.)*?ANSWER"
  matches <- regmatches(extracted_text, regexpr(pattern, extracted_text, perl = TRUE))
  if (length(matches) > 0) {
    matches <- gsub("\n", "", matches)
    matches <- gsub("ANSWER$", "", matches)
    matches <- trimws(matches)
    question <- matches
  } else {
    question <- NA
  }
  
  # Extract Answer
  pattern <- "(?s)\\bANSWER\\b.+"
  start_index <- grep("\\bANSWER\\b", extracted_text)
  if (length(start_index) > 0) {
    combined_text <- paste(extracted_text[start_index:length(extracted_text)], collapse = "")
    matches <- regmatches(combined_text, regexpr(pattern, combined_text, perl = TRUE))
    if (length(matches) > 0) {
      cleaned_matches <- gsub("\\n", "", matches)
      cleaned_matches <- gsub("ANSWER.*?\\)", "", cleaned_matches)
      answer <- cleaned_matches
    } else {
      answer <- NA
    }
  } else {
    answer <- NA
  }
  
  list(
    quest_num = question_number,
    ministry = ministry,
    member = member_details,
    question = question,
    answer = answer
  )
}

# Extract the question ID from the link
extract_question_id <- function(link) {
  pattern <- "annex/(\\d+)/([A-Z]+\\d+)\\.pdf"
  matches <- regmatches(link, regexec(pattern, link))
  if (length(matches[[1]]) > 2) {
    question_id <- paste0(matches[[1]][2], ":", matches[[1]][3])
    return(question_id)
  } else {
    return(NA)
  }
}


for (i in 1:nrow(empty_text_rows)) {
  extracted_text <- empty_text_rows$extracted_text[i]
  link <- empty_text_rows$link[i]
  
  empty_text_rows$question_id[i] <- extract_question_id(link)
  
  result <- process_row(extracted_text)
  
  empty_text_rows$quest_num[i] <- result$quest_num
  empty_text_rows$ministry[i] <- result$ministry
  empty_text_rows$member[i] <- result$member
  empty_text_rows$question[i] <- result$question
  empty_text_rows$answer[i] <- result$answer
}


#Filling the Missing Ministry
for (i in which(is.na(empty_text_rows$ministry))) {
  extracted_text <- empty_text_rows$extracted_text[i]
  
  # Split the extracted text into lines
  text_lines <- unlist(strsplit(extracted_text, "\n"))
  
  # Search for "Ministry of..." in the first 10 lines
  for (line in text_lines[1:10]) {
    if (grepl("(?i)Ministry\\s+of", line)) {
      # Extract the ministry information until newline character
      ministry <- sub("(?i)(^.*?Ministry\\s+of\\s.*?)\\s*\n.*", "\\1", line)
      
      # Update the dataframe
      empty_text_rows$ministry[i] <- ministry
      break  # Stop searching if found
    }
  }
}

empty_text_rows$member[255] <- "SHRI Sunil Kumar Mondal"
empty_text_rows$member[257] <- "SHRI ADHIKARI DPEPAK (DEY)"
empty_text_rows$member[346] <- "SHRI NABA KUMAR SARANIA"
empty_text_rows$member[367] <- "SHRI K. NAVASKANE "
empty_text_rows$member[376]<- " SHRI NALIN KUMAR KATEEL"
empty_text_rows$member[394]<- "SHRI VISHNU DAYAL RAM"
empty_text_rows$member[395]<- "SHRI RODNAL NAGAR, SHR ANIL FIROAIVA, SHRI DHARAMBIR SINGH,SHRI CHANDRA PRAKASH JOSH, SHRIMATI HALA ROY"
empty_text_rows$member[421]<- "KUNWAR DANISH ALE"
empty_text_rows$member[450] <- "SHRI SUDHEER GUPTA,SHRI RAVIKISHAN,SUBRAT PATHAK, SHRI RAVINDRA KUSHWAHA, SHRI SHRIRANGAPPA BARNE, SHRI MANOJ TIWVARI, SHRI PRATAPRAO JADHAV, SHRI BIDYUT BARAN MAHATO, SHRI DHAIRVASHEEL SAMBHAJIRAO MANE, SHRI SANJAY SADASIVRAO MANDLIK"
empty_text_rows$member[31] <- "MS S.JOTHIMAN"
empty_text_rows$member[476] <- "DR.SHASHI THAROOR"
empty_text_rows$member[481] <- " SHRI LORHO S.PFOZE"
empty_text_rows$member[541]<- "Parvesh Sahib Singh Verma"
empty_text_rows$member[541]<- "Shri Parvesh Sahib Singh Verma"
empty_text_rows$member[550]<- "SHRI ABDUL KHALEQUE"
empty_text_rows$member[603]<- "Shrimati Saugata Roy, Shri Tholkappiyan Thirumavalavan "
empty_text_rows$member[775]<-"SHRI RAjU BISTA"
empty_text_rows$member[895] <- " SHRIMATI GEETA KORA"
empty_text_rows$member[952] <- "SHRI KUNWAR DANISH ALI"
empty_text_rows$member[977] <-"SHRI HANUMAN BENIWAL"
empty_text_rows$member[980] <-"SHRIMATI RITA BAHUGUNA JOSHI"
empty_text_rows$member[1037] <- "Shrimati Saugata Roy"
empty_text_rows$member[1058]<- "SHRI V.K. SREEKANDAN"
empty_text_rows$member[1059]<- "SHRI MOHAMMED BASHEER"
empty_text_rows$member[1135]<- "DR NISHIKANT DUBEY"
empty_text_rows$member[1139]<-"SHRI GIRISH CHANDRA"
empty_text_rows$member[1167]<- "SHRI M.K.RAGHAVAN" 
empty_text_rows$member[1184]<-"SHRI Nitesh Ganga Deb, SHRI KAUSHALENDRA KUMAR,SHRI Rajiv Ranjan Singh alias Lalan Singh"
empty_text_rows$member[1191]<- "SHRI SUKHBIR SINGH JAUNAPURIA"
empty_text_rows$member[1253]<- "SHRI TALARI RANGAIAH"
empty_text_rows$member[1276]<- "SHRI LAVU SRI KRISHNA DEVARAYALU"
empty_text_rows$member[1288]<- "SHRI ET MOHAMMED BASHEER,DR. NISHIKANT DUBEY"
empty_text_rows$member[1343]<-"SHRIMATI MANEKA SANJAY GANDHI"
empty_text_rows$member[1449]<-"SHRI M.K.RAGHAVAN "
empty_text_rows$member[1511]<- " SADHVI PRAGYA SINGH THAKUR"
empty_text_rows$member[1540]<- "SHRI Naranbhai Bhikhabhai Kachhadiya,SHRI BHAGIRATH CHOUDHARY,Shri Parbatbhai Savabhai Patel "
empty_text_rows$member[1584]<- "SHRI TALARI RANGAIAH "
empty_text_rows$member[1604]<-"Shrimati Saugata Roy"
empty_text_rows$member[1615]<- " KUNWAR DANISH ALI"
empty_text_rows$member[1615]<- "SHRI KUNWAR DANISH ALI"
empty_text_rows$member[1669]<- "SHRI PARVESH SAHIB SINGH VERMA"
empty_text_rows$member[1671]<-"SHRIMATI JASKAUR MEENA"
empty_text_rows$member[1697]<-"SHRI VISHNU DUTT SHARMA"
empty_text_rows$member[1758] <- "DR. SANJEEV KUMAR SINGARI "
empty_text_rows$member[1767] <-"SHRI DHAL SINGH BISEN"
empty_text_rows$member[1814] <-"SHRIMATI Nusrat Jahan Ruhi"
empty_text_rows$member[1816] <- "SHRI Gaurav Gogoi"
empty_text_rows$member[1823] <- "Shrimati Saugata Roy"
empty_text_rows$member[1829]<- "COL. RAJYAVARDHAN RATHORE"
empty_text_rows$member[1844]<-" SHRI Manish Tewari"
empty_text_rows$member[1853]<-"SHRI K. Shanmuga Sundaram"
empty_text_rows$member[1875]<-"SHRI VINCENT H. PALA"
empty_text_rows$member[1899]<-"SHRI SANJAY JAISWAL"
empty_text_rows$member[1915]<-"SADHVI PRAGYA SINGH THAKUR"
empty_text_rows$member[1944]<-"SHRI K. Shanmuga Sundaram"
empty_text_rows$member[1970]<-"SHRI PRASUN BANERJEE "
empty_text_rows$member[1985]<- "SHRI Rajiv Pratap Rudy"
empty_text_rows$member[2030]<-"SHRI P. R. Natarajan"
empty_text_rows$member[2099]<-"SHRI K. Shanmuga Sundaram"
empty_text_rows$member[2192]<-"SHRIMATI POONAM MAHAJAN"
empty_text_rows$member[2195]<-"SHRIMATI CHINTA ANURADHA"
empty_text_rows$member[2195]<-"SHRI Pradyut Bordoloi"
empty_text_rows$member[2195]<-"SHRIMATI CHINTA ANURADHA"
empty_text_rows$member[2208]<-"SHRI Pradyut Bordoloi"
empty_text_rows$member[2273]<-"SHRIMATI Rita Bahuguna Joshi"
empty_text_rows$member[2306]<-"Shri Ramesh Bidhuri"
empty_text_rows$member[2372]<-"SHRI Sanjay Jaiswal"
empty_text_rows$member[2464]<-"SHRI S. R. Parthiban"
empty_text_rows$member[2498]<-"SHRI V. Vaithilingam"
empty_text_rows$member[2503]<-"COL. RAJYAVARDHAN RATHORE"
empty_text_rows$member[2516]<-"SHRI Su Thirunavukkarasar"
empty_text_rows$member[2621]<-"SHRIMATI Raksha Nikhil Khadse"
empty_text_rows$member[2664]<-"Shri Pankaj Chaudhary"
empty_text_rows$member[2685]<-"SHRI P.P. Mohammed Faizal"
empty_text_rows$member[2696]<-"ADV. ADOOR PRAKASH"
empty_text_rows$member[2712]<-"SHRI Ajay Mishra Teni"
empty_text_rows$member[2742]<-"SHRI Tejasvi Surya"
empty_text_rows$member[2861]<-"SHRI Ramapati Ram Tripathi"
empty_text_rows$member[2934]<- "SHRI Sanjaykaka Ramchandra Patil"

empty_text_row <- empty_text_rows
for (i in 1:nrow(empty_text_row)) {
  if (is.na(empty_text_row$member[i])) {
    pattern_member <- "(?i)(SHR|SHRI|SHIRI|SHIRIMATI|SHRIMATI|KUMARI|ADV|DR|DRT|SMT|PROF|MR|MRS|SUKUMARI|SRI|SRIMATI|MS)\\s*\\S*\\s+[A-Z]+(?:\\s+[A-Z]+)*(.*?)(will|wil)"
    extracted <- regmatches(empty_text_row$extracted_text[i], gregexpr(pattern_member, empty_text_row$extracted_text[i], ignore.case = TRUE))
    
    if (length(extracted[[1]]) > 0) {
      matched_substring <- paste(extracted[[1]][1], extracted[[1]][2], sep = " ") # Join titles and text
      cleaned_text <- gsub("\\*|\\d+|\\.|\\n|(W|w)(i?)l(l?)|\\:", "", matched_substring)
      cleaned_text <- gsub("^\\s*,\\s*", "", cleaned_text)
      cleaned_text <- gsub("\\s*,", ",", cleaned_text)
      cleaned_text <- gsub("\\s+$", "", cleaned_text)
      empty_text_row$member[i] <- cleaned_text
    } else {
      empty_text_row$member[i] <- NA
    }
  }
}
empty_text_row <- empty_text_row[, !(colnames(empty_text_row) %in% c("X","link","text"))]


#Splitting extracted text in question-answers for those having member names and extracted text
rows_with_member_only <- which(!is.na(empty_text_row$member) & is.na(empty_text_row$question) & is.na(empty_text_row$answer))
empty_text_row[rows_with_member_only,] <- empty_text_row[rows_with_member_only,] %>%
  mutate(
    question = sub(".*\\bWILL\\b(.*?)\\banswer\\b.*", "\\1", extracted_text, ignore.case = TRUE),
    answer = sub(".*\\banswer\\b(.*)", "\\1", extracted_text, ignore.case = TRUE)
  )

empty_text_row$question_id <- gsub(":", "", empty_text_row$question_id)
empty_text_row$question_id <- gsub("([[:digit:]]{4})([[:alpha:]]{2})", "\\1\\2:", empty_text_row$question_id)
empty_text_row$question_id[503:length(empty_text_row$question_id)] <- gsub("([A-Za-z]{2})", "\\1:", empty_text_row$question_id[503:length(empty_text_row$question_id)])

combined_dfs <- combined_df

matching_indices <- match(empty_text_row$question_id, combined_dfs$question_id, nomatch = 0)
combined_dfs[matching_indices, c("question", "answer", "ministry", "member")] <- empty_text_row[c("question", "answer", "ministry", "member")]



na_row_numbers <- which(rowSums(is.na(combined_dfs[, c("member", "question", "answer")])) == 3)

# Get corresponding question_id values
corresponding_question_ids <- combined_dfs$question_id[na_row_numbers]

# Create a dataframe
result_df <- as.data.frame(cbind(na_row_numbers, corresponding_question_ids))

result_df_links <- list()

# Loop through each value in corresponding_question_ids and create links
for (question_id in result_df$corresponding_question_ids) {
  link <- create_link(question_id)
  result_df_links <- c(result_df_links, list(link))
}
result_df$link <- unlist(result_df_links)

extract_text_from_pdf <- function(pdf_link) {
  tryCatch({
    text <- pdf_text(pdf_link)
    return(text)
  }, error = function(e) {
    return(NA)
  })
}

# Apply the function to each link in the link column
result_df$text <- sapply(result_df$link, extract_text_from_pdf)
result_df <- result_df[!is.na(result_df$text), ]

process_row <- function(extracted_text) {
  if (any(is.na(extracted_text)) || all(nchar(extracted_text) == 0)) {
    return(list(
      extracted_text = NA,
      quest_num = NA,
      ministry = NA,
      member = NA,
      question = NA,
      answer = NA
    ))
  } else {
    # Concatenate multi-line text into a single string
    extracted_text <- paste(extracted_text, collapse = " ")
    
    # Remove Hindi characters
    pattern <- "[\u0900-\u097F]+"
    extracted_text <- gsub(pattern, "", extracted_text)
    
    # Extract Ministry
    pattern_ministry <- "(?i)(^.*?Minist.*\\s+of\\s.*?)\\s*\n"
    ministry <- NA
    for (line in unlist(strsplit(extracted_text, "\n"))[1:1]) {
      if (grepl("(?i)Minis.*\\s+of", line)) {
        ministry <- sub(pattern_ministry, "\\1", line)
        break
      }
    }
    # Extract Question Number
    pattern <- "(?i)QUESTION NO\\.?\\s?\\*?\\-?(\\d+)"
    question_number_match <- regmatches(extracted_text, regexec(pattern, extracted_text))
    question_number <- if (length(question_number_match[[1]]) > 1) question_number_match[[1]][2] else NA
    
    # Extract Member who asked the question
    pattern_member <- "(?i)\\b(SHRI|SHR|SHIRI|SHIRIMATI|SHRIMATI|KUMARI|ADV|DR|DRT|SMT|PROF|MR|MRS|SUKUMARI|SRI|SRIMATI|MS)\\.?(\\s+?)[A-Za-z]+(?:\\s+[A-Za-z]+)*(.*?)((?i)(will|wil|wiil))"
    extracted <- regmatches(extracted_text, gregexpr(pattern_member, extracted_text, ignore.case = TRUE))
    
    if (length(extracted[[1]]) > 0) {
      matched_substring <- paste(extracted[[1]][1], extracted[[1]][2], sep = " ") # Join titles and text
      cleaned_text <- gsub("\\*|\\d+|\\.|\\n|(W|w)(i?)l(l?)|\\:", "", matched_substring)
      cleaned_text <- gsub("^\\s*,\\s*", "", cleaned_text)
      cleaned_text <- gsub("\\s*,", ",", cleaned_text)
      cleaned_text <- gsub("\\s+$", "", cleaned_text)
      member_details <- cleaned_text
    } else {
      member_details <- NA
    }
    
    question <- gsub("(?i).*?([^[:alnum:]]|^)WILL\\s+(.*?)\\s*\\ba(\\s*)n(\\s*)s(\\s*).*?\\b.*", "\\2", extracted_text)
    answer <- gsub("(?is).*\\b(ans.*)$", "\\1", extracted_text, perl = TRUE)
    
    return(list(
      extracted_text = extracted_text,
      quest_num = question_number,
      ministry = ministry,
      member = member_details,
      question = question,
      answer = answer
    ))
  }
}

# Apply the process_row function to each row of the text column and create new columns
results_df<- result_df

processed_data <- lapply(result_df$text, process_row)

# Convert the processed data into a data frame
processed_df <- as.data.frame(do.call(rbind, processed_data))

# Add the processed data as new columns to results_df
results_df$quest_num <- processed_df$quest_num
results_df$ministry <- processed_df$ministry
results_df$member <- processed_df$member
results_df$question <- processed_df$question
results_df$answer <- processed_df$answer
results_df <- results_df %>%
  rename(question_id = corresponding_question_ids)
colnames(results_df)
matching_indices <- match(results_df$question_id, combined_dfs$question_id, nomatch = 0)
combined_dfs[matching_indices, c("question", "answer", "ministry", "member")] <- results_df[c("question", "answer", "ministry", "member")]
combined_dfs <- combined_dfs %>%
  filter(!(is.na(member) & is.na(question) & is.na(answer)))

final_df <- data.frame(
  question_id = combined_dfs$question_id,
  quest_num = combined_dfs$quest_num,
  ministry = combined_dfs$ministry,
  member = combined_dfs$member,
  question = combined_dfs$question,
  answer = combined_dfs$answer
)

sum(is.na(final_df$member=="NA"))
write.csv(final_df, "final_ls.csv", row.names = TRUE)
