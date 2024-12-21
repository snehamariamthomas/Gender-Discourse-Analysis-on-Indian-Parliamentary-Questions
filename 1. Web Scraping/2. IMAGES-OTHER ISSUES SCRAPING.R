library(openxlsx)
library(XML)
library(RCurl)
library(pdftools)
library(stringr)
library(data.table)
library(tesseract)

full_file_path <- "path/LS_2019_2024_csv.csv"

# Attempt to read the CSV file
c <- fread(full_file_path, 
                   sep=",",  # Specify the separator
                   quote='"',  # Specify the quote character
                   fill=TRUE,  # Fill missing columns with NA
                   na.strings=c("NA", ""),  # Treat "NA" and empty strings as missing
                   encoding="UTF-8",  # Specify the encoding
                   showProgress=FALSE)


c <- c[!duplicated(c$link), ]

# ALL QUESTIONS WHICH HAVE EITHER QUESTION/ANSWER COLUMN BLANK
#e: ANY ONE IS MISSING
d <- which((is.na(c$question_text))|(is.na(c$answer_text))|c$question_text=="NA"|c$answer_text=="NA"|c$question_text==""|c$answer_text=="")
e<- c[d,]
#for (i in seq_len(nrow(e))) 
for (i in seq_len(nrow(e))) {
  lin <- e$link[i]
  
  # Try to extract text from the PDF
  tryCatch({
    text <- paste(pdf_text(lin), collapse = "\n")  # Concatenate text from all pages
    e$full[i] <- text
  }, error = function(e) {
    # If an error occurs, print the error message and move to the next iteration
    cat("Error occurred for PDF at index", i, ":", conditionMessage(e), "\n")
    e$full[i] <- NA  # Optionally mark as NA to indicate failure
  })
  
  Sys.sleep(1)  # Optional sleep to avoid overwhelming the server
}  
e$full<-trimws(e$full)
check <- e[duplicated(e$full), ]

check$full[!(check$full == "")&check$question_text!="NA"&check$answer_text!="NA"] <- "Link not Available"
check$full[check$full != "" & check$question_text == "NA" & check$answer_text == "NA"] <- "Link not Available"
#l<-check[135:137,]<-"" #Images
#check$id[check$full=="Link not Available"]


#UPDATING FOR THOSE ROWS WHERE LINK NOT AVAILABLE
update_ids <- check$id[check$full == "Link not Available"]
# Iterate over each id and update e$full
for (id in update_ids) {
  # Find the index in 'e' that matches the 'id'
  e_index <- which(e$id == id)
  
  # Find the index in 'check' that matches the 'id'
  check_index <- which(check$id == id)
  
  # Update the 'full' column in 'e' with the corresponding 'full' value from 'check'
  e$full[e_index] <- check$full[check_index]
}

#e$full[e$id=="172AU:2161"]<-"Will the MINISTER OF FINANCE be pleased to state: a) whether there is any proposal before the Government to give permanent status to the Finance Commission; b) if so, the details thereof and if not, the reasons therefor; c) whether it is true that the Reserve Bank of India is also in favour of giving permanent status to the Finance Commission; and  d) if so, the details thereof? ANSWER  THE MINISTER OF STATE IN THE MINISTRY OF FINANCE (SHRI ANURAG SINGH THAKUR) a) No, Government of India is not considering any proposal to give permanent status to Finance Commission.  b) Does not arise. c) Reserve Bank of India is also not considering any proposal to give permanent status to Finance Commission.  d) Does not arise. "
#e$question_text[e$id=="172AU:2161"]<-"Will the MINISTER OF FINANCE be pleased to state: a) whether there is any proposal before the Government to give permanent status to the Finance Commission; b) if so, the details thereof and if not, the reasons therefor; c) whether it is true that the Reserve Bank of India is also in favour of giving permanent status to the Finance Commission; and  d) if so, the details thereof"

#write.csv(e,"missing_text_e.csv")

# TAKING OUT COLUMN WHICH DID NOT EXTRACT ANYTHING FROM PDF: for IMAGE PDF
h<-which(e$full == "" | is.na(e$full) | e$full == "NA" | e$full=="character(0)")
i<-e[h,] #515 observations
#write.csv(i,"missing_text_image_pdf.csv")

i<- read.csv("path/missing_text_image_pdf.csv")


# Loop through each row of the dataframe
ocr_engine <- tesseract()

# Loop through each row of the dataframe
for (row in 284:nrow(i)) {
  # Extract the PDF link from the current row
  pdf_link <- i$link[row]
  
  # Download the PDF file
  temp_file <- tempfile(fileext = ".pdf")
  download.file(pdf_link, temp_file, mode = "wb")
  
  # Convert PDF to an image (assuming there's only one page per PDF)
  pdf_as_image <- pdf_convert(temp_file, dpi = 600)
  
  # Perform OCR on the image
  extracted_text <- ocr(pdf_as_image, engine = ocr_engine)
  
  # Store the extracted text in the 'full' column
  i$full[row] <- extracted_text
  
  # Remove the temporary files
  file.remove(temp_file)
  file.remove(pdf_as_image)
}
#write.csv(i,"image_pdf_extracted.csv")

# Now, the 'full' column of the dataframe 'i' should contain the extracted text from PDFs

# Now, the 'full' column of the dataframe 'i' should contain the extracted text from all pages of the PDFs
#ALL FULL COLUMN EXTRACTED


# merging e(all rows with missing question/answer) with i(image pdf)
e<-e[,-1]
i<-i[,-(1:2)]

library(dplyr)
result <- e %>%
  left_join(i, by = "id", suffix = c(".e", ".i")) %>%
  mutate(full = ifelse(!is.na(full.i), full.i, full.e)) %>%
  select(-full.e, -full.i)
colnames(result)

e$full <- result$full
e$full<-trimws(e$full)
#write.csv(e,"missing_text_final_extract.csv")

#_________________________________________________________________
# CASE 1: FIRST MANAGING WHICH ARE BOTH NA

full_file_path <- "path/missing_text_final_extract.csv"

# Attempt to read the CSV file
e <- fread(full_file_path, 
           sep=",",  # Specify the separator
           quote='"',  # Specify the quote character
           fill=TRUE,  # Fill missing columns with NA
           na.strings=c("NA", ""),  # Treat "NA" and empty strings as missing
           encoding="UTF-8",  # Specify the encoding
           showProgress=FALSE)
first_row <- as.character(e[1, ])
colnames(e)<-first_row
e<-e[-1,]

e$question_text[e$full == "Link not Available"] <- "Link not Available"
e$answer_text[e$full == "Link not Available"] <- "Link not Available"
both_na <- which(e$question_text=="NA"& e$answer_text=="NA")
both_nas<- e[both_na,] #827

both_nas$quest <- NA
extracted_texts <- vector(mode = "character", length = nrow(both_nas))

for (row in 1:nrow(both_nas)) {
  # Split the member column by commas and select the last value
  member_values <- strsplit(both_nas$member[row], ",")[[1]]
  last_member_value <- tolower(trimws(member_values[length(member_values)]))
  
  # Preprocess the text in the full column to remove extra spaces and new lines
  preprocessed_text <- gsub("\\s+", " ", both_nas$full[row])  # Replace multiple spaces and new lines with a single space
  
  # Convert the preprocessed text to lowercase
  preprocessed_text <- tolower(preprocessed_text)
  
  # Find all occurrences of "will the"
  will_the_positions <- str_locate_all(preprocessed_text, "will the|willthe|wil the|will he|wlllthe")
  
  # Initialize quest text for this row
  extracted_text <- "No matching 'will the' found in text."
  
  if (length(will_the_positions[[1]]) > 0) {
    # Loop through each occurrence of "will the"
    for (pos in will_the_positions[[1]]) {
      start_index <- pos[1]
      
      # Slice the text starting from the start_index to the end
      text_after_start <- substr(preprocessed_text, start_index + 1, nchar(preprocessed_text))
      
      # Find the earliest occurrence of "answer" or "a n s w e r" in the sliced text
      answer_pos <- str_locate(text_after_start, "answer|an s w e r")[[1]][1]
      an_swer_pos <- str_locate(text_after_start, "a n s w e r")[[1]][1]
      
      # Adjust positions relative to the original text
      if (!is.na(answer_pos)) {
        answer_pos <- start_index + answer_pos
      }
      if (!is.na(an_swer_pos)) {
        an_swer_pos <- start_index + an_swer_pos
      }
      
      # Check if both answer_pos and an_swer_pos are NA
      if (all(is.na(c(answer_pos, an_swer_pos)))) {
        extracted_text <- "No matching 'answer' or 'a n s w e r' found after 'will the'."
      } else {
        # Get the earliest valid position among the found positions
        end_index <- min(c(answer_pos, an_swer_pos), na.rm = TRUE)
        
        # Extract the relevant text ensuring we include the entire "answer" or "a n s w e r" pattern
        extracted_text <- substr(preprocessed_text, start_index, end_index + nchar("a n s w e r") - 1)
        break  # Stop searching after the first valid occurrence
      }
    }
  } else {
    extracted_text <- "No 'will the' found in text."
  }
  
  # Assign the extracted text to the corresponding element in the extracted_texts vector
  extracted_texts[row] <- extracted_text
}

# Assign the extracted_texts vector to the "quest" column in both_nas
both_nas$quest <- extracted_texts

#ANSWE FOR THESE COLUMNS
both_nas$ans<-NA
# Create an empty vector to store extracted "ans" texts
extracted_ans <- vector(mode = "character", length = nrow(both_nas))

# Loop through each row of the data frame
for (row in 1:nrow(both_nas)) {
  # Extracted text from the "quest" column
  quest_text <- both_nas$quest[row]
  
  # Full text after preprocessing
  preprocessed_text <- gsub("\\s+", " ", both_nas$full[row])  # Replace multiple spaces and new lines with a single space
  preprocessed_text <- tolower(preprocessed_text)  # Convert to lowercase
  
  # Find the position where the "quest" text ends in the "full" text
  quest_end_pos <- which(str_detect(preprocessed_text, fixed(quest_text)))[1] + nchar(quest_text) - 1
  
  # Extract the remaining text from quest_end_pos till the end of the full text
  if (!is.na(quest_end_pos)) {
    extracted_ans_text <- substr(preprocessed_text, quest_end_pos + 1, nchar(preprocessed_text))
  } else {
    extracted_ans_text <- "No matching 'quest' text found in 'full' text."
  }
  
  # Assign the extracted text to the corresponding element in the extracted_ans vector
  extracted_ans[row] <- extracted_ans_text
}

# Assign the extracted_ans vector to the "ans" column in both_nas
both_nas$ans <- extracted_ans

#to start answer column correctly
for (row in 1:nrow(both_nas)) {
  # Find the position of "answer" or "a n s w e r"
  ans_start_pos <- str_locate(both_nas$ans[row], "(answer|a n s w e r)")[1]
  
  # Remove everything before "answer" or "a n s w e r"
  if (!is.na(ans_start_pos)) {
    both_nas$ans[row] <- substr(both_nas$ans[row], ans_start_pos, nchar(both_nas$ans[row]))
  } else {
    both_nas$ans[row] <- "No matching 'answer' or 'a n s w e r' found."
  }
}


# ISSUES WHEN BOTH WERE NA
rows_with_no <- grep("^No", both_nas$quest)
both_nas_no <- both_nas[rows_with_no,]
both_nas_no$fulls <- tolower(gsub("\n", " ", both_nas_no$full))
both_nas_no$extracted_quest <- sub(".*?(\\bwill\\b.*)", "\\1", both_nas_no$fulls)

remove_after_last_minister <- function(sentence) {
  # Find the last occurrence of "minister" in the sentence
  last_minister_index <- max(gregexpr("\\bminister\\b", sentence)[[1]])
  
  # If "minister" is found, remove everything after it, otherwise return the original sentence
  if (last_minister_index > 0) {
    return(substr(sentence, 1, last_minister_index - 1))
  } else {
    return(sentence)
  }
}

# Apply the function to each row of the extracted_quest column
both_nas_no$new_column <- sapply(both_nas_no$extracted_quest, remove_after_last_minister)
setnames(both_nas_no, "new_column", "quest_final")
both_nas_no$extracted_quest[51]
both_nas_no$quest_final[4]<-"will the minister of finance be pleased to state:  (a) whether investment proposals for march,2}lg quarter were lower than that of december, 2018 quarter and even corresponding quarter of2017;  (b) if so, the details thereof;  (c) whether it is true that during the last two years, investment proposals on trailing four-quarter average basis has fallen from 7 trillion rupees to2.5 trillion rupees; and  (d) if so, the details thereof along with the reasons therefor?"
both_nas_no$quest_final[5]<- "will the minister of shipping be pleased to state:                   qldqft.{f,{q-fi  (a) whether the government proposes to have a dredge plan for river brahmaputra;  (b) if so, the details thereof;  (c) the time by which the aforesaid waterway is likely to become operational;  (d) whether any study has been conducted for the development of waterway between          dhubri and sadia via guwahati on the river brahmaputra; and  (e)      if so, the details thereof and if not, the reasons therefor?"
both_nas_no$quest_final[2]<- "Pdf extraction issues"
both_nas_no$quest_final[19]<- "Pdf extraction issues"
both_nas_no$quest_final[51]<-"Pdf extraction issues"
both_nas_no$quest_final[20]<-"will the minister of housing and urban affairs be pleased to state:  (a)    the number of dwelling units proposed to be constructed in kidwai        nagar east and deadlines fixed for finishing the projects along with        the number of units made habitable so far, type-wise; (b)    whether there are reports of inordinate delay in providing physical        possession to the allottees of different categories due to which        allottees had to go through tremendous hardship; (c)    if so, the reasons therefor along with the dates of technical and        physical possession of the said flats, type-wise; (d)    whether, due to ongoing construction works on the project site        thousands of residents in said units have to suffer heavy air        pollution; (e)    if so, the details thereof along with the reaction of the government        thereto; and (f)    the measures taken for speedy completion of the project along with        the preemptive measures taken to avoid such conditions in        forthcoming projects in sarojini nagar and netaji nagar areas?"
both_nas_no$quest_final[36]<-"will the minister of human resource development be pleased to state:   (a) whether the government has taken stringent measures to protect the students from north eastern part of our country studying at various institutions across the country from the discrimination based on their features in the wake of the covid-19 epidemic;  (b) if so, the details thereof; and  (c) if not, the reasons therefor? "
both_nas_no$quest_final[47]<-"will the minister of gonsumer affairs, food and publtc distrtbutton scsrtfdr erri, urc.r sitr {rffifi, ffd{ur {,+ be pteased to state:  (a) the details of the countqls ranking in the gtobal hunger lndex report during the last three years and the current year, year-wisel  (b) whether the country is traiting year by year, if so, the reasons therefor;  (c) the manner in which the government wi achieve the target of zero hunger with this rankingl and  (d) the manner in which the government proposes to come out of this?"
both_nas_no$quest_final[52] <-"Pdf extraction issues"
both_nas_no$quest_final[53] <- "Pdf extraction issues"
both_nas_no$quest_final[54] <-"Pdf extraction issues"
both_nas_no$quest_final[55] <-"will the minister of gonsumer affairs' food and publig dlstrlbutlon 5q'ef1.trr arrd, uru sik €rffiq' iff,{ut d* be pleased to state: (a) the estimated number of peopte in the country presently availing free rations under national food security act (nfsa), state'wise; (b) whether the govemrnent is aware that nearly {.97 crore poor padicularty from the most vulnerabte groups have been left out and a substantial number of duplicate and fake ration card holders are availing benefits; (c) if so, whether the union government has asked states to launch a special drive to identify the most vulnerable groups and to extend benefits to the deserving; (d) if so, the action taken by the states on gentre's directive, state-wise; and (e) if not, the reasons therefor?"
both_nas_no$quest_final[35]<-"will the minister of consumer affairs, food and public d st rt b ut o n 5csfral xrf,*, qrcq 3tt trffifi furcq ri* be pl eased to state :  i            i     (a) the total storage capacity created under private entrepreneurs guarantee (peg) scheme since the year 2014-15 especially in maharashtra, the district-wise details thereof;  (b) the number of private godowns taken on rent in maharashtra, district-wise;  (c) the number of govemment and private godowns constructed in maharashtra since 2o{&1 5, district-wise;  (d) the quantum of foodgrains damaged yearly due to shortage of godownsl and  (e) whether the government is likely to make available funds for it separatel!"
both_nas_no$quest_final[46]<-"will the minister of gonsumer affairs, food and publtc distrtbutton ecstrftr err*, ur{c gik srffi6, futrrur t'+ be pteased to state: (a) whether the govemment is formulating any policy for the cash patment to sugarcane farmers; (b) if so, the details thereof; (c) if not, whether the govemment is ptanning to launch any scheme in this regard; (d) whether the government has brought any schemes to train the farmerc; and (e) if so, the details of the amount disbursed under the sald scheme so far with particular reference to my constituency bijnor?  "
both_nas_no$quest_final[57]<- "will the minister of micro, small and medium enterprises be pleased to state:  (a)   the details of new micro, small and medium enterprises set up in the state of punjab and haryana under programmes like prime minister’s employment generation programme (pmegp) during each of the last three years and the current year; (b)   the details of new micro-enterprises set up in the parliamentary constituency of fatehgarh sahib, punjab during each of the last three years; (c)   the details of the funds sanctioned and allocated for the execution of such programmes during the last three years; and (d)   whether the government introduced new programmes to facilitate the setting up of new enterprises post covid-19 lockdown and if so, the details thereof?"
both_nas_no$quest_final[63] <-"will the minister of micro, small and medium enterprises be pleased to state:  (a)   the details of formal and informal employment in msmes during the last three years; (b)   whether the government has taken any steps to boost employment and improve employment conditions in msmes; and (c)   if so, the details thereof and if not, the reasons therefor?  "

both_nas_no$ans_final <- mapply(function(x, y) gsub(y, "", x, fixed = TRUE), both_nas_no$extracted_quest, both_nas_no$quest_final)
both_nas_no$ans_final[both_nas_no$quest_final == "Pdf extraction issues"] <- "Pdf extraction issues"
both_nas_no$id<- trimws(both_nas_no$id)


# Copy both_nas to merged_data
merged_data <- both_nas

update_ids <- both_nas_no$id
# Iterate over each id and update e$full
for (id in update_ids) {
  # Find the index in 'e' that matches the 'id'
  nas_index <- which(merged_data$id == id)
  
  # Find the index in 'check' that matches the 'id'
  nas_no_index <- which(both_nas_no$id == id)
  
  # Update the 'full' column in 'e' with the corresponding 'full' value from 'check'
  merged_data$quest[nas_index] <- both_nas_no$quest_final[nas_no_index]
  merged_data$ans[nas_index]<- both_nas_no$ans_final[nas_no_index]
}

both_nas <- merged_data
both_nas$ans<-trimws(both_nas$ans)
both_nas$quest<-trimws(both_nas$quest)

#grep("^No",both_nas$ans): some missed out
both_nas$ans[47]<-"(a) & (b) : There are three integrated programmes with four years B.Ed. course approved by the\nNational Council for Teacher Education (NCTE). One programme was notified in 2014 and the\nother two four year Integrated Teacher Education Programmes (ITEP) with in-built\nspecialization for primary (including pre-primary) and secondary, both for Science and Arts\nstreams were notified vide notification dated 29th March, 2019.\n\n        For a person to become eligible for appointment as a teacher in schools, the NCTE has\nnotified the minimum qualifications vide notification dated 25th August, 2010 as amended from\ntime to time. The details are at link: http://ncte.gov.in/Website/MinimumQualifications.aspx .\n\n(c)    : As education is a subject in the Concurrent list of the Constitution, the recruitment and\nservice conditions of teachers are in the domain of respective State Government/UT\nAdministration.\n\n "
both_nas$ans[58]<-"(a) & (b) :       The Central Government has introduced a 4-year Integrated Teacher Education\nProgramme (ITEP) for pre-service training, to offer graduation with certain specializations built into it\nnamely Primary and Secondary Education. In this regard, a Gazette Notification dated 29 th March,\n2019 has been published and applications have been invited from 3rd June, 2019.\n\n       Further, for in-service training of teachers, school heads and other functionaries, at the elementary\nlevel, approval has been given for an Integrated Teacher Training Programme to be conducted through\nNational Council of Educational Research and Training (NCERT) and National Institute of\nEducational Planning and Administration (NIEPA), in all States and UTs.\n\n(c) & (d) : The Central Government has taken several initiatives to improve the quality of education:\n(i).      The Central Government has launched an Integrated Scheme for School Education – Samagra\nShiksha, from 2018-19 which subsumes the erstwhile centrally sponsored schemes of Sarva Shiksha\nAbhiyan (SSA), Rashtriya Madhyamik Shiksha Abhiyan (RMSA) and Teacher Education (TE). Under\nSamagra Shiksha, funds are given to all States and UTs for various interventions to improve the\nquality of education such as training of in-service teachers, headmasters and principals, remedial\nteaching for academically weaker students, provision of library grants to schools, ICT and digital\ninitiatives, strengthening of teacher education institutions, Rashtriya Avishkar Abhiyan, Padhe Bharat\nBadhe Bharat, etc.\n\n(ii).    In order to focus on quality education, the Central rules to the Right of Children to Free and\nCompulsory Education (RTE) Act have been amended to include reference on class-wise, subject-wise\nLearning Outcomes for all elementary classes. The National Council of Educational Research and\nTraining (NCERT) conducted a National Achievement Survey, under which learning outcomes of\nstudents were evaluated, through a District level sampling and gaps were identified.\n(iii).   Government of India has decided to participate in the Programme for International Students\nAssessment (PISA) to be conducted by the Organization for Economic Cooperation and Development\n(OECD) in 2021.\n(iv).    Approval has been given for conducting a Census based audit called Shagunotsav of all\ngovernment and government aided schools in all States and UTs. Further, in 2019-20 approval has\nbeen given for conducting a School Based Assessment (SBA) of all elementary stage students, to\nevaluate learning outcomes.\n(v).     The online D.El.Ed. course was started from 3rd October, 2017 and 9,58,513 teachers have\nsuccessfully completed the training.\n\n(vi).    MHRD has designed a 70 indicators based matrix Performance Grading Index (PGI) to\ngrade the States and UTs.        To collect timely and accurate data, an Educational Management\nInformation System called UDISE+ (UDISE plus) has been launched in 2018-19.\n\n(vii).   In 2019-20, approval has been given for setting up Youth and Eco Club in all Government\nSchools across the country. In order to experience and celebrate the rich cultural diversity of India and\nto encourage experimental learning, Rangotsav was organized in schools in 2018-19.\n\n         All the above mentioned quality interventions are provided at par to schools of rural and urban\nareas. Additionally, the Central Government has issued advisories to States and UTs for redeployment\nof teachers and to ensure that all school teachers should spend adequate time serving in rural areas\nthrough a transparent policy."
both_nas$ans[79]<- "(a) to (b): As per the data available in Unified District Information System for Education\n(U-DISE), 2017-18 (Provisional), there is no such category of social work teachers.\nEducation is in the concurrent list of the Constitution. The recruitment, service conditions\nand deployment of teachers come under the purview of the State/Union Territory (UT)\nGovernment. However, Ministry of Human Resource Development has been requesting all\nthe States and UT Governments for filling-up the vacant posts of teachers and their rational\ndeployment, for which the Ministry issued advisories to all States and UTs from time to\ntime.\n\nFurther, the Central Government through the centrally sponsored scheme of Samagra\nShiksha, provides assistance to the States and UTs for deployment of additional teachers to\nmaintain appropriate Pupil Teacher Ratio as per the prescribed norms for various levels of\nschooling. Further, as per the Right of Children to Free and Compulsory Education (RTE)\nAct, 2009, financial assistance for part time instructors for art education, health and\nphysical education and work education is also provided to the States and UTs.\n\n(c): No such proposal is under consideration at this stage."
both_nas$ans[80]<-"(a) to (d) :    National Council of Educational Research and Training (NCERT) has developed\nNational Curriculum Framework (NCF, 2005) which emphasises on inculcating moral values,\nattitudes and skills required for living in harmony with oneself and with others. Issues related to\nMoral Values, wherever possible have been incorporated in the textbooks developed for school\nstage as well as in the entire school life-the curriculum, classroom climate, school management,\nteaching-learning, teacher-pupil relationships etc.\n\n\n        As per information received from Government of Jharkhand, they have adopted NCF,\n2005 and NCERT curriculum with local content. Moral education has been integrated in the text\nbooks in Elementary Education. In addition to this, a supplementary book for class III to class\nVIII on moral values has been developed by Jharkhand Council of Educational Research and\nTraining (JCERT) and moral values books for class VI to VIII have been printed and distributed\nfree of cost. For class IX to XII State has adopted NCERT text books.\n\n\n        As per information received from University Grants Commission (UGC), moral education\nis embedded in the syllabi of all courses in the stream of Humanities and Social Sciences at\nUniversity level. Further, some universities in the country are also running courses on spiritual\nand religious studies."
both_nas$ans[122]<-"(a) to (b): Yes, Sir. The Central Government has launched an Integrated Scheme for School\nEducation- Samagra Shiksha, from 2018-19 which subsumes the erstwhile centrally sponsored\nschemes of Sarva Shiksha Abhiyan, Rashtriya Madhyamik Shiksha Abhiyan (RMSA) and\nTeacher Education Scheme (TE). It is an overarching programme for the school education sector\nextending from pre-school to class XII and aims to ensure inclusive and equitable quality\neducation at all levels of school education. The Scheme for school education ‘Samagra\nShiksha’ envisages the ‘school’ as a continuum from primary, upper primary, secondary to\nhigher secondary levels. It has been observed that separate schemes have created an artificial\ndivide of levels i.e., elementary and secondary within the “School Education Sector”. This may\nhave led to a duplication of efforts towards implementing similar interventions and achieving\nsimilar objectives. A single programme will lead to an optimal utilization of budgetary\nallocations and effective use of human resources and institutional structures.\n\n(c) & (d): Education is in the Concurrent List of the Constitution, a majority of the schools are\nunder the jurisdiction of respective States and Union Territories (UT). Hence, the recruitment,\nservice conditions and redeployment of teachers and other staff are primarily in the domain of\nrespective State Governments and UT Administrations. As per information received from State\n\nGovernment of Karnataka, the State Government Order has specified the posts for the office of\nthe State Project Director, District office, and Block office. As per the Karnataka Government\nOrder, 150 District Primary Education Programme (DPEP) posts and 743 outsourced posts have\nbeen continued under Samagra Shiksha."

both_nas$question_text<-both_nas$quest
both_nas$answer_text<-both_nas$ans

#both_nas <- both_nas[, !(names(both_nas) %in% c("quest", "ans"))]


mergeds_data <- e

update_ids <- both_nas$id
# Iterate over each id and update e$full
for (id in update_ids) {
  # Find the index in 'e' that matches the 'id'
  e_index <- which(mergeds_data$id == id)
  
  # Find the index in 'check' that matches the 'id'
  nas_index <- which(both_nas$id == id)
  
  # Update the 'full' column in 'e' with the corresponding 'full' value from 'check'
  mergeds_data$question_text[e_index] <- both_nas$question_text[nas_index]
  mergeds_data$answer_text[e_index]<- both_nas$answer_text[nas_index]
}
e<-mergeds_data


#_________________________________________________________

#NOW E WHICH WAS INITIALLY MISSING VALUE FOR both QUESTION/ANSWER_TEXT
#HAS FILLED UP THOSE VALUES WHERE BOTH WERE NA

#CASE 2: question_text is NA

qt_na <- which(is.na(e$question_text)|e$question_text=="NA")
qt_nas <- e[qt_na,]
qt_nas$full<-trimws(qt_nas$full)


qt_nas$fulls <- tolower(gsub("[\r\n]", "", qt_nas$full))
qt_nas$fulls <- gsub("\\s+", " ", qt_nas$fulls)
qt_nas$extracted_quest <- sub(".*?(will.*)", "\\1", qt_nas$fulls)
qt_nas$quest_final <- sub("(.*?(answer|a n s w e r)).*", "\\1", qt_nas$extracted_quest)
qt_nas$quest_final <- gsub("\\s+", " ", qt_nas$quest_final)
qt_nas$quest_final <- trimws(qt_nas$quest_final)

filt_rows <- qt_nas[which(!grepl("^w",qt_nas$quest_final)),]



qt_nas$qt_final <- mapply(function(x, y) gsub(y, "", x, fixed = TRUE), qt_nas$full,qt_nas$answer_text)
qt_nas$qt_final <- tolower(gsub("\n", " ", qt_nas$qt_final))
qt_nas$qt_final <- tolower(gsub("\r", " ", qt_nas$qt_final))
qt_nas$qt_final<-trimws(qt_nas$qt_final)

qt_nas$extracted_quest <- sub(".*?(\\bwillthe\\b.*|\\bwill\\b.*|\\bwil\\b.*|\\bwlllthe\\b.*|\\buyill\\b.*|\\bwillttc\\b.*|\\bwilt\\b.*)", "\\1", qt_nas$qt_final)
filtered_rows <- qt_nas[!grepl("^w", qt_nas$extracted_quest), ]
filtered_rows$extracted_quest <- sub(".*?(\\ba\\).*)", "\\1\\2", filtered_rows$extracted_quest)
filtered_rows$extracted_quest<-trimws(filtered_rows$extracted_quest)
#which(!grepl("^a)",filtered_rows$extracted_quest))
filtered_rows$qt_final[73]
filtered_rows$extracted_quest[23]<-"Will the Minister of Parliamentary Affairs be pleased to state (a) whether the Government maintains a database on Pre-legislative Consultation Policy (PLCP) compliance since its institutionalisation in 2014 (b) if so, the details thereof and if not, the reasons therefore; and (c) the details of the bills passed in the Parliament since inception of PLCP of which the number of bills that complied with the requisite PLCP conditions?"
filtered_rows$extracted_quest[25]<-"lent rnade by the(3overnment on the digital     :ndia land recor‖ modernization prograrnrne(d!lrmp)since its launch:    (b)the detalls of nuttber of land records which have been modernized and updated,     state‐ wise abng wlh the probに ms   faced by governmentinthb regard;     v肥                                                     me tta賦  思 1篇l∬腫恵:『it::鷲1:胤                                  is°  (e)the detans ofthe funds a‖ ocated under dilrmp to statesノ uts for suⅳ eys and re…     suⅳ eys across the countfy during each ofthe lastthtte years and the current year;    o whether the govemment has lxed any ttmeframe to compiete the land records     modemizalon pю gramme and r so,the detalls there"
filtered_rows$extracted_quest[73]<-"whethcr 5 53 crorc hccttcs ofland is tying unused despitc implemcntation of lntegrated waste        land dcvclopment programmc,drought pronc arca programme and dcscrt dcvclopmcnt        programme by thc govemmentin thc countγ ,if so,thc details thcreol and     (lll whethcr the govcmment has reduccd the target set in this regard as the funds a1loctted for         making thc land arable during the eleventh five year pian has not been spent and if so,the        rcasons thercfor?"



qt_mergeds_data <- qt_nas

update_ids <- filtered_rows$id
# Iterate over each id and update e$full
for (id in update_ids) {
  # Find the index in 'e' that matches the 'id'
  e_index <- which(qt_mergeds_data$id == id)
  
  # Find the index in 'check' that matches the 'id'
  nas_index <- which(filtered_rows$id == id)
  
  # Update the 'full' column in 'e' with the corresponding 'full' value from 'check'
  qt_mergeds_data$extracted_quest[e_index] <- filtered_rows$extracted_quest[nas_index]
}
qt_nas <- qt_mergeds_data

library(dplyr)
colnames(qt_nas)[colnames(qt_nas) == ""] <- "X"
qt_nas <- qt_nas %>%
  mutate(f_q = str_extract(extracted_quest, ".*?(?=(?:answer|a n s w e r))"))
qt_nas$f_q<-trimws(qt_nas$f_q)

qt_nas$fulls <- tolower(gsub("[\r\n]", "", qt_nas$full))

qt_nas$f_q[qt_nas$X=="1458"]<-"will the minlster of goilsumer affairs, food at{d publtcdtstribution b$itfir arr*, ur<_a at{ gr{qfrfi futr{q d'* be pteased to state:(a) the main features of pradhan mantri garib kalyan anna yoina(pmgkay) along with the details of the amount santioned, allocated andutilized under this scheme so far in all the states including raiasthan;(b) the details of the number of beneficiaries covered under the saidscheme so far in dausa, sriganganagar and hanumangarh districts inraiasthan;(c) whether the government has received any complaints regardingirregularities in the said scheme in various states including rajasthan; and(d) if so, the details thereof along with the details of the strict stepstaken by the government in this regard?"
qt_nas$f_q[qt_nas$X=="1469"] <- "will the minlster of consuter affaire, food and pubt.tc distribution be pteased to state:(a) whether the govemment ls dlstributing food grains undor the atma nlrbharbharat abhiyan (anba) to migrant workers, if so, the detaits thereof;(b) whether the govenrment has any data of the nunrber of migrant wortarc in the country, if so, tha detaals thercof, state-wlse and district-waso; and(c) the total quantity of food gralns dlstributed to migrant workers undor anba?"
qt_nas$f_q[qt_nas$X=="1898"] <-"(a) the number of beneficiaries benefitted under pradhan  yolana (pilcl(y) tn tadhya pradesh (tp), distiict                                                                    irantri gadb katyan                                                      wise;  (b) whether the govemment has received any complaint regarding committed during imptementation of this scheme;                           irregurarities                                                      and (c) :f so, the details thereof?"
qt_nas$f_q[qt_nas$X=="1781"]<-"(a) the quantaty of paddy procured during year 2021 fuom state of punjabduring the last five yearc;(b) the quantity of paddy procured att over tndia state-wlse during thelast flve yearcl(c) the price of paddy procured at for the last fiye years for all states;(d) the reasons of delayed paddy procurement this season in punlab; and(e) whether global rice commodity prlces positively affect paddyprocurement prices in lndia and if so, the detaits thereof and if not, thereasons therefor?"
qt_nas$f_q[qt_nas$X=="1673"]<- "(a) whether there has been a global increase in the prlces of edible oils,if so, the details thereof;(b) whether the prices of essential commodities have increaseddrastically after covid-{9 lockdown period due to which consumens areforced to bear the brunt of inflation;(c) if so, the details of ancrease in prices of essential commodities andedible oils during the last three yeats and the reasons therefo4(d) whether the government have taken steps to control inflation, reducethe prices of daily use commodities and edible oil and also enhance theproduction of edible oils and if sor the details thereof;(e) whether unlimited storage of foodgrains is permi({ed by amending theessential gommodities act, {955 ln 2o2o and(0 whether companies are hoarding edible oils due to which prices ofthe same are increasing and if so, the steps taken to check hoading?"
qt_nas$f_q[qt_nas$X=="1451"] <-"(a) the details of the requirement and availability of foodgrains alongwith the present storage of food grains in the state of maharashtra;(b) the percentage of foodgrains getting damagedr rotten and stolenduring storage and transportation and the measures taken by thegovernment to prevent the loss of food grains due to these reasons;(c) whether foodgrains are being damaged more due to open storageand shortage of warehouses; and(d) if sor the steps taken by government to provide warehouses inadequate number and safe storage?"
qt_nas$f_q[qt_nas$X=="2198"]<-"(a) the number and capacrty ot warehouses construeted in the eountry, state-wise inctuding ralasthan durine the last three yeano;(b) the basis on which decision is taken to construct warehouses in anystate;(c) whether there is any pran to construct new warrhouses in raiasthanin the near future;(d) lf so, the detaits thereof; and(e) if not, the ns therefor?"
qt_nas$f_q[qt_nas$X=="1987"]<-"(a) the status of the 45 warehouse proiects sanctioned under the warehouse infrastructure fund for andhra pradesh; (b) whether the warehouse proiects sanctioned for avanigadda, l(anchkacherla, andigama, vut/yuru in krishna district been withdrawn;(c) if sor the proiect-wase detafls thereof and the reaso[s for withdrawat;(d) the details of procedure to get the proiects re-sanctioned; and(e) the details of non-operationar, ditapidated/ end-of-tine godowns inkrishna district?"
qt_nas$f_q[qt_nas$X=="1971"]<-"(a) whether the government has received any pnoposar frommarketang companies to sefl mini lpg cyrinders                        oirthe                                               through ration/fair priceshops an the country;(b) if so, the details thereof;(c) whether this has been imptemented in pilot                                        a      mode in the country;and(d) if so, the details thereof?"
qt_nas$f_q[qt_nas$X=="1445"]<-"(a) the number of procurement centres set up by various procurementagencies during the tast five yearc in raiasthan alongwith the quantum olpaddy and wheat procured, agency'wise and district'wise;(b) the arrangement for safe storage of crops brought by/from thefarmers, district-wisel and(c) the quantum of paddy, wheat and other crops got destroyed in theabsence of safe storage of procured crops in raiasthan during the lastfive yearc, district-wise?"
qt_nas$f_q[qt_nas$X=="1369"]<-"(a)the efforts made by the govemmcnt for producing error‐ free, tamper‐ proof and effortles sacccssible land rccords which hvec beecn thc pю      longcd govcmance challengc h thc country;and(b)the stcps taken by the govenlment to flnd asolution for computehzation ofland records whchhas bccn dragging on for dccadcs due to lack ofcapacity,will and rcsources and land bcinga state subjectt?"
qt_nas$f_q[qt_nas$X=="1365"]<-"(a) the details of the agreements entered into with private playerc for construction of sitos and other food storage facalities for the food corporation of india (fci) since 2ol4 including details of the private investors, state-wise;(b) the details of paynents made by fcl to private players for storage of foodgrains since 2ol4r year wise (c) the details of outstanding dues to fci and the loans availed by fcl since 2o14, year-wisel(d) whether fgt has requested for equity infusion and             so the details thereof including the equity infused by the governmentl                                                             'f (e) whether the increase in outstanding dues of gl is hampering its operationsl and(0 if so,the steps being taken to strengthen fcl and modernise existing storage facilities and augment capacity in the public sector?"
qt_nas$f_q[qt_nas$X=="2634"]<- "(a) whether the government has set up a target to construct 40 kilometers ofnational highway (nh) every day in the current financial year and if so, theextent to which it has been achieved so far;(b) whether the government has identified projects which help to provide logisticefficiency to the indian economy and if so, the details thereof, state-wise,particularly in maharashtra and odisha;(c) whether the government has identified congestion points and economiccentres which have huge traffic flow amongst them for efficient and smooth roadconnectivity;(d) if so, the details thereof, state-wise, particularly in maharashtra and odisha;and(e) whether the government proposes to securitise some nhs and expresswaysand if so, the details thereof, state-wise?"
qt_nas$f_q[qt_nas$X=="2462"] <- "PDF extraction issues"
qt_nas$f_q[qt_nas$X=="1333"]<-"(a)     the number of rti applications filed, answered and rejected, year-wise since 2014;(b)     the number of appeals/complaints made, answered and rejected by central information        commision (cic) and state information commissions (sics);(c)     the year-wise number of unanswered and pending appeals made to cic and sics and the        reasons therefor;(d)     the number of sanctioned posts and vacancies separately for cic and sics; and(e)     the reasons for not filling up the vacancies?"
qt_nas$f_q[qt_nas$X=="1100"]<-"(a) whether it is a fact that lot of payments to several states are pendingagainst procurement of food grains;(b) if so, the details thereof' state'wise ;(c) whether the government has set any target for the procurement of foodgrains in the country; and(d) if sor the details thereof, state'wise and items'wise? "
qt_nas$f_q[qt_nas$X=="1097"]<-" (a) whether the antodaya anna yojana (aay) is                          \"\".\"\",\"\",  goyernment in all the states                                                        being  imptemented   by the  (b) if so, the totat quantum and vatue of food grains  scheme during each of the last three years,                                                          altocated under the sa:d                                              state-wise;  (c) the number of famiries being benefited undet the                                                         said scheme, state-wise;  (d) the efforts made by the covemment to ensure scheme reach the targeted beneficiaries;                                                          that the benefits of the (e) whether the government has taken any steps to                                                           doubre the coverage o, the antodaya anna yoiana; and (0    if so, the details thereof and if not, the reasons                                                            thercfor?"
qt_nas$f_q[qt_nas$X=="1032"]<- "(a) the details of percentage of households covered under food securityscheme in the state of telanganar district-wise;(b) the details of funds allocated and released for the said schemeduring the last five years to the state of telangana, year-wise;(c) the details of funds not utilised by the state government, year'wise;(d) whether it is true that the percentage of households covered underthe food security act in the state of telangana is very low as comparedto the target fixed;(e) if so, the reasons therefo4 and(f)  whether the government proposes to add more beneficiaries underthe said scheme and if so, the details thereofil"
qt_nas$f_q[qt_nas$X=="912"]<-"(a) whether union government has implemented the goods and services tax (gst) system    across the country for collection of tax from the states;(b) if so, the details thereof;(c) the total amount collected by union government under gst tax system from maharashtra     state in the year 2019; and(d) the steps being taken by the union government to refund rs.15558 crore outstanding     amount of gst to the state government of maharashtra indicating the manner in which it     will be done?"

#which(is.na(qt_nas$f_q))
#82   86  103  105  323  432  453  467  731  882  921 1115 1139 1182
#1209 1415 1618 1630 1762 2170

qt_nas$fulls[1209]
qt_nas$f_q[82]<-"(a) the number of proposals received from assam under district disability rehabilitation centres oo : | .—  (ddrc) and assistance to disabled persons for purchase/fitting of aids and appliances (adip) . joo scheme during the year 2017-18; so ee pe }. (b) the number of proposals for which approval has been accorded; re an qa | (c) the number of proposals pending for approval; and oo | oo : - |a (d) the time by which the pending proposals are likely to be approved?"
qt_nas$f_q[86]<-"(a) whether the government is aware that the steps taken to rehabilitate stray cows is         insufficient; and         (b) if so, the details of the measures adopted by the government and details thereof?"
qt_nas$f_q[103]<-"(a) whether the government has formulated any comprehensive scheme to address the    problem of drinking water and supply of safe drinking water in the country;(b) if so, the details thereof along with the amount earmarked for the said purpose during the     current year;(c) the administrative mechanism designed to implement the said ambitious scheme across     the country; and(d) whether the government is likely to accord priority in the implementation of the said    scheme to those places where groundwater is polluted and if so, the details thereof?"
qt_nas$f_q[105]<-"in the country;         of proposals lying pending with\\*'         oeretopment of waterways                                                                                     since wateruays                                         initiated the proposals for their approvirl(b)\\u,,     whether the   government    has                                                              and         itr\"\".o\"\"i t\"\"o\"\"e the transport cost significantly;                                                                                        in this regard?                                    and the  future plan of action of the governntent(c)      if so, the details thereof"
qt_nas$f_q[323]<-"(a)     whether a number of projects are being undertaken and executed by        national building construction corporation limited during the last        five years, if so, the details thereof;(b)     whether nbcc is also undertaking projects in foreign countries, if        so, the details thereof, country-wise along with the quantum of        foreign exchange involved;(c)     whether nbcc is the implementing agency for executing projects        under the atal mission for rejuvenation and urban transformation,        pradhan mantri gram sadak yojana, solid waste management and        development works in north-eastern region and if so, the details        thereof along with success achieved in each project;(d)     whether nbcc is also working as project management consultant        for the redevelopment of pragati maidan project of india trade        promotion organisation and government colonies; and(e)     if so, the time by which the work on redevelopment of government        colonies will commence and get completed?"
qt_nas$f_q[432]<-"(a) whether the national building construction corporation (nbcc)       owes large sums of money to its vendors;   (b) if so, the quantum of sum/payment pending, vendor-wise; and   (c) the details and reasons of such pendency?"
qt_nas$f_q[453]<-"Will the Minister of Parliamentary Affairs be pleased to state (a) whether the Government maintains a database on Pre-legislative Consultation Policy (PLCP) compliance since its institutionalisation in 2014 (b) if so, the details thereof and if not, the reasons therefore; and (c) the details of the bills passed in the Parliament since inception of PLCP of which the number of bills that complied with the requisite PLCP conditions?"
qt_nas$f_q[467]<-"lent rnade by the(3overnment on the digital     :ndia land recor‖ modernization prograrnrne(d!lrmp)since its launch:    (b)the detalls of nuttber of land records which have been modernized and updated,     state‐ wise abng wlh the probに ms   faced by governmentinthb regard;     v肥                                                     me tta賦  思 1篇l∬腫恵:『it::鷲1:胤                                  is°  (e)the detans ofthe funds a‖ ocated under dilrmp to statesノ uts for suⅳ eys and re…     suⅳ eys across the countfy during each ofthe lastthtte years and the current year;    o whether the govemment has lxed any ttmeframe to compiete the land records     modemizalon pю gramme and r so,the detalls there"
qt_nas$f_q[731]<-"(a) whether the government is having a roadmap for regulating aerosports in oethe country so as to practice it in a more safe manner; ©(d) if so, the details of the status of issue of cars for each disciplines ofserosports, as recognized by world air sports federation; and |. ere is(c) whether the government has identified any organisation to monitorthefatal accidents occurring in aerosports?"
qt_nas$f_q[882]<-"(a) the details of goods and services tax (gst) dues pending from companies in the last three years, year-wise,(b) the expectations of the government in the next two years;(c) whether it is a fact that the gst collected last year was way short of the revenue as proposed in budget2020-21;(d) if so, the details thereof and if, not the proposed policies of the government to bridge the shortfall in thisyear’s revenue; and(e) the proactive measures/action taken by the government in this regard to boost the economy?"
qt_nas$f_q[921]<-"(a) whether it is a fact that a ban was imposed on bitcoin trading in the country and if so, the          details thereof:    (b) whether other crvplocurrencies are still under the ban and if so. the details thereof;    (c) whether it is true that despite the ban, illegal trading of cryptocurrency is still going on in          the country;    (d) if so, whether the government proposes to issue strict guidelines in this regard keeping          in view the risk involved therein: and    (e) if so, the details thereofand, ifnot, the reasons therefor?"
qt_nas$f_q[1115]<-"whethcr 5 53 crorc hccttcs ofland is tying unused despitc implemcntation of lntegrated waste      land dcvclopment programmc,drought pronc arca programme and dcscrt dcvclopmcnt      programme by thc govemmentin thc countγ ,if so,thc details thcreol and   (lll whethcr the govcmment has reduccd the target set in this regard as the funds a1loctted for       making thc land arable during the eleventh five year pian has not been spent and if so,the      rcasons thercfor?"
qt_nas$f_q[1139]<-"a) whether lndia has the potential to become the leader of global shipietycting ousiness and if so the details thereof;(b) the details of ship recycling units in the country' state-wise;(c) the details of the ships recycled in the country during the last fiveyears, year-wise;(d) the steps being taken by the government to establish lndia as the shiprecycling hub of the world;                                       with a- law for this purpose and ifrc\\ whether the government has come upthereof:  and!\"\"j, tn\"\"'j\"\"i\"\"ii\"\" ,r.\"\"g with prominent features                                                           boost the economic /fl   whether there are any estimates on the potential to il*-n,\"\"\"\"'\"\"\"\"', *\"\"rn oi              ship recycling units in the country and if                            \"\".,uori\"\"n'ng so, the details thereof?"
qt_nas$f_q[1182]<-"(a) whether the government has assessed the status of land reforms in various states; (b)if so, details tehereof, state-wise; (c) whether the government has given any directions for speedy implementation of land reforms; (d) if so, details thereof and (e) whether government has issued any guidelines in this regard"
qt_nas$f_q[1209]<-"(a)     whether the      promotion of employees particularly the        employees of     obc category is pending in directorate of        printing, and(b)     if so, the details thereof and the reasons therefor including the        date from which these pending promotions are likely to be        done?"
qt_nas$fulls[2170]
qt_nas$f_q[1415]<-"a) the steps taken by the government to preserve and promote indian culture in the     country during the last five years;b) the details of new programmes formulated to promote and create awareness about    indian culture during the said period; andc) the state/ut-wise details of the funds released to ngos for this purpose during the    said period?"
qt_nas$f_q[1618]<-"(a)                        and d~tails of the 'upgradation of forecast system' programme under atmosphere and               climate esear~h - modelling observing system and services (across) scheme;(b)            whether ,he prowamme has resulted in increased accuracy of weather forecast;(c)            if so, the detail~ thereof and if not, the reasons therefor; and(d)            the steps taken rbythe government to improve the accuracy of india meteorological department               (imd) f recast in the country, specially in the state of west bengal?"
qt_nas$f_q[1630]<-"(a) whether as per the data of world bank, india’s gross domestic product (gdp) per capita    per year is at usd 2277 while bangladesh’s is at usd 2503 during 2021;(b) if so, the details thereof;(c) whether in 2013, india’s gdp per capita per year was at usd 1449 while bangladesh’s    was at usd 981; and(d) if so, the reasons for india’s gdp lagging behind countries like bangladesh?"
qt_nas$f_q[1762]<-"(a) whether the government has invited proposals under the second phase of the rs.19,500 crore production-linked incentive (pli) scheme for the solar sector;(b) if so, the details thereof;(c) whether it is also true that the incentives would be disbursed for five years after thecommissioning of solar photovoltaic manufacturing plants on sales of high-efficiency solarphotovoltaic modules(d) if so, the details thereof; and(e) the total number of manufacturers benefited under pli-i along with the amount disbursedthereunder?"
qt_nas$f_q[2170]<-"(a) whether the official growth rate figures of the country do not take into account theunorganised sector of the economy, especially the micro and small-scale sectors;(b) if so, the details thereof and the reasons therefor;(c) whether the government is aware that if the unorganised sectors areindependently accounted for, the economy would be stagnant and not growing at 8per cent in 2021-22, as claimed officially;(d) if so, whether the government has any official figures for the unorganised sectorand whether it intends to incorporate those figures in calculating the official growthrate and if so, the details thereof; and(e) if not, the reasons therefor?"

qt_nas$f_q <- gsub("\\s+", " ", qt_nas$f_q)

q_mergeds_data <- e
update_ids <- qt_nas$id
# Iterate over each id and update e$full
for (id in update_ids) {
  # Find the index in 'e' that matches the 'id'
  e_index <- which(q_mergeds_data$id == id)
  
  # Find the index in 'check' that matches the 'id'
  nas_index <- which(qt_nas$id == id)
  
  # Update the 'full' column in 'e' with the corresponding 'full' value from 'check'
  q_mergeds_data$question_text[e_index] <- qt_nas$f_q[nas_index]
  
}

e<-q_mergeds_data

#_________________________________________________________

#NOW E needs only those columns where answer_text is NA

#CASE 3: answer_text is NA

ans_na <- which(is.na(e$answer_text)|e$answer_text==""|e$answer_text=="NA")
ans_nas <- e[ans_na,]

ans_nas$fulls <- tolower(gsub("[\r\n]", "", ans_nas$full))
ans_nas$fulls <- gsub("\\s+", " ", ans_nas$fulls)
ans_nas$extracted_quest <- sub(".*?(\\bwill\\b.*)", "\\1", ans_nas$fulls)
ans_nas$quest_final <- sub("(.*?(answer|a n s w e r)).*", "\\1", ans_nas$extracted_quest)
ans_nas$quest_final <- gsub("\\s+", " ", ans_nas$quest_final)
ans_nas$quest_final <- trimws(ans_nas$quest_final)

#Hindi: 8,104,130
ans_nas$quest_final[3]<-"will the minister of finance be pleased to state :-   a) whether there is a rise in circulation of fake currency in the country and malda region         of west bengal is one of the major sources of its circulation;   b)    if so, the details thereof during the last four years along with the loss suffered by the         country's economy due to such counterfcit curency;   c)    whether the banks have any policy to replace the fake notes or mutilated or scribbled         new currencies in order to avoid any financial losses to general public and if so, the         details thereof;   d) whether the govemment has made any efforts to identifu the sources of entry of         counterfeit curency in the country and if so, the details thereof; and   e)    the corrective steps being taken by the govemment /the reserve bank of india to         preventcirculation offake currency notes in the country? "
ans_nas$quest_final[11]<-"will the minister of information and broadcasting                                                                 be pleased to state:      (a) the percentage breakup of films by language registered with cbfc, year-          wise since 2008;      (b) the number of sanskrit cinematic films (feature film, documentary and          others) which have received cbfc certification in india, year-wise since          2008;      (c) whether the government intends to provide incentives for the production,          release and distribution of sanskrit language films and if so, the details          thereof; and      (d) the number of sanskrit cinematic films registered with cbfc and the number          of sanskrit cinematic films as a percentage of the total number of films with          cbfc?"
ans_nas$quest_final[13] <-"will the minister of fisheries, animal husbandry and dairyingमत्स्यपाऱन, पशुपाऱन और डेयरी मंत्रीbe pleased to state:(a) whether dairy business has almost come to a standstill due to corona pandemic and lockdown due to which lakhs of litres of milk of dairy farmers got spoiled and it has brought all the farmers dependent on dairy business on the verge of starvation;(b) whether the government has made any effective policy to conduct survey at the village level regarding the losses incurred by them and compensate them; and(c) if so, the details thereof?"
ans_nas$quest_final[28] <-"will the minister of ayush be pleased to state:(a) whether the research is being undertaken on various aspects of medicinal plants in thecountry;(b) if so, the details thereof, state/ut-wise; and(c) the details of institutions involved in such research work, state/ut-wise includinggujarat?"
ans_nas$quest_final[53]<-"will the minister of agriculture and farmers welfare                 कृ ष एवं कसान क याणमं ी be pleased to state:(a) whether the union government has received any complaints from various stategovernments regarding supply of spurious pesticides and fertilizers by some companieswhich is adversely affecting the agricultural production and if so, the details thereof;(b) the number of samples of various pesticides and fertilizers tested and those found to besub-standard during each of the last three years and the current year;(c) the number of cases regarding selling of spurious pesticides/fertilizers reported duringthe said period;(d) the action taken by the government against such erring companies;(e) whether there is any mechanism to monitor the sale of spurious pesticides/fertilizersand if so, the details thereof; and (f) the steps taken by the government to provide quality pesticides and fertilizers to thefarmers?"
ans_nas$quest_final[57]<-"will the minister of culture be pleased to state:(a)   whether the government is implementing a kala sanskriti vikas yojana (ksvy) and      if so, the details thereof and the aims and objective of ksvy;(b)   the challenges faced by the government while implementing ksvy;(c)   the details of achievement made under ksvy during the last three years and the      current year;(d)   the number of cultural organizations who have been given financial assistance for      the promotion of art and culture during the last three years and the current year;(e)   whether any financial help like scholarship and honorarium is being provided to the      artists for propagating, spreading and building their local culture in different parts of      the country during the last three years and the current year and if so, the details      thereof, state/ut-wise; and(f)   the other steps taken by the government for promotion of art and culture?"
ans_nas$quest_final[62]<-"will the minister of jal shakti be pleased to state:(a) the details of the total number of irrigation schemes including pradhan mantri krishi sinchai yojana(pmksy) introduced by the government during the last three years;(b) the percentage of cropped area covered under different forms of irrigation during the last three years,year and state-wise;(c) whether the government has achieved its target of implementation of irrigation schemes and if so,the details thereof, state-wise; and(d) the details of total funds allocated and expenditure incurred for each scheme during the last threeyears, state and year-wise? "
ans_nas$quest_final[75]<-"will the minister of finance be pleased to state:   (a) the number of central public sector units (cpsus) in the country that are making a   profit and also under loss during the last three years, state-wise;   (b) the details of profit making and under loss cpsus, state-wise; and   (c) the details of amount submitted by cpsus to the central treasury as a share of profit   during the last three years, state-wise?"
ans_nas$quest_final[92]<- "will the minister of environment, forest and climate change be pleased tostate:(a) whether it is a fact that the level of air pollutionhas increased significantly in varous parts    of the countryincluding delhi in comparison to the last three years, ifso, the details    thereof;(b) the steps taken by the government to ensurethat the issues of high pollution do not occur    in thenct again in the context of supreme court ban on industrial and contruction    activities in the months ofnovember and december, 2021;(c) whether the ministry has been alloted funds under union budget 2020-21 for tackling of    air pollutionin delhi ncr, if so, the details thereof including theallocation for the last    three financial years;(d) whether it is a fact that the polluted air frompakistan is affecting delhi, if so, the details    thereofincluding the findings/research/scientific experiments inthis regard; and(e) whether the government is considering to makeany changes in the current policy to    monitor the pollutionin view of the increasing level of air pollution, if so, thedetails    thereof?"


split_fulls <- strsplit(ans_nas$fulls, ans_nas$quest_final, fixed = TRUE)

# Extract the part after quest_final
ans_nas$ans_final <- sapply(split_fulls, function(x) tail(x, n = 1))
ans_nas$ans_final[3] <- "(a) sir, the data as reported by rbi and seizure of ficn by statefut police (scrbs) & other agencies as reported by national crime records bureau reveal that there is a declining trend in the circulation of fake indian counterfeit notes (ficn) in the country. it was reported by west bengal police that flow of ficns continues from indo-bangladesh border, particularly fiom malda area. however, all such ficns were of low quality i.e. computer generated/manipulated. (b) after the withdrawal of legal tender status of specified bank notes ofdenomination ofrs.l000and rs.500 on smnovember, 2016, there have been no reported cases of scizure of high quality fakeindian currency notes of rs.2000 and rs.500 denomination till early 2019. as such, there does not appearto be any appreciable loss now.(c) as per reserve bank of india's master circular dated july 1,2019, no credit to customer's account isto be given for counterfeil noles, ifany, detected in t}-te tender received over the counter or at the back-office/currency chest. reserve bank of india has advised all the bank to accept mutilated/defective/solid notes at all theirbranches vide master circular on \"\"facility for exchange ofnotes & coins\"\" dated july 1,2019. however,small finance bank and payment banks may exchange mutilated and defective notes at their option. themutilated notes are exchar.rged at all bank branches in terms of reserve bank of lndia (note refund)ruies,2009, as amended in 2018.(d) & (e) govemment has taken various steps to check the smuggling and circulation of fake indiancurrency notes in the country, which inter-alia, include:(i) fake indian currency notes (ficn) coordination group (fcord) has been formed by the ministryof home affairs (mha) to share inlelligences/information among the security agencies of thestates/centre to counter the problem of circulation of fake currency in the country. as per the report ofthe agencies, there have been instances where it has been found that the fake currency has been smuggledfrom the neighboring counlries.(ii) a terror funding and fake currency cell (tffc) has been constiluted in national investigationagency q.{ia) to investigate tcrror funding and fake currency cases.(iii) a memorandum of urrderstanding (mou) has been signed between india and bangladesh to prevenland counter smuggling and circulation offake currency notes.(iv) security at the international border has been strengthened by using new surveillances technology,deploying additional manporver for round the clock surveillance, cstablishing observation posts along theintemational border erectior.r ofborder fencing and intensive patrolling. i:***trr"
ans_nas$ans_final[53] <- sub(".*?(\\(a\\):.*$)", "\\1", ans_nas$ans_final[53], perl = TRUE)
ans_nas$ans_final[57] <- sub(".*?(\\(a\\) yes,.*$)", "\\1", ans_nas$ans_final[57], perl = TRUE)
ans_nas$ans_final[75]<- sub(".*?(\\(a\\) the state-wise.*$)", "\\1", ans_nas$ans_final[75], perl = TRUE)
ans_nas$ans_final[92]<- sub(".*?(\\(a\\) air quality index.*$)", "\\1", ans_nas$ans_final[92], perl = TRUE)
ans_nas$ans_final[105] <- paste0("(a)", ans_nas$ans_final[105])
ans_nas$ans_final <- sub(".*?a\\)", "a)", ans_nas$ans_final, perl = TRUE)
#which(!grepl("^a\\)", ans_nas$ans_final))

ans_nas$ans_final[1]<-"PDF extraction issues"
ans_nas$ans_final[8] <- "PDF content in Hindi"
ans_nas$ans_final[104] <- "PDF content in Hindi"
ans_nas$ans_final[130] <- "PDF content in Hindi"

ans_nas$quest_final[1]<-"PDF extraction issues"
ans_nas$quest_final[8] <- "PDF content in Hindi"
ans_nas$quest_final[104] <- "PDF content in Hindi"
ans_nas$quest_final[130] <- "PDF content in Hindi"


a_mergeds_data <- e
update_ids <- ans_nas$id
# Iterate over each id and update e$full
for (id in update_ids) {
  # Find the index in 'e' that matches the 'id'
  e_index <- which(a_mergeds_data$id == id)
  
  # Find the index in 'check' that matches the 'id'
  nas_index <- which(ans_nas$id == id)
  
  # Update the 'full' column in 'e' with the corresponding 'full' value from 'check'
  a_mergeds_data$question_text[e_index] <- ans_nas$quest_final[nas_index]
  a_mergeds_data$answer_text[e_index] <- ans_nas$ans_final[nas_index]
}
e<-a_mergeds_data 


e$answer_text<-trimws(e$answer_text)
e$question_text<-trimws(e$question_text)

duplicates <- duplicated(e$question_text)
duplicate_rows <- e[duplicates, ]
e$ministry[e$id=="173AU:8"]<-"FINANCE"
write.xlsx(e,"FINAL_MISSING_FOUND.xlsx")
write.csv(e,"FINAL_MISSING_FOUND.csv")

fin_mergeds_data <- c
update_ids <- e$id
# Iterate over each id and update e$full
for (id in update_ids) {
  # Find the index in 'e' that matches the 'id'
  e_index <- which(fin_mergeds_data$id == id)
  
  # Find the index in 'check' that matches the 'id'
  nas_index <- which( e$id == id)
  
  # Update the 'full' column in 'e' with the corresponding 'full' value from 'check'
  fin_mergeds_data$question_text[e_index] <- e$question_text[nas_index]
  fin_mergeds_data$answer_text[e_index] <- e$answer_text[nas_index]
}

#write.csv(fin_mergeds_data,"2019-2024.csv")
#write.xlsx(fin_mergeds_data,"2019-2024.xlsx")
