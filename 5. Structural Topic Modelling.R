library("quanteda")
library("stm")
library("tidyverse")
library("stringi")
library("lubridate")
library("openxlsx")
library("readxl")

#Analysing Themes within Women Centric Questions 
gender_quest <- read_excel("path/Women_Centric.xlsx")
gender_quest <- gender_quest %>% as_tibble()

#Aggregating Gender column to determine if male only group or female only group or mixed gender group

get_gender_label <- function(gender_string) {
  # Split the gender string by comma and remove leading/trailing whitespaces
  gender_split <- trimws(unlist(strsplit(gender_string, ",")))
  
  # Check if "Female" is present in the gender string
  has_female <- any(grepl("\\bFemale\\b", gender_split, ignore.case = TRUE))
  
  # Check if "Male" is present in the gender string
  has_male <- any(grepl("\\bMale\\b", gender_split, ignore.case = TRUE))
  
  # Determine the label based on the presence of "Female" and "Male"
  if (has_female && has_male) {
    return("Both")
  } else if (has_female) {
    return("Female")
  } else if (has_male) {
    return("Male")
  } else {
    return(NA)  
  }
}

# Apply the function to each row of the dataframe and assign labels to a new column
gender_quest$gender_label <- apply(gender_quest, 1, function(row) get_gender_label(row["gender"]))

gender_quest <- gender_quest %>% 
  filter(!is.na(gender_label))

#gender_quest$full_text <- stri_unescape_unicode(gender_quest$full_text) # unescaping unicodes

gender_quest_stm<- gender_quest %>%
  select(id,date,ls_number,full_text,gender_label) %>% drop_na()

# Creating a day of the year column
gender_quest_stm$date <- as_datetime(gender_quest_stm$date)
gender_quest_stm$day_of_year <- yday(gender_quest_stm$date)

#Gender
gender_quest_stm$gender_label <- factor(gender_quest_stm$gender_label,levels = c("Female","Male","Both"))


# Creating corpus
gender_stm_corpus <- corpus(gender_quest_stm$full_text,
                       docvars = gender_quest_stm[,c("id", "date", "day_of_year",
                                       "gender_label", "ls_number")])

#Added to list after developing wordclouds for each theme
tokens_to_remove <- c("sh","whether","smt","km","thereof","abovementioned","jammu","kashmir","srinagar","himachal","pradesh","punjab","ladakh","leh","shimla","chandigarh","uttarakhand","dehradun","haryana","delhi","uttar","lucknow","rajasthan","jaipur","gujarat","gandhinagar","maharashtra","mumbai","karnataka","bangalore","bengaluru","goa","panjim","kerala","thiruvananthapuram","tamil","nadu","chennai","andhra","telangana","hyderabad","odisha","orissa","bhubaneshwar","chattisgarh","ranchi","madhya","bhopal","bihar","patna","sikkim","gangtok","arunachal","assam","itanagar","dispur","meghalaya","shillong","nagaland","kohima","manipur","imphal","mizoram","aiswal","aizwal","tripura","agartala","andaman","nicobar","leh","ladakh","pondicherry","puducherry","daman","diu","dadra","nagar","haveli","lakshadweep","blair","kavaratti","able","pursue","nr","abc","sh","irani","kumar","bindra", "awaited","project","according","access","accessing","uc","child","children","girl","girls","government", "details", "state", "scheme", "women", "national", "development", "india", "taken", "country", "years", "rs", "per", "year", "states", "number", "programme", "including", "also", "schemes", "act", "central", "last", "three", "various", "total", "funds", "new", "areas", "provided", "cases", "steps", "minister", "services", "etc", "indian", "given", "released", "steps", "level", "etc", "district", "given", "implementation", "action", "ii", "i", "iii", "released", "ltd", "time", "state", "sector", "plan", "made", "provide", "regard", "nil", "annexure", "th", "current", "received", "facilities", "persons", "lakh", "lakhs", "thousand", "thousands", "rupees", "special", "system", "centre", "districts", "available", "set", "committee", "sabha", "amount", "one", "year", "said", "support", "age", "core", "union", "institutions", "report", "based", "lok", "sabha", "rajya", "sabha", "governments", "crore", "crores", "measures", "projects", "rate", "providing", "scheduled", "regarding", "policy", "however", "management", "construction", "reported", "state-wise", "affairs", "nil", "control", "activities", "department", "departments", "society", "fund", "ms", "high", "low", "data", "community", "ensure", "referred", "two", "following", "guidelines", "order", "proposal", "proposed", "due", "statesuts", "ut", "uts", "group", "name", "iv", "provides", "cost", "may", "issued", "issuing", "issues", "rs", "like", "launched", "service", "free", "consucted", "among", "part", "action", "basis", "working", "setting", "set", "increase", "five", "sir", "allocated", "include", "statewise", "approved", "general", "country", "details", "status", "agencies", "since", "distt", "ut-wise", "concerned", "provisions", "production", "section", "law", "central", "different", "upto", "proposals", "improve", "well", "across", "existing", "likely", "centrally", "dated", "units", "female", "allocation", "use", "present", "planning", "complaints", "jammu", "board", "effective", "major", "world", "related", "shall", "process", "officers", "strengthening", "registered", "issues", "purpose", "lakh", "date", "additional", "view", "kashmir", "review", "make", "annual", "singh", "identified", "addition", "annexurei", "take", "grant", "promote", "sponsored", "nagar", "subject", "recommended", "recommend", "till", "within", "required", "wise", "post", "jammu", "kashmir", "daman", "diu", "dadra", "haveli", "levels", "every", "held", "govt", "na", "rules", "utsvi", "pradesh", "specific", "crores", "members", "member", "list", "grants", "per", "sabha", "starred", "unstarred", "question", "questions", "respect", "respectively", "smt", "help", "progress", "stateut", "measures", "day", "first", "namely", "across", "place", "region", "need", "participation", "several", "ie")

# Tokenize and preprocess the text data
gender_dfm <- gender_stm_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE,
         remove_url = TRUE) %>%
  tokens_remove(stopwords("en"), padding = TRUE) %>%
  tokens_remove(tokens_to_remove) %>% 
  tokens_remove("^[a-zA-Z]$", valuetype = "regex") %>%
  tokens_ngrams(n = 1:2) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 5)

#Converting DFM suitable to apply Structural Topic Modelling
gender_stm <- convert(gender_dfm , to = "stm")

# k specifies the no of topics to be generated. Done through iterative process
stmodel <- stm(documents = gender_stm$documents, vocab = gender_stm$vocab,
               K =12, prevalence =~ gender_label + s(day_of_year),
               data = gender_stm$meta, verbose = TRUE, init.type = "Spectral")
#Visualisation: The plot visualizes the proportion of each topic across the corpus
plot(stmodel)
labelTopics(stmodel)

# Shows the most probable words and their prevalence to help interpret the modeled topics.
cloud(stmodel, topic =12, scale = c(2,.25))
png("frex_plot.png", width = 7 * 100, height = 7 * 100, res = 100)
# Generate the plot
plot(stmodel, type = 'labels', topics = c(1,2,3,4,5,6,7,9,10,11,12), labeltype = 'frex', main = 'FREX')
dev.off()

# It estimates how the prevalence of topics (1 to 12) is influenced by gender_label

effect_estimates <- estimateEffect(1:12 ~ gender_label + s(day_of_year), stmodel, meta = gender_stm$meta)
summary(effect_estimates)
plot(effect_estimates, covariate = "gender_label", 
     topics = c(1,2,3,4,5,6,7,9,10,11,12),
     model = stmodel, method = "difference",
     cov.value1 = "Female", cov.value2 = "Male",
     xlab = "Female Parliamentarians                                                                                        Male Parliamentarians", 
     main = "Analyzing Parliamentary Discourse: Questions about Women by Gender",
     xlim = c(-.1, .1), labeltype = "custom", 
     custom.labels = c("Banking and Rural Finance", "Water Infrastructure", 
                       "Education", "Nutrition", "Social Protection", 
                       "Medical Infrastructure", "Law Enforcement", 
                       "Manufacturing & Allied Sectors", "Agriculture", 
                       "Public Health", "Sports"))

# How each gender dicusses each topic differnetly
stmodel_prev <- stm(
  documents = gender_stm$documents,
  vocab = gender_stm$vocab,
  K = 12,  # Number of topics
  prevalence = ~ gender_label+ s(day_of_year) , 
  content = ~gender_label,
  data = gender_stm$meta,
  verbose = TRUE,
  init.type = "Spectral"
)
plot(stmodel_prev)
cloud(stmodel_prev, topic = 2, scale = c(2,.25))
plot(stmodel_prev, type = "perspectives", topics = 3)

prep_effect <- estimateEffect(1:12 ~ gender_label + s(day_of_year), 
                              stmobj = stmodel_prev, 
                              metadata = gender_stm$meta,
                              uncertainty = "Global")
