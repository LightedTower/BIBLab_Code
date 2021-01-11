
# load packages
library(tidyverse)
library(psych)

# ==============================================================================
#Pull in data file & set working directory
delay_dir <-  "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Delayed Discount"
setwd(delay_dir)

# list the files in current directory
list.files(pattern = ".csv$")   # CHANGE THIS TO MATCH INPUT FILE TYPE

# create a list of the files
list_delayfilenames <- list.files(pattern = ".csv$")
list_delayfilenames

# empty list to receive files
list_delay <- list()

for (i in 1:length(list_delayfilenames)) {
  list_delay[[i]] <- read_csv(list_delayfilenames[i])
}

#BOTS and REPEATERS file imports
BOT <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/UnusableDataBots_DATA_LABELS_2020-08-15_0959.csv")
REPEATER <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/RepeatersAcrossStudy_Gorilla_2020-08-15_1050.csv")
MOOD <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/MoodScoresz&IDs.csv")

#View(BOT)
#View(REPEATER)
#View(MOOD)

# ==============================================================================
data_delay_combined <- as.data.frame(do.call(rbind, list_delay))

data_delay_stripped <- data_delay_combined %>%
  select("Participant Public ID",
         "Task Name",
         "Task Version",
         "Trial Number",
         "Amount1",
         "Amount2",
         "Response",
         "Reaction Time")

#View(data_delay_stripped)


##Remove Bots from and repeate participants from 'data_delay_stripped'
data_delay_noBOTS <- data_delay_stripped[!(BOT$Gorilla.public.id == data_delay_stripped$`Participant Public ID`),]
#View(data_delay_noBOTS)

data_delay_excluded <- data_delay_noBOTS[!(REPEATER$Gorilla.public.id == data_delay_stripped$`Participant Public ID`),]
#View(data_delay_excluded)

# ==============================================================================
# Summary table for mean and sd of RTs
summary_data_delay <- data_delay_excluded %>%
  group_by(`Participant Public ID`) %>%
  summarize(RT_mn = mean(`Reaction Time`, na.rm = TRUE),
            RT_sd = sd(`Reaction Time`, na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(`Reaction Time`, na.rm = TRUE)/RT_mn))

#View(summary_data_delay)
#257 entries
# ==============================================================================

# Count delayed reward responses responses for each question

#List unique ids for participants
listID      <- unique(data_delay_excluded$`Participant Public ID`) %>%
  na.omit()

#View(listID)
#256 entries

#List of response options for participants 
listprompts <- list("Receive $55 in 117 days",     
                    "Receive $75 in 61 days",      
                    "Receive $25 in 53 days",      
                    "Receive $85  in 7 days",  
                    "Receive $25  in 19 days",  
                    "Receive $50  in 160 days",  
                    "Receive $35  in 13 days",  
                    "Receive $60  in 14 days",  
                    "Receive $80 in 162 days",
                    "Receive $55  in 62 days",
                    "Receive $30 in 7 days",
                    "Receive $75  in 119 days",
                    "Receive $35  in 186 days",
                    "Receive $50  in 21 days",
                    "Receive $85 in 91 days",
                    "Receive $60 in 89 days",
                    "Receive $85  in 157 days",
                    "Receive $35  in 29 days",
                    "Receive $80  in 14 days",
                    "Receive $30  in 179 days",
                    "Receive $50 in 30 days",
                    "Receive $30 in 80 days",
                    "Receive $75  in 20 days",
                    "Receive $60 in 111 days",
                    "Receive $80 in 30 days",
                    "Receive $25 in 136 days",
                    "Receive $55  in 7 days")


countrows <- list()

##What does this do?
for (i in 1:length(listprompts)) {
  tempdat <- data_delay_excluded %>%
    filter(Amount2 == listprompts[i])
  
  listcount <- list()
  
##What does this do?
  for (j in 1:length(listID)) {
    listcount[j] <- tempdat %>%
      filter(`Participant Public ID` == listID[j], Response == listprompts[i]) %>%
      nrow()

##What does this do?   
    countrows[[i]] <- matrix(data = listcount,
                           ncol = length(listID),
                           nrow = 1,
                           dimnames = list(listprompts[i], listID),
                           byrow = TRUE) %>%
      as.data.frame()
  }
  
}
# ==============================================================================
##What does this block do?
countmatrix  <- do.call(rbind, countrows)

countmatrix2 <- data.frame(lapply(countmatrix, as.character), stringsAsFactors=FALSE)

rownames(countmatrix2) <- listprompts

#View(countmatrix)
#View(countmatrix2)
#256 entries in both

# ==============================================================================
#Write results to output files
write.table(countmatrix2, file = "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Delayed Discount/Outputs/CountedDelayDiscount.csv")
write.csv(summary_data_delay,"C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Delayed Discount/Outputs/DelayDiscount_Summary.csv")
write.csv(listID, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Delayed Discount/Outputs/IDs_Included.csv")
