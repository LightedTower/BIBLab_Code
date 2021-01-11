
# ==============================================================================
# load packages
library(tidyverse)
library(psych)

# ==============================================================================
#This chunk imports the data files

#!!!Change for other scripts line 12 & 15
analyze_me <- "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task"

# set working directory to location of files on computer; defined above
setwd(analyze_me)

# list the files in current directory
list.files(pattern = ".csv$")

# create a list of the files
list_filenames <- list.files(pattern = ".csv$")
list_filenames

#Create empty list for datasheets
list_simons <- list()

#Imputs files in directory into the empty list created above
for (i in 1:length(list_filenames)){
  list_simons[[i]] <- read_csv(list_filenames[[i]])
}

#Read in Exclusion Files
BOT <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/UnusableDataBots_DATA_LABELS_2020-08-15_0959.csv")
REPEATER <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/RepeatersAcrossStudy_Gorilla_2020-08-15_1050.csv")
MOOD_SCORES <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/MoodScoresz&IDs.csv")

#View variables as output tab
#View(BOT)
#View(REPEATER)

# ==============================================================================
##Binds data into dataframe for analysis
data_simons_combined <- as.data.frame(do.call(rbind, list_simons))

#Select specific columns from data sheets
data_simons_stripped <- data_simons_combined %>%
  select("Gorilla.Public.Id",
         "Participant Private ID",
         "Zone Type",
         "Reaction Time",
         "Correct",
         "Go"
  )

#filters dataframe by zonetype to get rid of all but participant response
data_simons_filtered <- filter(data_simons_stripped, `Zone Type` == "response_keyboard")

#View variables as output tab
#View(data_simons_combined)
#View(data_simons_stripped)
#View(data_simons_filtered)
# ==============================================================================
#Remove exclusions from data_simons_filtered

data_simons_strippedBOT <- data_simons_filtered[!(BOT$Gorilla.public.id == data_simons_stripped$Gorilla.Public.Id),]
#data_simons_strippedBOT

data_simons_stripREPEATE <- data_simons_strippedBOT[!(REPEATER$List.Public.ID == data_simons_strippedBOT$Gorilla.Public.Id),]
#data_simons_stripREPEATE

#View variables as output tab
#View(data_simons_strippedBOT)
#View(data_simons_stripREPEATE)

# ==============================================================================
#Defines the type of question as congruent or incongruent
data_simons_stripREPEATE$`Question Type` <- 'Undefined'

data_simons_stripREPEATE <- mutate(
  data_simons_stripREPEATE,
  `Question Type` = if_else(
    Go != "LEFTleft.jpg" & Go != "RIGHTright.jpg",
    "Incongruent",
    "Congruent",
    missing = "Undefined"
  )
)

#View variables as output tab
#View(data_simons_stripREPEATE)

#New ==============================================================================
#Merges MOOD_SCORES with data_simons_filtered so that the mood and ID's from Redcap
#are in the same place as the behavior scores.

##Note:Merge wouldnt work unless I had two different column names to merge together,
##not sure why.

data_simons_merged <- merge(MOOD_SCORES, data_simons_stripREPEATE, by.x ="Gorilla_Public_ID", by.y = "Gorilla.Public.Id")

#View variables as output tab
#View(data_simons_merged)

# ==============================================================================
#New ==============================================================================
#!!!Adjust so that it groups by private id, and prints out a list of those who the script is removing from the dataframe.!!!

#Filters out participants who's mean RT are considered Outliers (-2 <= Reaction Time Mean >= 2)
#data_simons_trimmed <- data_simons_merged %>%
#  filter(`Reaction Time` <= mean(data_simons_merged$`Reaction Time`, na.rm = TRUE) + 2 * sd(data_simons_merged$`Reaction Time`, na.rm = TRUE),
#        `Reaction Time` >= mean(data_simons_merged$`Reaction Time`, na.rm = TRUE) - 2 * sd(data_simons_merged$`Reaction Time`, na.rm = TRUE))

#View variables as output tab
#View(data_simons_trimmed)

# ==============================================================================
# New; Changed group by to `Gorilla_Public_ID`==================================
#Loop to caclculate overall Mean, SD, SE, and % correct
total_summary_zone_types <- data_simons_merged %>%
  group_by(Gorilla_Public_ID) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Congruent Mean, SD, SE, and % correct
congruent_summary_zone_types <- data_simons_merged %>%
  filter(`Question Type` == "Congruent") %>%
  group_by(Gorilla_Public_ID) %>%
  summarize(Con_RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            Con_RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            Con_RT_se = Con_RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/Con_RT_mn)
            #PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Incongruent Mean, SD, SE, and % correct
incongruent_summary_zone_types <- data_simons_merged %>%
  filter(`Question Type` == "Incongruent") %>%
  group_by(Gorilla_Public_ID) %>%
  summarize(IN_RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            IN_RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            IN_RT_se  = IN_RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/IN_RT_mn)
            #,PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#View variables as output tab
#View(total_summary_zone_types)
#View(congruent_summary_zone_types)
#View(incongruent_summary_zone_types)
# ==============================================================================
#New ==========================================================================
#This chunk creates 3 dataframes requireing the merge of multiple dataframes

#Creates a database of all summary data, combined with all demographic and behavioral data
complete_summary_full <- Reduce(merge, list(data_simons_merged , total_summary_zone_types, congruent_summary_zone_types, incongruent_summary_zone_types))
#View(complete_summary_full)
write.csv(complete_summary_full, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Simons_FullSummary_and_demographics.csv")

#Creates a database of all summary data
summary_full <- Reduce(merge, list(total_summary_zone_types, congruent_summary_zone_types, incongruent_summary_zone_types))
#View(summary_full)
write.csv(summary_full, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Simons_Summary.csv")

#Creates a database of all summary data, combined with all demographic data
complete_Summary_Demographic <- Reduce(merge, list(MOOD_SCORES, total_summary_zone_types, congruent_summary_zone_types, incongruent_summary_zone_types))
#View(complete_Summary_Demographic)
write.csv(complete_Summary_Demographic, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Simons_Summary_and_Demographics.csv")

# New ==============================================================================
#This chunk creates an output of the database, including only participants above a certain % correct.

#Creates output of those whose overall percent correct is 70% or above, and creates a csv
above_70_data <- complete_Summary_Demographic[!(complete_Summary_Demographic$PCorrect < "0.7"),]
#View(above_70_data)
write.csv(above_70_data, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Simon_above_.7_accuracy.csv")


#Creates output of those whose overall percent correct is 50% or above, and creates a csv
above_50_data <- complete_Summary_Demographic[!(complete_Summary_Demographic$PCorrect < "0.5"),]
#View(above_50_data)
write.csv(above_50_data, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Simon_above_.5_accuracy.csv")

#==============================================================================
#Graphs Overall

## Histogram for distribution of PCorrect
ggplot(data = total_summary_zone_types, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("simons Overall Prop Correct")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Overall_simons_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = total_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("simons Mean Overall Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Overall_simons_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = total_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("simons Overall Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Overall_simons_SD.pdf")

# ==============================================================================
#Graphs Congruent

## Histogram for distribution of PCorrect
#ggplot(data = congruent_summary_zone_types, aes(x = PCorrect)) +
  #geom_histogram() +
  #xlab("Proportion Correct") +
  #ggtitle("simons Congruent Prop Correct")
#ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Congruent_simons_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = congruent_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("simons Mean Congruent Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Congruent_simons_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = congruent_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("simons Congruent Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Congruent_simons_SD.pdf")

# ==============================================================================
#Graphs Incongruent

## Histogram for distribution of PCorrect
#ggplot(data = incongruent_summary_zone_types, aes(x = PCorrect)) +
  #geom_histogram() +
  #xlab("Proportion Correct") +
  #ggtitle("simons Incongruent Prop Correct")
#ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Incongruent_simons_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = incongruent_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("simons Mean Incongruent Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Inongruent_simons_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = incongruent_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("simons Incongruent Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/Inongruent_simons_SD.pdf")

# ==============================================================================

#Writes summaries to .csv
write.csv(data_simons_trimmed, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/simons_without_OL.csv")
write.csv(data_simons_merged, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/simons_data.csv")
write.csv(total_summary_zone_types, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/simons_Overall_Summary.csv")
write.csv(congruent_summary_zone_types, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/simons_Congruent_Summary.csv")
write.csv(incongruent_summary_zone_types, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task/Post_Bot_Repeater/simons_Incongruent_Summary.csv")
