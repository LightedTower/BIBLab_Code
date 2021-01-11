
# ==============================================================================
# load packages
library(tidyverse)
library(psych)

# ==============================================================================
#This chunk imports the data files

#Set definition of location for working Directory
analyze_me <- "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker"

# set working directory to location of files on computer; defined above
setwd(analyze_me)

# list the files in current directory
list.files(pattern = ".csv$")

# create a list of the files
list_filenames <- list.files(pattern = ".csv$")
list_filenames

#Create empty list for datasheets
list_flanker <- list()

#Imputs files in directory into the empty list created above
for (i in 1:length(list_filenames)) {
  list_flanker[[i]] <- read_csv(list_filenames[[i]])
}

#Read in Exclusion Files
BOT <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/UnusableDataBots_DATA_LABELS_2020-08-15_0959.csv")
REPEATER <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/RepeatersAcrossStudy_Gorilla_2020-08-15_1050.csv")
MOOD_SCORES <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/MoodScoresz&IDs.csv")

#View variables as output tab
#View(BOT)
#View(REPEATER)

# ==============================================================================
##Binds data files into a dataframe for analysis
data_flanker_combined <- as.data.frame(do.call(rbind, list_flanker))

#Select specific columns from data sheets
data_flanker_stripped <- data_flanker_combined %>%
  select("Gorilla.Public.Id",
         "Participant Private ID",
         "Zone Type",
         "Reaction Time",
         "Correct",
         "Target",
         "Distractor"
  )

#filters dataframe by zonetype to get rid of all but participant response
data_flanker_filtered <- filter(data_flanker_stripped, `Zone Type` == "response_keyboard")

#View variables as output tab
#View(data_flanker_combined)
#View(data_flanker_filtered)

# ==============================================================================
#Exclusion removal

#Remove Bots from data_flanker_stripped
data_flanker_strippedBOT <- data_flanker_filtered[!(BOT$Gorilla.public.id == data_flanker_stripped$Gorilla.Public.Id),]
#data_flanker_strippedBOT

#Remove second participantion of repeaters from data_flanker_stripped
data_flanker_stripREPEATE <- data_flanker_strippedBOT[!(REPEATER$List.Public.ID == data_flanker_strippedBOT$Gorilla.Public.Id),]
#data_flanker_stripREPEATE 

#View(data_flanker_strippedBOT)
#View(data_flanker_stripREPEATE)

# ==============================================================================
#Defines the type of question as congruent or incongruent
data_flanker_stripREPEATE$`Question Type` <- 'Undefined'

data_flanker_stripREPEATE <- mutate(
  data_flanker_stripREPEATE,
  `Question Type` = if_else(
    Target != Distractor,
    "Incongruent",
    "Congruent",
    missing = "Undefined"
  )
)
#View(data_flanker_stripREPEATE)

#New ==============================================================================
#Merges MOOD_SCORES with data_flanker_filtered so that the mood and ID's from Redcap
#are in the same place as the behavior scores.

##Note:Merge wouldnt work unless I had two different column names to merge together,
##not sure why.

data_flanker_merged <- merge(MOOD_SCORES, data_flanker_stripREPEATE, by.x ="Gorilla_Public_ID", by.y = "Gorilla.Public.Id")
#View(data_flanker_merged)

# ==============================================================================
# New ==============================================================================
#!!!Adjust so that it groups by private id, and prints out a list of those who the script is removing from the dataframe.!!!

#Filters out participants who's mean RT are considered Outliers (-2 <= Reaction Time Mean >= 2)
#data_flanker_trimmed <- data_flanker_merged %>%
#  filter(`Reaction Time` <= mean(data_flanker_merged$`Reaction Time`, na.rm = TRUE) + 2 * sd(data_flanker_merged$`Reaction Time`, na.rm = TRUE),
 #        `Reaction Time` >= mean(data_flanker_merged$`Reaction Time`, na.rm = TRUE) - 2 * sd(data_flanker_merged$`Reaction Time`, na.rm = TRUE))
#View(data_flanker_trimmed)

# ==============================================================================
# ==============================================================================
#Loop to caclculate overall Mean, SD, SE, and % correct
total_summary_zone_types <- data_flanker_merged %>%
  group_by(`Gorilla_Public_ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Congruent Mean, SD, SE, and % correct
congruent_summary_zone_types <- data_flanker_merged %>%
  filter(`Question Type` == "Congruent") %>%
  group_by(`Gorilla_Public_ID`) %>%
  summarize(con_RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            con_RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            con_RT_se  = con_RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/con_RT_mn)
            #PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Incongruent Mean, SD, SE, and % correct
incongruent_summary_zone_types <- data_flanker_merged %>%
  filter(`Question Type` == "Incongruent") %>%
  group_by(`Gorilla_Public_ID`) %>%
  summarize(In_RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            In_RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            In_RT_se  = In_RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/In_RT_mn)
            #PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#View(total_summary_zone_types)
#View(congruent_summary_zone_types)
#View(incongruent_summary_zone_types)
#==============================================================================
#New ==========================================================================
#This chunk creates 3 dataframes requireing the merge of multiple dataframes

#Creates a database of all summary data, combined with all demographic and behavioral data
complete_summary_full <- Reduce(merge, list(data_flanker_merged, total_summary_zone_types, congruent_summary_zone_types, incongruent_summary_zone_types))
#View(complete_summary_full)
write.csv(complete_summary_full, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Flanker_Full_Summary_and_demographics")

complete_summary <- Reduce(merge, list(total_summary_zone_types, congruent_summary_zone_types, incongruent_summary_zone_types))
#View(complete_summary)
write.csv(complete_summary, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Flanker_Summary")

complete_summary_demographics<- Reduce(merge, list(MOOD_SCORES,total_summary_zone_types, congruent_summary_zone_types, incongruent_summary_zone_types))
View(complete_summary_demographics)
write.csv(complete_summary_demographics, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Flanker_Summary_and_demographics")

#New ==============================================================================
#This chunk creates an output of the database including only participants above a certain % correct.

#Creates output of those whose overall percent correct is 70% or above, and creates a csv
above_70_data <- complete_summary_demographics[!(complete_summary_demographics$PCorrect < "0.7"),]
View(above_70_data)
write.csv(above_70_data, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Flanker_above_.7_accuracy.csv")

#Creates output of those whose overall percent correct is 70% or above, and creates a csv
above_50_data <- complete_summary_demographics[!(complete_summary_demographics$PCorrect < "0.5"),]
View(above_50_data)
write.csv(above_50_data, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Flanker_above_.5_accuracy.csv")

# ==============================================================================
#Graphs Overall

## Histogram for distribution of PCorrect
ggplot(data = total_summary_zone_types, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("Flanker Overall Prop Correct")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Overall_Flanker_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = total_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Flanker Mean Overall Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Overall_Flanker_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = total_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Flanker Overall Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Overall_Flanker_SD.pdf")

# ==============================================================================
#Graphs Congruent

## Histogram for distribution of PCorrect
#ggplot(data = congruent_summary_zone_types, aes(x = PCorrect)) +
#geom_histogram() +
#xlab("Proportion Correct") +
#ggtitle("Flanker Congruent Prop Correct")
#ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Congruent_Flanker_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = congruent_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Flanker Mean Congruent Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Congruent_Flanker_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = congruent_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Flanker Congruent Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Congruent_Flanker_SD.pdf")

# ==============================================================================
#Graphs Incongruent

## Histogram for distribution of PCorrect
#ggplot(data = incongruent_summary_zone_types, aes(x = PCorrect)) +
#geom_histogram() +
#xlab("Proportion Correct") +
#ggtitle("Flanker Incongruent Prop Correct")
#ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Incongruent_Flanker_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = incongruent_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Flanker Mean Incongruent Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Inongruent_Flanker_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = incongruent_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Flanker Incongruent Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Inongruent_Flanker_SD.pdf")

# ==============================================================================

#Writes summaries to .csv
write.csv(data_flanker_trimmed, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Flanker_without_OL.csv")
write.csv(data_flanker_merged, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Flanker_data.csv")
write.csv(total_summary_zone_types, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Flanker_Overall_Summary.csv")
write.csv(congruent_summary_zone_types, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Flanker_Congruent_Summary.csv")
write.csv(incongruent_summary_zone_types, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker/Post_Bot_Repeater_Removal/Flanker_Incongruent_Summary.csv")



