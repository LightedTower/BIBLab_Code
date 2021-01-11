
# ==============================================================================
# load packages
library(tidyverse)
library(psych)

# ==============================================================================
#This chunk imports the data files

#Set definition of location for working Directory
analyze_me <- "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search"

# set working directory to location of files on computer; defined above
setwd(analyze_me)

# list the files in current directory
list.files(pattern = ".csv$")

# create a list of the files
list_filenames <- list.files(pattern = ".csv$")
list_filenames

#Create empty list for datasheets
list_visual <- list()

#Imputs files in directory into the empty list created above
for (i in 1:length(list_filenames)) {
  list_visual[[i]] <- read_csv(list_filenames[[i]])
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
data_visual_combined <- as.data.frame(do.call(rbind, list_visual))

#Select specific columns from data sheets
data_visual_stripped <- data_visual_combined %>%
  select("Gorilla.Public.Id",
         "Participant Private ID",
         "Zone Type",
         "Reaction Time",
         "Correct",
         "Answer"
  )

#filters dataframe by zonetype to get rid of all but participant response
data_visual_filtered <- filter(data_visual_stripped, `Zone Type` == "response_keyboard")


#View variables as output tab
#View(data_visual_combined)
#View(data_visual_filtered)

# ==============================================================================
#Exclusion removal

#Remove Bots from data_visual_stripped
data_visual_strippedBOT <- data_visual_filtered[!(BOT$Gorilla.public.id == data_visual_filtered$Gorilla.Public.Id),]
#data_visual_strippedBOT

#Remove second participantion of repeaters from data_visual_stripped
data_visual_stripREPEATE <- data_visual_strippedBOT[!(REPEATER$List.Public.ID == data_visual_strippedBOT$Gorilla.Public.Id),]
#data_visual_stripREPEATE 

#View variables as output tab
#View(data_visual_strippedBOT)
#View(data_visual_stripREPEATE)

# ==============================================================================
# Defines the type of question as congruent or incongruent
data_visual_stripREPEATE$`Question Type` <- "Undefined"

data_visual_stripREPEATE <- mutate(
  data_visual_stripREPEATE,
  `Question Type` = if_else(
    Answer != "Present",
    "Incongruent",
    "Congruent",
    missing = "Undefined"
  )
)

#View variables as output tab
#View(data_visual_stripREPEATE)

#New ==============================================================================
#Merges MOOD_SCORES with data_visual_filtered so that the mood and ID's from Redcap
#are in the same place as the behavior scores.

##Note:Merge wouldnt work unless I had two different column names to merge together,
##not sure why.

data_visual_merged <- merge(MOOD_SCORES, data_visual_stripREPEATE, by.x ="Gorilla_Public_ID", by.y = "Gorilla.Public.Id")

#View variables as output tab
#View(data_visual_merged)

# ==============================================================================
#New ==============================================================================
#!!!Adjust so that it groups by private id, and prints out a list of those who the script is removing from the dataframe.!!!

#Filters out participants who's mean RT are considered Outliers (-2 <= Reaction Time Mean >= 2)
#data_visual_trimmed <- data_visual_merged %>%
#  filter(`Reaction Time` <= mean(data_visual_merged$`Reaction Time`, na.rm = TRUE) + 2 * sd(data_visual_merged$`Reaction Time`, na.rm = TRUE),
#         `Reaction Time` >= mean(data_visual_merged$`Reaction Time`, na.rm = TRUE) - 2 * sd(data_visual_merged$`Reaction Time`, na.rm = TRUE))
#View(data_visual_trimmed)

#View variables as output tab
#View(data_visual_trimmed)

# ==============================================================================
# New; Changed filter by to public id===========================================
#Loop to caclculate overall Mean, SD, SE, and % correct
total_summary_zone_types <- data_visual_merged %>%
  group_by(`Gorilla_Public_ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Image Present Mean, SD, SE, and % correct
congruent_summary_zone_types <- data_visual_merged %>%
  filter(`Answer` == "Present") %>%
  group_by(`Gorilla_Public_ID`) %>%
  summarize(con_RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            con_RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            con_RT_se  = con_RT_sd/ sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/con_RT_mn)
            #PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Image Absent Mean, SD, SE, and % correct
incongruent_summary_zone_types <- data_visual_merged %>%
  filter(`Answer` == "Absent") %>%
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
complete_Summary_Full <- Reduce(merge,list(data_visual_merged, total_summary_zone_types, congruent_summary_zone_types, incongruent_summary_zone_types))
#View(complete_Summary_Full)
write.csv(complete_Summary_Full, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Visual_Fullsummary_and_demographics.csv")

complete_Summary <- Reduce(merge,list(total_summary_zone_types, congruent_summary_zone_types, incongruent_summary_zone_types))
#View(complete_Summary)
write.csv(complete_Summary, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Visual_summary_combined.csv")

complete_Summary_Demographic <- Reduce(merge,list(MOOD_SCORES, total_summary_zone_types, congruent_summary_zone_types, incongruent_summary_zone_types))
#View(complete_Summary_Demographic)
write.csv(complete_Summary_Demographic, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Visual_summary_and_demographics.csv")

#New ==============================================================================
#This chunk creates an output of the database, including only participants above a certain % correct.

#Creates output of those whose overall percent correct is 70% or above, and creates a csv
above_70_data <- complete_Summary_Demographic[!(complete_Summary_Demographic$PCorrect < "0.7"),]
write.csv(above_70_data, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Visual_above_.7_accuracy.csv")
#View(above_70_data)

#Creates output of those whose overall percent correct is 70% or above, and creates a csv
above_50_data <- complete_Summary_Demographic[!(complete_Summary_Demographic$PCorrect < "0.5"),]
write.csv(above_50_data, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Visual_above_.5_accuracy.csv")
#View(above_50_data)

# ===========================================================================
# ==============================================================================
#Graphs Overall

## Histogram for distribution of PCorrect
ggplot(data = total_summary_zone_types, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("Visual Overall Prop Correct")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Overall_Visual_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = total_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Visual Overall Mean Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Overall_Visual_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = total_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Visual Overall Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Overall_Visual_Standard Deviation.pdf")

# ==============================================================================
#Graphs Image Present/Congruent

## Histogram for distribution of PCorrect
#ggplot(data = congruent_summary_zone_types, aes(x = PCorrect)) +
#geom_histogram() +
#xlab("Proportion Correct") +
#ggtitle("Visual Congruent Prop Correct")
#ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Congruent_Visual_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = congruent_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Visual Mean Congruent Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Congruent_Visual_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = congruent_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Visual Congruent Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Congruent_Visual_SD.pdf")

# ==============================================================================
#Graphs Image Absent/Incongruent

## Histogram for distribution of PCorrect
#ggplot(data = incongruent_summary_zone_types, aes(x = PCorrect)) +
#geom_histogram() +
#xlab("Proportion Correct") +
#ggtitle("Visual Incongruent Prop Correct")
#ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Incongruent_Visual_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = incongruent_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Visual Mean Incongruent Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Inongruent_Visual_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = incongruent_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Visual Incongruent Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Inongruent_visual_SD.pdf")

# ==============================================================================

#Writes summaries to .csv
write.csv(data_visual_trimmed, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/NoOutliers")
write.csv(data_visual_merged, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Visual_data.csv")
write.csv(total_summary_zone_types, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Visual_Overall_Summary.csv")
write.csv(congruent_summary_zone_types, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Visual_Congruent_Summary.csv")
write.csv(incongruent_summary_zone_types, "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search/Post_Bot_Repeater/Visual_Incongruent_Summary.csv")



