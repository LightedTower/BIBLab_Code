
#Packages ==============================================================================
# load packages
library(tidyverse)
library(psych)

#Read ==============================================================================
#This chunk imports the data files

#!!!Change for other scripts line 12 & 15
analyze_me <- "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Simons"

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
MOOD_SCORES <- read.csv("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Redcap/VBAConline-PilotDemographicRepo_DATA_LABELS_2021-02-10_1128.csv")

#View variables as output tab
#View(BOT)
#View(REPEATER)
#View(MOOD_SCORES)

#Bind and select==============================================================================
##Binds data into dataframe for analysis
data_simons_combined <- as.data.frame(do.call(rbind, list_simons))

#Select specific columns from data sheets
data_simons_stripped <- data_simons_combined %>%
  select("Participant Public ID",
         "Participant Private ID",
         "Zone Type",
         "Reaction Time",
         "Correct",
         "Go"
  )

#filters dataframe by zonetype to get rid of all but participant response
data_simons_filtered <- filter(data_simons_stripped, `Zone Type` == "response_keyboard")

#Counts the total number of unique IDs
Total_count <- unique(data_simons_filtered$`Participant Public ID`)
length(Total_count)

#View variables as output tab
#View(data_simons_stripped)
#View(data_simons_filtered)

#Add Type Column==============================================================================
#Defines the type of question as congruent or incongruent
data_simons_filtered$`Question Type` <- 'Undefined'

data_simons_filtered <- mutate(
  data_simons_filtered,
  `Question Type` = if_else(
    Go != "LEFTleft.jpg" & Go != "RIGHTright.jpg",
    "Incongruent",
    "Congruent",
    missing = "Undefined"
  )
)

#View variables as output tab
#View(data_simons_filtered)

##Filter out bots and repeaters ==================================================================
#Remove exclusions from data_simons_filtered

data_simons_strippedBOT <- data_simons_filtered[!(BOT$Gorilla.public.id == data_simons_stripped$Participant Public ID),]
#data_simons_strippedBOT

data_simons_stripREPEATE <- data_simons_strippedBOT[!(REPEATER$List.Public.ID == data_simons_strippedBOT$Participant Public ID),]
#data_simons_stripREPEATE

#View variables as output tab
#View(data_simons_strippedBOT)
#View(data_simons_stripREPEATE)


#Merge Mood scores==========================================================================================
#Merges MOOD_SCORES with data_simons_filtered so that the mood and ID's from Redcap
#are in the same place as the behavior scores.

##Note:Merge wouldnt work unless I had two different column names to merge together,
##not sure why.

#!If this errors, then we need to change the colomn names to match here and all other "Participant.Public.ID"
#!will need to be changed to "Gorilla_Public_ID" for the script.

data_simons_merged <- merge(MOOD_SCORES, data_simons_stripREPEATE, by.x ="Gorilla_Public_ID", by.y = "Participant Public ID")

#View variables as output tab
#View(data_simons_merged)

#Under development; Trim Outliers ==============================================================================
#!!!Adjust so that it groups by private id, and prints out a list of those who the script is removing from the dataframe.!!!

#Filters out participants who's mean RT are considered Outliers (-2 <= Reaction Time Mean >= 2)
#data_visual_trimmed <- data_visual_merged %>%
#  filter(`Reaction Time` <= mean(data_simons_merged$`Reaction Time`, na.rm = TRUE) + 2 * sd(data_simons_merged$`Reaction Time`, na.rm = TRUE),
#         `Reaction Time` >= mean(data_simons_merged$`Reaction Time`, na.rm = TRUE) - 2 * sd(data_simons_merged$`Reaction Time`, na.rm = TRUE))
#View(data_visual_trimmed)

#View variables as output tab
#View(data_visual_trimmed)

#Create Summaries==============================================================
#Loop to caclculate overall Mean, SD, SE, and % correct
total_summary_zone_types <- data_simons_merged %>%
  group_by(`Participant Public ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Congruent Mean, SD, SE, and % correct
congruent_summary_zone_types <- data_simons_merged %>%
  filter(`Question Type` == "Congruent") %>%
  group_by(`Participant Public ID`) %>%
  summarize(Con_RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            Con_RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            Con_RT_se = Con_RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/Con_RT_mn)
            #PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Incongruent Mean, SD, SE, and % correct
incongruent_summary_zone_types <- data_simons_merged %>%
  filter(`Question Type` == "Incongruent") %>%
  group_by(`Participant Public ID`) %>%
  summarize(IN_RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            IN_RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            IN_RT_se  = IN_RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/IN_RT_mn)
            #,PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#View variables as output tab
#View(total_summary_zone_types)
#View(congruent_summary_zone_types)
#View(incongruent_summary_zone_types)

#Merge all the things==============================================================================
##This chunk binds the 3 summary dataframes together, and then merges that with the mood scores 

#Binds congruent and incongruent summaries into one dataframe by row
simons_bind_a <- rbind(congruent_summary_zone_types,incongruent_summary_zone_types)
#View(simons_bind_a)

#Binds Overall, with the congruent and incongruent summaries by row
simons_bind_b <-rbind(total_summary_zone_types, simons_bind_a)
#View(simons_bind_b)

#Binds total summary's with percent correct by row
simons_bind_c <-merge(total_summary_zone_types, Percent_correct)
#View(simons_bind_c)
write.csv(simons_bind_c, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Simons_participant_summaries")

#Binds congruent  summarie and Percent Correct into one dataframe by row
simons_Con_PC <- merge(congruent_summary_zone_types, Percent_correct)
#View(simons_Con_PC)
write.csv(simons_Con_PC, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Simons_Congruent_Summaries.csv")

#Binds incongruent  summarie and Percent Correct into one dataframe by row
simons_Incon_PC <- merge(incongruent_summary_zone_types, Percent_correct)
#View(simons_INcon_PC)
write.csv(simons_Incon_PC, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Simons_Incongruent_Summaries.csv")

#Merges mood score data with all summary data, and percent correct score
simons_summaries_layoutA <- merge(MOOD_SCORES, simons_bind_b)
simons_summaries_layoutB<- merge(simons_summaries_layoutB, Percent_correct)
#View(simons_summaries_layoutA)
#View(simons_summaries_layoutB)
write.csv(simons_summaries_layoutB, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Simons_Summaries_All_LayoutB.csv")

#Merges mood score data with Congruent and Incongruent summary data, and percent correct score
simons_summaries_Con_IN_layoutA <- merge(MOOD_SCORES,simons_bind_a)
simons_summaries_Con_IN_layoutB <- merge(simons_summaries_Con_IN_layoutA, Percent_correct)
#View(simons_summaries_Con_IN_layoutA)
#View(simons_summaries_Con_IN_layoutB)
write.csv(simons_summaries_Con_IN_layoutB, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Simons_Summaries_Con_IN_LayoutB.csv")

#Merges the overall trial data with the percent correct data
simons_summaries_all_scores <- merge(data_simons_filtered , Percent_correct)
View(simons_summaries_all_scores)
write.csv(simons_summaries_all_scores, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Simons_Summaries_All_trials.csv")

#Filter by over 50 and 70 percent correct ==========================================================================
#Creates output of those whose overall percent correct is 70% or above, and creates a csv

above_70_data_all <- simons_summaries_layoutB[!(simons_summaries_layoutB$PCorrect < "0.7"),]
write.csv(above_70_data_all, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/simons_ALL_above_.7_accuracy.csv")
#View(above_70_data_all)

#Creates output of those whose overall percent correct is 50% or above, and creates a csv
above_50_data_all <- simons_summaries_layoutB[!(simons_summaries_layoutB$PCorrect < "0.5"),]
write.csv(above_50_data_all, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/simons_ALL_above_.5_accuracy.csv")
#View(above_50_data_all)

#Creates output of those whose overall percent correct is 70% or above, and creates a csv

above_70_data <- simons_summaries_Con_IN_layoutB [!(simons_summaries_Con_IN_layoutB$PCorrect < "0.7"),]
write.csv(above_70_data, "C:C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/simons_CON_IN_above_.7_accuracy.csv")
#View(above_70_data)

#Creates output of those whose overall percent correct is 50% or above, and creates a csv
above_50_data <- simons_summaries_Con_IN_layoutB [!(simons_summaries_Con_IN_layoutB$PCorrect < "0.5"),]
write.csv(above_50_data, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/simons_CON_IN_above_.5_accuracy.csv")
#View(above_50_data)

#Creates output of those whose overall percent correct is 70% or above, and creates a csv
above_70_data_merged <- simons_summaries_all_scores[!(simons_summaries_all_scores$PCorrect < "0.7"),]
write.csv(above_70_data_merged, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/simons_all_above_.7_accuracy.csv")
#View(above_70_data_merged)

#Creates output of those whose overall percent correct is 50% or above, and creates a csv
above_50_data_merged <- simons_summaries_all_scores[!(simons_summaries_all_scores$PCorrect  < "0.5"),]
write.csv(above_50_data_merged, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/simons_all_above_.5_accuracy.csv")
#View(above_50_data_merged)

#Excluded ID's ==========================================================================
#Counts the number of unique ID's in order to get a total excluded at each point

Total_counta <- unique(data_simons_merged$Gorilla_Public_ID)
length(Total_counta)

Total_above70 <- unique(above_70_data_merged$Gorilla_Public_ID)
length(Total_above70)

total_excludeda <- (length(Total_count) - length(Total_counta))
total_excludeda

total_excludedb <- (length(Total_counta) - length(Total_above70))
total_excludedb
#Graphs Overall==============================================================================
#Creates histograms based on the overall summaries

## Histogram for distribution of PCorrect
ggplot(data = simons_bind_c, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("simons Overall Prop Correct")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Overall_simons_Percent_Correct.jpeg")

## Histogram for distribution of RT_mn
ggplot(data = simons_bind_c, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("simons Mean Overall Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Overall_simons_Mean.jpeg")

## Histogram for distribution of RT_sd
ggplot(data = simons_bind_c, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("simons Overall Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Overall_simons_SD.jpeg")

#Graphs Congruent ==============================================================================
#Creates histograms based on congruent summaries created

## Histogram for distribution of PCorrect
#ggplot(data = simons_Con_PC, aes(x = PCorrect)) +
  #geom_histogram() +
  #xlab("Proportion Correct") +
  #ggtitle("simons Congruent Prop Correct")
#ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Congruent_simons_Percent_Correct.jpeg")

## Histogram for distribution of RT_mn
ggplot(data = simons_Con_PC, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("simons Mean Congruent Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Congruent_simons_Mean.jpeg")

## Histogram for distribution of RT_sd
ggplot(data = simons_Con_PC, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("simons Congruent Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Congruent_simons_SD.jpeg")

#Graphs Incongruent ==============================================================================
#Creates histograms based on incongruent summaries created

## Histogram for distribution of PCorrect
#ggplot(data = simons_Incon_PC, aes(x = PCorrect)) +
  #geom_histogram() +
  #xlab("Proportion Correct") +
  #ggtitle("simons Incongruent Prop Correct")
#ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Incongruent_simons_Percent_Correct.jpeg")

## Histogram for distribution of RT_mn
ggplot(data = simons_Incon_PC, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("simons Mean Incongruent Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Inongruent_simons_Mean.jpeg")

## Histogram for distribution of RT_sd
ggplot(data = simons_Incon_PC, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("simons Incongruent Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Inongruent_simons_SD.jpeg")

#Graphs Con and Incongruent =========================================================================
#Creates boxplot based on congruent and incongruent summaries created
ggplot(data = simons_summaries_Con_IN_layoutB, mapping = aes(x = reorder(Type, RT_mn, median, na.rm = TRUE), y = RT_mn))+
  geom_boxplot()+
  labs(x = "Question Type", y = "Reaction Time (milliseconds)")+
  coord_flip()+
  ggtitle("Mean Reaction Times")+
  ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Con_Incon_MN_BOX.jpeg")

ggplot(data = simons_summaries_Con_IN_layoutB, mapping = aes(x = reorder(Type, RT_sd, median, na.rm = TRUE), y = RT_sd))+
  geom_boxplot()+
  labs(x = "Question Type", y = "Reaction Time (milliseconds)")+
  coord_flip()+
  ggtitle("Standard Deviation of Reaction Times")+
  ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Simons/Con_Incon_SD_BOX.jpeg")

