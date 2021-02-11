
#Packages =====================================================================
# load packages
library(tidyverse)
library(psych)

#Read =========================================================================
##Reads and selects needed columns from data sheets

#!!!Change for other scripts line 12 & 15
analyze_me <- "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Visual Search"

# set working directory to location of files on "Laptop A" comp
setwd(analyze_me)

# list the files in current directory
list.files(pattern = ".csv$")

# create a list of the files
list_filenames <- list.files(pattern = ".csv$")
list_filenames

list_visual <- list()

####Read files in path
for (i in 1:length(list_filenames)) {
  list_visual[[i]] <- read_csv(list_filenames[[i]])
}

#Read in Exclusion Files
BOT <- read.csv("file path ")
REPEATER <- read.csv("file path ")
MOOD_SCORES <- read.csv("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Redcap/VBAConline-PilotDemographicRepo_DATA_LABELS_2021-02-10_1128.csv")

#View variables as output tab
#View(BOT)
#View(REPEATER)
#View (MOOD_SCORES)

#Bind and Select =========================================================================
##Binds data into dataframe for analysis
data_visual_combined <- as.data.frame(do.call(rbind, list_visual))

#Select specific columns from data sheets
data_visual_stripped <- data_visual_combined %>%
  select("Participant Public ID",
         "Participant Private ID",
         "Zone Type",
         "Reaction Time",
         "Correct",
         "Answer"
  )

data_visual_filtered <- filter(data_visual_stripped, `Zone Type` == "response_keyboard")

#Counts the total number of unique IDs
Total_count <- unique(data_visual_filtered$`Participant Public ID`)
length(Total_count)

#View variables as output tab
#View(data_visual_combined)

#Add Type Column ========================================================================
# Defines the type of question as congruent or incongruent
data_visual_filtered$`Question Type` <- "Undefined"

data_visual_filtered <- mutate(
  data_visual_filtered,
  `Question Type` = if_else(
    Answer != "Present",
    "Incongruent",
    "Congruent",
    missing = "Undefined"
  )
)

#View variables as output tab
View(data_visual_filtered)

#Remove Bots and repeaters ================================================================
#Exclusion removal

#Remove Bots from data_visual_stripped
data_visual_strippedBOT <- data_visual_filtered[!(BOT$Participant Public ID == data_visual_filtered$Participant Public ID),]
#data_visual_strippedBOT

#Remove second participantion of repeaters from data_visual_stripped
data_visual_stripREPEATE <- data_visual_strippedBOT[!(REPEATER$List.Public.ID == data_visual_strippedBOT$Participant Public ID),]
#data_visual_stripREPEATE 

#View variables as output tab
#View(data_visual_strippedBOT)
#View(data_visual_stripREPEATE)


#Merge Mood Scores ==============================================================================
#Merges MOOD_SCORES with data_visual_filtered so that the mood and ID's from Redcap
#are in the same place as the behavior scores.

##Note:Merge wouldnt work unless I had two different column names to merge together,
##not sure why.

data_visual_merged <- merge(MOOD_SCORES, data_visual_stripREPEATE, by.x ="Gorilla_Public_ID", by.y = "Participant Public ID")

#View variables as output tab
View(data_visual_merged)

#Under development; Trim Outliers ==============================================================================
#!!!Adjust so that it groups by private id, and prints out a list of those who the script is removing from the dataframe.!!!

#Filters out participants who's mean RT are considered Outliers (-2 <= Reaction Time Mean >= 2)
#data_visual_trimmed <- data_visual_merged %>%
#  filter(`Reaction Time` <= mean(data_visual_merged$`Reaction Time`, na.rm = TRUE) + 2 * sd(data_visual_merged$`Reaction Time`, na.rm = TRUE),
#         `Reaction Time` >= mean(data_visual_merged$`Reaction Time`, na.rm = TRUE) - 2 * sd(data_visual_merged$`Reaction Time`, na.rm = TRUE))
#View(data_visual_trimmed)

#View variables as output tab
#View(data_visual_trimmed)

#Create Summaries ==============================================================================
#!!!Check the dataset info is being pulled from here if you are not just running the entire script

#Loop to caclculate Overall Mean, SD, SE, and % correct, and adds a Type column
total_summary_zone_types <- data_visual_stripREPEATE %>%
  group_by(`Participant Public ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            #PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )
total_summary_zone_types$Type <- "Overall"

#Loop to caclculate Image Present Mean, SD, SE, and % correct, and adds a Type column
congruent_summary_zone_types <- data_visual_stripREPEATE %>%
  filter(`Answer` == "Present") %>%
  group_by(`Participant Public ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd/ sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn)
            #PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )
congruent_summary_zone_types$Type <- "Congruent"

#Loop to caclculate Image Absent Mean, SD, SE, and % correct, and adds a Type column
incongruent_summary_zone_types <- data_visual_stripREPEATE %>%
  filter(`Answer` == "Absent") %>%
  group_by(`Participant Public ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn)
            #PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )
incongruent_summary_zone_types$Type <- "Incongruent"

#Calculates percent correct
Percent_correct <- data_visual_stripREPEATE %>%
  group_by(`Participant Public ID`) %>%
  summarize(PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE))

#View(total_summary_zone_types)
#View(congruent_summary_zone_types)
#View(incongruent_summary_zone_types)

#Merge all the things==============================================================================
##This chunk binds the 3 summary dataframes together, and then merges that with the mood scores 

#Binds congruent and incongruent summaries into one dataframe by row
visual_bind_a <- rbind(congruent_summary_zone_types,incongruent_summary_zone_types)
#View(visual_bind_a)

#Binds Overall, with the congruent and incongruent summaries by row
visual_bind_b <-rbind(total_summary_zone_types, visual_bind_a)
#View(visual_bind_b)

#Binds total summary's with percent correct by row
simons_bind_c <-merge(total_summary_zone_types, Percent_correct)
View(simons_bind_c)
write.csv(simons_bind_c, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/Visual_participant_summaries")

#New Binds congruent  summarie and Percent Correct into one dataframe by row
visual_Con_PC <- merge(congruent_summary_zone_types, Percent_correct)
#View(visual_Con_PC)
write.csv(visual_Con_PC, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/visual_Congruent_Summaries.csv")

#New Binds incongruent  summarie and Percent Correct into one dataframe by row
visual_Incon_PC <- merge(incongruent_summary_zone_types, Percent_correct)
#View(visual_INcon_PC)
write.csv(visual_Incon_PC, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/visual_Incongruent_Summaries.csv")

#Merges mood score data with all summary data, and percent correct score
visual_summaries_layoutA <- merge(MOOD_SCORES, visual_bind_b)
visual_summaries_layoutB<- merge(visual_summaries_layoutB, Percent_correct)
#View(visual_summaries_layoutA)
#View(visual_summaries_layoutB)
write.csv(visual_summaries_layoutB, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/visual_Summaries_All_LayoutB.csv")

#Merges mood score data with Congruent and Incongruent summary data, and percent correct score
visual_summaries_Con_IN_layoutA <- merge(MOOD_SCORES,visual_bind_a)
visual_summaries_Con_IN_layoutB <- merge(visual_summaries_Con_IN_layoutA, Percent_correct)
#View(visual_summaries_Con_IN_layoutA)
#View(visual_summaries_Con_IN_layoutB)
write.csv(visual_summaries_Con_IN_layoutB, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/visual_Summaries_Con_IN_LayoutB.csv")

#Merges the overall trial data with the percent correct data
visual_summaries_all_scores <- merge(data_visual_filtered , Percent_correct)
#View(visual_summaries_all_scores)
write.csv(visual_summaries_all_scores, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/visual_Summaries_All_trials.csv")

#Filter by over 50 and 70 percent correct ==========================================================================
#Creates output of those whose overall percent correct is 70% or above, and creates a csv

above_70_data_all <- visual_summaries_layoutB[!(visual_summaries_layoutB$PCorrect < "0.7"),]
write.csv(above_70_data_all, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/visual/visual_ALL_above_.7_accuracy.csv")
#View(above_70_data_all)

#Creates output of those whose overall percent correct is 50% or above, and creates a csv
above_50_data_all <- visual_summaries_layoutB[!(visual_summaries_layoutB$PCorrect < "0.5"),]
write.csv(above_50_data_all, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/visual/visual_ALL_above_.5_accuracy.csv")
#View(above_50_data_all)

#Creates output of those whose overall percent correct is 70% or above, and creates a csv

above_70_data <- visual_summaries_Con_IN_layoutB [!(visual_summaries_Con_IN_layoutB$PCorrect < "0.7"),]
write.csv(above_70_data, "C:C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/visual_CON_IN_above_.7_accuracy.csv")
#View(above_70_data)

#Creates output of those whose overall percent correct is 50% or above, and creates a csv
above_50_data <- visual_summaries_Con_IN_layoutB [!(visual_summaries_Con_IN_layoutB$PCorrect < "0.5"),]
write.csv(above_50_data, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/visual_CON_IN_above_.5_accuracy.csv")
#View(above_50_data)

#Creates output of those whose overall percent correct is 70% or above, and creates a csv
above_70_data_merged <- visual_summaries_all_scores[!(visual_summaries_all_scores$PCorrect < "0.7"),]
write.csv(above_70_data_merged, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/visual_all_above_.7_accuracy.csv")
#View(above_70_data_merged)

#Creates output of those whose overall percent correct is 50% or above, and creates a csv
above_50_data_merged <- visual_summaries_all_scores[!(visual_summaries_all_scores$PCorrect  < "0.5"),]
write.csv(above_50_data_merged, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/visual_all_above_.5_accuracy.csv")
#View(above_50_data_merged)

#Excluded ID's ====================================================================
#Counts the number of unique ID's in order to get a total excluded at each point

Total_counta <- unique(data_visual_merged$`Participant Public ID`)
length(Total_counta)

Total_above70 <- unique(above_70_data_all_trial$`Participant Public ID`)
length(Total_above70)

total_excludeda <- (length(Total_count) - length(Total_counta))
total_excludeda

total_excludedb <- (length(Total_counta) - length(Total_above70))
total_excludedb

##Graphs Overall =========================================================================
#Creates histograms based on the overall summaries

## Histogram for distribution of PCorrect
ggplot(data = simons_bind_c, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("Visual Overall Prop Correct")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/Overall_visual_Percent_Correct.jpeg")

## Histogram for distribution of RT_mn
ggplot(data = simons_bind_c, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Visual Overall Mean Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/Overall_visual_Mean.jpeg")

## Histogram for distribution of RT_sd
ggplot(data = simons_bind_c, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Visual Overall Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/Overall_visual_SD.jpeg")

#Graphs Image Present/Congruent ==============================================================================
#Creates histograms based on congruent summaries created

## Histogram for distribution of PCorrect
#ggplot(data = congruent_summary_zone_types, aes(x = PCorrect)) +
#geom_histogram() +
#xlab("Proportion Correct") +
#ggtitle("Visual Congruent Prop Correct")
#ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/Congruent_visual_Percent_Correct.jpeg")

## Histogram for distribution of RT_mn
ggplot(data = visual_Con_PC, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Visual Mean Congruent Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/Congruent_visual_Mean.jpeg")

## Histogram for distribution of RT_sd
ggplot(data = visual_Con_PC, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Visual Congruent Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/Congruent_visual_SD.jpeg")

#Graphs Image Absent/Incongruent==============================================================================
#Creates histograms based on incongruent summaries created

## Histogram for distribution of PCorrect
#ggplot(data = incongruent_summary_zone_types, aes(x = PCorrect)) +
#geom_histogram() +
#xlab("Proportion Correct") +
#ggtitle("Visual Incongruent Prop Correct")
#ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/Incongruent_visual_Percent_Correct.jpeg")

## Histogram for distribution of RT_mn
ggplot(data = visual_Incon_PC, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Visual Mean Incongruent Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/Inongruent_visual_Mean.jpeg")

## Histogram for distribution of RT_sd
ggplot(data = visual_Incon_PC, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Visual Incongruent Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/Inongruent_visual_SD.jpeg")

#Graphs Con and Incongruent ==============================================================================
#Creates boxplot based on congruent and incongruent summaries created
ggplot(data = visual_summaries_Con_IN_layoutB, mapping = aes(x = reorder(Type, RT_mn, median, na.rm = TRUE), y = RT_mn))+
  geom_boxplot()+
  labs(x = "Question Type", y = "Reaction Time (milliseconds)")+
  coord_flip()+
  ggtitle("Mean Reaction Times")+
  ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/Con_Incon_MN_BOX.jpeg")

ggplot(data = visual_summaries_Con_IN_layoutB, mapping = aes(x = reorder(Type, RT_sd, median, na.rm = TRUE), y = RT_sd))+
  geom_boxplot()+
  labs(x = "Question Type", y = "Reaction Time (milliseconds)")+
  coord_flip()+
  ggtitle("Standard Deviation of Reaction Times")+
  ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Visual Search/Con_Incon_SD_BOX.jpeg")