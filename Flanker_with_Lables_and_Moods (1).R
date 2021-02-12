
#Packages ==============================================================================
# load packages
library(tidyverse)
library(psych)

#Read ==============================================================================
#This chunk imports the data files

#Set definition of location for working Directory
analyze_me <- "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Flanker"

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
BOT <- read.csv("file path ")
REPEATER <- read.csv("file path ")
MOOD_SCORES <- read.csv("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Redcap/VBAConline-PilotDemographicRepo_DATA_LABELS_2021-02-10_1128.csv")

#View variables as output tab
#View(BOT)
#View(REPEATER)

#Bind and Select ================================================================
##Binds data files into a dataframe for analysis
data_flanker_combined <- as.data.frame(do.call(rbind, list_flanker))

#Select specific columns from data sheets
data_flanker_stripped <- data_flanker_combined %>%
  select("Participant Public ID",
         "Participant Private ID",
         "Zone Type",
         "Reaction Time",
         "Correct",
         "Target",
         "Distractor"
  )

#filters dataframe by zonetype to get rid of all but participant response
data_flanker_filtered <- filter(data_flanker_stripped, `Zone Type` == "response_keyboard")

Total_count <- unique(data_flanker_filtered$`Participant Public ID`)
length(Total_count)

#View variables as output tab
#View(data_flanker_combined)
#View(data_flanker_filtered)

#Add Type Column ===========================================================
#Defines the type of question as congruent or incongruent
data_flanker_filtered$`Question Type` <- 'Undefined'

data_flanker_filtered <- mutate(
  data_flanker_filtered,
  `Question Type` = if_else(
    Target != Distractor,
    "Incongruent",
    "Congruent",
    missing = "Undefined"
  )
)
#View(data_flanker_filtered)

##Remove Bots and repeaters ==========================================================
#Exclusion removal

#Remove Bots from data_flanker_stripped
data_flanker_strippedBOT <- data_flanker_filtered[!(BOT$`Participant Public ID` == data_flanker_stripped$`Participant Public ID`),]
#data_flanker_strippedBOT

#Remove second participantion of repeaters from data_flanker_stripped
data_flanker_stripREPEATE <- data_flanker_strippedBOT[!(REPEATER$List.Public.ID == data_flanker_strippedBOT$`Participant Public ID`),]
#data_flanker_stripREPEATE 

#View(data_flanker_strippedBOT)
#View(data_flanker_stripREPEATE)
#Merge Mood scores ==============================================================================
#Merges MOOD_SCORES with data_flanker_filtered so that the mood and ID's from Redcap
#are in the same place as the behavior scores.

##Note:Merge wouldnt work unless I had two different column names to merge together,
##not sure why.

data_flanker_merged <- merge(MOOD_SCORES, data_flanker_stripREPEATE, by.x ="Gorilla_Public_ID", by.y = "`Participant Public ID`")

#View variables as output tab
View(data_flanker_merged)


# Under development; Trim Outliers===================================================
#!!!Adjust so that it groups by private id, and prints out a list of those who the script is removing from the dataframe.!!!

#Filters out participants who's mean RT are considered Outliers (-2 <= Reaction Time Mean >= 2)
#data_flanker_trimmed <- data_flanker_merged %>%
#  filter(`Reaction Time` <= mean(data_flanker_merged$`Reaction Time`, na.rm = TRUE) + 2 * sd(data_flanker_merged$`Reaction Time`, na.rm = TRUE),
#        `Reaction Time` >= mean(data_flanker_merged$`Reaction Time`, na.rm = TRUE) - 2 * sd(data_flanker_merged$`Reaction Time`, na.rm = TRUE))

#View variables as output tab
#View(data_flanker_trimmed)

#Create Summaries ==============================================================================
#!!!Check the dataset info is being pulled from here if you are not just running the entire script

#Loop to caclculate overall Mean, SD, SE, and % correct
total_summary_zone_types <- data_flanker_filtered %>%
  group_by(`Participant Public ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            #PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )
total_summary_zone_types$Type <- "Overall"

#Loop to caclculate Congruent Mean, SD, SE, and % correct
congruent_summary_zone_types <- data_flanker_stripREPEATE %>%
  filter(`Question Type` == "Congruent") %>%
  group_by(`Participant Public ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn)
            #PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )
congruent_summary_zone_types$Type <- "Congruent"

#Loop to caclculate Incongruent Mean, SD, SE, and % correct
incongruent_summary_zone_types <- data_flanker_stripREPEATE %>%
  filter(`Question Type` == "Incongruent") %>%
  group_by(`Participant Public ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn)
            #PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )
incongruent_summary_zone_types$Type <- "Incongruent"

#Calculates percent correct
Percent_correct <- data_flanker_stripREPEATE %>%
  group_by(`Participant Public ID`) %>%
  summarize(PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE))


#View(total_summary_zone_types)
#View(congruent_summary_zone_types)
#View(incongruent_summary_zone_types)

#Merge all the things ==============================================================================
##This chunk binds the 3 summary dataframes together, and then merges that with the mood scores 

#Binds congruent and incongruent summaries into one dataframe by row
flanker_bind_a <- rbind(congruent_summary_zone_types,incongruent_summary_zone_types)
#View(flanker_bind_a)

#Binds Overall, with the congruent and incongruent summaries by row
flanker_bind_b <-rbind(total_summary_zone_types, flanker_bind_a)
#View(flanker_bind_b)

#Binds total summary's with percent correct by row
flanker_bind_c <-merge(total_summary_zone_types, Percent_correct)
#View(flanker_bind_c)
write.csv(flanker_bind_c, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Flanker_participant_summaries.csv")

#New Binds congruent  summarie and Percent Correct into one dataframe by row
Flanker_Con_PC <- merge(congruent_summary_zone_types, Percent_correct)
#View(Flanker_Con_PC)
write.csv(Flanker_Con_PC, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Flanker_participant_summaries.csv")

#New Binds incongruent  summarie and Percent Correct into one dataframe by row
Flanker_Incon_PC <- merge(incongruent_summary_zone_types, Percent_correct)
#View(Flanker_INcon_PC)
write.csv(Flanker_Incon_PC, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Flanker_Incongruent_Summaries.csv")

#Merges mood score data with all summary data, and percent correct score
flanker_summaries_layoutA <- merge(MOOD_SCORES, flanker_bind_b)
flanker_summaries_layoutB <- merge(flanker_summaries_layoutA, Percent_correct)
#View(flanker_summaries_layoutA)
#View(flanker_summaries_layoutB)
write.csv(flanker_summaries_layoutB, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/flanker_Summaries_All_LayoutB.csv")

#Merges mood score data with Congruent and Incongruent summary data, and percent correct score
flanker_summaries_Con_IN_layoutA <- merge(MOOD_SCORES,flanker_bind_a)
flanker_summaries_Con_IN_layoutB <- merge(flanker_summaries_Con_IN_layoutA, Percent_correct)
#View(flanker_summaries_Con_IN_layoutA)
#View(flanker_summaries_Con_IN_layoutB)
write.csv(flanker_summaries_Con_IN_layoutB, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/flanker_Summaries_Con_IN_LayoutB.csv")

#Merges the overall trial data with the percent correct data
flanker_summaries_all_scores <- merge(data_flanker_merged, Percent_correct)
#View(flanker_summaries_all_scores)
write.csv(flanker_summaries_all_scores, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/flanker_Summaries_All_trials.csv")

#Filter by over 50 and 70 percent correct ================================================================
#Creates output of those whose overall percent correct is 70% or above, and creates a csv

above_70_data_all <- flanker_summaries_layoutB[!(flanker_summaries_layoutB$PCorrect < "0.7"),]
write.csv(above_70_data_all, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/flanker_ALL_above_.7_accuracy.csv")
#View(above_70_data_all)

#Creates output of those whose overall percent correct is 50% or above, and creates a csv
above_50_data_all <- flanker_summaries_layoutB[!(flanker_summaries_layoutB$PCorrect < "0.5"),]
write.csv(above_50_data_all, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/flanker_ALL_above_.5_accuracy.csv")
#View(above_50_data_all)

#Creates output of those whose overall percent correct is 70% or above, and creates a csv
above_70_data <- flanker_summaries_Con_IN_layoutB [!(flanker_summaries_Con_IN_layoutB$PCorrect < "0.7"),]
write.csv(above_70_data, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/flanker_CON_IN_above_.7_accuracy.csv")
#View(above_70_data)

#Creates output of those whose overall percent correct is 50% or above, and creates a csv
above_50_data <- flanker_summaries_Con_IN_layoutB [!(flanker_summaries_Con_IN_layoutB$PCorrect < "0.5"),]
write.csv(above_50_data, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/flanker_CON_IN_above_.5_accuracy.csv")
#View(above_50_data)

#Creates output of those whose overall percent correct is 70% or above, and creates a csv
above_70_data_merged <- flanker_summaries_all_scores[!(flanker_summaries_all_scores$PCorrect < "0.7"),]
write.csv(above_70_data_merged, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/flanker_allmerged_above_.7_accuracy.csv")
#View(above_70_data_merged)

#Creates output of those whose overall percent correct is 50% or above, and creates a csv
above_50_data_merged <- flanker_summaries_all_scores[!(flanker_summaries_all_scores$PCorrect  < "0.5"),]
write.csv(above_50_data_merged, "C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/flanker_allmerged_above_.5_accuracy.csv")
#View(above_50_data_merged)

#Excluded ID's ==========================================================================
#Counts the number of unique ID's in order to get a total excluded at each point

Total_counta <- unique(data_flanker_merged$`Participant Public ID`)
length(Total_counta)

Total_above70 <- unique(above_70_alldata$`Participant Public ID`)
length(Total_above70)

total_excludeda <- (length(Total_count) - length(Total_counta))
total_excludeda

total_excludedb <- (length(Total_counta) - length(Total_above70))
total_excludedb
#Graphs Overall==============================================================================
#Creates histograms based on the overall summaries

## Histogram for distribution of PCorrect
ggplot(data = flanker_bind_c, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("Flanker Overall Prop Correct")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Overall_Flanker_Percent_Correct.jpeg")

## Histogram for distribution of RT_mn
ggplot(data = flanker_bind_c, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Flanker Mean Overall Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Overall_Flanker_Mean.jpeg")

## Histogram for distribution of RT_sd
ggplot(data = flanker_bind_c, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Flanker Overall Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Overall_Flanker_SD.jpeg")

#Graphs Congruent ==============================================================================
#Creates histograms based on congruent summaries created

## Histogram for distribution of PCorrect
#ggplot(data = congruent_summary_zone_types, aes(x = PCorrect)) +
#geom_histogram() +
#xlab("Proportion Correct") +
#ggtitle("Flanker Congruent Prop Correct")
#ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Congruent_Flanker_Percent_Correct.jpeg")

## Histogram for distribution of RT_mn
ggplot(data = Flanker_Con_PC, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Flanker Mean Congruent Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Congruent_Flanker_Mean.jpeg")

## Histogram for distribution of RT_sd
ggplot(data = Flanker_Con_PC, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Flanker Congruent Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Congruent_Flanker_SD.jpeg")

#Graphs Incongruent ==============================================================================
#Creates histograms based on incongruent summaries created

## Histogram for distribution of PCorrect
#ggplot(data = incongruent_summary_zone_types, aes(x = PCorrect)) +
#geom_histogram() +
#xlab("Proportion Correct") +
#ggtitle("Flanker Incongruent Prop Correct")
#ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Incongruent_Flanker_Percent_Correct.jpeg")

## Histogram for distribution of RT_mn
ggplot(data = Flanker_Incon_PC, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Flanker Mean Incongruent Reaction Time")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Inongruent_Flanker_Mean.jpeg")

## Histogram for distribution of RT_sd
ggplot(data = Flanker_Incon_PC, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Flanker Incongruent Standard Deviation")
ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Inongruent_Flanker_SD.jpeg")

#Graphs Con and Incongruent ==============================================================================
#Creates boxplot based on congruent and incongruent summaries created
ggplot(data = flanker_summaries_Con_IN_layoutB, mapping = aes(x = reorder(Type, RT_mn, median, na.rm = TRUE), y = RT_mn))+
  geom_boxplot()+
  labs(x = "Question Type", y = "Reaction Time (milliseconds)")+
  coord_flip()+
  ggtitle("Mean Reaction Times")+
  ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Flanker_Con_Incon_MN_BOX.jpeg")

ggplot(data = flanker_summaries_Con_IN_layoutB, mapping = aes(x = reorder(Type, RT_sd, median, na.rm = TRUE), y = RT_sd))+
  geom_boxplot()+
  labs(x = "Question Type", y = "Reaction Time (milliseconds)")+
  coord_flip()+
  ggtitle("Standard Deviation of Reaction Times")+
  ggsave("C:/AnalyzeMe!/VBAC_Online/Pilot_Data/Gorilla/Script_Reports/Flanker/Flanker_Con_Incon_SD_BOX.jpeg")


