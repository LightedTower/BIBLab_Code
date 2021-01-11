# load packages
library(tidyverse)
library(psych)

# ==============================================================================
##Reads and selects needed columns from data sheets

#!!!Change for other scripts line 12 & 15
analyze_me <- "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Visual Search"

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
BOT <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/UnusableDataBots_DATA_LABELS_2020-08-15_0959.csv")
REPEATER <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/RepeatersAcrossStudy_Gorilla_2020-08-15_1050.csv")

#View variables in console
list_visual
#BOT
#REPEATER

#View variables as output tab
#View(BOT)
#View(REPEATER)
# ==============================================================================
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

#View variables as output tab
#View(data_visual_combined)
View(data_visual_filtered)

# ==============================================================================
#Exclusion removal

#Remove Bots from data_visual_stripped
data_visual_strippedBOT <- data_visual_filtered[!(BOT$Gorilla.public.id == data_visual_stripped$`Participant Public ID`),]
#data_visual_strippedBOT

#Remove second participantion of repeaters from data_visual_stripped
data_visual_stripREPEATE <- data_visual_strippedBOT[!(REPEATER$List.Public.ID == data_visual_strippedBOT$`Participant Public ID`),]
#data_visual_stripREPEATE 

#View(data_visual_strippedBOT)
#View(data_visual_stripREPEATE)
# ==============================================================================
# Defines the type of question as congruent or incongruent
data_visual_stripREPEATE$`Question Type` <- 'Undefined'

data_visual_stripREPEATE <- mutate(
  data_visual_stripREPEATE,
  `Question Type` = if_else(
    Answer != "Present",
    "Incongruent",
    "Congruent",
    missing = "Undefined"
  )
)

View(data_visual_stripREPEATE)
# ==============================================================================
#Loop to caclculate overall Mean, SD, SE, and % correct
total_summary_zone_types <- data_visual_stripREPEATE %>%
  group_by(`Participant Private ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Image Present Mean, SD, SE, and % correct
congruent_summary_zone_types <- data_visual_stripREPEATE %>%
  filter(`Answer` == "Present") %>%
  group_by(`Participant Private ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Image Absent Mean, SD, SE, and % correct
incongruent_summary_zone_types <- data_visual_stripREPEATE %>%
  filter(`Answer` == "Absent") %>%
  group_by(`Participant Private ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )


View(data_visual_stripREPEATE)
View(total_summary_zone_types)
View(congruent_summary_zone_types)
View(incongruent_summary_zone_types)

# ==============================================================================
#Graphs Overall

## Histogram for distribution of PCorrect
ggplot(data = total_summary_zone_types, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("Visual Overall Prop Correct")
ggsave("Overall_Visual_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = total_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Visual Overall Mean Reaction Time")
ggsave("Overall_Visual_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = total_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Visual Overall Standard Deviation")
ggsave("Overall_Visual_Standard Deviation.pdf")

# ==============================================================================
#Graphs Image Present/Congruent

## Histogram for distribution of PCorrect
ggplot(data = congruent_summary_zone_types, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("Visual Congruent Prop Correct")
ggsave("Congruent_Visual_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = congruent_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Visual Mean Congruent Reaction Time")
ggsave("Congruent_Visual_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = congruent_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Visual Congruent Standard Deviation")
ggsave("Congruent_Visual_SD.pdf")

# ==============================================================================
#Graphs Image Absent/Incongruent

## Histogram for distribution of PCorrect
ggplot(data = incongruent_summary_zone_types, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("Visual Incongruent Prop Correct")
ggsave("Incongruent_Visual_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = incongruent_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Visual Mean Incongruent Reaction Time")
ggsave("Inongruent_Visual_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = incongruent_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Visual Incongruent Standard Deviation")
ggsave("Inongruent_visual_SD.pdf")

# ==============================================================================

#Writes summaries to .csv
write.csv(data_visual_stripREPEATE, "Visual_data.csv")
write.csv(total_summary_zone_types, "Visual_Overall_Summary.csv")
write.csv(congruent_summary_zone_types, "Visual_Congruent_Summary.csv")
write.csv(incongruent_summary_zone_types, "Visual_Incongruent_Summary.csv")



