
# ==============================================================================
# load packages
library(tidyverse)
library(psych)

# ==============================================================================
##Reads and selects needed columns from data sheets

#!!!Change for other scripts line 12 & 15
analyze_me <- "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Flanker"

# set working directory to location of files on "Laptop A" comp
setwd(analyze_me)

# list the files in current directory
list.files(pattern = ".csv$")

# create a list of the files
list_filenames <- list.files(pattern = ".csv$")
list_filenames

#Create empty list for datasheets
list_flanker <- list()

####Read files in path
for (i in 1:length(list_filenames)) {
  list_flanker[[i]] <- read_csv(list_filenames[[i]])
}

#Read in Exclusion Files
BOT <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/UnusableDataBots_DATA_LABELS_2020-08-15_0959.csv")
REPEATER <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/RepeatersAcrossStudy_Gorilla_2020-08-15_1050.csv")

#View variables in console
list_flanker
#BOT
#REPEATER

#View variables as output tab
#View(BOT)
#View(REPEATER)

# ==============================================================================
##Binds data files into dataframe for analysis
data_flanker_combined <- as.data.frame(do.call(rbind, list_flanker))

#Select specific columns from data sheets
data_flanker_stripped <- data_flanker_combined %>%
  select("Participant Private ID",
         "Participant Public ID",
         "Zone Type",
         "Reaction Time",
         "Correct",
         "Target",
         "Distractor"
  )

data_flanker_filtered <- filter(data_flanker_stripped, `Zone Type` == "response_keyboard")

#View variables as output tab
#View(data_flanker_combined)
View(data_flanker_filtered)
# ==============================================================================
#Exclusion removal

#Remove Bots from data_flanker_stripped
data_flanker_strippedBOT <- data_flanker_filtered[!(BOT$Gorilla.public.id == data_flanker_stripped$`Participant Public ID`),]
#data_flanker_strippedBOT

#Remove second participantion of repeaters from data_flanker_stripped
data_flanker_stripREPEATE <- data_flanker_strippedBOT[!(REPEATER$List.Public.ID == data_flanker_strippedBOT$`Participant Public ID`),]
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

View(data_flanker_stripREPEATE)
# ==============================================================================
#Loop to caclculate overall Mean, SD, SE, and % correct
total_summary_zone_types <- data_flanker_stripREPEATE %>%
  group_by(`Participant Private ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Congruent Mean, SD, SE, and % correct
congruent_summary_zone_types <- data_flanker_stripREPEATE %>%
  filter(`Question Type` == "Congruent") %>%
  group_by(`Participant Private ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Incongruent Mean, SD, SE, and % correct
incongruent_summary_zone_types <- data_flanker_stripREPEATE %>%
  filter(`Question Type` == "Incongruent") %>%
  group_by(`Participant Private ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

View(total_summary_zone_types)
View(congruent_summary_zone_types)
View(incongruent_summary_zone_types)
# ==============================================================================
#Graphs Overall

## Histogram for distribution of PCorrect
ggplot(data = total_summary_zone_types, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("Flanker Overall Prop Correct")
ggsave("Overall_Flanker_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = total_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Flanker Mean Overall Reaction Time")
ggsave("Overall_Flanker_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = total_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Flanker Overall Standard Deviation")
ggsave("Overall_Flanker_SD.pdf")

# ==============================================================================
#Graphs Congruent

## Histogram for distribution of PCorrect
ggplot(data = congruent_summary_zone_types, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("Flanker Congruent Prop Correct")
ggsave("Congruent_Flanker_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = congruent_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Flanker Mean Congruent Reaction Time")
ggsave("Congruent_Flanker_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = congruent_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Flanker Congruent Standard Deviation")
ggsave("Congruent_Flanker_SD.pdf")


# ==============================================================================
#Graphs Incongruent

## Histogram for distribution of PCorrect
ggplot(data = incongruent_summary_zone_types, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("Flanker Incongruent Prop Correct")
ggsave("Incongruent_Flanker_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = incongruent_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("Flanker Mean Incongruent Reaction Time")
ggsave("Inongruent_Flanker_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = incongruent_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("Flanker Incongruent Standard Deviation")
ggsave("Inongruent_Flanker_SD.pdf")

# ==============================================================================

#Writes summaries to .csv
write.csv(data_flanker_stripREPEATE, "Flanker_data.csv")
write.csv(total_summary_zone_types, "Flanker_Overall_Summary.csv")
write.csv(congruent_summary_zone_types, "Flanker_Congruent_Summary.csv")
write.csv(incongruent_summary_zone_types, "Flanker_Incongruent_Summary.csv")
