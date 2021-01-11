
# ==============================================================================
# load packages
library(tidyverse)
library(psych)

# ==============================================================================
##Reads and selects needed columns from data sheets

#!!!Change for other scripts line 12 & 15
analyze_me <- "C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Simon's Task"

# set working directory to location of files on "Laptop A" comp
setwd(analyze_me)

# list the files in current directory
list.files(pattern = ".csv$")

# create a list of the files
list_filenames <- list.files(pattern = ".csv$")
list_filenames

list_simons <- list()

#Read files in path
for (i in 1:length(list_filenames)){
  list_simons[[i]] <- read_csv(list_filenames[[i]])
}

#Read in Exclusion Files
BOT <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/UnusableDataBots_DATA_LABELS_2020-08-15_0959.csv")
REPEATER <- read.csv("C:/AnalyzeMe!/VBAC_Aging/Subject/Raw/Gorilla/Exclusions/RepeatersAcrossStudy_Gorilla_2020-08-15_1050.csv")

#View variables in console
list_simons
#BOT
#REPEATER

#View variables as output tab
#View(BOT)
#View(REPEATER)

# ==============================================================================
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

data_simons_filtered <- filter(data_simons_stripped, `Zone Type` == "response_keyboard")

#View(data_simons_combined)
#View(data_simons_stripped)
#View(data_simons_filtered)
# ==============================================================================
#Remove exclusions from data_simons_filtered

data_simons_strippedBOT <- data_simons_filtered[!(BOT$Gorilla.public.id == data_simons_stripped$`Participant Public ID`),]

#data_simons_strippedBOT

data_simons_stripREPEATE <- data_simons_strippedBOT[!(REPEATER$List.Public.ID == data_simons_strippedBOT$`Participant Public ID`),]
#data_simons_stripREPEATE

View(data_simons_strippedBOT)
View(data_simons_stripREPEATE)

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

View(data_simons_stripREPEATE)

# ==============================================================================
#Loop to caclculate overall Mean, SD, SE, and % correct
total_summary_zone_types <- data_simons_stripREPEATE %>%
  group_by(`Participant Private ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Congruent Mean, SD, SE, and % correct
congruent_summary_zone_types <- data_simons_stripREPEATE %>%
  filter(`Question Type` == "Congruent") %>%
  group_by(`Participant Private ID`) %>%
  summarize(RT_mn = mean(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_sd = sd(as.numeric(`Reaction Time`), na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(as.numeric(`Reaction Time`), na.rm = TRUE)/RT_mn),
            PCorrect = mean(as.numeric(`Correct`),na.rm = TRUE)
  )

#Loop to caclculate Incongruent Mean, SD, SE, and % correct
incongruent_summary_zone_types <- data_simons_stripREPEATE %>%
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
  ggtitle("simons Overall Prop Correct")
ggsave("Overall_simons_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = total_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("simons Mean Overall Reaction Time")
ggsave("Overall_simons_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = total_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("simons Overall Standard Deviation")
ggsave("Overall_simons_SD.pdf")

# ==============================================================================
#Graphs Congruent

## Histogram for distribution of PCorrect
ggplot(data = congruent_summary_zone_types, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("simons Congruent Prop Correct")
ggsave("Congruent_simons_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = congruent_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("simons Mean Congruent Reaction Time")
ggsave("Congruent_simons_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = congruent_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("simons Congruent Standard Deviation")
ggsave("Congruent_simons_SD.pdf")

# ==============================================================================
#Graphs Incongruent

## Histogram for distribution of PCorrect
ggplot(data = incongruent_summary_zone_types, aes(x = PCorrect)) +
  geom_histogram() +
  xlab("Proportion Correct") +
  ggtitle("simons Incongruent Prop Correct")
ggsave("Incongruent_simons_Percent_Correct.pdf")

## Histogram for distribution of RT_mn
ggplot(data = incongruent_summary_zone_types, aes(x = RT_mn)) +
  geom_histogram() +
  xlab("Mean") +
  ggtitle("simons Mean Incongruent Reaction Time")
ggsave("Inongruent_simons_Mean.pdf")

## Histogram for distribution of RT_sd
ggplot(data = incongruent_summary_zone_types, aes(x = RT_sd)) +
  geom_histogram() +
  xlab("SD") +
  ggtitle("simons Incongruent Standard Deviation")
ggsave("Inongruent_simons_SD.pdf")

# ==============================================================================

#Writes summaries to .csv
write.csv(data_simons_stripREPEATE, "simons_data.csv")
write.csv(total_summary_zone_types, "simons_Overall_Summary.csv")
write.csv(congruent_summary_zone_types, "simons_Congruent_Summary.csv")
write.csv(incongruent_summary_zone_types, "simons_Incongruent_Summary.csv")
