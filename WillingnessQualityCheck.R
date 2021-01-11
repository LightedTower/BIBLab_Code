
# load packages
library(tidyverse)
library(psych)

# ==============================================================================

#data_willing <- read_csv(
#  "C:/Users/mohem/OneDrive/Desktop/Gorilla Tasks/Willingness to Pay/data_exp_11947-v10_task-9bsh.csv")

#View(data_willing)

willing_dir <- "C:/Users/mohem/OneDrive/Desktop/Gorilla Tasks/Willingness to Pay/"

setwd(willing_dir)



# list the files in current directory
list.files(pattern = ".csv$")   # CHANGE THIS TO MATCH INPUT FILE TYPE

# create a list of the files
list_willingfilenames <- list.files(pattern = ".csv$")
list_willingfilenames

# empty list to receive files
list_willing <- list()

for (i in 1:length(list_willingfilenames)) {
  list_willing[[i]] <- read_csv(list_willingfilenames[i])
}




# ==============================================================================
data_willing_combined <- as.data.frame(do.call(rbind, list_willing))




data_willing_stripped <- data_willing_combined %>%
  select("Participant Private ID",
         "Task Version",
         "Trial Number",
         "Reaction Time",
         "Response",
         "Target")

View(data_willing_stripped)

# Summary table for mean and sd of RTs
summary_willing_delay <- data_willing_stripped %>%
  group_by(`Participant Private ID`) %>%
  summarize(RT_mn = mean(`Reaction Time`, na.rm = TRUE),
            RT_sd = sd(`Reaction Time`, na.rm = TRUE),
            RT_se  = RT_sd / sqrt(sum(`Reaction Time`, na.rm = TRUE)/RT_mn))

View(summary_willing_delay)