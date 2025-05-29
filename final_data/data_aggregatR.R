###############################################################################
######## SET Aggregator ##############################################
################################################################################
### load in packages 
library(tidyverse) ## for efficient cleaning of data frames 
library(wru) ## for predicting race of profs 
library(foreign) ## for reading in of csv files and such 
library(rstudioapi) ## for efficient grabbing of working directory info 
library(ggplot2) ## for cool plots 
library(irr)
library(arm)
library(peRspective)
library(grid)
library(stargazer)
###setting directory 
main_wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(main_wd)

### read in the data 
list.files()
onu_df_final <-readRDS("full_onu_merged_data.rds")
unc_df_final <-readRDS("labeled_unc_data.rds")
osu_df_final <-read.csv("labeled_osu_data.csv")

### now, ensure column consistency 
names(osu_df_final)
names(unc_df_final)
names(onu_df_final)

### will need to remove the RMP specific content 
osu_df_final <- subset(osu_df_final, select = -c(quality_of_class,difficulty_of_class,class_code,
                                                 prof_firstname,prof_lastname,outrage_agg,
                                                 personal_attack_agg,prejudice_agg))
unc_df_final <- subset(unc_df_final, select = -c(quality_of_class,difficulty_of_class,class_code,
                                                 prof_firstname,prof_lastname))
onu_df_final <- subset(onu_df_final, select=-c(pid,question))
### add coded toxic to unc data 
unc_df_final$coded_toxic <- 0
unc_df_final$coded_toxic[unc_df_final$total_toxicity>0] <- 1


### add some things to the onu data 
onu_df_final$row <- seq.int(1,nrow(onu_df_final))
onu_df_final$college <- "OHIO NORTHERN UNIVERSITY"
colnames(onu_df_final)[colnames(onu_df_final)=="comment_clean"] <- "comment"
## coded toxic field 
onu_df_final$coded_toxic <- 0
onu_df_final$coded_toxic[onu_df_final$total_toxicity>0] <- 1

##checknames
names(osu_df_final)
names(unc_df_final)
names(onu_df_final)
onu_df_final <- subset(onu_df_final, select=-c( firstname,lastname))
### good. Now, we should be able to combine 
all_college_df <- rbind(osu_df_final,unc_df_final,onu_df_final)

### now that we have all of these data, let's also create a new row id field 
all_college_df$new_row_id <- seq.int(1,nrow(all_college_df))

## new threshold as well for toxic 
all_college_df$coded_toxic2 <- 0
all_college_df$coded_toxic2[all_college_df$total_toxicity>=2] <- 1

### now lets save the data 
saveRDS(all_college_df, "all_college_df.rds")
write.csv(all_college_df, "all_college_df.csv", row.names = F)

