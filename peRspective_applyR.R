###############################################################################
######## peRspective scriptR -- UNC and OSU ##############################################
################################################################################
### load in packages 
library(tidyverse) ## for efficient cleaning of data frames 
library(wru) ## for predicting race of profs 
library(foreign) ## for reading in of csv files and such 
library(rstudioapi) ## for efficient grabbing of working directory info 
library(ggplot2) ## for cool plots 
library(irr)
library(arm)
## 
library("remotes")
install_github("cran/peRspective") # use to install if not done already
library(peRspective)
library(grid)
library(stargazer)
###setting directory 
main_wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(main_wd)

### import faculty characteristics data 
osu_faculty_demos <- read.csv("OSU_faculty_demos.csv")
unc_faculty_demos <- read.csv("UNC_faculty_demos.csv")

### analyze race missings 
table(osu_faculty_demos$non_white) ## 42 missing 
table(unc_faculty_demos$non_white) ## none missing 

### subset osu 
osu_faculty_demos_miss <- subset(osu_faculty_demos, non_white==99)
osu_faculty_demos <- subset(osu_faculty_demos, non_white!=99)

## rename col 
colnames(osu_faculty_demos_miss)[3] <- "surname"
colnames(osu_faculty_demos_miss)[2] <- "first"

osu_faculty_demos_miss <-  predict_race(osu_faculty_demos_miss, surname.only = T, 
                                        names.to.use = "surname, first" )
### we are just going with white and non white, so can use basic thresholds 
osu_faculty_demos_miss$non_white[osu_faculty_demos_miss$pred.whi>0.5] <- 0
osu_faculty_demos_miss$non_white[osu_faculty_demos_miss$pred.whi<0.5] <- 1

### now, rename
colnames(osu_faculty_demos_miss)[2] <- "prof_firstname"
colnames(osu_faculty_demos_miss)[3] <- "prof_lastname"

## now, drop the wru stuff 
osu_faculty_demos_miss <- osu_faculty_demos_miss[,1:8]

## rebind 
osu_faculty_demos <- rbind(osu_faculty_demos, osu_faculty_demos_miss)

### api key
pr_api <- "" # set here with the appropriate API key 

###calculation of the OSU inter reliability 
### Read in the csv data 
local_files <-list.files(full.names=T)
local_files <- local_files[grepl(".csv",local_files)] ## subset to only include csv files 

## read in the final data 
gabe_coms <- read.csv(local_files[grepl("gabe", local_files) & grepl("final", local_files)])
koen_coms <- read.csv(local_files[grepl("koen", local_files) & grepl("final", local_files)]) 

### since dealing with the final data, should be good to go. 
dim(gabe_coms) ## 33 cols, 2800 rows  
sum(is.na(gabe_coms$out_emo_lang)==T) # no missing 
sum(is.na(koen_coms$out_emo_lang)==T) # no missing 

### lets create an aggregate index of the dims of interest 
koen_coms$outrage_agg <- rowSums(koen_coms[,grepl("out_", colnames(koen_coms))])
koen_coms$personal_attack_agg <- rowSums(koen_coms[,grepl("pa_", colnames(koen_coms))])
koen_coms$prejudice_agg <- rowSums(koen_coms[,grepl("pb_", colnames(koen_coms))])
### now same for Gabe 
gabe_coms$outrage_agg <- rowSums(gabe_coms[,grepl("out_", colnames(gabe_coms))])
gabe_coms$personal_attack_agg <- rowSums(gabe_coms[,grepl("pa_", colnames(gabe_coms))])
gabe_coms$prejudice_agg <- rowSums(gabe_coms[,grepl("pb_", colnames(gabe_coms))])

## get rid of thumbs data 
gabe_coms <- subset(gabe_coms, select=-c(thumbs_up,thumbs_down))
koen_coms <- subset(koen_coms, select=-c(thumbs_up,thumbs_down))

## let's get total toxicity 
gabe_coms$total_toxic <- gabe_coms$outrage_agg + gabe_coms$personal_attack_agg + gabe_coms$prejudice_agg
koen_coms$total_toxic <- koen_coms$outrage_agg + koen_coms$personal_attack_agg + koen_coms$prejudice_agg


### now let's get the score 
## bind into matrix 
test_mat_osu <- matrix(cbind(koen_coms$total_toxic,gabe_coms$total_toxic),ncol=2)

## ICC here 
toxic_icc_osu <- icc(test_mat_osu, "oneway", "agreement")

## split for 1100
test_mat_osu1100 <- test_mat_osu[1:1100,] 
toxic_icc_osu1100 <- icc(test_mat_osu1100, "oneway", "agreement")
# .658 for these, 95% of .623 to .69 
test_mat_osu1100other <- test_mat_osu[1101:2800,] 
toxic_icc_osu1100oth <- icc(test_mat_osu1100other, "oneway", "agreement")
# .61, and 95% CI of .58 to .64 ; not sig diff 

### now, get the total difference between these
test_mat_osu_df <- as.data.frame(test_mat_osu)
test_mat_osu_df$abs_diff <- abs(test_mat_osu_df$V1-test_mat_osu_df$V2)
table(test_mat_osu_df$abs_diff[1:1100])
length(which(test_mat_osu_df$abs_diff[1:1100] > 0))



### read in the full OSU data 
rmp_df_osu <- read.csv("text_cleaning_data/scored_rmp_data_osu_final.csv") # this is the combination
# of the work by Koen and Gabe. We will need to ensure that we also get at the index level checks 
### create ID field 
rmp_df_osu$row_id <- seq.int(nrow(rmp_df_osu))

### good, now let's run the peRspective stuff 
models2run <- c("TOXICITY","SEVERE_TOXICITY","INSULT")

# set api key in the environment
Sys.setenv(perspective_api_key = pr_api)


### run here 
persp_rmp_output_osu <- rmp_df_osu %>%
  prsp_stream(text = comment,
              text_id = row_id,
              score_model = models2run)
### save the data 
saveRDS(persp_rmp_output_osu, "persp_rmp_output_osu.rds")
persp_rmp_output_osu <- readRDS("persp_rmp_output_osu.rds")
### now merge with the data 
rmp_df_osu2 <- merge(rmp_df_osu,persp_rmp_output_osu,by.x="row_id",by.y="text_id" )

### good, now find the average 
rmp_df_osu2$total_toxicity = 
  (rmp_df_osu2$outrage_agg+rmp_df_osu2$prejudice_agg+rmp_df_osu2$personal_attack_agg)/2
quantile(rmp_df_osu2$total_toxicity,seq(0,1,by=0.05)) # so about 10% have one element; max is 5 

### let's check how many are not 0 
length(which(rmp_df_osu2$total_toxicity>0)) # 434 comments; lets subset these out, and then randomly sample 
# the non-toxic 
## create score that is the sum of toxicity and severe 
rmp_df_osu2$TOXICITY2 <- rmp_df_osu2$TOXICITY+rmp_df_osu2$SEVERE_TOXICITY
rmp_df_osu2toxic <- subset(rmp_df_osu2, total_toxicity >0 )
rmp_df_osu2nontoxic <- subset(rmp_df_osu2, total_toxicity ==0 )



###find the dist
quantile(rmp_df_osu2nontoxic$TOXICITY2,seq(0,1,by=0.05)) # for non-tox, 0.1 is just below 90th 
quantile(rmp_df_osu2toxic$TOXICITY2,seq(0,1,by=0.05)) ### for the toxic labels, 0.1 is about 35th pct 

### quick test of range 
test_sub <- subset(rmp_df_osu2toxic,TOXICITY2 <= 0.10 )
quantile(test_sub$total_toxicity, seq(0,1,by=0.05)) # so within these data, the limit is 3
table(test_sub$total_toxicity)
table(rmp_df_osu2toxic$total_toxicity) # so in this regard, total of 20 data at 3, and of these, 
# 90% still in the data. For 2-2.5, 13 excluded, 84 present. Therefore, 86 percent retained. OVerall, not 
# too bad. IF we are able to keep a false pos rate of 10 percent, it shouldn't be too bad 


### now randomly sample 
set.seed(1337)
rmp_df_osu2nontoxic_sub<-sample_n(rmp_df_osu2nontoxic, nrow(rmp_df_osu2toxic))

### now bind the data 
test_set_osu_rmp <- rbind(rmp_df_osu2toxic,rmp_df_osu2nontoxic)

###save here 
saveRDS(test_set_osu_rmp, "test_set_osu_rmp.rds")
test_set_osu_rmp <- readRDS("test_set_osu_rmp.rds")
names(test_set_osu_rmp)
test_set_osu_rmp$coded_toxic = 0
test_set_osu_rmp$coded_toxic[test_set_osu_rmp$total_toxicity>0] = 1
###export the data here 
write.csv(test_set_osu_rmp,"final_data/labeled_osu_data.csv",row.names = F)


### read in the OSU data 
test_set_osu_rmp <- read.csv("final_data/labeled_osu_data.csv")
## now run the reg 
## let's do a simple linear model 
test_lm <- lm((TOXICITY2)*100 ~ total_toxicity, data=test_set_osu_rmp)
summary(test_lm) # each additional score associated with an increase of 12 points. At range of 5, 
# a score of 5 would be equal to 82. Within bounds. Good enough 

ggplot_persp_corr <- ggplot(test_set_osu_rmp, aes(x=total_toxicity,y=(TOXICITY2)*100,
                                                  group=as.factor(coded_toxic),
                                                  col=as.factor(coded_toxic),
                                                  shape=as.factor(coded_toxic))) +
  geom_point(alpha=0.5, size=2) +
  theme_minimal() + ## cleans up the presentation of the plot 
  labs(title="Comparison of correlation of manual coding on peRspective scores",
       x="Aggregated manual coding of comments", y="peRspective Toxicity score", 
       caption=paste0("X-axis reflects the average of the two coders, with an ICC of 0.633. 
                      R-squared = 0.3263, coef = 12.2794"))+
  scale_color_manual(name = "Manual coding", values= c("gray70", "gray10"), labels = c("Not toxic", "Toxic")) +
  ylim(0,100)+
  scale_shape_manual(name = "Manual coding", values=c(15,17), labels = c("Not toxic", "Toxic")) + 
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")
ggplot_persp_corr # ok, this is what we want; get ri of the \n, 
# grayscale, add transparency, change to triangles and squares
# 

ggsave("plots/coding_perspective_corr_plot_gs.png" ,ggplot_persp_corr, 
       scale=1,width=9,height=6,units = c("in"), dpi=400,bg="white")

### can we create a ggplot with insult as a cross ref? 
ggplot_persp_insult <- ggplot(test_set_osu_rmp, aes(x=(INSULT)*100,y=(TOXICITY2)*100,
                                                  group=as.factor(coded_toxic),
                                                  col=as.factor(coded_toxic))) +
  geom_point() +
  theme_minimal() + ## cleans up the presentation of the plot 
  labs(title="Comparison of correlation of manual coding \n on peRspective scores",
       x="Aggregated manual coding of comments", y="peRspective Toxicity score", 
       caption=paste0("X-axis reflects the average of the two coders, with an ICC of 0.633. /n 
                      R-squared = 0.3263, coef = 12.2794"))+
  scale_color_discrete(name = "Manual coding", labels = c("Not toxic", "Toxic")) + ylim(0,100)
ggplot_persp_insult

##check num of obs 
length(which(rmp_df_osu2nontoxic$INSULT>.20 & rmp_df_osu2nontoxic$TOXICITY2>.1)) # 61 obs 
length(which(rmp_df_osu2toxic$INSULT>.20 & rmp_df_osu2toxic$TOXICITY2>.1)) # 160.

## this is a good boost, as it would be prob of 72 pct toxicity given the label 

### hmmm. Going with this, it would increase FN, but also potentially clean the data A LOT more 
quantile(rmp_df_osu2toxic$INSULT, seq(0,1,by=0.05))
quantile(rmp_df_osu2nontoxic$INSULT, seq(0,1,by=0.05)) ## looking at this, about 40% of the toxic data 
# have scores below 0.05 on insult. HOWEVER, that discards 90% of the nontoxic data. Let's check this out 
### could play around with the insult at 0.1 - 0.2; will want to look back into 


### go with new cut off 
length(which(rmp_df_osu2nontoxic$INSULT>.15 & rmp_df_osu2nontoxic$TOXICITY2>.1)) # 185 obs 
length(which(rmp_df_osu2toxic$INSULT>.15 & rmp_df_osu2toxic$TOXICITY2>.1)) # 264.
## nah, doesn't do much, since they are the same obs; we could look at the type of comments left over, though

insult_sub_tox <- subset(rmp_df_osu2toxic, INSULT >= 0.2 )
table(insult_sub_tox$total_toxicity)
table(rmp_df_osu2toxic$total_toxicity) ## most of the movement does seem to happen with the scores 2 and 
#below. For everything else above, most the obs are kept 



### get the top comments 
rmp_df_osu2nontoxic <- rmp_df_osu2nontoxic[order(rmp_df_osu2nontoxic$TOXICITY2, decreasing = T),]

#let's export this as a csv to rate 
write.csv(rmp_df_osu2nontoxic, "rmp_df_osu2nontoxic.csv")

### read in the weighted data 
rmp_df_osu2nontoxic_coded <- read.csv("rmp_df_osu2nontoxic_manual_search.csv")

### create new field for reason ; clean up data 
rmp_df_osu2nontoxic_coded$reason <- rmp_df_osu2nontoxic_coded$reason_fp1
rmp_df_osu2nontoxic_coded$reason[str_detect(rmp_df_osu2nontoxic_coded$reason_fp1, "sexiness rating")] <-
  "sexiness rating"
rmp_df_osu2nontoxic_coded$reason[str_detect(rmp_df_osu2nontoxic_coded$reason_fp1, "attack on other students")] <-
  "attack on peers"
rmp_df_osu2nontoxic_coded$reason[str_detect(rmp_df_osu2nontoxic_coded$reason_fp1, "negative sentiment")] <-
  "negative sentiment"
rmp_df_osu2nontoxic_coded$reason[str_detect(rmp_df_osu2nontoxic_coded$reason_fp1, "negative senntiment")] <-
  "negative sentiment"
rmp_df_osu2nontoxic_coded$reason[str_detect(rmp_df_osu2nontoxic_coded$reason_fp1, "swear")] <-
  "swear"
## get rid of blanks 
rmp_df_osu2nontoxic_coded <- subset(rmp_df_osu2nontoxic_coded, reason!="" )

# get top comments 
stargazer(rmp_df_osu2nontoxic_coded[1:20,c(41,44,10)], summary=FALSE, rownames=F, type="html",
          font.size = "small", out="plots/top_fp_osu.html", column.sep.width="10pt")




## collapse the data 
rmp_df_osu2nontoxic_coded <- rmp_df_osu2nontoxic_coded %>%
  group_by(reason) %>%
   tally()
## create percent 
rmp_df_osu2nontoxic_coded$percent <- (rmp_df_osu2nontoxic_coded$n/sum(rmp_df_osu2nontoxic_coded$n))*100


## now we will want to create a bar chart 
temp_barplot <-  ggplot(data=rmp_df_osu2nontoxic_coded, aes(x= reorder(reason, +percent), percent)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() + ggtitle("Reasons for peRspective False Positives, OSU") + 
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15)) + ylim(0,60) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) + coord_flip() +
  geom_text(aes(label=n), hjust=-1) + xlab("Reason")
temp_barplot
### now export 
ggsave("plots/osu_false_positive_plot.png" ,temp_barplot, 
       scale=1,width=9,height=6,units = c("in"), dpi=400,bg="white")



#xtable(rmp_df_osu2nontoxic[1:10,c(9,40)], include.rownames=FALSE)
#head(rmp_df_osu2nontoxic$TOXICITY2,200)
stargazer(rmp_df_osu2nontoxic[1:20,c(9,40)], summary=FALSE, rownames=F, type="html",
          font.size = "small", out="plots/top_fp_osu.html", column.sep.width="10pt")

#### get racial info done 

rmp_df_osu3 <- merge(rmp_df_osu2, osu_faculty_demos, 
                     by=c("prof_firstname","prof_lastname", "college"), all.x=F)
### get demos 
table(rmp_df_osu3$male)

table(rmp_df_osu3$non_white,rmp_df_osu3$male)


### we will want to create bar charts now ; let's subset to at least 
rmp_df_osu3 <- subset(rmp_df_osu3, total_toxicity>0) %>%
  group_by(non_white,male) %>%
  summarise(med_total_toxicity=median(total_toxicity), low95total_toxicity=quantile(total_toxicity,0.025),
            upp95total_toxicity=quantile(total_toxicity,0.975), med_TOXICITY=median(TOXICITY2), 
            low95TOXICITY=quantile(TOXICITY2,0.025),
            upp95TOXICITY=quantile(TOXICITY2,0.975),
            med_insult=median(INSULT),   low95INSULT=quantile(INSULT,0.025),
            upp95INSULT=quantile(INSULT,0.975))
## create labels 
rmp_df_osu3$group <- ""
rmp_df_osu3$group[rmp_df_osu3$male==0 & rmp_df_osu3$non_white==0] <- "White women"
rmp_df_osu3$group[rmp_df_osu3$male==1 & rmp_df_osu3$non_white==0] <- "White men"
rmp_df_osu3$group[rmp_df_osu3$male==0 & rmp_df_osu3$non_white==1] <- "Non-white women"
rmp_df_osu3$group[rmp_df_osu3$male==1 & rmp_df_osu3$non_white==1] <- "Non-white men"

## interesting. minorities not getting the worst ; apply the bar plot 

osu_demos_barplot <-  ggplot(rmp_df_osu3,aes(group, med_TOXICITY*100)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=low95TOXICITY*100, ymax=upp95TOXICITY*100), width=.2,
                position=position_dodge(.9)) + 
  theme_minimal() + ggtitle("Distribution of peRspective Toxicity Scores by Race and Sex") + 
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15)) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.4, hjust=0.25)) +
  labs(x="Demographic group", y="Toxicity score", 
       caption=paste0("Data subsetted to comments manually classified as having non-zero toxicity",sep="\n",
                      "Bars reflect median values. Error bars reflect 95% confidence intervals."))
osu_demos_barplot
ggsave("plots/osu_perspective_toxicity_demos.png" ,osu_demos_barplot, 
       scale=1,width=9,height=6,units = c("in"), dpi=400,bg="white")
### now create for manual coding 
osu_demos_barplot_mc <-  ggplot(rmp_df_osu3,aes(group, med_total_toxicity)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=low95total_toxicity, ymax=upp95total_toxicity), width=.2,
                position=position_dodge(.9)) + 
  theme_minimal() + ggtitle("Distribution of manually coded Toxicity Scores \n by Race and Sex") + 
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15)) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.4, hjust=0.25)) +
  labs(x="Demographic group", y="Toxicity score", 
       caption=paste0("Data subsetted to comments manually classified as having non-zero toxicity",sep="\n",
                      "Bars reflect median values. Error bars reflect 95% confidence intervals."))
osu_demos_barplot_mc
ggsave("plots/osu_perspective_manualcoded_demos.png" ,osu_demos_barplot_mc, 
       scale=1,width=9,height=6,units = c("in"), dpi=400,bg="white")



#############################################################################################
################## UNC Section #############################################################
### unc 500 obs 
unc_df_persp <- readRDS("perspective_unc_data.rds")


### read in the UNC data 
unc_df_k <- read.csv("faculty characteristics web scraping - unc_comments_koen.csv")
unc_df_g <- read.csv("faculty characteristics web scraping - unc_comments_gabe.csv")

## get rid of the Bowen data 

unc_df_k <- subset(unc_df_k, college  != "OHIO STATE UNIVERSITY" )
unc_df_g <- subset(unc_df_g, college  != "OHIO STATE UNIVERSITY" )

### now sort correctly 
unc_df_g <- unc_df_g[order(unc_df_g$row_id), ]
unc_df_k <- unc_df_k[order(unc_df_k$row_id), ]

### limit to above threshold 
unc_df_g <- subset(unc_df_g, above_threshold == 1)
unc_df_k <- subset(unc_df_k, above_threshold == 1)

## drop thumbs data 
unc_df_g <- subset(unc_df_g, select=-c(thumbs_up,thumbs_down))

### ensure same cols and first 500 rows 
ncol(unc_df_k)==ncol(unc_df_k) # TRUE; good 

## check the diffs in the fields 
colSums(unc_df_k[,10:29])
colSums(unc_df_g[,10:29])


### now do the averaging and such for consistency. 
names(unc_df_k) # 10 - 29 is the total index 

### results matrix 
unc_results_diffs <- unc_df_g[,10:29] - unc_df_k[,10:29]

### let's bind the data, and then subset if diff 
unc_results_diffs2 <- cbind(unc_df_g[1:9], unc_results_diffs)

## create a col on num of diffs 
unc_results_diffs2$total_diffs <- rowSums(unc_results_diffs2[,-c(1:9)] != 0) 
unc_results_diffs2 <- subset(unc_results_diffs2, total_diffs != 0)

## get table of diffs 
table(unc_results_diffs2$total_diffs)

## that worked. Now let's get this exported 
#write.csv(unc_results_diffs2, "unc500diffs.csv", row.names = F)


### now let's subset the data 


k_agg <- rowSums(unc_df_k[,10:29])
g_agg <- rowSums(unc_df_g[,10:29])


## bind into matrix 
test_mat_unc <- matrix(cbind(k_agg,g_agg),ncol=2)


## ICC here 
toxic_icc <- icc(test_mat_unc, "oneway", "agreement")
toxic_icc

######## Proceed to combine the two for the UNC data 
combined_unc_coding <- unc_df_k[,10:32] + unc_df_g[,10:32]
## now divide by 2 
combined_unc_coding <- combined_unc_coding/2

### create total toxicity field 
combined_unc_coding$total_toxicity <- rowSums(combined_unc_coding[,1:20])
summary(combined_unc_coding$total_toxicity) ## seems about right 

## merge with the id info from unc 
unc_results_merge <- cbind(unc_df_g[,1:9],combined_unc_coding)

## ok, now let's get the information from the peRspective on 
### winnow down the data to just persp of interest 

unc_results_merge <- merge(unc_results_merge, unc_df_persp, by.x="row_id", by.y="text_id" )

## create toxicity 2 field 
unc_results_merge$TOXICITY2 <- unc_results_merge$TOXICITY+unc_results_merge$SEVERE_TOXICITY

### now that we have the toxicity data, we will want to create the same graphics and such that we did for 
# OSU 

### get num of toxic comments and such 
length(which(unc_results_merge$total_toxicity>0))

## get quantile range 
quantile(unc_results_merge$total_toxicity,seq(0,1,by=0.05))

###export to final data 
saveRDS(unc_results_merge, "final_data/labeled_unc_data.rds")
# write.csv(test_set_unc_rmp, "final_data/labeled_unc_data.csv",row.names = F )

### let's split the data and measure range of the toxicity scores 
unc_results_merge_toxic <- subset(unc_results_merge, total_toxicity >0 )
unc_results_merge_nontoxic <- subset(unc_results_merge, total_toxicity ==0 )

## quant range 
quantile(unc_results_merge_toxic$TOXICITY2, seq(0,1,by=0.05))
quantile(unc_results_merge_nontoxic$TOXICITY2, seq(0,1,by=0.05))

### lets randomly sample the nontoxic data for the regression 
set.seed(1337)
unc_results_merge_nontoxic_sub<-sample_n(unc_results_merge_nontoxic, nrow(unc_results_merge_toxic))

### now bind the data 
test_set_unc_rmp <- rbind(unc_results_merge_toxic,unc_results_merge_nontoxic_sub)


### how does total toxicity relate to the scores? looks like it 
toxic_model_unc <- lm( test_set_unc_rmp$TOXICITY2*100 ~ test_set_unc_rmp$total_toxicity)
summary(toxic_model_unc)

## not toxic coding 
test_set_unc_rmp$coded_toxic = 0
test_set_unc_rmp$coded_toxic[test_set_unc_rmp$total_toxicity>0] = 1

##export as csv to final data 
write.csv(test_set_unc_rmp, "final_data/labeled_unc_data.csv",row.names = F )

### now create the ggplot 
ggplot_persp_corr_unc <- ggplot(test_set_unc_rmp, aes(x=total_toxicity,y=(TOXICITY2)*100,
                                                  group=as.factor(coded_toxic),
                                                  col=as.factor(coded_toxic))) +
  geom_point() +
  theme_minimal() + ## cleans up the presentation of the plot 
  labs(title="Comparison of correlation of manual coding \n on peRspective scores",
       x="Aggregated manual coding of comments", y="peRspective Toxicity score", 
       caption=paste0("X-axis reflects the average of the two coders, with an ICC of 0.0768. 
                      R-squared = 0.3188, coef = 7.5 "))+
  scale_color_discrete(name = "Manual coding", labels = c("Not toxic", "Toxic")) + ylim(0,100)+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")
ggplot_persp_corr_unc
ggsave("plots/coding_perspective_corr_plot_unc.png" ,ggplot_persp_corr_unc, 
       scale=1,width=9,height=6,units = c("in"), dpi=400,bg="white")

### now identify the results by race, like we did for the OSU data


unc_results_merge2 <- merge(unc_results_merge, unc_faculty_demos, 
                     by=c("prof_firstname","prof_lastname", "college"), all.x=F)
## get dist 
table(unc_results_merge2$non_white, unc_results_merge2$male)

### we will want to create bar charts now ; let's subset to at least 
unc_results_merge2 <- subset(unc_results_merge2, total_toxicity>0) %>%
  group_by(non_white,male) %>%
  summarise(med_total_toxicity=median(total_toxicity), low95total_toxicity=quantile(total_toxicity,0.025),
            upp95total_toxicity=quantile(total_toxicity,0.975), med_TOXICITY=median(TOXICITY2), 
            low95TOXICITY=quantile(TOXICITY2,0.025),
            upp95TOXICITY=quantile(TOXICITY2,0.975),
            med_insult=median(INSULT),   low95INSULT=quantile(INSULT,0.025),
            upp95INSULT=quantile(INSULT,0.975))
## create labels 
unc_results_merge2$group <- ""
unc_results_merge2$group[unc_results_merge2$male==0 & unc_results_merge2$non_white==0] <- "White women"
unc_results_merge2$group[unc_results_merge2$male==1 & unc_results_merge2$non_white==0] <- "White men"
unc_results_merge2$group[unc_results_merge2$male==0 & unc_results_merge2$non_white==1] <- "Non-white women"
unc_results_merge2$group[unc_results_merge2$male==1 & unc_results_merge2$non_white==1] <- "Non-white men"

## interesting. minorities not getting the worst ; apply the bar plot 

unc_demos_barplot <-  ggplot(unc_results_merge2,aes(group, med_TOXICITY*100)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=low95TOXICITY*100, ymax=upp95TOXICITY*100), width=.2,
                position=position_dodge(.9)) + 
  theme_minimal() + ggtitle("Distribution of peRspective Toxicity Scores by Race and Sex") + 
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15)) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.4, hjust=0.25)) +
  labs(x="Demographic group", y="Toxicity score", 
       caption=paste0("Data subsetted to comments manually classified as having non-zero toxicity",sep="\n",
                      "Bars reflect median values. Error bars reflect 95% confidence intervals."))
unc_demos_barplot
ggsave("plots/unc_perspective_toxicity_demos.png" ,unc_demos_barplot, 
       scale=1,width=9,height=6,units = c("in"), dpi=400,bg="white")
### now create for manual coding 
unc_demos_barplot_mc <-  ggplot(unc_results_merge2,aes(group, med_total_toxicity)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=low95total_toxicity, ymax=upp95total_toxicity), width=.2,
                position=position_dodge(.9)) + 
  theme_minimal() + ggtitle("Distribution of manually coded Toxicity Scores \n by Race and Sex") + 
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15)) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.4, hjust=0.25)) +
  labs(x="Demographic group", y="Toxicity score", 
       caption=paste0("Data subsetted to comments manually classified as having non-zero toxicity",sep="\n",
                      "Bars reflect median values. Error bars reflect 95% confidence intervals."))
unc_demos_barplot_mc
ggsave("plots/unc_perspective_manualcoded_demos.png" ,unc_demos_barplot_mc, 
       scale=1,width=9,height=6,units = c("in"), dpi=400,bg="white")

### check how many toxic comments that we have above 2 at this point 
length(which(unc_results_merge$total_toxicity>=2)) # 44
length(which(rmp_df_osu2$total_toxicity>=2)) ## 114 
