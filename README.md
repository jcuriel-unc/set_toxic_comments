# set_toxic_comments
A replication repository for Automating Identification of Toxic Comments in Student Evaluations of Teaching

Read me for: Suzelis, Koen, Gabriel Mott and John A Curiel. 2025. "Automating Identification of Toxic Comments in Student Evaluations of Teaching." Journal of Academic Ethics. 

## peRspective_applyR.R - The script to 1) estimate the race of the missing faculty's racial information and, 2) apply the Perspective scores to the OSU and UNC Rate My Professor data. 

  ### packages: tidyverse, wru, foreign, rstudioapi, ggplot2, irr, arm, peRspective, grid, stargazer, remotes

  ### Input files: 
    faculty characteristics web scraping - osu_comments_koen_final.csv - The manually coded ratings of RMP comments by coder 1
    faculty characteristics web scraping - osu_comments_gabe_final.csv - The manually coded ratings of RMP comments by coder 2 
    text_cleaning/scored_rmp_data_osu_final.csv - The combined and final aggregated and scored manually coded data. 

  ### Intermediary files: 

    persp_rmp_output_osu.rds - The saved output of the peRspective scores for the OSU data 

  ### Output files 

  
    
