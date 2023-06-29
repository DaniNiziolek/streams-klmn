
# Script produces a table of data completion rate (%) 


# set data, variables, and output file names ####

# user dataframe - change Your_DF
DF <- Your_DF

# sampling unit identifier (e.g. plot number) - Change ' Current date column name ' (leave quotes)
names(DF[,c("Current ID column name")]) <- "ID"

# sampling date - Change ' Current date column name ' (leave quotes)
names(DF[,c(" Current date column name ")]) <- "Date"

# list variables for which missing values/%completion is to be computed
# do not list ID columns
Variables <- c("ColumnEx1","ColumnEx2","ColumnEx3","ColumnEx4",".....")

# name output files
CSVoutput <- "name of the .csv containing %completion rate by year across all parks"
kableOutput # if not kable whatever package is used for figure generation
#####

# my df, variables ####
DF <- read.csv("reach-level streams data.csv") # BigDF_og
Variables <- c("Nitrogen_tot","Phosphorus_tot","Sodium_diss","Potassium_diss","Calcium_diss","Magnesium_diss",
               "sulfate_diss","Chlorine_diss","Org_carbon_diss","Water_temp","Air_temp","Slope","Temp","SC","DO",
               "ORP","Turb","DOsat","TDS","pH","Ht","Dist","Ht_avg","Dist_avg","SedMean1","SedMed1","LSUB_DMM",
               "MeanPer","PercentPool","PercentFast","RiparianCover","habCover","Riparian_Disturbance","ABF_15_0103",                   
               "ABF_15_0306","ABF_15_0608","ABF_15_08","ABF_515_0103","ABF_515_0306","ABF_515_0608","ABF_515_08",
               "ABF_155_0103","ABF_155_0306","ABF_155_0608","ABF_155_08","c1t_awd","c2t_awd","c3t_awd","c4t_awd",
               "c5t_awd","c1v_awd","c2v_awd","c3v_awd","c4v_awd","c5v_awd","BF_15_0306","BF_15_0103","BF_15_0608",
               "BF_15_08","BF_515_0103","BF_515_0306","BF_515_0608","BF_515_08","BF_155_0103","BF_155_0306",
               "BF_155_0608","BF_155_08","c1t_wd","c2t_wd","c3t_wd","c4t_wd","c5t_wd","c1v_wd","c2v_wd","c3v_wd",
               "c4v_wd","c5v_wd","LRBS_TST","Total.Taxa.Richness","Invert_Divers","HBI",
               "EPA.West.Wide.Invertebrate.MMI","OoverE0.5","OoverE0.1")
##### 

# Runs automatically (one hopes) to produce CompletionRate_tbl ####
# tally up n(missing values) in each column (columns listed in Variables)
SumIsNA <- function(x){sum(is.na(x))}
#also define DF <- User's_Input_Data; change Reach_Name to SamplUnits <- DF$sample_unit_ID_col

n_NA_obs <- DF %>%
  dplyr::mutate(Year = substr(Start_Date,1,4))%>%
  dplyr::select(Year,Reach_Name,tidyselect::any_of(Variables)) %>%
  dplyr::group_by(Year) %>% # variable Year will be created in code from Start_Date, can leave like this
  dplyr::summarize(across(Variables,SumIsNA),
                   N = length(unique(Reach_Name)))

# calculate % completion from # missing
PercFun <- function(cols){(({n_NA_obs$N} - cols)/{n_NA_obs$N})*100}

CompletionRate_tbl <- n_NA_obs %>%
  dplyr::select(everything())%>%
  dplyr::mutate(across(Variables,PercFun))

# save resulting table to csv ####
write.csv(CompletionRate_tbl,paste("CSVoutput",".csv",sep=""))
#####




# make the table pretty, save as PDF for now but won't export to image once we transition to markdown ####
library(knitr)
library(kableExtra)
library(formattable)
library(dplyr)
RuleMissings<- function(x) {
   ifelse(x < 95.0,
          cell_spec(x, "html", color = "red", bold=T),
          cell_spec(x, "html", color = "black"))}

Compl<-CompletionRate_tbl %>%
  
  dplyr::mutate(across(Variables,RuleMissings)) %>%
  dplyr::select(everything()) %>%
  
  
  knitr::kable("html", escape = F, row.names=FALSE,align=c(rep("r",3),"c")) %>% 
  #turns it into kable object so column_spec can take it
  kableExtra::column_spec(c(1,2,3), width = "1cm") %>% #set width of column two (wraps header text)
  kableExtra::column_spec(4:ncol(), width = "1.5cm") %>%
  kableExtra::kable_styling("hover", full_width = F) #makes the table space out nicely

png("data completeness rate 2011_21.png")
print(Compl)
dev.off()
#####


# Theoretically any table written in R and displayed in a quarto/markdown is 508-a-ok, 
# so just transition over to one of those with the functioning code, I guess?

