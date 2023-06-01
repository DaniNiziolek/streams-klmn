
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



# Runs automatically (one hopes) ####
# tally up n(missing values) in each column (columns listed in Variables)
SumIsNA <- function(x){sum(is.na(x))}
#also define DF <- User's_Input_Data; change Reach_Name to SamplUnits <- DF$sample_unit_ID_col

n_NA_obs <- DF %>%
  mutate(Year = substr(Start_Date,1,4))%>%
  dplyr::select(Year,Reach_Name,any_of(Variables)) %>%
  dplyr::group_by(Year) %>% # variable Year will be created in code from Start_Date, can leave like this
  dplyr::summarize(across(Variables,SumIsNA),
                   N = length(unique(Reach_Name)))

# calculate % completion from # missing
PercFun <- function(cols){(({n_NA_obs$N} - cols)/{n_NA_obs$N})*100}

CompletionRate_tbl <- n_NA_obs %>%
  select(everything())%>%
  mutate(across(Variables,PercFun))

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

TBL_Slp_missing<-missing_SLP[,c(1,2,4,6)]#subset and order columns to match other %missing tables' format;
# year, NReaches VISITED, NReached Sampled, %Complete
names(TABLE)<-c("Year","Total Samples","Slope")


png("Slope missing values 2011_21.png")
TBL_Slp_missing %>%
  mutate(
   Slope = RuleMissings(TBL_Slp_missing$Slp_ComplRt)
  ) %>%
  select("Year","Total_reaches_visited","NSamp_reaches","Slope") %>%
  kable("html", escape = F, row.names=FALSE,align=c(rep("r",3),"c"),
       col.names=c("Year","Reaches visited","Reaches sampled",
                   "Slope data completeness (%)")) %>% #turns it into kable object so column_spec can take it
  column_spec(c(1,2,3), width = "1cm") %>% #set width of column two (wraps header text)
  column_spec(4, width = "1.5cm") %>%
  kable_styling("hover", full_width = F) #makes the table space out nicely
dev.off()

#####


# Theoretically any table written in R and displayed in a quarto/markdown is 508-a-ok, 
# so just transition over to one of those with the functioning code, I guess?

