
# Script produces a table of data completion rate (%) 





# Set user's input dataframe to DF




# List the variables over which NA values are to be assessed
Variables <- c("ColumnEx1","ColumnEx2","ColumnEx3","ColumnEx4",".....")


# tally up n(missing values) in each column (columns listed in Variables)
SumIsNA <- function(x){sum(is.na(x))}
#also define DF <- User's_Input_Data; change Reach_Name to SamplUnits <- DF$sample_unit_ID_col

n_NA_obs <- Macros %>%
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




# save resulting table to csv
# (I would prefer to have this print out a nicely formatted kable table and things like that, 
# can we figure out which package(s) are 508 compliant)
