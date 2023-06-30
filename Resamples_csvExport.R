


# packages ####
require(maditr)
require(tidyverse)
#####

FullDF <- read.csv("N:/Working/Niziolek/reach-level streams data (incl. resamps).csv")%>%
  mutate(Dup = rowSums(.[,c("Ten_Duplicate","Site_Duplicate")]),
         Year = substr(.$Start_Date,1,4)) # Year already exists from the Macros DF, but need NAs filled in


# coping with multiple resamples at single reaches ####
duplicatingRows <- FullDF[c(which(FullDF$Dup == 0 & FullDF$Reach_Name == "CRLAWQ14" & FullDF$Year == 2021)),]
duplicatingRows$Reach_Name <- "CRLAWQ14_1"
# now change ONE of the duplicates' names
FullDF[c(which(FullDF$Site_Duplicate == 1 & FullDF$Reach_Name == "CRLAWQ14" & FullDF$Year == 2021)),]$Reach_Name <- "CRLAWQ14_1"
# merge and clear duplicatingRows
FullDF <- rbind(FullDF,duplicatingRows)
rm(duplicatingRows)

duplicatingRows <- FullDF[c(which(FullDF$Dup == 0 & FullDF$Reach_Name == "LAVOWQ27" & FullDF$Year == 2017)),]
duplicatingRows$Reach_Name <- "LAVOWQ27_1"
# now change ONE of the duplicates' names
FullDF[c(which(FullDF$Site_Duplicate == 1 & FullDF$Reach_Name == "LAVOWQ27" & FullDF$Year == 2017)),]$Reach_Name <- "LAVOWQ27_1"
# merge and clear duplicatingRows
FullDF <- rbind(FullDF,duplicatingRows)
rm(duplicatingRows)


# widen and subset to resampled reaches, export to csv ####
# MODIFY THIS ####
# dcast outputs data.table which uses scary syntax I don't know
ResampDF <- maditr::dcast(FullDF,Reach_Name+Year~Dup,
                          value.var = names(FullDF[,c(3,6:91)]))
ResampDF <- ResampDF[c(which(!is.na(ResampDF$Start_Date_1))),]

write.csv(ResampDF,"resamples, reach-level dataframe.csv")
#####
ResampDF<-read.csv("resamples, reach-level dataframe.csv")


# vector of column names to tally up non-NAs from:
RS_vars <- as.vector(outer(names(FullDF),rep(1),paste,sep="_"))





#     Maybe, instead of trying to subset the columns at this stage, we just produce a by-year
#  sum of non-NA values for each column... 
# then we can reach back into the missing values total#reaches for a denominator for select columns






# below is attempting to print 



names(ResampDF)[names(ResampDF)%in%RS_vars] #this prints the column names that occur in the dataset
length(which(is.na(ResampDF$item) == FALSE)) # this, when 'item' is swapped for an actual column, gives the right output

# for(item in RS_vars){ length(which(is.na(ResampDF$item) == FALSE)) } # this doesn't work, though just the length part gives values

for(i in names(ResampDF)%in%RS_vars){ length(which(is.na(ResampDF$i) == FALSE)) }



lengthTBL(RS_vars)
rm(t,i,item)

print(names(ResampDF)[names(ResampDF)%in%RS_vars])

lengthTBL<-function(i){length(which(is.na(ResampDF$i) == FALSE))}

sapply(ResampDF[names(ResampDF)%in%RS_vars],FUN=lengthTBL) 



#   returns all zeros, but in the format I need    #################
RS_vars <- as.vector(outer(names(FullDF),rep(1),paste,sep="_"))

lengthTBL<-function(i){length(which(is.na(ResampDF$i) == FALSE))}

sapply(names(ResampDF)[names(ResampDF)%in%RS_vars],FUN=lengthTBL)


#ok so what does just this do
length(which(is.na(ResampDF$Total.Taxa.Richness_1) == FALSE))
# that works... 



############
apply(ResampDF[names(ResampDF)%in%RS_vars],2,FUN=lengthTBL)

ResampDF[,c(names(ResampDF)[names(ResampDF)%in%RS_vars])]
names(ResampDF)[names(ResampDF)%in%RS_vars]

ResampDF[,c("Reach_Name","Year","Start_Date_0","Start_Date_1","Nitrogen_tot_0","Nitrogen_tot_1","Phosphorus_tot_0")]
# okay so this (above) works

# SARAH said this should work but it still only returns a list of the column names that overlap,
# not the dataframe subset to those columns as I intend to have
ResampDF<-as.data.frame(ResampDF)
ResampDF[,names(ResampDF)[names(ResampDF)%in%RS_vars]]


ResampDF[,c(names(ResampDF)[names(ResampDF)%in%RS_vars])]

head(ResampDF)


#  THIS RETURNS A VECTOR OF PROPERLY FORMATTED COLUMN NAMES i think
cat("\"",names(ResampDF)[names(ResampDF)%in%RS_vars],"\"",sep="\", \"")
# EXCEPT the "NULL" at the end, why is that here    
ResampDF[,c(cat("\"",names(ResampDF)[names(ResampDF)%in%RS_vars],"\"",sep="\", \""))] 

test<-ResampDF[paste("\"",names(ResampDF)[names(ResampDF)%in%RS_vars],"\"",sep="\", \"")]

ResampDF[,c('c4t_awd_1')]

list(names(ResampDF)[names(ResampDF)%in%RS_vars],sep=)

# loop length(which(is.na(ResampDF$Nitrogen_tot_1) == FALSE)) through names(ResampDF)[names(ResampDF)%in%RS_vars], 
#       saving the output to one column of a DF and names(ResampDF)[names(ResampDF)%in%RS_vars] to a second column


lengthTBL <- function(x){ t<-print(length(which(is.na(ResampDF$x) == FALSE))) }






