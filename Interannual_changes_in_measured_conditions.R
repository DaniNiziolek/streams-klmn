# year to year change table, 
# mostly dumping things here so I can delete them from my parameter files because 
# it's an UNBEARABLY ANNOYING mess I do not feel like fixing right now <<<333 

require(ggplot2)

DF <- read.csv(file.choose())  # the postdoc at my old lab just showed me file.choose, 
# I prefer this over having people type in their file name I think... thoughts?


# this needs to become a function with this aes(y) set up to take a list of variables
fig1 <- ggplot(DF, aes(x = Year, y = Total.Taxa.Richness, 
                           group = Reach_Name, colour = Reach_Name)) +
  geom_line() +
  theme(legend.position = 'none') + # find a way to cram guides=collect into this pipe for when that inevitably becomes needed
  facet_wrap(Park ~ .,ncol=2,scales="free_x")+
  scale_x_continuous#(breaks = SEE BELOW)
 

# If I make this a for loop saving to object P can I pdf(unlist(P),onefile=T) to get a different page for every set of plots?
# If I do THAT can I set the page headers to loop through the list of variables? Or is there a cleaner way, might merit a Names list?

pdf(' NAME .pdf')
print(fig1)
dev.off()


# BELOW is here :) 

# nonfunctional, all plots go to the else case (2011 tried in and out of quotes - same outcome)
# also tried Year swapped for x, shouldn't matter at all but I did try
function(x){
  if(min(x)=="2011"){seq(2011,2021,2)} #this is the bitch, this is the line that's pushing me
  else{seq(2012,2021,1)}#RIGHT to the edge today. I hate her. She hates me. No one is having fun.
}

# true case prints only 2011 (no other values on x), false case prints nothing
function(x){
ifelse(min(x) < 2012,
       {seq(2011,2021,2)},
       {seq(2012,2021,1)}) }

# and if we combine the two,
function(x){
  ifelse(min(x)=="2011", {seq(2011,2021,2)} 
         {seq(2012,2021,1)}) }
# nah we get the same as the first ifelse. what the hell

