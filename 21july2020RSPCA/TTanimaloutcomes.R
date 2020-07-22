#July 21, 2020 - Tidy Tuesday - RSPCA data


rm(list = ls())
# Get the Data

# Or read in the data manually

animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')

#lets look at our categories
unique(animal_outcomes$animal_type)
unique(animal_outcomes$year)
unique(animal_outcomes$outcome)

#Let's answer the following questions:
#Which territory rehomed the most of each type of animal each year?
#Which territory euthanized the most of each type of animal each year?

#this should be relatively simple graphing, and to do so, i just need to separate out my datasets
#ooooh but now also need to turn the counties into a single column - use gather
library(tidyverse)

euthan = animal_outcomes %>% 
  filter(outcome == "Euthanized") %>%
  select(-`Total`) %>% #drops the Total column
  gather(`ACT`, `NSW`,`NT`,`QLD`,`SA`,`TAS`,`VIC`,`WA`,
         key = "county", value = "number")
rehome = animal_outcomes %>% 
  filter(outcome == "Rehomed") %>% 
  select(-`Total`) %>%
  gather(`ACT`, `NSW`,`NT`,`QLD`,`SA`,`TAS`,`VIC`,`WA`,
         key = "county", value = "number")

#Subset each by animal
#make a list of the animals so we can put them in a for loop:
alist = unique(euthan$animal_type)
alist #cool, gives us the 6 different animals we want

#set up a for loop to create separate dfs for each animal
#for euthanasia data
for (i in alist) {
  assign(paste0(i,"_","E"), euthan %>% 
           filter(grepl(i,animal_type)))
}
#for rehoming data
for (i in alist) {
  assign(paste0(i,"_","H"), rehome %>% 
           filter(grepl(i,animal_type)))
}
#rename to eliminate spaces
OtherE = `Other Animals_E`
OtherH = `Other Animals_H`


#time to play with plotting

library(ggplot2)
#install wes anderson color palettes
install.packages("wesanderson")
library(wesanderson)

#want to use IsleofDogs1 plus the red and bottlegreen in Cavalcanti1
install.packages("paletteer")
?wes_palette
wes_palettes$Cavalcanti1
Cavalcantisub = c("#81A88D", "#972D15")
Cavalcantisub

unique(euthan$county)

#Euthanization Plots
c1 = ggplot(Cats_E, aes(fill=county, y=number, x=year)) + geom_bar(position="stack", stat="identity") + theme_classic() + theme(legend.position="bottom") + ggtitle("Cats") + xlab("Year") + ylab("Number Euthanized") + scale_fill_manual(values = c(wes_palette("IsleofDogs1"),Cavalcantisub))

d1 = ggplot(Dogs_E, aes(fill=county, y=number, x=year)) + geom_bar(position="stack", stat="identity") + theme_classic() + theme(legend.position="none") + ggtitle("Dogs") + xlab("Year") + ylab("Number Euthanized") + scale_fill_manual(values = c(wes_palette("IsleofDogs1"),Cavalcantisub))

h1 = ggplot(Horses_E, aes(fill=county, y=number, x=year)) + geom_bar(position="stack", stat="identity") + theme_classic() + theme(legend.position="none") + ggtitle("Horses") + xlab("Year") + ylab("Number Euthanized") + scale_fill_manual(values = c(wes_palette("IsleofDogs1"),Cavalcantisub))

l1 = ggplot(Livestock_E, aes(fill=county, y=number, x=year)) + geom_bar(position="stack", stat="identity") + theme_classic() + theme(legend.position="none") + ggtitle("Livestock") + xlab("Year") + ylab("Number Euthanized") +  scale_fill_manual(values = c(wes_palette("IsleofDogs1"),Cavalcantisub))

o1 = ggplot(OtherE, aes(fill=county, y=number, x=year)) + geom_bar(position="stack", stat="identity") + theme_classic() + theme(legend.position="none") + ggtitle("Other Animals") + xlab("Year") + ylab("Number Euthanized") +  scale_fill_manual(values = c(wes_palette("IsleofDogs1"),Cavalcantisub))

w1 = ggplot(Wildlife_E, aes(fill=county, y=number, x=year)) + geom_bar(position="stack", stat="identity") + theme_classic() + theme(legend.position="none") + ggtitle("Wildlife") + xlab("Year") + ylab("Number Euthanized") + scale_fill_manual(values = c(wes_palette("IsleofDogs1"),Cavalcantisub))

#arranging all 6 plots together
library(grid)
library(gridExtra)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(c1)

grid.arrange(arrangeGrob(c1+theme(legend.position="none"),d1,h1,l1,w1,o1,nrow=3),mylegend,heights=c(10, 1))



#Rehoming Plots
c2 = ggplot(Cats_H, aes(fill=county, y=number, x=year)) + geom_bar(position="stack", stat="identity") + theme_classic() + theme(legend.position="bottom") + ggtitle("Cats") + xlab("Year") + ylab("Number Rehomed") + scale_fill_manual(values = c(wes_palette("IsleofDogs1"),Cavalcantisub))

d2 = ggplot(Dogs_H, aes(fill=county, y=number, x=year)) + geom_bar(position="stack", stat="identity") + theme_classic() + theme(legend.position="none") + ggtitle("Dogs") + xlab("Year") + ylab("Number Rehomed") + scale_fill_manual(values = c(wes_palette("IsleofDogs1"),Cavalcantisub))

h2 = ggplot(Horses_H, aes(fill=county, y=number, x=year)) + geom_bar(position="stack", stat="identity") + theme_classic() + theme(legend.position="none") + ggtitle("Horses") + xlab("Year") + ylab("Number Rehomed") + scale_fill_manual(values = c(wes_palette("IsleofDogs1"),Cavalcantisub))

l2 = ggplot(Livestock_H, aes(fill=county, y=number, x=year)) + geom_bar(position="stack", stat="identity") + theme_classic() + theme(legend.position="none") + ggtitle("Livestock") + xlab("Year") + ylab("Number Rehomed") +  scale_fill_manual(values = c(wes_palette("IsleofDogs1"),Cavalcantisub))

o2 = ggplot(OtherH, aes(fill=county, y=number, x=year)) + geom_bar(position="stack", stat="identity") + theme_classic() + theme(legend.position="none") + ggtitle("Other Animals") + xlab("Year") + ylab("Number Rehomed") +  scale_fill_manual(values = c(wes_palette("IsleofDogs1"),Cavalcantisub))

w2 = ggplot(Wildlife_H, aes(fill=county, y=number, x=year)) + geom_bar(position="stack", stat="identity") + theme_classic() + theme(legend.position="none") + ggtitle("Wildlife") + xlab("Year") + ylab("Number Rehomed") + scale_fill_manual(values = c(wes_palette("IsleofDogs1"),Cavalcantisub))

#arranging all 6 plots together

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(c2)

grid.arrange(arrangeGrob(c2+theme(legend.position="none"),d2,h2,l2,o2,mylegend,nrow=3))
