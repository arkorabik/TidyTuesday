#Tidy Tuesday Data Visualization 
#May 25, 2020 - COCKTAILS!

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(readr)
library(ggthemes)
JUICE <- read_csv("Desktop/TidyTuesday/25may2020COCKTAILS/JUICE.csv")

#first we're gonna play around with assigning colors to specific juices
#JUICE$ingredient = as.factor(JUICE$ingredient)

#lvls  <- levels(JUICE$ingredient)
#lvls
#lvls = as.factor(lvls)

#C = c("red", "darkred","orangered4","darkorange2","palegreen2","greenyellow","peachpuff4","red4","purple4","sandybrown","yellow1","olivedrab1","darkgoldenrod1","orange","palevioletred4","yellow3","maroon4","firebrick1","tomato3","salmon","tan1")
#C = as.factor(C)
#names(C) = lvls

#jcscale <- scale_fill_manual(name="lvls", values=C)

#honestly, none of my efforts to create a shortcut for colors worked, so I'll just do it manually

#If I wanted to make every cocktail in "Mr. Boston's Bartender Guide", how much juice would I need to buy? -------------------------
jsumtot = JUICE %>% 
  group_by(ingredient) %>% 
  summarize(Amt = sum(measure_amt))
sum(jsumtot$Amt) #703.75 (only 5.5 gallons!)

ggplot(jsumtot, aes(x=reorder(ingredient, Amt), y=Amt, fill=ingredient)) + 
  geom_bar(stat="identity", show.legend = FALSE) + 
  xlab("Juice Type") + 
  ylab("Volume of Juice - fl. oz.") + 
  ggtitle("How Much Juice Do I Need to Buy?") + 
  scale_fill_manual(values=c("red", "darkred","orangered4","darkorange2","palegreen2","greenyellow","peachpuff4","red4","purple4","sandybrown","yellow1","olivedrab1","darkgoldenrod1","orange","palevioletred4","yellow3","maroon4","firebrick1","tomato3","salmon","tan1")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))


#How many cocktails use each juice? -------------------------
jtypes = JUICE %>% 
  group_by(ingredient) %>% 
  tally() 

ggplot(jtypes, aes(x=reorder(ingredient, -n), y=n, fill=ingredient)) + 
  geom_bar(stat="identity", show.legend = FALSE) + 
  xlab("Juice Type") + 
  ylab("Number of Cocktails Using A Juice") + 
  ggtitle("How Popular is This Juice?") +
  scale_fill_manual(values=c("red", "darkred","orangered4","darkorange2","palegreen2","greenyellow","peachpuff4","red4","purple4","sandybrown","yellow1","olivedrab1","darkgoldenrod1","orange","palevioletred4","yellow3","maroon4","firebrick1","tomato3","salmon","tan1")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#What category of drink contains the most juicy drinks? -------------------------
cats = JUICE %>% 
  group_by(category) %>% 
  tally() 

ggplot(cats, aes(x=reorder(category, n), y=n, fill=category)) + 
  geom_bar(stat="identity", show.legend = FALSE) + 
  xlab("Cocktail Category") +
  ylab("") +
  ggtitle("Number of Drinks Containing Juice in 'Mr. Boston's Bartender Guide'") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(size = 12, face = "bold"))
  
#What are the juiciest drinks? What are the least juicy drinks? -------------------------
#want to make stacked barplot in these next three graphs
howjuicy = JUICE %>% 
  group_by(name) %>% 
  summarize(Amt = sum(measure_amt))

juiciest = top_n(howjuicy, 10)
unjuicy = top_n(howjuicy, -10)

#sort out the cocktail in JUICE that appear in our list of the juiciest cocktails
juicydf = semi_join(JUICE,juiciest)
#do the same for unjuicy cocktails
unjuicydf = semi_join(JUICE,unjuicy)

#spreading the ingredients column into each of their own columns - this helps us see how many juices are in this df
juicy2 = juicydf %>% 
  spread(ingredient,measure_amt, fill = 0)
unjuicy2 = unjuicydf %>% 
  spread(ingredient,measure_amt, fill = 0)
#so we're looking at 4 different juices for the unjuicy babies and 8 juices for the juicy babies


#plotting it out
#juicy cocktails
ggplot(juicydf, aes(x = name, y=measure_amt, fill=ingredient)) + 
  geom_col(aes(fill=ingredient)) +
  xlab("Cocktail") + 
  ylab("Volume of Juice - fl. oz.") + 
  ggtitle("The Juiciest Drinks") + 
  scale_fill_manual(values=c("red4","purple4","sandybrown","yellow1","olivedrab1","orange","tomato3","tan1")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#unjuicy cocktails
ggplot(unjuicydf, aes(x = name, y=measure_amt, fill=ingredient)) + 
  geom_col(aes(fill=ingredient)) +
  xlab("Cocktail") + 
  ylab("Volume of Juice - fl. oz.") + 
  ggtitle("The Least Juicy Drinks") + 
  scale_fill_manual(values=c("orangered4","greenyellow","yellow1","orange")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))




#Let's look at Juicy Gins ----------------------------
gins = JUICE %>% 
  filter(category == "Gin") 

#code for gins2 and gins3 isn't really necessary, but gins2 does help us see what juices we are working with
#spreading the ingredients column into each of their own columns
gins2 = gins %>% 
  spread(ingredient,measure_amt, fill = 0)

#getting sum total of juices
gins3 = gins2 %>% mutate(TAmt = rowSums(.[6:10]))

#plotting it out
ggplot(gins, aes(x = name, y=measure_amt, fill=ingredient)) + 
  geom_col(aes(fill=ingredient)) +
  xlab("Gin Cocktail") + 
  ylab("Volume of Juice - fl. oz.") + 
  ggtitle("Juicy Gin Drinks") + 
  scale_fill_manual(values=c("sandybrown","yellow1","olivedrab1","orange","yellow3")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.grid.major = element_blank (), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))


