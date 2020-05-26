#Tidy Tuesday - 5.25.2020

rm(list=ls())
library(tidyverse)

# Get the Data

boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

#checking to see if there are multiple versions of the same ingredient
bcingreds <- boston_cocktails %>% 
  group_by(ingredient) %>% 
  tally() 

#renaming some duplicates
which(boston_cocktails=="bitters", arr.ind=TRUE)
boston_cocktails[3010,5] = "Bitters"

#re-running bcingreds to see if "bitters" still exists
bcingreds <- boston_cocktails %>% 
  group_by(ingredient) %>% 
  tally() 

#Yay it worked!

#Alright, now let's define our goal:
#I want to see how much juice is commonly used in cocktails. So first, I'm going to filter out the juices and clean up the duplicate names in the data set. Then I'm going to try and make a plot examining how much juice is used in cocktails. Hmm maybe determine what is the juciest and least juciest cocktail?

#First, let's try to filter out juices
juices <- boston_cocktails %>% 
  filter(grepl("juice",ingredient) | grepl("Juice",ingredient))
#dope that took a couple of tries but it worked! Let's see how many types of juice we are working with
#checking to see if there are multiple versions of the same ingredient
jtypes <- juices %>% 
  group_by(ingredient) %>% 
  tally() 

#Okay, so it looks like there are several cap/no cap issues to fix (check stri_trans_totitle), but first it looks like there are several rows with multiple ingredients listed, so we have to deal with that first
?separate_rows
juices2 = juices %>% 
  separate_rows(ingredient, sep=",")

#Yay! That worked! Okay, so now we run that chunk again also separating out ingredients joined by "and" (but not "or" - we'll get to that later), and then again filter out non-juice ingredients

juices2 = juices %>% 
  separate_rows(ingredient, sep=",") %>% 
  separate_rows(ingredient, sep="and")

juices3 <- juices2 %>% 
  filter(grepl("juice",ingredient) | grepl("Juice",ingredient))

#Alright, now we will start to consolidate duplicate ingredients. Let's look at what what we have first:
jtypes2 <- juices3 %>% 
  group_by(ingredient) %>% 
  tally() 

#cool, we are only looking at 1 ingredient per entry (with exception of "or" but we'll get to that later)
#Let's see if stri_trans_totitle will help us out
?stri_trans_totitle
library(stringi)
juices3$ingredient = stri_trans_totitle(juices3$ingredient)
#let's try to group this again and see if it's any better
jtypes3 <- juices3 %>% 
  group_by(ingredient) %>% 
  tally() 

#Dope! That took care of the caps/no caps problem. But now we have an issue with the fact that "Juice of Orange" is different from "Orange Juice"
#Let's see if we can find all the locations for "Juice of Orange"
index = which(juices3=="Juice Of Orange", arr.ind=TRUE)
#Can we rename all of them? Let's try:
juices3[index] = "Orange Juice"
#check the list to see if it worked
jtypes4 <- juices3 %>% 
  group_by(ingredient) %>% 
  tally() 

#so that worked - Juice of Orange is gone, but now there are 2 "Orange Juice" options? Does one of them have a space after? 
index2 = which(juices3=="Orange Juice ", arr.ind=TRUE)
#Nope, that's not the issue. I'm not sure why there are two entries, but looking back at jtypes3, that issue is there as well.

#Maybe it would just be best to separate all the fruit types out now and make separate data frames with different juice types.

#But before we make all the different dfs, it's time to deal with the "or" issue. At this point, I'm just going to deeme "or" entries as "Choice Citrus", since most seem to be a choice between lemon or lime, etc.
index2 = which(juices3=="Fresh Lemon Or Lime Juice", arr.ind=TRUE)
index3 = which(juices3=="Fresh White Or Ruby Red Grapefruit Juice", arr.ind=TRUE)
index4 = which(juices3=="Juice Of Lemon Or Lime", arr.ind=TRUE)
index5 = which(juices3=="Lemon Or Lime Juice", arr.ind=TRUE)
index6 = which(juices3=="Pineapple Or Grapefruit Juice", arr.ind=TRUE)
juices3[index2] = "Citrus Juice of Choice"
juices3[index3] = "Citrus Juice of Choice"
juices3[index4] = "Citrus Juice of Choice"
juices3[index5] = "Citrus Juice of Choice"
juices3[index6] = "Citrus Juice of Choice"
#Check that there are no more "or" entries
jtypes5 <- juices3 %>% 
  group_by(ingredient) %>% 
  tally() 
#Yep! All Good! Also, I feel like there's a way to turn the above 10 lines into a function or a loop somehow, I just got too lazy to do so.
#But I am going to try and use a "for" function for the next bit

#make a list of all the general juice categories we want to look at:
jlist = c("Cranberry", "Lime", "Lemon", "Orange", "Pineapple", "Apple", "Black Cherry", "Blood Orange", "Passion Fruit", "Clam", "Grape", "Carrot", "Celery", "Grapefruit", "Mango", "Pomegranate", "Raspberry", "Tomato", "Tomato-Clam", "V8","Citrus Juice of Choice")

#set up a for loop to create 21 separate dfs for each category.

for (i in jlist) {
  assign(i, juices3 %>% 
           filter(grepl(i,ingredient)))
}
#YASSSSSSS SO MUCH WORK SAVED

lst <- setNames(vector("list", length(jlist)), jlist)
for (i in jlist) {
  lst[[i]] <- juices3 %>% 
    filter(grepl(i,ingredient))
}

#Oof but there are a few dfs with spaces in the name which will be difficult to deal with so gotta rename
BlackCherry=`Black Cherry`
BloodOrange=`Blood Orange`
CitrusJuiceofChoice=`Citrus Juice of Choice`
PassionFruit=`Passion Fruit`
TomatoClam=`Tomato-Clam`

#NOW - the next step is to go through each of these 21 dfs and a) make sure no stray juices got in and b) make sure all the ingredient names are standardized.
#Maybe bind_rows can do this for us really easily?
?bind_rows
Cleanish = bind_rows(lst, .id = NULL)
#nah that didn't work. Let's do this manually

#1 - Apple Juice
Apple$ingredient
Apple$ingredient[] = "Apple Juice"

#2 - Black Cherry Juice
BlackCherry$ingredient
BlackCherry$ingredient[] = "Black Cherry Juice"

#3 - Blood Orange Juice
BloodOrange$ingredient
BloodOrange$ingredient[] = "Blood Orange Juice"

#4 - Carrot Juice
Carrot$ingredient
Carrot$ingredient[] = "Carrot Juice"

#5 - Celery Juice
Celery$ingredient
Celery$ingredient[] = "Celery Juice"

#6 - Citrus Juice of Choice
CitrusJuiceofChoice$ingredient
CitrusJuiceofChoice$ingredient[] = "Citrus Juice of Choice"

#7 - Clam Juice
Clam$ingredient
Clam2 = Clam %>% filter(ingredient!="Tomato-Clam Juice")
Clam2$ingredient

#8 - Cranberry Juice
Cranberry$ingredient
Cranberry$ingredient[] = "Cranberry Juice"
Cranberry$ingredient

#9 - Grape Juice
Grape$ingredient
Grape2 = Grape %>% filter(!grepl("Grapefruit",ingredient))
Grape2$ingredient
Grape2$ingredient[] = "Grape Juice"
Grape2$ingredient

#10 - Grapefruit Juice
Grapefruit$ingredient
Grapefruit$ingredient[] = "Grapefruit Juice"
Grapefruit$ingredient

#11 - Lemon Juice
Lemon$ingredient
Lemon$ingredient[] = "Lemon Juice"
Lemon$ingredient

#12 - Lime Juice
Lime$ingredient
Lime$ingredient[] = "Lime Juice"
Lime$ingredient

#13 - Mango Juice
Mango$ingredient
Mango$ingredient[] = "Mango Juice"

#14 - Orange Juice
Orange$ingredient
Orange2 = Orange %>% filter(!grepl("Blood",ingredient))
Orange2$ingredient
Orange2$ingredient[] = "Orange Juice"
Orange2$ingredient

#15 - Passion Fruit Juice
PassionFruit$ingredient
PassionFruit$ingredient[] = "Passion Fruit Juice"
PassionFruit$ingredient

#16 - Pineapple Juice
Pineapple$ingredient
Pineapple$ingredient[] = "Pineapple Juice"
Pineapple$ingredient

#17 - Pomegranate Juice
Pomegranate$ingredient
Pomegranate$ingredient[] = "Pomegranate Juice"

#18 - Raspberry Juice
Raspberry$ingredient
Raspberry$ingredient[] = "Raspberry Juice"

#19 - Tomato Juice
Tomato$ingredient
Tomato2 = Tomato %>% filter(ingredient!="Tomato-Clam Juice")
Tomato2$ingredient
Tomato2$ingredient[] = "Tomato Juice"
Tomato2$ingredient

#20 - Tomato-Clam Juice
TomatoClam$ingredient
TomatoClam$ingredient[] = "Tomato-Clam Juice"

#21 - V8 Juice
V8$ingredient
V8$ingredient[] = "V8 Cocktail Juice"


#now that's over, we can combine all the dfs back into a single clean df
Cleanse = bind_rows(Cranberry, Lime, Lemon, Orange2, Pineapple, Apple, BlackCherry, BloodOrange, PassionFruit, Clam2, Grape2, Carrot, Celery, Grapefruit, Mango, Pomegranate, Raspberry, Tomato2, TomatoClam, V8,CitrusJuiceofChoice)

#I now see where i effed up with the bind rows short cut - let's try again with the df lst we built
C2 = bind_rows(lst, .id="Juice")
#so that does work, but it doesn't account for accidentially duplicated entries (i.e. "Blood Orange" getting sorted into Orange).
#So we'll stick with using Cleanse from here on out, but it's nice to know that trick is available

#checking to make sure there are only 21 juice types listed in Cleanse
jtypes6 = Cleanse %>% 
  group_by(ingredient) %>% 
  tally() 

#Perfect!
#Last step in data cleaning: separting the last row into amount and measurement (Just to confirm everything is measured in oz). But first let's find some weird spots
jtypes7 = Cleanse %>% 
  group_by(measure) %>% 
  tally() 
#yep, got a couple strange buddies. But there are only 6 of them, so we'll just mess with them manually and then split the column. Basic

#Let's see if we can find all the locations for "For glass"
indexA = which(Cleanse=="For glass", arr.ind=TRUE)
indexA
#it seems like this is just a rinse of the glass in lime juice, so we're just going to delete this row. 
Cleanse = Cleanse[-c(179),]
jtypes7 = Cleanse %>% 
  group_by(measure) %>% 
  tally() 
#yay now it's gone.

#we're gonna rename the "1/2 or 1" value as "3/4 oz". Why not average it out right?
which(Cleanse=="1/2 or 1", arr.ind=TRUE)
Cleanse[670,6] = "3/4 oz"
jtypes7 = Cleanse %>% 
  group_by(measure) %>% 
  tally() 
#and, dealt with!

#Now we are going to convert the fraction entries to decimal places
indexB = which(Cleanse=="1 1/2 oz", arr.ind=TRUE)
Cleanse[indexB] = "1.5 oz"

indexC = which(Cleanse=="1/2 oz", arr.ind=TRUE)
Cleanse[indexC] = "0.5 oz"

indexD = which(Cleanse=="1/4 oz", arr.ind=TRUE)
Cleanse[indexD] = "0.25 oz"

indexE = which(Cleanse=="3/4 oz", arr.ind=TRUE)
Cleanse[indexE] = "0.75 oz"

#Recheck the list to make sure all our edits are good
jtypes8 = Cleanse %>% 
  group_by(measure) %>% 
  tally() 
#Majestic!

#And now for the final step - splitting the "measure" column and converting the number to a number
?separate
Cleanse2 = separate(Cleanse, measure, into = c("measure_amt", "measure_unit"), sep=" ")
Cleanse2$measure_amt = as.numeric(Cleanse2$measure_amt)

#WHOOHOO! Now we're gonna export this as a csv so  that we can start a new script focusing on plotting
write_csv(Cleanse2, "Desktop/TidyTuesday/25may2020COCKTAILS/JUICE.csv")
