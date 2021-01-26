#Tidy Tuesday - 1/26/2021

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-01-26')
tuesdata <- tidytuesdayR::tt_load(2021, week = 5)

plastics <- tuesdata$plastics

# Or read in the data manually

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

# checking out what I'm working with --------------------------------------

unique(plastics$country)
unique(plastics$year)

#STEPS I NEED TO TAKE --------------------------------------

#1 - remove data from 2020
#2 - need to create separate dfs for each continent
#3 - for each continent group by country - take the sum of total number of plastics (so need to sum across columns and parent company)

#initial data cleaning ---------------------

library(tidyverse)

#selecting the columns of interest and only data from 2019
p1 = plastics %>% 
  select(country, year, grand_total) %>% 
  filter(year == 2019) 

#calculating the total amount of plastic from each country in 2019
p2 = p1 %>% 
  group_by(country) %>% 
  summarize(TOTALAmt = sum(grand_total))

#need to separate the countries into 6 dfs representing each continent

Americas = p2 %>% 
  filter(grepl("Argentina",country) | grepl("Brazil",country) | grepl("Colombia",country) | grepl("ECUADOR",country) | grepl("Canada",country) | grepl("Mexico",country) | grepl("United States of America",country))


Asia_Aus = p2 %>% 
  filter(grepl("Australia",country) | grepl("Bangladesh",country) | grepl("Bhutan",country) | grepl("China",country) | grepl("Hong Kong",country) | grepl("India",country) | grepl("Indonesia",country) | grepl("Japan",country) | grepl("Malaysia",country) | grepl("Maldives",country) | grepl("Philippines",country) | grepl("Sri Lanka",country) | grepl("Taiwan_ Republic of China (ROC)",country) | grepl("Thailand",country) | grepl("Turkey",country) | grepl("United Arab Emirates",country) | grepl("Vietnam",country))

Africa = p2 %>% 
  filter(grepl("Benin",country) | grepl("Burkina Faso",country) | grepl("Cameroon",country) | grepl("Cote D_ivoire",country) | grepl("Ghana",country) | grepl("Kenya",country) | grepl("NIGERIA",country) | grepl("Rwanda",country) | grepl("South Africa",country) | grepl("Tanzania",country) | grepl("Tunisia",country))

Europe = p2 %>% 
  filter(grepl("Bulgaria",country) | grepl("Cyprus",country) | grepl("France",country) | grepl("Germany",country) | grepl("Ireland",country) | grepl("Montenegro",country) | grepl("Latvia",country) | grepl("Luxembourg",country) | grepl("Italy",country) | grepl("Netherlands",country) | grepl("Portugal",country) | grepl("Slovenia",country) | grepl("Spain",country) | grepl("Switzerland",country) | grepl("Ukraine",country) | grepl("United Kingdom",country))

#correcting weird all caps country names
Americas[5,1] = "Ecuador"
Africa[7,1] = "Nigeria"

# palette play --------------------------------

devtools::install_github("ropenscilabs/ochRe")
# CRAN version
install.packages("ochRe")

library(ochRe)

?ochre_pal
#get the hexcodes of the colors in a palette (bc I can't figure out how to use the palette directly :( )
library(scales)
show_col(ochre_pal(palette = "emu_woman_paired")(20))

plasticcolors = c("#AB7E37","#7E2B2B","#5D2224","#3E3338","#7E6738","#2A3447","#6D272E","#A88882","#5A5755","#705A2C","#131114","#667783","#704245","#766B70","#9D8B61","#3A3A43","#A8A3A2")


#cool now it's time to play with graphs! ------------------------------

Amplot <- Americas %>%
  ggplot(aes(x=country, y=TOTALAmt, fill = country)) +
  geom_col() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  xlab("") +
  ylab("") +
  coord_polar(clip = "off") +
  labs(title = 'The Americas') +
  theme_void() +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 9),
        axis.text.y = element_blank(),
        panel.spacing = unit(4, "lines")) +
  scale_fill_manual(values = plasticcolors)

Amplot

Afplot <- Africa %>%
  ggplot(aes(x=country, y=TOTALAmt, fill = country)) +
  geom_col() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  xlab("") +
  ylab("") +
  coord_polar(clip = "off") +
  labs(title = 'Africa') +
  theme_void() +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 9),
        axis.text.y = element_blank(),
        panel.spacing = unit(4, "lines")) +
  scale_fill_manual(values = plasticcolors)

Afplot

AAplot <- Asia_Aus %>%
  ggplot(aes(x=country, y=TOTALAmt, fill = country)) +
  geom_col() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  xlab("") +
  ylab("") +
  coord_polar(clip = "off") +
  labs(title = 'Asia and Australia') +
  theme_void() +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 9),
        axis.text.y = element_blank(),
        panel.spacing = unit(4, "lines")) +
  scale_fill_manual(values = plasticcolors)

AAplot

Eurplot <- Europe %>%
  ggplot(aes(x=country, y=TOTALAmt, fill = country)) +
  geom_col() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  xlab("") +
  ylab("") +
  coord_polar(clip = "off") +
  labs(title = 'Europe') +
  theme_void() +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 9),
        axis.text.y = element_blank(),
        panel.spacing = unit(4, "lines")) +
  scale_fill_manual(values = plasticcolors)

Eurplot

#group them all together
library(gridExtra)
?grid.arrange
grid.arrange(grobs = list(Eurplot, Amplot, Afplot, AAplot), top = "Plastic Pollution by Continent", nrow = 2, ncol = 2)


#Future plot issues to work on - figure out how to deal with overlapping text and how to determine whether the different plots are on the same scale.









