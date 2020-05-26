#Reject code

colors <- c(
  "red4", "olivedrab1", "yellow1", 
  "orange", "yellow3", "red", 
  "darkred", "orangered4", "palevioletred4", 
  "peachpuff4", "purple4", "darkorange2", 
  "palegreen2", "sandybrown", "darkgoldenrod1", 
  "maroon4", "firebrick1", "tomato3", 
  "salmon", "tan1","greenyellow")
names(colors) = c(
  "Cranberry Juice", "Lime Juice", "Lemon Juice", 
  "Orange Juice", "Pineapple Juice", "Apple Juice", 
  "Black Cherry Juice", "Blood Orange Juice", "Passion Fruit Juice", 
  "Clam Juice", "Grape Juice", "Carrot Juice", 
  "Celery Juice", "Grapefruit Juice", "Mango Juice", 
  "Pomegranate Juice", "Raspberry Juice", "Tomato Juice", 
  "Tomato-Clam Juice", "V8 Cocktail Juice","Citrus Juice of Choice")

colScale <- scale_colour_manual(name = "ingredient",values = colors)
fillScale <- scale_fill_manual(name = "ingredient",values = colors)

#lvls  <- levels(JUICE$ingredient)
#cols  <- setNames(hcl(h=seq(15, 375, length=length(lvls)), l=65, c=100),lvls)
#cols[c("Cranberry Juice", "Lime Juice", "Lemon Juice", "Orange Juice", "Pineapple Juice", "Apple Juice", "Black Cherry Juice", "Blood Orange Juice", "Passion Fruit Juice", "Clam Juice", "Grape Juice", "Carrot Juice", "Celery Juice", "Grapefruit Juice", "Mango Juice", "Pomegranate Juice", "Raspberry Juice", "Tomato Juice", "Tomato-Clam Juice", "V8 Cocktail Juice","Citrus Juice of Choice")] <- c("red4", "olivedrab1", "yellow1", "orange", "yellow3", "red", "darkred", "orangered4", "palevioletred4", "peachpuff4", "purple4", "darkorange2", "palegreen2", "sandybrown", "darkgoldenrod1", "maroon4", "firebrick1", "tomato3", "salmon", "tan1","greenyellow")

#vars  <- levels(JUICE$ingredient)
#jcolors <- character(length(vars))
#jcolors[vars=="Cranberry Juice"] <- "red4"
#jcolors[vars=="Lime Juice"] <- "olivedrab1"
#jcolors[vars=="Lemon Juice"] <- "yellow1"
#jcolors[vars=="Orange Juice"] <- "orange"
#jcolors[vars=="Pineapple Juice"] <- "yellow3"
#jcolors[vars=="Apple Juice"] <- "red"
#jcolors[vars=="Black Cherry Juice"] <- "darkred"
#jcolors[vars=="Blood Orange Juice"] <- "orangered4"
#jcolors[vars=="Passion Fruit Juice"] <- "palevioletred4"
#jcolors[vars=="Clam Juice"] <- "peachpuff4"
#jcolors[vars=="Grape Juice"] <- "purple4"
#jcolors[vars=="Carrot Juice"] <- "darkorange2"
#jcolors[vars=="Celery Juice"] <- "palegreen2"
#jcolors[vars=="Grapefruit Juice"] <- "sandybrown"
#jcolors[vars=="Mango Juice"] <- "darkgoldenrod1"
#jcolors[vars=="Pomegranate Juice"] <- "maroon4"
#jcolors[vars=="Raspberry Juice"] <- "firebrick1"
#jcolors[vars=="Tomato Juice"] <- "tomato3"
#jcolors[vars=="Tomato-Clam Juice"] <- "salmon"
#jcolors[vars=="V8 Cocktail Juice"] <- "tan1"
#jcolors[vars=="Citrus Juice of Choice"] <- "greenyellow"