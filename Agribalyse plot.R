#This code will create a vilion graph for Agribalyse data 

#1.0. Clear the environment
rm(list=ls())

#2.0. Set WD

setwd( "C:/Users/navoda/OneDrive - Deakin University/Planet A work/Codes/R files/Chart")

#3.0. Load the libraries
library(readxl)
library(dplyr)
library(ggplot2)

#4.0. Loading the database 
Agribalyse = read_xlsx ("Agribalyse_3.1_steps_modified.xlsx")

#5.0 Calculating the farm contribution 
Agribalyse$FGClimatechange = (Agribalyse$`Climate change Agriculture`/Agribalyse$`Climate change Total`)*100
#Agribalyse$FGClimatechange = Agribalyse$`Climate change Total`
Farmcontribution = data.frame(Agribalyse$`Food Category`, Agribalyse$FGClimatechange ,Agribalyse$`Colour group`)
Level_order = c('Vegetables','Fruits','Nuts, legumes and oilseeds', 'Fish and seafood products','Milk and dairy products','Eggs','Animal oils and fats','Meat and meat products','Flours and pie crusts','Breakfast cereals and cookies','Pasta, rice and cereals','Bread, cakes and pastries','Vegetable oils and fats','Culinary aids','Mixed dishes','Baby foods','Ice creams and sorbets','Sugar based confectionery','Dairy based confectionery','Non-alcoholic drinks','Alcoholic drink')

write.csv(Agribalyse, 'Agribalyse_percentage.csv')

#6.0. Ploting the violin graph
#6.1. plotting the data frame
ggplot(Farmcontribution, aes(x = factor(Agribalyse..Food.Category.,levels = Level_order))) +
  
#6.4. Add the points
geom_point(aes(y = Agribalyse.FGClimatechange, color = Agribalyse..Colour.group.),size = 2.0, alpha = 0.4) + 

#6.2. adding violin plot
#geom_violin(aes(y = Agribalyse.FGClimatechange), alpha = 0, draw_quantiles = c(.50)) +
  
#6.3. Add box plot
geom_boxplot(aes(y = Agribalyse.FGClimatechange),width = 0.5, fill = "white", outlier.color = "black", alpha = 0)+
  

facet_grid(~factor(Agribalyse..Colour.group., levels = c('Fresh vegetable and fruits', 'Animal-based foods','Processed foods','Confectionery','Beverages')),
             scales = "free_x", 
             space = "free_x",
             switch = "x")  +
  
#6.5. Changing the themes
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11))+
labs(x = " ", y = "%Farm level GHG emissions to total emissions", color = "")+ 
theme(panel.background = element_blank())+ 
theme(axis.line = element_line(color='black'))+ 
theme(legend.position = "none") +
theme(axis.text.x = element_text(size = 9))+
theme(axis.text.y = element_text(size = 9)) + 
theme(axis.title.y = element_text(size = 9))+
theme(axis.title.x = element_text(size = 9))+ 
theme(strip.text = element_text(size = 7.5), strip.background = element_blank(),strip.placement = "bottom")
  





