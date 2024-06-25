#This code will generate charts using FAO data

#1. Clear the environment
rm(list=ls())

#2.0. Set WD

setwd( "C:/Users/navoda/OneDrive - Deakin University/Planet A work/Codes/R files/FAO")

#3.0. Load the libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

#4.0. Loading the database 
FAO = read_xlsx("FAOSTAT_data_world.xlsx")

#5.0. Seperating only the Agrifood systems 
FAO_Agri = subset(FAO, FAO$Group == "Farm Gate"| FAO$Group == "Waste"| FAO$Group == "Processing"| FAO$Group == "Retail"| FAO$Group == "Consumption"| FAO$Group == "Transport"| FAO$Group == "Fertilizers"| FAO$Group == "Landuse")
FAO_GHGe = subset(FAO_Agri, FAO_Agri$Element == "Emissions (CO2eq) (AR5)")

Level_order = c('Farm Gate','Fertilizers','Processing', 'Retail', 'Transport', 'Consumption', 'Waste','Landuse')

#6.0. Calculating percentage contribution 
FAO_data_group = FAO_GHGe %>%
  group_by(Year,Group) %>%
  summarise(n = sum(Value), .groups = 'drop') %>% 
  mutate(Total = (n /1000000))%>% 
  mutate(Group = factor(Group, levels = c('Farm Gate','Fertilizers','Processing', 'Retail', 'Transport', 'Consumption', 'Waste','Landuse')))
 
#7.0. Area graph 
A = 
  ggplot(FAO_data_group, aes(x=Year, y=Total, fill=Group)) + 
  geom_area(alpha = 0.7, linewidth = 0.2, colour = 'black') +
  theme_classic() +
  scale_y_continuous(expand = c(0,0))+ 
  scale_x_continuous(breaks = seq(1990,2021,5), expand = c(0,0))+
  labs(y = "GHGe (Gt-CO2eq)", x = " ", fill = "")+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.title.x = element_text(size = 14))+
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Dark2")
  

#8.0. Bar graph by stages - 2021 
#8.1. Seperating year 2021 and calcuate the values by groups
FAO_2021 = subset(FAO_Agri, FAO_Agri$Year == "2021")
FAO_2021 = subset(FAO_2021, FAO_2021$Broad_group == "Farm Gate"| FAO_2021$Broad_group == "Pre and post farm")

FAO_2021_group = FAO_2021 %>%
  group_by(Group,Broad_group) %>%
  summarise(n = sum(Value)) %>% 
  mutate(Total = (n /1000000)) %>%
  mutate(Group = factor(Group, levels = c('Farm Gate','Fertilizers','Processing', 'Retail', 'Transport', 'Consumption', 'Waste')))

#8.2. Plotting bar graph for stages 
B1 = 
  ggplot(data = FAO_2021_group, aes(x = Group , y = Total))+ 
  geom_bar(stat="identity",fill = 'darkgrey')+ 
  theme_classic()+ 
  labs(y = "GHGe (Gt-CO2eq)", x = "")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.title.x = element_text(size = 14))+
  scale_y_continuous(expand = c(0,0))
  
#8.3. Staked bar graph for stages 
B2 = 
  ggplot()+
  geom_bar(data = FAO_2021_group, aes(x = Broad_group, y = Total , fill = Group), stat = 'identity', width = 0.4)+
  theme_classic()+ 
  labs(y = "GHGe (Gt-CO2eq)", x = " ", fill = "")+
  scale_y_continuous(expand = c(0,0))+ 
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.title.x = element_text(size = 14))+ 
  theme(legend.position = "none", legend.box = "vertical")+
  theme(legend.text=element_text(size = 14))+
  scale_fill_brewer(palette = "Dark2")
  

#9.0. Stacked bar graph by gases - 2021 
#9.1. Calculations for the graphs - Back calculation
FAO_data_2021_gases = FAO_2021 %>%
  group_by(Group, Element) %>%
  summarise(n = sum(Value))%>%
  mutate(percentage = (n / sum(n)*100))

#9.2. Loading the FAO gas percentages 
FAO_gases = read_xlsx("Updated_FAO_gases_2021_30042024_2.xlsx")
FAO_gases$n = FAO_gases$n/1000000
FAO_gases = subset(FAO_gases, FAO_gases$Group == "Consumption"| FAO_gases$Group == "Farm Gate"| FAO_gases$Group == "Fertilizers"| FAO_gases$Group == "Processing"| FAO_gases$Group == "Retail"| FAO_gases$Group == "Transport"| FAO_gases$Group == "Waste")

#9.3. Plotting stacked bar graph for gases
C = 
  ggplot()+
  geom_bar(data = FAO_gases, aes(x = factor(Group,levels = Level_order), y = n , fill = Element), stat = 'identity', width = 0.5)+
  theme_classic()+ 
  labs(y = "GHGe (Gt-CO2eq)", x = " ", fill = "")+
  scale_y_continuous(expand = c(0,0))+ 
  theme(plot.margin = unit(c(0.5, 0, 0, 0.1), 
                           "inches"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 14))+
  theme(axis.text.y = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.title.x = element_text(size = 14))+ 
  theme(legend.position = "none", legend.box = "vertical")+
  theme(legend.text=element_text(size = 14))+
  guides(fill =guide_legend(nrow=3, byrow=TRUE)) +
  scale_fill_brewer(palette = "Set1")
  
#10.0. Contributions by country - 2021
#10.1. Loading the country data set 
FAO_country = read_xlsx("FAOSTAT_data_country.xlsx")

#10.2. Separating the countries 
FAO_country_specific = subset(FAO_country, FAO_country$`Area Code (M49)` == 32| FAO_country$`Area Code (M49)` == 36| FAO_country$`Area Code (M49)` == 159 | FAO_country$`Area Code (M49)` == 818| FAO_country$`Area Code (M49)` == 250 | FAO_country$`Area Code (M49)` == 356| FAO_country$`Area Code (M49)` == 826 | FAO_country$`Area Code (M49)`== 840)

#10.3. Seperating out only relevent agri food total emission
FAO_country_agri = subset(FAO_country_specific, FAO_country_specific$Group == "Farm Gate"| FAO_country_specific$Group == "Waste"| FAO_country_specific$Group == "Processing"| FAO_country_specific$Group == "Retail"| FAO_country_specific$Group == "Consumption"| FAO_country_specific$Group == "Transport"|FAO_country_specific$Group == "Fertilizers")
FAO_country_GHGe = subset(FAO_country_agri, FAO_country_agri$Element == "Emissions (CO2eq) (AR5)")


#10.4. Calculating percentage contribution 
FAO_country_percent = FAO_country_GHGe %>%
  group_by(Area,Group) %>%
  summarise(n = sum(Value)) %>%
  mutate(percentage = (n / sum(n)*100)) %>%
  mutate(Group = factor(Group, levels = c('Farm Gate','Fertilizers','Processing', 'Retail', 'Transport', 'Consumption', 'Waste','Landuse')))

#10.5. Plotting stacked bar graph by process and country
D = 
  ggplot()+
  geom_bar(data = FAO_country_percent, aes(x = Area, y = percentage , fill = Group), stat = 'identity', width = 0.5)+
  theme_classic()+ 
  labs(y = "Share of emissions", x = " ", fill = "")+
  theme(plot.margin = unit(c(0.3, 0.1, 0.2, 0.1), 
                           "inches"))+
  scale_y_continuous(expand = c(0,0))+ 
  theme(axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust=1))+
  theme(axis.text.y = element_text(size = 14)) + 
  theme(axis.title.y = element_text(size = 14))+
  theme(axis.title.x = element_text(size = 14))+ 
  theme(legend.position = "none", legend.box = "vertical")+
  theme(legend.text=element_text(size = 14))+
  guides(fill =guide_legend(nrow=3, byrow=TRUE)) +
  scale_fill_brewer(palette = "Dark2")
  

#11. Arranging 4 plots in a grid 
#plot_grid(A,B2,C,D, labels = c("A","B","C","D"))
Bottom = plot_grid(C, D, labels = c("C", "D"),ncol = 2, nrow = 1)
Top = plot_grid(A, B2,NULL, labels = c("A","B",""),ncol = 3, rel_widths = c(3,2,1),align = "v")
Combined = plot_grid(Top, Bottom, nrow = 2)

#12. Adding the legends to the plots 
#12.1. Plotting the graphs with plot 
A1 = 
  ggplot(FAO_data_group, aes(x=Year, y=Total, fill=Group)) + 
  geom_area(alpha = 0.7, linewidth = 0.2, colour = 'black') +
  labs(y = "GHGe (Gt-CO2eq)", x = " ", fill = "")+
  theme_classic() +
  theme(legend.position = "bottom")+
  theme(legend.text=element_text(size = 14))+
  guides(fill =guide_legend(nrow=3, byrow=TRUE)) +
  scale_fill_brewer(palette = "Dark2")

C1 = 
  ggplot()+
  geom_bar(data = FAO_gases, aes(x = factor(Group,levels = Level_order), y = n , fill = Element), stat = 'identity', width = 0.5)+
  theme_classic()+ 
  labs(y = "GHGe (Gt-CO2eq)", x = " ", fill = "")+
  theme(legend.position = "bottom", legend.box = "vertical")+
  theme(legend.text=element_text(size = 14))+
  guides(fill =guide_legend(nrow=3, byrow=TRUE)) +
  scale_fill_brewer(palette = "Set1")

#12.2. Extracting the legend 
get_only_legend = function(plot) { 
  plot_table = ggplot_gtable(ggplot_build(plot)) 
  legend_plot = which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend = plot_table$grobs[[legend_plot]] 
  return(legend) 
} 

# extract legend from plot1 using above function 
Legend_A1 = get_only_legend(A1) 
Legend_C1 = get_only_legend(C1) 

#13. Adding the legends of the images
Legends = plot_grid(Legend_C1, Legend_A1,ncol = 2, nrow = 1)
plot_grid(Combined,Legends, nrow = 2, rel_heights = c(6,1))

