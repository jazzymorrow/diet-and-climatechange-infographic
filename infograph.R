library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(plotly)
library(ggrepel)
#install.packages("readr")

##tidy tuesday data
food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')
##preliminary exploration 
View(food_consumption)
length(unique(food_consumption$food_category))
length(unique(food_consumption$country))

#####################################################
##              Data tidying                       ##
#####################################################
##renaming samples
food_consumption[food_consumption$food_category=="Wheat and Wheat Products",
                 "food_category"] <- "Wheat"
food_consumption[food_consumption$food_category=="Milk - inc. cheese",
                 "food_category"] <- "Milk & Cheese"
food_consumption[food_consumption$food_category=="Nuts inc. Peanut Butter",
                 "food_category"] <- "Nuts"

## add a animal product and  column 
food_consumption$food_type <- ifelse(food_consumption$food_category %in% 
                                       c("Pork","Poultry","Beef",
                                         "Lamb & Goat","Fish","Milk & Cheese"),
                                     "Animal product", "Plant-based")



#####################################################
##                   Plot 1                        ##
#####################################################

## boxplot of co2 per food type 
ggplot()+geom_boxplot(data = food_consumption,
                  aes(x = reorder(food_category,co2_emmission, median, na.rm=T), 
                      y = co2_emmission, 
                      fill = food_type, col = food_type)) + 
  theme_minimal()+
  scale_y_sqrt(breaks = c(100,500,1000,1500))+ 
  scale_fill_manual("Food type",values = c("Animal product" = "#fc8d59", 
                               "Plant-based" = "#91cf60"))+
  scale_color_manual("Food type",values = c("Animal product" = "#d73027", 
                                "Plant-based" = "#1a9850"))+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(x = "Food category", y = "CO2 emissions (kg/person/year)")

##################################################
##                   Plot 2                     ##
##################################################
## plot of percentage mean consumption versus co2 emmission

## percentage of animal product in diet per country
countries_consumption <- food_consumption %>%
  group_by(country, food_type) %>%
  summarise(total_consume = sum(consumption)) 

countries_consumption <- countries_consumption %>% 
  pivot_wider(names_from = food_type, values_from = total_consume) %>%
  mutate(percent_meat = (`Animal product`/(`Plant-based`+`Animal product`))*100)

##countries total emisions from diet 
total_co2 <- food_consumption %>%
  group_by(country) %>%
  summarise(total_co2 = sum(co2_emmission))

##combine co2 emmission with percentage meat
meat_vs_co2 <- countries_consumption %>%
  select(country, percent_meat) %>%
  left_join(total_co2)

## plot
ggplot() + geom_point(data = meat_vs_co2, aes(x = percent_meat, y = total_co2,
                                              colour = percent_meat),
                      show.legend = FALSE, cex = 2) +
  geom_smooth(data = meat_vs_co2, aes(x = percent_meat, y = total_co2), 
              method = "lm", colour = "black",fill = "#d73027", 
              lty = 2, se = FALSE) + 
  theme_minimal() + 
  labs(x = "Percentage of animal products in diet (%)", 
       y = "Total CO2 from diet (kg/person/year)") +
  scale_colour_gradient(low = "#1a9641", high = "#d7191c", 
                        breaks = c(0,48,52,100)) +
  geom_label_repel(data = meat_vs_co2[meat_vs_co2$country %in% 
                                        c("Australia","Liberia",
                                          "New Zealand","Argentina", 
                                          "Mozambique", "India", 
                                          "USA", "Finland", 
                                          "Uganda", "South Korea", 
                                          "Japan", "Bangladesh", "UK"),],
                   aes(label = country,x = percent_meat, 
                       y = total_co2, colour = percent_meat),
                   show.legend = FALSE,
                   size = 4,
                   nudge_y = 0.8, 
                   segment.size  = 0.3, 
                   label.padding = 0.1,
                   segment.color = "grey50", 
                   min.segment.length = 0)

##############################################
##          Interactive version of plot     ##
##############################################

countries <- ggplot() + geom_point(data = meat_vs_co2, 
                                   aes(x = percent_meat, y = total_co2,
                                       label = country), cex = 1 , pch = 20) +
  geom_smooth(data = meat_vs_co2, aes(x = percent_meat, y = total_co2), 
              method = "lm", colour = "#1a9850",
              fill = "#d73027", lty = 2, se = FALSE) + 
  theme_minimal() + labs(x = "Percentage of animal products in diet (%)", 
                         y = "Total CO2 from diet (kg/person/year)") +
  geom_text(data = meat_vs_co2[meat_vs_co2$country %in% 
                                 c("Australia","Liberia",
                                  "Argentina", "Mozambique"),],
            aes(label = country,x = percent_meat, y = total_co2, size = 0.5), 
            nudge_y = 0.75)


ggplotly(countries)

