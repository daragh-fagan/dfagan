library(readr)
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_metadata_continents_ <- read_csv("unicef_metadata (continents).csv")


# Package installations
install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gapminder")
install.packages("scales")
install.packages("forecast")



#package libraries
library("tidyverse")
library(plotly)
library(dplyr)
library(ggplot2)
library(gapminder)
library(scales)
library(readr)
library(maps)


#Vis 1 

map_data_join <- full_join(unicef_indicator_1, map_world, by = c("country" = "region"))


ggplot(map_data_join) + 
  aes(x = long, y = lat, group = group, fill = obs_value) + 
  geom_polygon(color = "white", size = 0.1) +  
  scale_fill_gradient(low = "red", high = "green", name = "%", 
                      limits = c(min(map_data_join$obs_value, na.rm = TRUE), 
                                 max(map_data_join$obs_value, na.rm = TRUE)),
                      breaks = pretty_breaks(n = 5)) +  
  labs(
    title = "Average % exclusively breastfed for first 48 hours",
    caption = "Source: UNICEF"
  ) + 
  theme_void() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.3, face = "bold"),  
    plot.caption = element_text(size = 10, hjust = 0.5),  
    legend.position = "right",  
    legend.title.align = 0.5,  
    legend.text = element_text(size = 12),  
    plot.margin = margin(10, 10, 10, 10)  
  ) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))  








#Vis 2 

country_averages <- data_join %>%
  group_by(country, Continent) %>%
  summarise(AvgLifeExpectancy = round(mean(`Life expectancy at birth, total (years)`, na.rm = TRUE), 2),
            AvgBreastfeedingRate = round(mean(obs_value, na.rm = TRUE), 2)) %>%
  ungroup()

scatter_plot <- ggplot(country_averages, aes(x = AvgLifeExpectancy, y = AvgBreastfeedingRate, 
                                             text = paste("Country: ", country, "<br>",
                                                          "Avg. Life Expectancy: ", AvgLifeExpectancy, " years<br>",
                                                          "Avg. Breastfeeding Rate: ", AvgBreastfeedingRate, "%"), 
                                             color = Continent)) +
  geom_point(alpha = 0.7, size = 2.5) +
  scale_color_manual(values = c("Africa" = "orange", "Asia" = "red", 
                                "Europe" = "blue", "North America" = "hotpink", 
                                "Oceania" = "lightgreen", "South America" = "purple")) +
  geom_hline(yintercept = mean(country_averages$AvgBreastfeedingRate, na.rm = TRUE), linetype = "dashed") +
  geom_vline(xintercept = mean(country_averages$AvgLifeExpectancy, na.rm = TRUE), linetype = "dashed") +
  labs(title = "Relationship between Life Expectancy and Breastfeeding",
       x = "Avg. Life expectancy at birth, total (years)", 
       y = "Avg. % exclusively breastfed for first 48 hours",
       color = "Continent") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text( hjust = 0.6, size = 16, face = "bold"))


plotly_scatter_plot <- ggplotly(scatter_plot, tooltip = "text")

plotly_scatter_plot

#Vis 3 

avg_breastfeeding_by_continent <- data_join %>%
  filter(!is.na(Continent) & Continent != "NA") %>%
  group_by(Continent) %>%
  summarise(AverageBreastfeedingRate = mean(obs_value, na.rm = TRUE)) %>%
  mutate(AverageBreastfeedingRate = round(AverageBreastfeedingRate, 2)) %>%
  ungroup()

continent_colors <- c("Africa" = "orange", "Asia" = "red", 
                      "Europe" = "blue", "North America" = "hotpink", 
                      "Oceania" = "lightgreen", "South America" = "purple")

ggplot_bar_chart <- ggplot(avg_breastfeeding_by_continent, aes(x = Continent, y = AverageBreastfeedingRate, fill = Continent)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) + 
  scale_fill_manual(values = continent_colors) + 
  labs(title = "Average Breastfeeding Rate by Continent",
       x = "Continent",
       y = "Average Breastfeeding Rate (%)") +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "grey20", size = 12), 
    axis.text.y = element_text(color = "grey20", size = 12), 
    plot.title = element_text(size = 16, face = "bold", hjust = 0.6), 
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)), 
    plot.caption = element_text(size = 8, margin = margin(t = 10)), 
    plot.background = element_rect(fill = "white", color = NA), 
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(color = "#e1e1e1"), 
    plot.margin = unit(c(1, 1, 1, 1), "cm") 
  )

plotly_bar_chart <- ggplotly(ggplot_bar_chart, tooltip = c("x", "y"))

plotly_bar_chart


#Vis 4 

avg_bf_rates <- data_join %>%
  group_by(country) %>%
  summarise(AverageBreastfeeding = mean(obs_value, na.rm = TRUE)) %>%
  ungroup()

lowest_bf_countries <- avg_bf_rates %>%
  arrange(AverageBreastfeeding) %>%
  top_n(-6, AverageBreastfeeding)

filtered_gdp_data <- data_join %>%
  filter(country %in% lowest_bf_countries$country)


TimeSeries <- ggplot(filtered_gdp_data, 
                     aes(x = year, 
                         y = `GDP per capita (constant 2015 US$)`, 
                         group = country, 
                         color = country)) +
  geom_line(size = 0.5) +  
  geom_point(size = 0.7) +  
  labs(title = "GDP per Capita Over Time", 
       subtitle = "Countries with the lowest breastfeeding rates",
       x = "Year", 
       y = "GDP per Capita (constant 2015 US$)") +
  theme_minimal(base_size = 14) +  
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"), 
    plot.subtitle = element_text(size = 12, face = "bold"), 
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12), 
    axis.text.x = element_text(angle = 45, vjust = 0.5), 
    panel.grid.major = element_line(color = "#cbcbcb"), 
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA) 
  ) +
  scale_x_continuous(breaks = seq(min(filtered_gdp_data$year), max(filtered_gdp_data$year), by = 10)) +
  scale_y_continuous(labels = label_comma(accuracy = 0.01)) +
  scale_color_brewer(palette = "Set2")  


interactive_TimeSeries <- ggplotly(TimeSeries, tooltip = c("x", "y", "color"))


interactive_TimeSeries




TimeSeries <- ggplot(filtered_gdp_data, 
                     aes(x = year, 
                         y = `GDP per capita (constant 2015 US$)`, 
                         group = country, 
                         color = country)) +
  geom_line(size = 0.5) +  
  geom_point(size = 0.7) +  
  labs(title = "GDP per Capita Over Time", 
       subtitle = "Countries with the lowest breastfeeding rates",
       x = "Year", 
       y = "GDP per Capita (constant 2015 US$)",
       color = "Country") +  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    panel.grid.major = element_line(color = "#cbcbcb"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "right",  
    legend.title.align = 0.5,
    legend.text = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = seq(min(filtered_gdp_data$year), max(filtered_gdp_data$year), by = 10)) +
  scale_y_continuous(labels = label_comma(accuracy = 0.01)) +
  scale_color_brewer(palette = "Set2")  

interactive_TimeSeries <- ggplotly(TimeSeries, tooltip = c("x", "y", "color"))

interactive_TimeSeries






