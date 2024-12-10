#Install relevant packages
install.packages("readxl")
install.packages("here")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("countrycode", repos = "https://cloud.r-project.org/")
install.packages("gganimate")
install.packages("gifski")


#Load necessary libraries
library(readxl)
library(here)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(countrycode)
library(gganimate)
library(gifski)

#Load data into R 
data <- read_xlsx(here("data", "WorldLifeExp.xlsx"))

#Checking the first 6 rows of my data
head(data)

#Removing unneccessary columns
data <- data %>%
  select(-`Country Code`, -`Series Code`, -`2023 [YR2023]`)  

#Removing rows with no value assigned
data <- data %>%
  filter(!if_any(everything(), ~ . == ".."))

#Renaming columns so it is easier to read
data <- data %>%
  rename(`1990` = `1990 [YR1990]`, `1991` = `1991 [YR1991]`, `1992` = `1992 [YR1992]`, `1993` = `1993 [YR1993]`, `1994` = `1994 [YR1994]`, `1995` = `1995 [YR1995]`, `1996` = `1996 [YR1996]`, `1997` = `1997 [YR1997]`, `1998` = `1998 [YR1998]`, `1999` = `1999 [YR1999]`, `2000` = `2000 [YR2000]`, `2001` = `2001 [YR2001]`, `2002` = `2002 [YR2002]`, `2003` = `2003 [YR2003]`, `2004` = `2004 [YR2004]`, `2005` = `2005 [YR2005]`, `2006` = `2006 [YR2006]`, `2007` = `2007 [YR2007]`, `2008` = `2008 [YR2008]`, `2009` = `2009 [YR2009]`, `2010` = `2010 [YR2010]`, `2011` = `2011 [YR2011]`, `2012` = `2012 [YR2012]`, `2013` = `2013 [YR2013]`, `2014` = `2014 [YR2014]`, `2015` = `2015 [YR2015]`, `2016` = `2016 [YR2016]`, `2017` = `2017 [YR2017]`, `2018` = `2018 [YR2018]`, `2019` = `2019 [YR2019]`, `2020` = `2020 [YR2020]`, `2021` = `2021 [YR2021]`, `2022` = `2022 [YR2022]`)

data <- data %>%
  rename(`Population Group` = `Series Name`)

#Converting relevant columns to numeric
data <- data %>%
  mutate(across(c(`1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, 
                  `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, 
                  `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, 
                  `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`), 
                ~ as.numeric(as.character(.))))

#Rounding up life expectancy values to two decimal points
data <- data %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

#Renaming variables so it is easier to read 
data <- data %>%
  mutate(`Population Group` = recode(
    `Population Group`,
    `Life expectancy at birth, total (years)` = "Total",
    `Life expectancy at birth, male (years)` = "Male",
    `Life expectancy at birth, female (years)` = "Female"
  ))

#Correcting the way some countries were typed so it matches data in countrycode package
manual_corrections <- data.frame(
  Country = c("Bahamas, The", "Congo, Rep.", "Congo, Dem. Rep.", "Egypt, Arab Rep.", "Gambia, The.", "Hong Kong SAR, China", "Korea, Rep.", "Iran, Islamic Rep.", "Korea, Dem. People's Rep.", "Lao PDR", "Kyrgyz Republic", "Micronesia, Fed. Sts.", "Russian Federation", "Syrian Arab Republic", "Venezuela, RB", "Yemen, Rep.", "Viet Nam"),
  Corrected = c("Bahamas", "Republic of the Congo", "Democratic Republic of the Congo", "Egypt", "Gambia", "Hong Kong", "South Korea", "Iran", "North Korea", "Laos", "Kyrgyzstan", "Micronesia (Federated States of)", "Russia", "Syria", "Venezuela", "Yemen", "Vietnam")
)

data <- data %>%
  left_join(manual_corrections, by = c("Country Name" = "Country")) %>%
  mutate(`Country Name` = if_else(is.na(Corrected), `Country Name`, Corrected)) %>%
  select(-Corrected)

#Using the countrycode package and creating a new column for continent and sorting countries under it
data <- data %>%
  mutate(Continent = countrycode(`Country Name`, origin = "country.name", destination = "continent"))

#Manually assigning Kosovo to Europe as countrycode couldn't identify its continent
data <- data %>%
  mutate(Continent = if_else(`Country Name` == "Kosovo", "Europe", Continent))

# Renaming "Americas" to "North & South America" in the Continent column
data <- data %>%
  mutate(Continent = if_else(Continent == "Americas", "North & South America", Continent))

#Moving the Continent column next to the Country Name column for better readability
data <- data %>%
  select(1, Continent, everything())

#Saving Cleaned Data
write.csv(data, here("data", "cleaneddata.csv"), row.names = FALSE)

#Isolating the total life expectancy rows so I can compare total life expectancy across continents 
total_data <- data %>%
  filter(`Population Group` == "Total")

#Reshaping my data into long format 
long_data <- total_data %>%
  pivot_longer(
    cols = `1990`:`2022`,  #Columns for each year
    names_to = "Year",     #Name for the new column with years
    values_to = "Life Expectancy"  #Name for the new column with values
  ) %>%
  mutate(Year = as.numeric(Year))  #Convert year to numeric

#Aggregate the data to calculate the total average life expectancy for each continent and year
continent_data <- long_data %>%
  group_by(Continent, Year) %>%
  summarise(`Average_Life_Expectancy` = mean(`Life Expectancy`, na.rm = TRUE), .groups = "drop")

str(continent_data$Year) #Ensuring year column is properly structured

range(continent_data$Year, na.rm = TRUE)  #Check range

continent_data <- continent_data %>% filter(!is.na(Year) & Year != Inf) #Removes rows with invalid or missing year values

#Saving Final Cleaned Data
write.csv(continent_data, here("data", "finalcleaneddata.csv"), row.names = FALSE)

#Plotting the graph
p <- ggplot(continent_data, aes(x = Year, y = Average_Life_Expectancy, color = Continent, group = Continent)) +
  geom_line(linewidth = 1.2, alpha = 0.7) +  #Lines for each continent
  labs(title = "Life Expectancy Over Time by Continent",
       x = "Year",
       y = "Average Life Expectancy") 

#Saving the graph into figs folder
ggsave("figs/initialgraph.png", plot = p, width = 8, height = 6, dpi = 300)

#Setting a theme for the graph
graph_theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  #Main title in bold and centralised
  plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),  #Subtitle in italic and centralised
  axis.text.x = element_text(angle = 45, hjust = 1),  #Rotate x-axis labels
  legend.title = element_text(face = "bold", size = 12),  #Legend title in bold
  legend.text = element_text(size = 12)  #Legend text size
)

#Colour coding each continent according to the Olympics
continent_colours <- c(
  "Europe" = "#0066B3", 
  "Asia" = "#FFCC00",   
  "Africa" = "#000000",
  "Oceania" = "#009639",  
  "North & South America" = "#F44336" 
)

#Plotting a better graph with the changes
p2 <- ggplot(continent_data, aes(x = Year, y = Average_Life_Expectancy, color = Continent, group = Continent)) +
  geom_line() +  
  geom_point(size = 2) +  #Points for each year for each continent
  labs(
    title = "Life Expectancy Over Time by Continent",
    subtitle = "Year: 1990 to 2022",
    x = "Year",
    y = "Average Life Expectancy") +
  scale_x_continuous(
    breaks = seq(min(continent_data$Year), max(continent_data$Year), by = 1)) + #Show a label every year
  scale_y_continuous(
    breaks = seq(0, 100, by = 5),  #Adjust y-axis ticks for more specific ranges
    limits = c(0, 100) ) +            #Set the y-axis range
  scale_color_manual(values = continent_colours) +  #Apply Olympic colours
  graph_theme

#Saving the graph
ggsave("figs/secondgraph.png", plot = p2, width = 12, height = 6, dpi = 300)

#Animating the plot
animated_plot <- p2 +
  labs(subtitle = "Year: {round(frame_along, 0)}") + #Round the year to remove decimals
  transition_reveal(Year) + #Animates the line to reveal over time

anim <- animate(animated_plot, width = 1200, height = 600, fps = 20, duration = 10, renderer = gifski_renderer())
anim_save("figs/animated_plot.gif", animation = anim)
anim

