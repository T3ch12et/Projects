#<<<<<<< HEAD
#<<<<<<< HEAD
#=======

#install.packages("dslabs")
#install.packages("outliers")
library("ggplot2")
library("wbstats")
library("dplyr")
library("tidyr")
library("maps")
library("dslabs")
library("knitr")
library("RColorBrewer")
library("maps")
library("mapproj")
library("outliers")
library("httr")
options(scipen = 999)

#####################################################################################################################################################
# Question1. How does the recent output of the three major greenhouse gasses in the US compare to the historical change in greenhouse gasses worldwide?
#####################################################################################################################################################

# Selecting the required data.

q3_gas_data <- wb(country = "USA", indicator = c("EN.ATM.CO2E.KT", "EN.ATM.METH.KT.CE", "EN.ATM.NOXE.KT.CE"), mrv = 50, return_wide = TRUE) %>%
  mutate(date = as.numeric(date)) %>% 
  filter(date > 1969 & date < 2013) %>% 
  mutate(CO2 = EN.ATM.CO2E.KT, Methane = EN.ATM.METH.KT.CE, Nitrous_Oxide = EN.ATM.NOXE.KT.CE) %>% 

#Gathering the data to make it readable and convenient to plot.
  
  gather(key = gas, value = kt_produced, 
         CO2, 
         Methane,
         Nitrous_Oxide) %>% 
  select(date, gas, kt_produced)

# Creating the plot.

q3_gas_plot <- ggplot(q3_gas_data)+
  geom_point(mapping = aes(x = date, y = kt_produced, color = gas))+
  geom_line(mapping = aes(x = date, y = kt_produced, color = gas))+
  geom_smooth(mapping = aes(x = date, y = kt_produced), color = "Black", method = "loess", formula = "y ~x", na.rm = TRUE)+
  labs(
    title = "Greenhouse Gas Emission in the US Over Time",
    x = "Year",
    y = "Amount of Gas Produced (kt)"
  )+
  scale_colour_discrete(name = "Gas", 
                        labels = c("CO2", "Methane", "Nitrous Oxide"))

q3_gas_history_plot <- ggplot(greenhouse_gases)+
  geom_point(mapping = aes(x = year, y = concentration, color = gas))+
  geom_line(mapping = aes(x = year, y = concentration, color = gas))+
  geom_smooth(mapping = aes(x = year, y = concentration), color = "Black", method = "loess", formula = "y ~x", na.rm = TRUE)+
  labs(
    title = "Greenhouse Gas Emission Over Time",
    x = "Year",
    y = "Concentration in Atmosphere"
  )+
  scale_colour_discrete(name = "Gas", 
                        labels = c("CO2 (ppm)", "Methane (ppb)", "Nitrous Oxide (ppb)"))


summary(q3_gas_data$kt_produced)
summary(greenhouse_gases$concentration)


####################################################################################################
# Question2. Whats the impact of carbon dioxide emission on the economic growth over the past 20 years?
####################################################################################################

# Selecting the required data. 

economic_damage <- wb(country = "countries_only", indicator = c("NY.ADJ.DCO2.CD", "NY.ADJ.SVNG.CD"),
                      mrv = 20,
                      return_wide = TRUE)

# Summary of our selected data

economic_damage <- na.omit(economic_damage)
summ_damage <- summary(economic_damage)

# Calculating the values of outliers.

outlier(economic_damage$NY.ADJ.DCO2.CD, opposite = FALSE)
outlier(economic_damage$NY.ADJ.SVNG.CD, opposite = FALSE)

# Changing the date type.

economic_damage <- mutate(economic_damage, date = as.numeric(date))

# Gathering the data to make it readable and convenient to plot. 

economic_damage <- gather(economic_damage,
                          key = Adjusted_savings,
                          value = Damage_caused,
                          -c("date", "iso3c", "iso2c", "country"), 
                          na.rm = FALSE)

# Changing the names. 

economic_damage$Adjusted_savings[economic_damage$Adjusted_savings == "NY.ADJ.DCO2.CD"] <- "CO2 Damage"
economic_damage$Adjusted_savings[economic_damage$Adjusted_savings == "NY.ADJ.SVNG.CD"] <- "Including particulate damage"

# Creating a plot.

economic_damage_plot <-ggplot(data = economic_damage) +
  
  # Plot including both point and line geometry and facets. 
  
  geom_point(aes(x = date, y = Damage_caused)) +
  geom_line(aes(x = date, y = Damage_caused, color = Adjusted_savings)) +
  facet_grid(. ~Adjusted_savings) +
  
  # Using scale to pick color palette.
  
  scale_color_brewer(palette = "Dark2") +
  
  # Specifying appropriately detailed title and axis labels for chart.
  
  labs(title = "The economical damage caused by CO2 emissions",
       x = "Year", 
       y = "Adjusted net Savings")+
  
  theme(axis.text.x = element_text(size = rel(1), angle = 90))

################################################################################################
# Question3. What is the impact of carbon dioxide emissions on the land and ocean abnormalities?
################################################################################################

# Selecting the required data. 
emissions_anomaly <-  select(temp_carbon, carbon_emissions, land_anomaly, ocean_anomaly)

# Summary of our selected data

emissions_anomaly <- na.omit(emissions_anomaly)
summary(emissions_anomaly)

# Calculating the values of outliers.

outlier(emissions_anomaly$carbon_emissions, opposite = FALSE)
outlier(emissions_anomaly$land_anomaly, opposite = FALSE)
outlier(emissions_anomaly$ocean_anomaly, opposite = FALSE)

# Gathering the data to make it readable and convenient to plot. 

emissions_anomaly <-gather(emissions_anomaly,
                           key = Anomaly_type,
                           value = Anomaly ,
                           -c("carbon_emissions"), 
                           na.rm = FALSE)  


# Creating a plot.

emissions_anomaly_plot <- ggplot(data = emissions_anomaly) +
  
  # Plot including both point and line geometry and facets. 
  
  geom_point(aes(x = carbon_emissions , y = Anomaly)) +
  geom_line(aes(x = carbon_emissions, y = Anomaly, color = Anomaly_type)) +
  
  # Using scale to pick color palette.
  
  scale_color_brewer(palette = "Dark2") +
  
  # Specifying appropriately detailed title and axis labels for chart.
  
  labs(title = "Impact of CO2 emissions on land and ocean anomaly",
       x = "Carbon Emissions", 
       y = "Anomaly")


##############################################################################################################
# Question4.  What are the top 12 countries that were effected and how was agriculture output effected in 2009?
##############################################################################################################


#Source: data.worldbank.org
#https://data.worldbank.org/indicator/EN.CLC.MDAT.ZS?view=chart
#https://data.worldbank.org/indicator/NV.AGR.TOTL.ZS?view=chart

atmosphere_temp <- wb(country = "countries_only", indicator = c("EN.CLC.MDAT.ZS", "NV.AGR.TOTL.ZS"), mrv = 20, return_wide = TRUE)

#change to long format on the data wrangling
eco_agriculture <- atmosphere_temp %>%
  mutate(date = as.numeric(date),
         pop_percent_affected = EN.CLC.MDAT.ZS,
         gdp_gross_output = NV.AGR.TOTL.ZS) %>% 
  filter(date == "2009") %>% 
  gather( 
    key = climate_effects,
    value = percentage,
    c(pop_percent_affected, gdp_gross_output)
  ) %>%
  arrange(percentage) %>% 
  slice(278:279, 276, 345, 267, 282, 264, 357, 262, 307, 259, 324, 252, 353,
        250, 329, 237, 319, 235, 316, 231, 339, 228, 317) %>% 
  select(iso3c, date, country, climate_effects, percentage)

eco_agriculture$num <- c(1:24)

#Takes the for loop of i of vector of numbers from 1:24, then it maps that to ii of the iso3c codes
for (i in 1:length(eco_agriculture$iso3c)) {
  ii = eco_agriculture$iso3c[i]
  if (ii == "SWZ") {
    eco_agriculture$num[i] = 1
  } else if (ii == "MWI") {
    eco_agriculture$num[i] = 2
  } else if (ii == "CHN") {
    eco_agriculture$num[i] = 3
  } else if (ii == "NER") {
    eco_agriculture$num[i] = 4
  } else if (ii == "ERI") {
    eco_agriculture$num[i] = 5
  } else if (ii == "GUY") {
    eco_agriculture$num[i] = 6
  } else if (ii == "KHM") {
    eco_agriculture$num[i] = 7
  } else if (ii == "KEN") {
    eco_agriculture$num[i] = 8
  } else if (ii == "TJK") {
    eco_agriculture$num[i] = 9
  } else if (ii == "ALB") {
    eco_agriculture$num[i] = 10
  } else if (ii == "KIR") {
    eco_agriculture$num[i] = 11
  } else if (ii == "BGD") {
    eco_agriculture$num[i] = 12
  }
}

eco_agr_viz <- ggplot(data = eco_agriculture) + 
  geom_col(mapping = aes(x = num, y = percentage, fill = climate_effects), position = position_dodge()) + 
  theme(axis.text.x = element_text(face = "bold", color = "black", 
                                   size = 5, angle = 90)) +
  scale_x_continuous(breaks=1:12, labels=c("SWZ", "MWI", "CHN", "NER", "ERI", "GUY", "KHM", "KEN", "TJK", "ALB", "KIR", "BGD")) +
  labs(
    title = "Climate Change Effects of Population and Markets in 2009",
    x = "country",
    y = "percentage affected"
  ) +
  scale_fill_discrete(name = "Type of Outcome", labels = c("GDP Gross Output %", "Population Affected %")) +
  facet_wrap(~iso3c)

summary(eco_agriculture)
outlier(eco_agriculture$percentage, opposite = FALSE)

eco_summary <- atmosphere_temp %>%
  mutate(date = as.numeric(date),
         pop_percent_affected = EN.CLC.MDAT.ZS,
         gdp_gross_output = NV.AGR.TOTL.ZS) %>% 
  filter(date == "2009")

climate_summary <- na.omit(eco_summary)
climate_sum_pop <- summary(eco_summary$pop_percent_affected)
climate_sum_gdp <- summary(eco_summary$gdp_gross_output)

############################################################################################################
# Question5. Which countries have had the highest contribution of greenhouse gas emissions from 1990 to 2018? 
############################################################################################################

# Selecting the required data for Data set #1
greenhouse_gases <- wb(country = "countries_only", indicator = c("EN.ATM.GHGT.ZG"), mrv = 50)

# Selecting the required data forData set #2
emission_gas <- arrange(greenhouse_gases, desc(value)) %>% 
  head(50)

# Creating the Plot
country_emission <- ggplot(data = emission_gas) + 
  geom_col(mapping = aes(x = reorder(iso3c, value), y = value)) +
  labs(
    title = "Highest Greenhouse Gas Emssion by Country", 
    x = "Country", 
    y = emission_gas[["indicator"]], 
    color = "Default"
  )

summary(country_emission$value)




#>>>>>>> Section_2
#=======
#>>>>>>> Section_3
