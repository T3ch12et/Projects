#install.packages("shiny")
#install.packages("dslabs")
#install.packages("outliers")
#install.packages("reshape2")
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
library("shiny")
library("reshape2")
options(scipen = 999)

# Selecting the required data. 
economic_damage <- wb(country = "countries_only", indicator = c("NY.ADJ.DCO2.GN.ZS"),
                      mrv = 20,
                      return_wide = TRUE)

economic_damage <- na.omit(economic_damage)

# Changing the date type.
economic_damage <- mutate(economic_damage, date = as.numeric(date))

# Gathering the data to make it readable and convenient to plot. 
economic_damage <- economic_damage %>% 
  select(country, NY.ADJ.DCO2.GN.ZS ) %>% 
  group_by(country) %>% 
  summarise(average = mean(NY.ADJ.DCO2.GN.ZS)) %>% 
  arrange(desc(average))

top_3 <- economic_damage %>% 
  top_n(3)

top_5 <- economic_damage %>% 
  top_n(5)

top_10 <- economic_damage %>% 
  top_n(10)


# Selecting the required data. 
emissions_anomaly <-  select(temp_carbon, carbon_emissions, land_anomaly, ocean_anomaly)

emissions_anomaly <- na.omit(emissions_anomaly)

# Gathering the data to make it readable and convenient to plot. 
emissions_anomaly <-gather(emissions_anomaly,
                           key = Anomaly_type,
                           value = Anomaly ,
                           -c("carbon_emissions"), 
                           na.rm = FALSE)  

# Selecting the required data. 
gas_data <- wb(country = "USA", indicator = c("EN.ATM.CO2E.KT", "EN.ATM.METH.KT.CE", "EN.ATM.NOXE.KT.CE"), 
               mrv = 50, return_wide = TRUE) %>%
  mutate(date = as.numeric(date)) %>% 
  filter(date > 1969 & date < 2013) %>% 
  mutate(CO2 = EN.ATM.CO2E.KT, Methane = EN.ATM.METH.KT.CE, Nitrous_Oxide = EN.ATM.NOXE.KT.CE) %>% 
  
  # Gathering the data to make it readable and convenient to plot. 
  gather(key = gas, value = kt_produced, 
         CO2, 
         Methane,
         Nitrous_Oxide) %>% 
  select(date, gas, kt_produced)

q3_gas_plot <- ggplot(gas_data)+
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


countries <- unique(eco_agriculture$iso3c)

greenhouse_gases2 <- wb(country = c("AFG", "CAN", "KOR", "BRA", "GNQ"), indicator = c("EN.ATM.GHGT.ZG"), mrv = 50, return_wide = TRUE)
greenhouse_gases2$data <- as.numeric(greenhouse_gases2$date)

colnames(greenhouse_gases2)[colnames(greenhouse_gases2) == "EN.ATM.GHGT.ZG"] <- "percent"


server <- function(input, output) {
  output$outputText <- renderText({
    paste("The average damage for this country is ", filter(economic_damage, country == input$Average_Damage) %>% select(average), "% of GNI")
  })
  output$eco_damage_plot <- renderPlot({
    
    if (input$n_var == 3) {
      x <- top_3[["country"]]
      y <- top_3[["average"]]
    } else if (input$n_var == 5) {
      x <- top_5[["country"]]
      y <- top_5[["average"]]
    } else {
      x <- top_10[["country"]]
      y <- top_10[["average"]]
    }
    # Create column chart
    ggplot() +
      geom_col(mapping = aes(reorder(x, y),
                             y,
                             fill = x
      )) +
      geom_text(aes(x = x, y = y, label = y), position = position_dodge(width=0.9),  size=3) +
      labs(
        x = "Country", y = "Average Economic Damage (% of GNI)",
        title = "Economic Damage Caused by CO2 Emissions in terms of % of GNI"
      ) + scale_fill_discrete(name = "Countries")
  })  
  
  
  output$plot <- renderPlot(ggplot(filter(emissions_anomaly, Anomaly_type == input$variable, carbon_emissions <= input$integer))+
                              geom_point(mapping = aes(x = carbon_emissions, y = Anomaly, color = Anomaly_type))+
                              geom_line(mapping = aes(x =  carbon_emissions, y = Anomaly, color = Anomaly_type))+
                              geom_smooth(mapping = aes(x = carbon_emissions, y = Anomaly), color = "Blue", method = "loess", formula = "y ~x", na.rm = TRUE)+
                              labs(
                                title = "Impact of carbon dioxide emissions on the land and ocean abnormalities",
                                x = "Carbon Emissions (millions of metric tons of carbon)",
                                y = "Anomaly (Celcius)"
                              )+
                              scale_colour_discrete(name = "Anomaly_type", 
                                                    labels = c(input$variable)))
  
  output$q3_plot <- renderPlot(ggplot(filter(gas_data, gas == input$variable1, date <= input$integer1))+
                                 geom_point(mapping = aes(x = date, y = kt_produced, color = gas))+
                                 geom_line(mapping = aes(x =  date, y = kt_produced, color = gas))+
                                 geom_smooth(mapping = aes(x =  date, y = kt_produced), color = "Black", method = "loess", formula = "y ~x", na.rm = TRUE)+
                                 labs(
                                   title = "Greenhouse Gas Emission in the US Over Time",
                                   x = "Year",
                                   y = "Amount of Gas Produced (kt)"
                                 )+
                                 scale_colour_discrete(name = "Gas", 
                                                       labels = c(input$variable1)))
  
  
  
  output$q3_plot <- renderPlot(ggplot(filter(gas_data, gas == input$variable1, date <= input$integer1))+
                                 geom_point(mapping = aes(x = date, y = kt_produced))+
                                 geom_line(mapping = aes(x =  date, y = kt_produced))+
                                 geom_smooth(mapping = aes(x =  date, y = kt_produced), color = "Black", method = "loess", formula = "y ~x", na.rm = TRUE)+
                                 labs(
                                   title = "Greenhouse Gas Emission in the US Over Time",
                                   x = "Year",
                                   y = "Amount of Gas Produced (kt)"
                                 )+
                                 scale_colour_discrete(name = "Gas", 
                                                       labels = c(input$variable1)))
  
  output$q3_plot2 <- renderPlot(ggplot(filter(greenhouse_gases, gas == input$variable2, year <= input$integer2))+
                                  geom_point(mapping = aes(x = year, y = concentration))+
                                  geom_line(mapping = aes(x = year, y = concentration))+
                                  geom_smooth(mapping = aes(x = year, y = concentration), color = "Black", method = "loess", formula = "y ~x", na.rm = TRUE)+
                                  labs(
                                    title = "Greenhouse Gas Emission Over Time",
                                    x = "Year",
                                    y = "Concentration in Atmosphere"
                                  )+
                                  scale_colour_discrete(name = "Gas", 
                                                        labels = c(input$variable2))
                                
                                
  )
  
  output$plot3 <- renderPlot({
    country_plot <- filter(eco_agriculture, iso3c == input$country_choice)
    climate_change_plot <- ggplot(data = country_plot) + 
      geom_col(mapping = aes(x = num, y = percentage, fill = climate_effects), position = position_dodge()) + 
      theme(axis.text.x = element_text(face = "bold", color = "black", 
                                       size = 7, angle = 90)) +
      scale_x_continuous(breaks=1:12, labels=c("SWZ", "MWI", "CHN", "NER", "ERI", "GUY", 
                                               "KHM", "KEN", "TJK", "ALB", "KIR", "BGD")) +
      labs(
        title = "Climate Change Effects of Population and Markets in 2009",
        x = "country",
        y = "percentage affected"
      ) +
      scale_fill_discrete(name = "Type of Outcome", labels = c("GDP Gross Output %", "Population Affected %")) +
      facet_wrap(~iso3c)
    
    
    #TRUE equals turned on i'm guessing. SO if true then it will show
    if (input$gdp_trend == TRUE) {
      climate_change_plot <- filter(eco_agriculture, iso3c == input$country_choice) %>% 
        filter(climate_effects == "gdp_gross_output")
      climate_change_plot <- ggplot(data = climate_change_plot) + 
        geom_col(mapping = aes(x = num, y = percentage, fill = climate_effects), position = position_dodge()) + 
        theme(axis.text.x = element_text(face = "bold", color = "black", 
                                         size = 7, angle = 90)) +
        scale_x_continuous(breaks=1:12, labels=c("SWZ", "MWI", "CHN", "NER", "ERI", "GUY", 
                                                 "KHM", "KEN", "TJK", "ALB", "KIR", "BGD")) +
        labs(
          title = "Climate Change Effects of Population and Markets in 2009",
          x = "country",
          y = "percentage affected"
        ) +
        scale_fill_discrete(name = "Type of Outcome", labels = c("GDP Gross Output %", "Population Affected %")) +
        facet_wrap(~iso3c)
    } else if (input$pop_trend == TRUE) {
      climate_change_plot <- filter(eco_agriculture, iso3c == input$country_choice) %>% 
        filter(climate_effects == "pop_percent_affected")
      climate_change_plot <- ggplot(data = climate_change_plot) + 
        geom_col(mapping = aes(x = num, y = percentage, fill = climate_effects), position = position_dodge()) + 
        theme(axis.text.x = element_text(face = "bold", color = "black", 
                                         size = 7, angle = 90)) +
        scale_x_continuous(breaks=1:12, labels=c("SWZ", "MWI", "CHN", "NER", "ERI", "GUY", 
                                                 "KHM", "KEN", "TJK", "ALB", "KIR", "BGD")) +
        labs(
          title = "Climate Change Effects of Population and Markets in 2009",
          x = "country",
          y = "percentage affected"
        ) +
        scale_fill_manual(values = "#2ab7ca", name = "Type of Outcome", labels = c("Population Affected %")) +
        facet_wrap(~iso3c)
    }
    return(climate_change_plot)
  })
  
  output$q5_plot <- renderPlot({
    ggplot(data = greenhouse_gases2) +
      geom_col(mapping = aes(x = date, y = percent, color = country)) +
      labs(
        title = "Highest Greenhouse Gas Emssion by Country", 
        x = "Country", 
        y = greenhouse_gases2[["indicator"]], 
        color = "Default") +
      scale_x_discrete(limits = input$greenhouse)
  })
}

