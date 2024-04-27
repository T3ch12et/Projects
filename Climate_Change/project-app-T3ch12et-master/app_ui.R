#install.packages("shinythemes")
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(shinythemes)



ui <- navbarPage(
  theme = shinytheme("cyborg"),
  "Climate Change",
  tabPanel("Introduction",
           hr(),
           tags$img(src = "motorcycle.gif", width = "100%", height = "230", length = "90"),
           titlePanel("Introduction"),
           mainPanel(
             h6(
               "The App demonstrates the consequences of climate changes and the way it has been a 
        growing problem over the years. In order to better understand changes in our climate, 
        we need to further be able to look at factors that contribute to this. 
        Climate change is a major factor our society needs to consider and understand 
        as it will be directly impacting our future. Even today, changes in our climate due to global warming, 
        deforestation, industrial practices and further made-made activities have negatively impacted thousands 
        of people and impacted their daily life.
        
        The data used in the following assignment talks about the values concerning the damage caused by the emission of 
        greenhouse gases such as carbon dioxide, methane and more. The two datasets used primarily represent the climate 
        data and how the earth is getting negatively impacted by made-made activities. The sole purpose of the app and
        our team was to make the user gain more knowledge about climate change and hopefully be more mindful about taking 
        care of our Earth, which is the only home we have."),  
             
             h3(HTML("<a href=
            'https://data.worldbank.org/indicator?tab=all'>
            <u> The World Bank </u> </a>")),
             
             
             h6("The World Bank comprises 189 member countries who are represented by a board of governors, who are the ultimate
        policymakers at the World Bank. At the World Bank, the Development Data Group coordinates statistical and data work 
        and maintains a number of macro, financial and sector databases. Working closely with the Bank’s regions and Global 
        Practices, the group is guided by professional standards in the collection, compilation and dissemination of data to 
        ensure that all data users can have confidence in the quality and integrity of the data produced.
        Much of the data comes from the statistical systems of member countries, and the quality of global data depends on 
        how well these national systems perform. The World Bank works to help developing countries improve the capacity, 
        efficiency and effectiveness of national statistical systems. Without better and more comprehensive national data, 
        it is impossible to develop effective policies, monitor the implementation of poverty reduction strategies, 
        or monitor progress towards global goals."),
             
             h3(HTML("<a href=
            ' https://cran.r-project.org/web/packages/dslabs/dslabs.pdf '>
            <u> Dslabs </u> </a>")),
             
             h6("The package “dslabs” is a data set from the Rstudio's source from Office of Scientific and Technical Information 
           of the U.S. Department of Energy. The data in this dataset is collected over the years of 1781 to 2018, which includes 
           the annual mean global temperature anomaly on land, sea and both combined as well. It also includs the annual carbon 
           emissions in millions of metric tons of carbon and gathered by the National Centers for Environmental Information."),
             
             h5(HTML("<a href=
            ' https://info201a-wi20.github.io/project-report-saejinm/'>
            <u> Check Data Report for more Details </u> </a>")),
             
             h5("Created by"),
             tags$li(class = "center_text", "Vanshika Goel"),
             tags$li(class = "center_text", "Sweena Kalamkar"),
             tags$li(class = "center_text", "Sage Myung"),
             tags$li(class = "center_text", "Brian Lukas"),
             tags$li(class = "center_text", "Emeka Emeche")),
           
           hr(),
           tags$img(src = "climate.jpg", width = "450", length = "1000", align = "right")
  ),
  
  
  tabPanel(
    "Economic Damages",
    hr(),
    tags$img(src = "emission.gif", width = "100%", height = "600", length = "300"),
    titlePanel("What is the average economic damage caused by CO2 Emissions?"),
    h6("In order to understand more about climate change, 
       we need to understand the impact of one of the most common greenhouse gases (CO2) 
       on the economic growth of all the nations worldwide. In today’s age, the main topic 
       of interest is the economic growth and the climate often tends to impact the economy 
       significantly and hence, it is essential to study this consequence of climate change."),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "n_var",
          label = "Top n countries",
          choices = c(3, 5, 10)),
        selectInput("Average_Damage", "Country", economic_damage$country),
        textOutput("outputText")),
      mainPanel(
        plotOutput("eco_damage_plot"),
        hr(),
        h6("This interactive graph will allow you to observe the average economic damage caused 
          by Carbon dioxide emissions in the top three, top five and top ten countries respectively according to the first widget. 
          In the following graph, the x-axis signifies the countries while the y-axis shows 
          the average value of all the economic damages caused in terms of % of GNI (Gross National Income). 
         While the drop-down menu option, allows the user to select any country of their choice and receive the mean value of 
           economic damages caused by carbon emissions for the past 20 years. This is an important aspect 
           of our app as the user is able to specifically narrow down the value for the country of their 
           interest and clearly make a comparison to the graphs which show the mean values of economic 
           damages for the top 3, top 5 and top 10 countries respectively. For instance, the country of Ubekistan 
           has a mean economic damage of 10.5081678577879% of GNI as represented in the top three countries with most damage. 
           The user can check the mean economic damage for Switzerland which appears as 0.170506982639502% of GNI and thus, make
           a comparision with the highest mean economic damage caused in the past 20 years.")
      )
    )),
  tabPanel(
    "Anomaly",
    hr(),
    tags$img(src = "anomaly.gif", width = "100%", height = "600", length = "300"),
    titlePanel("What is the impact of carbon dioxide emissions on the land and ocean anomalies?"),
    h6("To study climate change, we also need to understand the direct impact of Carbon Dioxide emissions
     on land and ocean anomalies as this is the deciding factor for weather and natural disasters. Global Warming and climate 
     change influence land and ocean to a great extent which are factors that account for the major changes in day-to-day 
     human activities hence, climate change can be accounted for by better understanding CO2 emissions and its impact on our Earth."),
    sidebarLayout(
      sidebarPanel(
        selectInput("variable", "Anomaly_type:",
                    c("land_anomaly", "ocean_anomaly")),
        sliderInput("integer", "carbon_emissions:",
                    min = 200, max = 1000,
                    value = 9855)),
      mainPanel(
        plotOutput("plot"),
        hr(),
        h6("As observed through the visualization, the Carbon dioxide emissions have been on a constant rise for the 
      past few years and this has had a significant impact upon the land and oceans of our earth.
      As specified by the line and point graph, we can conclude the trends that the land and ocean 
      anomalies follow as caused by the increase in CO2 emissions. Overall, the land anomalies 
      are higher as compared to the ocean anomalies as caused by the CO2 emissions as measured in millions of metric tons of carbon
      for the same time period. 
      The descriptive statistics of the visualization highlighted the average CO2 emissions to be 2942 while 
      the average effect upon land and ocean anomaly appears to be 0.03259 and 0.03252 respectively. The carbon dioxide 
      emission are measured in millions of metric tons of carbon while the land and ocean anomalies values represent the annual
      mean temperature over land or ocean in degree Celcius relative to the 20th century mean.
      This shows the mean correlation between the emissions and the anomalies that have been caused. 
      The main columns of interest in our dataset not have any missing values that might derail the 
         overall mean or statistical analysis of the data.")
      )
    )),
  tabPanel("Greenhouse Gas Emissions",
           hr(),
           tags$img(src = "greenhouse.gif", width = "100%", height = "600", length = "300"),
    h3("Major Greenhouse Gas Emissions in The United States Recently Compared to Historical Emissions Worldwide"),
    h2("US Data"),
    selectInput("variable1", "Gas:",
                c("CO2" = "CO2", "Methane" = "Methane", "Nitrous_Oxide" = "Nitrous_Oxide")),
    sliderInput("integer1", "Year:",
                min = 1970, max = 2016,
                value = 1970),
    plotOutput("q3_plot"),
    h2("Worldwide Historical Data"),
    selectInput("variable2", "Gas:",
                c("CO2" = "CO2", "Methane" = "CH4", "Nitrous Oxide" = "N2O")),
    sliderInput("integer2", "Year:",
                min = 20, max = 2000,
                value = 20),
    plotOutput("q3_plot2"),
    h6("Each graph has a slider that you can use to watch the change in greenhouse gas emissions over time, the slider correlates to the year, and for each year you add to the slider, another year of data is displayed. Also, each graph has a selector that allows you to choose which type of greenhouse gas you want to observe. 
The geom_smooth line on the Greenhouse Gas Emission Over Time graph exhibits a strong upward slope between the years 1750-2000, similarly,
      the Greenhouse Gas Emission Over Time in the US illustrates a similar trend between the years 1990-2010. 
      This provides evidence of a trend that requires a large scale view to really see, while the US specific graph shows a weaker slope than the world history graph, the enlightened reader will observe that these trends when compiled among many countries will provide the dramatic results shown on the world graph. 
      The mean for the US graph is 1979434, this value is substantially lower than the values of recent years, similarly for the historical graph,
the mean is 416.2, compared to the recent max of 1703.4. This shows the increasing trend of greenhouse gas emission and concentrations.
                Thus we have the conclusion that the climate crisis will require a cumulative effort on a world scale to combat, and no one is less responsible than anyone else."
    )
  ),
  tabPanel(
    title = "Population",
    hr(),
    tags$img(src = "agriculture.gif", width = "100%", height = "600", length = "300"),
    titlePanel(
      "Climate Change Effects of Population"),
    h3("How did the population percentage affect the GDP percentage value 
       of agricultural products of each country?"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "country_choice",
          label = "List of Countries",
          choices = countries,
          selected = "SWZ"
        ),
        checkboxInput(
          inputId = "gdp_trend",
          label = "GDP Gross Output %",
          value = FALSE
        ),
        pop_input <- checkboxInput(
          inputId = "pop_trend",
          label = "Population Affected %",
          value = FALSE
        )),
      mainPanel(
        plotOutput(outputId = "plot3"),
        hr(),
        p("This interactive graph will allow you to observe the percentage of the gdp output and the population
            affedted due to climate change in different countries. It shows the top 12 countries most affected in the population
            leading to changes in the gdp output. The x-axis lists the countries and y-axis shows the percentages of the variables 
            'GDP Output %' and 'Populated Affected %'."),
        p(
          "Data from",
          a(href = "https://data.worldbank.org/indicator/EN.CLC.MDAT.ZS?view=chart", 
            "https://data.worldbank.org/indicator/EN.CLC.MDAT.ZS?view=chart")),
        p(
          "Data from",
          a(href = "https://data.worldbank.org/indicator/NV.AGR.TOTL.ZS?view=chart", 
            "https://data.worldbank.org/indicator/NV.AGR.TOTL.ZS?view=chart"))
 ))), 
  tabPanel(
    title = "Summary of Greenhouse Gas Emission contribution",
    hr(),
    tags$img(src = "burn.gif", width = "100%", height = "600", length = "300"),
  sidebarLayout(
   sidebarPanel(
      selectInput("variable", "Different Countries:",
                    c("AFG" = "AFG", "CAN" = "CAN", "KOR" = "KOR", "BRA" = "BRA", "GNQ" = "GNQ")),
      sliderInput("integer", "Values:",
                    min = 1991, max = 2012,
                    value = 1991)), 
    mainPanel(
      plotOutput("q5_plot"),
      hr(),
      p("This graph will help to understand what countries have had the biggest impact on greehouse gas emission over time. Greenhouse Gas Emission is an inmportant factor to look into for climate change because any human-made contribution casuses a change in Earth's radiatvie balance. An increase to this or distrubtion will cause the overall temperature of Earth to increase immensely. This information will be even more important to look at other factors such as agrilcultre economics and carbon dioxide.
      It shows the different years on the x axis while it shows the percent on the y axis. By being able to see the different years, this is important to note becasue than we're able to see what factors have made it change over the years.  
      The audience can engage by looking at just a specific country but also by looking at different value sets in kt. This form of interaciton will be helpful and allow easier understanding of what exactly has changed in those speciffic countries. As it only has some of the popular countries, I thought this would be helpful factor into understanding the data. 
      Overall it can clearly show how much of a difference those countries values are combined from the years 1991 to 2012. The important of looking at this specific time zone is crucial because of how much changes to climate change there has been. Furthermore, with the increase in so much other factors, this is the data to show those who are unclear of how humans are causing cliamte change.")
      ))
  ),
tabPanel("Evaluation",
         hr(),
         tags$img(src = "leo.gif", width = "100%", height = "600", length = "300"),
         h6("Overall, as a way to understand climate change we must be able to understand the 
            different factors that have contributed to it. The first factor that comes to mind is the greenhouse gas 
            phenomenon.The greenhouse gases have an unique property which allows them to absorb infrared radiation, keeping 
            heat inside the Earth's atmosphere. This process itself contributes to climate change as the greenhouse gases 
            contribute in raising the overall temperature of the atmosphere.  
            
            Adding to this factor, carbon dioxide emmissions are important as carbon dioxide is the most common greenhuse gas.
            With this increase of this gas, we are able to understand how it has damaged the economies of several countries worldwide. 
            This has been an important observation as the users are able to directly correlate the impact of climate change (in terms 
            of carbon dioxide emissions) to the chaos created in our economies. Our team hopes that the users will be more precautious 
            regarding the emissions related to carbon in order to preserve our economies. 
            
            Carbon dioxide emissions are also directly linked to impacting the land and oceans of our Earth. By being able to understand 
            environmental changes, we can better support how things on our earth are changing. Land and ocean temperatures are crucial 
            to understand because they are an integral role in predicting the weather and upcoming natural disasters.Which further brings 
            concerns to how climate change is an ongoing problem that brings many worrisome effects on humans. 
            
            Although, we cannot deny the underlying truth about how climate change has already taken a toll on human lives. 
            
            One of the most significant truths has been the impact of limate change on agricultural production in multiple 
            countries. Our data has taken into account the top 12 countries that were affected and how their GDP output 
            changed because of this. 
            Agricultural production is important in many countries as this is a way humans and animals are able to satisfy 
            their basic need to survive. This is a crucial factor as it could possibly decrease survival rate for the humans
            and their economy. 
            
            Finally, we need to acknowledge that humans are the primary factor causing harm to ourselves and our home. 
            Only one country is not to blame, we must be able to determine which countries are contributing the most to 
            climate change in order to find methods to help decrease it before it impacts runs our of control. Being able 
            to determine over the past few years which countries had the highest greenhouse gas emission, gives us a better 
            understanding of how we can move forward to stop negatively impacting our Earth", align = "center")
))



 