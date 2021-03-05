# GEOG324 RShiny Group Project

##########################################################################################################

# Attach required packages
library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(glue)
library(plotly)
library(tmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)


##########################################################################################################

aboutText <- 
    "<h3>World Forestry Viewer</h3>
  <p>This is an R Shiny app which demonstrates web queries to map and plot wordwide forestry and CO2 emission levels.
  The data used are provided by UNEP (United Nations Environmental Programme) and its partners in the GEO (Global Environmental Outlook) report.
  The template and certain functionality have been built upon from code contained in the <i>NZ Earthquake Viewer</i> app developed by Matthew Wilson in 2018.</p>
  <hr/>
  <h4>Data providers</h4>
  <p>All data presented are copyright of the respective owners and they are gratefully acknowledged:</p>
  <ul>
    <p><b><a href='http://geodata.grid.unep.ch/api'>Geodata</a></b>, retrieved from the United Nations Environmental Data Explorer.</p>
  </ul>
  <hr/>
  <h4>App development</h4>
  <p>Part of the functionality presented is copyright of the respective owner and is gratefully acknowledged:</p>
  <ul>
    <p><b>NZ Earthquake Viewer</b>, created by Matthew Wilson (2018) and retrieved from GEOG324 Learn page in 2020.</p>
  </ul>
  <hr/>
  <h4>App licence</h4>
  <p>&#169; Copyright 2020 Amanda Pullan, Heng Sun, Conor Cussin and Josh Clark</p>"


##########################################################################################################

### GET UNEP VARIABLES & PREPARE THEM FOR MAPPING

# get world data for mapping etc.
world <- ne_countries(scale = "medium", returnclass = "sf")

# -------------------------------------------------------------------------------------------------

## Function 1: Downloads and extracts UNEP variables

get_variable <- function(unepValue, years) {
    
    varID <- as.character(unepValue)
    varURL <- glue("http://geodata.grid.unep.ch/api/countries/variables/{varID}/years/1951:2011")
    unepResponse <- GET(varURL)
    unepCurrent <- rawToChar(unepResponse$content) %>% fromJSON()
    names(unepCurrent) <- c("country", "year", "value")
    unepCurrent <- unepCurrent %>% filter(year %in% years)
    
    return(unepCurrent)
}

# -------------------------------------------------------------------------------------------------

## Function 2: Joins the UNEP variables to the World dataset

join_to_world <- function(unepVariable) {
    
    newVariable <- world %>%
        left_join(unepVariable, by = c("iso_a2" = "country")) %>%
        select(name_long, iso_a2, continent, income_grp, year, value, geometry)
    
    return(newVariable)
}

# -------------------------------------------------------------------------------------------------

# Get UNEP variables for bubbles

yearsCO2 = c(seq(2000, 2010))

# Total Population
worldPop <- get_variable(1810, yearsCO2)
names(worldPop) <- c("country", "year", "Population")

# Gross Domestic Product
worldGDP <- get_variable(753, yearsCO2)
names(worldGDP) <- c("country", "year", "GDP")

# Total CO2 Emissions
# Join to world data
worldCO2 <- get_variable(2309, yearsCO2) %>%
    join_to_world()
names(worldCO2)[6] <- "CO2"

# Join into one table
worldCO2 <- worldCO2 %>%
    left_join(worldPop, by = c("iso_a2" = "country", "year" = "year")) %>%
    left_join(worldGDP, by = c("iso_a2" = "country", "year" = "year"))

# Create new columns
worldCO2 <- worldCO2 %>%
    mutate(popCO2 = CO2 / Population) %>%
    mutate(gdpCO2 = CO2 / GDP)

# Remove Serbia
worldCO2 <- worldCO2 %>% filter(iso_a2 != "RS")


# -------------------------------------------------------------------------------------------------

# Get forest UNEP variables

yearsForest = c(2000, 2005, 2010)

# Proportion of Land Area Covered by Forest
forestProp <- get_variable(2238, yearsForest)
names(forestProp)[3] <- "forestProp"

# Forest Average Annual Change
annualChange <- get_variable(930, yearsForest)
names(annualChange)[3] <- "annualChange"

# Forest Harvest Rate
harvestRate <- get_variable(1966, yearsForest)
names(harvestRate)[3] <- "harvestRate"

# Forest Plantation Extent
plantExtent <- get_variable(199, yearsForest)
names(plantExtent)[3] <- "plantExtent"

# Forest within Protected Areas
protectForest <- get_variable(2202, yearsForest)
names(protectForest)[3] <- "protectForest"


# -------------------------------------------------------------------------------------------------

# Create main dataframe and join forest variables

# Create main df using forest area
worldForest <- get_variable(2136, yearsForest) %>%
    join_to_world()
names(worldForest)[6] <- "forestArea"

# Join variables to main df
worldForest <- worldForest %>%
    left_join(forestProp, by = c("iso_a2" = "country", "year" = "year")) %>%
    left_join(annualChange, by = c("iso_a2" = "country", "year" = "year")) %>%
    left_join(harvestRate, by = c("iso_a2" = "country", "year" = "year")) %>%
    left_join(plantExtent, by = c("iso_a2" = "country", "year" = "year")) %>%
    left_join(protectForest, by = c("iso_a2" = "country", "year" = "year"))


#data for scatter which need both base and bubble value
forestArea <- get_variable(2136, yearsForest)
names(forestArea)[3] <- "forestArea"
allData <- worldCO2 %>%
    left_join(forestArea, by = c("iso_a2" = "country", "year" = "year")) %>%
    left_join(worldPop, by = c("iso_a2" = "country", "year" = "year")) %>%
    left_join(worldGDP, by = c("iso_a2" = "country", "year" = "year"))%>%
    left_join(forestProp, by = c("iso_a2" = "country", "year" = "year")) %>%
    left_join(annualChange, by = c("iso_a2" = "country", "year" = "year")) %>%
    left_join(harvestRate, by = c("iso_a2" = "country", "year" = "year")) %>%
    left_join(plantExtent, by = c("iso_a2" = "country", "year" = "year")) %>%
    left_join(protectForest, by = c("iso_a2" = "country", "year" = "year"))

# -------------------------------------------------------------------------------------------------

# Create titles for each variable

titles = c("Forest Area (1000 ha)", "Proportion (% Total Area)",
           "Annual Change (1000 ha)", "Harvest Rate (% Total Volume)",
           "Plantation Extent (1000 ha)", "Protected Area (1000 ha)")


# Define selectable values for UI

variables = c("Forest Area" = "forestArea",
              "Forest Area / Land Area" = "forestProp",
              "Forest Average Annual Change" = "annualChange",
              "Forest Harvest Rate" = "harvestRate",
              "Forest Plantation Extent" = "plantExtent",
              "Forest within Protected Areas" = "protectForest")

bubble_var = c("Total CO2 Emissions" = "CO2",
               "CO2 Emissions Per Population" = "popCO2",
               "CO2 Emissions Per GDP" = "gdpCO2")
# year selection for variables
varYears = c(2000, 2005, 2010)


##########################################################################################################

# Begin Rshiny app

ui <- navbarPage("Forest Data", id="nav",
                 
                 # Map panel #################################################################
                 tabPanel("Forest Map",
                          
                          div(class="outer",
                              tags$head(
                                  
                                  # custom CSS to enable full screen map
                                  includeCSS("styles.css"),
                              ),
                              
                              
                              # include map in panel
                              tmapOutput(outputId = "map", width="100%", height="100%"),
                              
                              
                              # map controls
                              
                              absolutePanel(id = "controls",
                                            draggable = FALSE, left = 40, bottom = 100,
                                            width = "auto", height = "auto",
                                            
                                            h3("Base map selection"),
                                            
                                            # Choose variable to display on map
                                            selectInput(inputId = "var", label = "Variable",
                                                        choices = variables),
                                            
                                            # Choose year to display on map
                                            selectInput(inputId = "year", label = "Year",
                                                        choices = varYears,
                                                        selected = 2010),
                                            
                                            hr(), 
                                            
                                            h3("Bubble selection"),
                                            
                                            # Choose whether to display bubbles
                                            checkboxInput("bubbles", label = "Show Bubbles", value = FALSE),
                                            
                                            # Choose bubble variable to display
                                            radioButtons(inputId = "bubbleVar", label = "Bubbles",
                                                         choices = bubble_var),
                                            
                                            # Choose value range to display for bubbles
                                            sliderInput(inputId = "bubbleYear",
                                                        label = "Bubble year selection",
                                                        min = 2000, max = 2010,
                                                        value = 2010, step = 1, sep = "")
                              ),
                              
                              
                              # data source
                              tags$div(id="cite",
                                       'Data credit: ', tags$em('Geodata'), ', http://geodata.grid.unep.ch/api/.')
                          )
                 ),
                 
                 # # Data table panel ########################################################
                 
                 # add data table to the panel
                 
                 tabPanel("Data Table",
                          DT::dataTableOutput(outputId = "datatable"),
                          tags$div(id="cite",
                                   'Data credit: ', tags$em('Geodata'), ', http://geodata.grid.unep.ch/api/.')
                 ),
                 
                 
                 # Graphs panel ##############################################################
                 
                 # add graphs to the panel
                 
                 tabPanel("Graphs",
                          plotlyOutput(outputId = "plot"),
                          
                          # Selectable buttons
                          uiOutput("radioButtons"),
                          
                          # Choose plot
                          selectInput("plotType", 
                                      label = "Choose a plot to display",
                                      choices = list("Box plot",
                                                     "Column graph",
                                                     "Scatterplot",
                                                     "Histogram",
                                                     "Density plot"),
                                      selected = "Box plot"),
                          
                          #add x, y variable if we don't want to map from value from map
                          uiOutput("var1"),
                          uiOutput("var2"),
                          uiOutput("tempplotyear")
                          
                 ),
                 
                 # About panel ##############################################################################
                 tabPanel("About",
                          htmlOutput("about")
                 )
)


##########################################################################################################

server <- function(input, output, session) {
    
    # Function to create map layer
    create_map <- function(forestDF, forestVar, title, midpoint = NULL, palette = "Greens") {
        tm_shape(forestDF) +
            tm_fill(forestVar,
                    title = title,
                    style = "quantile",
                    midpoint = midpoint,
                    palette = palette,
                    zindex = 401)  +
            tm_borders(alpha = 0.5) +
            tm_scale_bar() +
            tm_view(view.legend.position = c("right", "bottom"))
    }
    
    # ----------------------------------------------------------------------------------------
    
    # Function to create map bubbles
    map_bubbles <- function(bubbleDF, bubbleVar, scale){
        tm_shape(bubbleDF) +
            tm_bubbles(bubbleVar, col = bubbleVar,
                       style = "quantile", palette = "Reds",
                       alpha = 0.5, scale = scale,
                       zindex = 402)
    }
    # ----------------------------------------------------------------------------------------
    
    # Create an event listener to react to events
    
    eventListen <- reactive({
        list(input$var,
             input$year,
             input$bubbles,
             input$bubbleVar,
             input$bubbleYear)
    })
    
    # Map panel ##############################################################################
    
    # Output map with default values
    
    output$map <- renderTmap({
        defaultValue = worldForest %>% filter(year == 2010)
        mapTitle = titles[1]
        create_map(defaultValue, "forestArea", mapTitle)
    })
    
    
    # ----------------------------------------------------------------------------------------
    
    # Update map layer with selected variable
    observeEvent(eventListen(),{
        # Filter data by selected year and update title
        midpoint = NULL
        newpalette = "Greens"
        forestDF <- worldForest %>% filter(year == input$year)
        if (input$var == "forestArea") {mapTitle = titles[1]}
        else if (input$var == "forestProp") {mapTitle = titles[2]}
        else if (input$var == "harvestRate") {mapTitle = titles[4]}
        else if (input$var == "plantExtent") {mapTitle = titles[5]}
        else if (input$var == "protectForest") {mapTitle = titles[6]}
        else if (input$var == "annualChange") {
            mapTitle = titles[3]
            midpoint = 0
            newpalette = "PRGn"}
        
        # ----------------------------------------------------------------------------------------
        
        # Update map with bubbles included
        if (input$bubbles) {
            bubbleDF = worldCO2 %>%
                filter(year == input$bubbleYear)
            
            if (input$bubbleVar == "CO2") {scale = 5}
            else if (input$bubbleVar == "popCO2") {scale = 3}
            else if (input$bubbleVar == "gdpCO2") {scale = 7}
            
            tmapProxy("map", session, {
                tm_remove_layer(401)
                tm_remove_layer(402) +
                    create_map(forestDF, input$var, mapTitle, midpoint, newpalette) +
                    map_bubbles(bubbleDF, input$bubbleVar, scale)
            })
        }
        
        # ----------------------------------------------------------------------------------------
        
        # Update map with no bubbles
        else {
            tmapProxy("map", session, {
                tm_remove_layer(401)
                tm_remove_layer(402) +
                    create_map(forestDF, input$var, mapTitle, midpoint, newpalette)
            })
        }
    })
    
    # Data table panel ##############################################################################
    
    output$datatable = DT::renderDataTable({
        # Remove geometry column from the data table
        # Then update value column title
        current = st_set_geometry(worldForest, NULL) %>%
            filter(year == input$year)
        if (input$var == "forestArea") {valueTitle = titles[1]}
        else if (input$var == "forestProp") {valueTitle = titles[2]}
        else if (input$var == "annualChange") {valueTitle = titles[3]}
        else if (input$var == "harvestRate") {valueTitle = titles[4]}
        else if (input$var == "plantExtent") {valueTitle = titles[5]}
        else if (input$var == "protectForest"){valueTitle = titles[6]}
        
        # ----------------------------------------------------------------------------------------
        
        # Prepare and display data
        
        current <- current %>%
            left_join(worldCO2, by = c("name_long" = "name_long", "year" = "year")) %>%
            select(name_long, input$var, CO2, Population, GDP) %>%
            na.omit()
        
        names(current)[1:3] = c("Name", valueTitle, "CO2 Emissions")
        current
    })
    # Graphs panel ##############################################################################
    #showing more input variables
    # select 1st input variable
    output$var1 <- renderUI({
        selectInput(inputId = "plotvar1", label = "Variable 1", choices = variables, selected = input$var)
    })
    
    # Select 2nd variable if scatterplot is selected
    output$var2 <- renderUI({
        if (input$plotType == 'Scatterplot') {selectInput(inputId = "plotvar2", label = "Variable 2", choices = bubble_var)}
        else return(NULL)
    })
    
    output$tempplotyear<- renderUI({
        { selectInput(inputId = "plotyear", label = "Select year", choices =varYears, input$year)}
    })
    
    # Show radio buttons for discrete variables
    # Or show nothing if continuous plot selected
    output$radioButtons <- renderUI({
        if(input$plotType == "Box plot" | input$plotType == "Scatterplot" | input$plotType == "Column graph") {
            radioButtons(inputId = "group", label = "Grouping",
                         choices = c("Continent" = "continent",
                                     "Income Group" = "income_grp"))
        } 
        else return(NULL)
    })
    
    # ----------------------------------------------------------------------------------------
    output$plot <- renderPlotly({
        
        # Filter dataframe to selected year
        # Then create relevant titles
        
        # 2nd df for scatterplot
        req(input$plotyear)
        bubbleDF2 <-allData  %>%
            filter(year == input$plotyear)
        
        # main df
        plotDF <- worldForest %>%
            filter(year == input$plotyear) %>%
            na.omit()
        
        req(input$plotvar1)
        if (input$plotvar1 == "forestArea") {
            plottitle <- titles[1]
            plotDF$forestArea <- (plotDF$forestArea / 10)
            label = "Hectares (Tens of Thousands)"}
        
        else if (input$plotvar1 == "forestProp") {
            plottitle <- titles[2]
            label <- "% of Total Land Area"}
        
        else if (input$plotvar1 == "annualChange") {
            plottitle <- titles[3]
            label <- "Hectares (Thousands)"}
        
        else if (input$plotvar1 == "harvestRate") {
            plottitle <- titles[4]
            label <- "% of Total Forest Volume"}
        
        else if (input$plotvar1 == "plantExtent") {
            plottitle <- titles[5]
            label <- "Hectares (Thousands)"}
        
        else if (input$plotvar1 == "protectForest") {
            plottitle <- titles[6]
            label <- "Hectares (Thousands)"}
        
        
        # ----------------------------------------------------------------------------------------
        
        # X value for box plot and other plots
        req(input$plotType)
        if(input$plotType == "Box plot" | input$plotType == "Column graph") {
            if (input$group == "continent") {
                xaxis <- "continent"
                xlabel <- "Continent"
                plotDF <- plotDF %>% filter(continent != "Seven seas (open ocean)")
                yaxis <-input$plotvar1}
            
            else {xaxis <- "income_grp"
            xlabel <- "Income Group"
            yaxis <-input$plotvar1}
        }
        else {xaxis <-input$plotvar1}
        
        
        # y value limit for scatterplots
        if (input$plotType == "Scatterplot"){
            req(input$plotvar2)
            yaxis <- input$plotvar2}
        
        # add y axis limit for box plot
        if(input$plotType == "Box plot"){
            if (input$plotvar1 == "forestProp")+{ylimit = c(0,100)}
            else if (input$plotvar1 == "annualChange"){ylimit = c(-2500,2500)}
            else if (input$plotvar1 == "harvestRate"){ylimit = c(0,10)}
            else if (input$plotvar1 == "plantExtent"){ylimit = c(0,3000)}
            else if (input$plotvar1 == "protectForest"){ylimit = c(0,5000)}
            else {ylimit = c(0,25000)}
        }
        # add y axis limit for scatterplots
        if(input$plotType == "Scatterplot"){
            if (input$plotvar2 == "CO2")+{ylimit = c(0,500000)}
            else if (input$plotvar2 == "popCO2"){ylimit = c(0,25)}
            else if (input$plotvar2 == "gdpCO2"){ylimit = c(0,0.00001)}
        }
        #add x limit to plots
        if(input$plotType %in% c("Scatterplot", "Histogram", "Density plot")){
            if (input$plotvar1 == "forestProp")+{xlimit = c(0,100)}
            else if (input$plotvar1 == "annualChange"){xlimit = c(-2500,2500)}
            else if (input$plotvar1 == "harvestRate"){xlimit = c(0,10)}
            else if (input$plotvar1 == "plantExtent"){xlimit = c(0,3000)}
            else if (input$plotvar1 == "protectForest"){xlimit = c(0,5000)}
            else {xlimit = c(0,25000)}
        }
        
        # ----------------------------------------------------------------------------------------
        
        # Add plots
        
        # Box plot
        
        if(input$plotType == "Box plot"){
            
            ggplotly(ggplot(plotDF,
                            aes_string(x = xaxis,
                                       y = yaxis,
                                       fill = xaxis)) +
                         geom_boxplot() +
                         labs(title = plottitle,
                              x = xlabel,
                              y = label) +
                         ylim(ylimit) +
                         theme(legend.position = "none",
                               plot.title = element_text(hjust = 0.5),
                               plot.margin=unit(c(0, 1, 1 , 1), "cm")))
        }
        # Column graph
        else if(input$plotType == "Column graph"){
            ggplotly(ggplot(plotDF,
                            aes_string(x = xaxis,
                                       y = yaxis,
                                       fill = xaxis)) +
                         geom_col() +
                         labs(title = plottitle,
                              x = xlabel,
                              y = label) +
                         theme(legend.position = "none",
                               plot.title = element_text(hjust = 0.5),
                               plot.margin=unit(c(0, 1, 1 , 1), "cm")))
        }
        
        # Scatterplot
        else if(input$plotType == "Scatterplot"){
            ggplotly(ggplot(bubbleDF2,
                            aes_string(x = xaxis,
                                       y = yaxis,
                                       fill = input$group)) +
                         geom_point() +
                         labs(title = plottitle) +
                         ylim(ylimit) +
                         xlim(xlimit) +
                         theme(plot.title = element_text(hjust = 0.5),
                               plot.margin=unit(c(0, 1, 1 , 1), "cm")))
            
        }
        
        # Histogram
        else if(input$plotType == "Histogram"){
            ggplotly(ggplot(plotDF,
                            aes_string(x = xaxis)) +
                         geom_histogram() +
                         labs(title = plottitle,
                              x = label) +
                         xlim(xlimit) +
                         theme(legend.position = "none",
                               plot.title = element_text(hjust = 0.5),
                               plot.margin=unit(c(0, 1, 1 , 1), "cm")))
        }
        
        # Density plot
        else if(input$plotType == "Density plot"){
            ggplotly(ggplot(plotDF,
                            aes_string(x = xaxis)) +
                         geom_density() +
                         labs(title = plottitle,
                              x = label) +
                         xlim(xlimit) +
                         theme(legend.position = "none",
                               plot.title = element_text(hjust = 0.5),
                               plot.margin=unit(c(0, 1, 1 , 1), "cm")))
        }
        
    })
    
    # About Tab ##################################################################################
    
    # Add text to the About tab
    output$about <- renderText({ aboutText })
    
    
}
##########################################################################################################

shinyApp(ui, server)



