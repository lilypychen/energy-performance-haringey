library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(sf)
library(lubridate)
library(tidyr)
library(ggplot2)
#library(shinylive)
#library(httpuv)

#setwd("~/Documents/Application_form/NGO_projects/cleaned/") 
## change working directory for local usage. 

#unzip("epc_postcode.gpkg.zip", overwrite = TRUE, 
#      junkpaths = FALSE, exdir=".")
sdf <- st_read("epc_postcode.gpkg") # load postcode ge
lsoa <- st_read("lsoa.shp") 

rec_sums = read.csv("recommendations_ratio.csv") 
c2 = read.csv("epc_stack.csv")  
c3 = read.csv( "epc_stack_potential.csv") 
cb = read.csv( "cb.csv") 
pt = read.csv("pt.csv")

total <- sdf %>% 
  st_drop_geometry() %>% 
  count(lsoa11cd, name = "total_counts")

# Calculate counts for each selected EPC rating
each_rate <- sdf %>% 
  st_drop_geometry() %>% 
  #filter(CURRENT_ENERGY_RATING %in% input$epc) %>%
  count(lsoa11cd, CURRENT_ENERGY_RATING, name = "counts") 

lsoa_c <- count(st_drop_geometry(sdf), lsoa11cd, name = "lsoa_counts")
each_rate_p <- sdf %>% 
  st_drop_geometry() %>% 
  #filter(CURRENT_ENERGY_RATING %in% input$epc) %>%
  count(lsoa11cd, POTENTIAL_ENERGY_RATING, name = "counts_p") %>% 
  left_join(lsoa_c, by = "lsoa11cd") %>% 
  mutate(ratio_p = round(100*counts_p/lsoa_counts, 1))

c2 = c2[c2$CURRENT_ENERGY_RATING != "INVALID!", ]


ui <- fluidPage(
  
  tags$style(HTML("
  html, body, .container-fluid, .sidebarLayout, .sidebarPanel {
    height: 100% !important;
    margin: 0;
    padding: 0;
  }
  .sidebarPanel {
    width: 25% !important; /* Adjust width based on screen size */
    height: 100% !important;
    overflow-y: auto; /* Allow scrolling if the content is too long */
  }
  
  .mainPanel {
    height: 100% !important;
  }
  
  
  ")),
  
  titlePanel("  Energy Efficiency & Social Housing in Haringey"), 
  
  sidebarLayout(
    
    sidebarPanel = sidebarPanel(
      #HTML("<b>------- In LSOA scale &/or POSTCODE ------------</b><br>"), 
      
      # varSelectInput 
      checkboxGroupInput("epc",  "Select Current Energy Efficiency Ratings (EER):", 
                         choices = c("A", "B", "C", "D", "E", "F", "G"), 
                         selected = NULL, inline = TRUE),
      
      sliderInput("imd", "Select Index of Multiple Deprivation (IMD) Decile Range:", 
                  min = 1, max = 9, value = c(1, 2), 
                  ticks = T, round = 0, width = "100%"),  # 2
      
      #sliderInput("years", "Select Year Range for EPC Inspection:", 
      #            min = 2006,  max = 2024, value = c(2015, 2024), 
      #            ticks = F, round = 0, sep = "", width = "100%"), 
      
      HTML("<b>Notes:</b><br>
            - HH = Households, which refers to each Energy Performance Certificate (EPC) inspected property.<br>
            - EER 'A' denotes the highest efficiency.<br>
            - IMD Decile 1 indicates the highest level of deprivation.<br>
            - Circle markers' radius sizes increase in intervals of 5.<br><br>"),
      
      fluidRow(
        column(12, plotOutput("leftSideBarPlot", height = "400px")), 
        column(12, plotOutput("socialHousingImpactPlot", height = "400px"))
        # recommendations and LED impact
      ), 
      
      HTML("<b>Data Sources:</b><br>
        - CDRC <a href='https://data.cdrc.ac.uk/dataset/index-multiple-deprivation-imd' target='_blank'>Index of Multiple Deprivation</a><br>
        - EERs & Improvements: <a href='https://epc.opendatacommunities.org/#register' target='_blank'>EPC Data</a> (2006 Oct - 2024 Jun)"
      )
      
      
    ), 
    mainPanel = mainPanel(
      leafletOutput(outputId = 'map'), 
      
      fluidRow(
        column(12, plotOutput("socialHousingEnergyPlot")) # current/potential energy ratings
        
      ),
      
      fluidRow(
        column(5, plotOutput("leftBarPlot", height = "410px")),
        column(6, plotOutput("rightBarPlot", height = "410px"))
      )
    )
  )
)

server <- function(input, output){ 
  
  
  q <- reactive({ 
    sdf %>% 
      st_drop_geometry() %>% 
      filter(
        #(INSPECTION_DATE >= as.Date(paste0(input$years[1], "-01-01")) & #  lubridate::ymd(input$years[1] + 1, truncated = 2L) 
        #   INSPECTION_DATE <= as.Date(paste0(input$years[2], "-12-31")) ) &  
          (SOA_decile >= input$imd[1] & SOA_decile <= input$imd[2])) %>% 
      
      count(lsoa11cd, POTENTIAL_ENERGY_RATING, name = "counts_p") %>% 
      group_by(lsoa11cd) %>% 
      mutate(
        selected_total_counts_p = sum(counts_p))
    
  })
  
  df = reactive({ 
    req(input$epc)
    
    c <- sdf %>% 
      st_drop_geometry() %>% 
      filter(
        #(INSPECTION_DATE >= as.Date(paste0(input$years[1], "-01-01")) & #  lubridate::ymd(input$years[1] + 1, truncated = 2L) 
        #  INSPECTION_DATE <= as.Date(paste0(input$years[2], "-12-31")) ) &  
          
          (SOA_decile >= input$imd[1] & SOA_decile <= input$imd[2]) & 
          #(CURRENT_ENERGY_RATING >= input$epc[1] & CURRENT_ENERGY_RATING <= input$epc[2])
          (CURRENT_ENERGY_RATING %in% input$epc)
      ) %>% 
      count(lsoa11cd, name = "rating_counts") 
    
    c %>% 
      left_join(q(), by = "lsoa11cd") %>% 
      left_join(total, by = "lsoa11cd") %>% 
      mutate(ratio = round(rating_counts/total_counts*100, 1))#, 
    #             ratio_change = ratio_p - ratio) 
  })
  
  pt_f <- reactive({
    pt %>% 
      filter(
        #(INSPECTION_DATE >= input$years[1] & INSPECTION_DATE <= input$years[2]) & 
          (SOA_decile >= input$imd[1] & SOA_decile <= input$imd[2]) & 
          (CURRENT_ENERGY_RATING %in% input$epc)
      ) 
  })
  
  pt_led <- reactive({
    pt_f() %>% 
      count(POSTCODE, led, lat, lon) #, not count EE difference
            # CURRENT_ENERGY_RATING, POTENTIAL_ENERGY_RATING)
  })
  
  pt_social <- reactive({
    pt_f() %>% 
      count(POSTCODE, social_rental, lat, lon) #, 
            #CURRENT_ENERGY_RATING, POTENTIAL_ENERGY_RATING)
  })
  
  
  
  
  output$map = renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -.11, lat = 51.59, zoom = 12.5) %>% # 1136 
      addFullscreenControl() %>%  # Add fullscreen control to the map
      addPolygons(data = lsoa, color = "black", stroke = 0, opacity = 0, group = "Boundary of Haringey") %>% 
      
      addLayersControl(
        overlayGroups = c("Boundary of Haringey", "LSOA by EER & IMD", "Selected LSOA Boundaries", 
                          "Community Building", "Improvement Measures (LED)", "Tenure Type"), 
        options = layersControlOptions(collapsed = TRUE)) 
    
  })
  
  observe({
    df_data <- df()
    pt_led_ <- pt_led()
    pt_social_ <- pt_social()
    
    epc_pal <- colorNumeric("YlOrRd", domain = c(0, 100)) 
    
    # Create custom popup content
    df_data <- df_data %>%
      group_by(lsoa11cd) %>%
      reframe(
        total_counts = first(total_counts),
        ratio = first(ratio),
        
        popup_content = paste0("<strong>LSOA Code: ", lsoa11cd, "</strong><br>",
                               "Total Certificates Assessed (LSOA): ", total_counts, 
                               "<br>Selected certificate count: ", rating_counts, " (", ratio, "%)<br>")#,

      ) %>% 
      left_join(lsoa, by = "lsoa11cd") %>% 
      st_as_sf(crs = st_crs(lsoa))
    
    leafletProxy("map", data = df_data) %>%
      clearGroup(c("LSOA by EER & IMD", 
                   "Selected LSOA Boundaries", 
                   "Community Building", 
                   "Improvement Measures (LED)", 
                   "Tenure Type")
                     ) %>% 
      clearControls() %>%  # Clear existing legends and other controls 
      addPolygons(
        fillColor = ~colorNumeric("YlOrRd", domain = df_data$ratio)(ratio), 
        fillOpacity = 1,
        weight = 0.5,
        smoothFactor = 0.2, 
        group = "LSOA by EER & IMD", 
        popup = ~popup_content
      ) %>% 
      
      addPolygons(
        stroke = T, 
        color = ~colorNumeric("YlOrRd", domain = df_data$ratio)(ratio), 
        fillOpacity = 0,
        weight = ~3 + ratio / 20, # Make boundary width proportional to the ratio
        smoothFactor = 0.2, 
        group = "Selected LSOA Boundaries", 
        popup = ~popup_content
      ) %>% 
      
      addCircleMarkers(data = pt_led_, lng = ~jitter(lon, 20), lat = ~jitter(lat, 20),
                       #popup = ~paste("Postcode: ", POSTCODE),  
                       label = ~paste0("HH: ", as.character(n), 
                                       "; ", POSTCODE, "; Recommendations: ", 
                                       ifelse(led == TRUE, "LED", "Others")
                                       ),
                       radius = ~findInterval(n, seq(0, 500, 5)) + 1, 
                       weight = 4,  
                       color = ~ifelse(led == TRUE, "#FF00FF", "#006400"), #  "#1F78B4",
                       opacity = 0, fillOpacity = 0.9, 
                       group = "Improvement Measures (LED)") %>%
      
      addCircleMarkers(data = pt_social_, lng = ~jitter(lon, 20), lat = ~jitter(lat, 20), 
                       label = ~paste0("HH: ", as.character(n), 
                                       "; ", POSTCODE, "; Tenure: ", 
                                       ifelse(social_rental == "Yes", "Social Rental", "Others")
                       ),
                       radius = ~findInterval(n, seq(0, 500, 5)) + 1, 
                       weight = 4, 
                       color = ~ifelse(social_rental == "Yes", "yellow", "navy"), #  "#33A02C",  
                       opacity = 0, fillOpacity = 0.9, 
                       group = "Tenure Type") %>%
      
      addMarkers(data = cb, lng = ~lon, lat = ~lat, 
                 popup = ~building_name, 
                 group = "Community Building") %>%
      
      addLegend(
        "bottomleft", 
        # pal = epc_pal, values = c(0, 100), # for fixed scale
        pal = colorNumeric("YlOrRd", domain = df_data$ratio), values = df_data$ratio, 
        title = "Selected</br>HH %", 
        opacity = .8
      ) %>% 
      
      # Legend for LED improvements
      addLegend(
        "bottomright",
        title = "Improvement<br>Types",
        colors = c("#FF00FF", "#006400"),
        labels = c("LED", "Others"),
        opacity = 0.8
      ) %>%
      
      # Legend for Social Housing
      addLegend(
        "bottomright",
        title = "Tenure Type",
        colors = c("yellow", "navy"),
        labels = c("Social Rental", "Owned/<br>Private Rental/<br>Unknown"),
        opacity = 0.8
      )
  })
  energy_colors <- c("A" = "#006400", "B" = "#228B22", "C" = "#ADFF2F", "D" = "#FFD700", "E" = "#FFA500", "F" = "#FF4500", "G" = "#FF0000")
  
  output$leftSideBarPlot <- renderPlot({
    # Create the ggplot
    ggplot(slice_head(rec_sums, n = 9), 
           aes(x = reorder(IMPROVEMENT_SUMMARY_TEXT, -ratio),
               y = ratio, fill = IMPROVEMENT_SUMMARY_TEXT)) +
      geom_bar(stat = "identity",  show.legend = FALSE) +
      labs(
        title = "Top 9 Energy Performance Improvement Measures",
        x = NULL, #"Improvement Measures",
        y = "Households Receiving Improvements (%)"
      ) + coord_flip() + 
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
      ) +
      scale_fill_brewer(palette = "Paired") + 
      geom_text(aes(label = paste0(ratio, "%")), 
                hjust = 0, vjust = 0.5, size = 5, 
                color = "black",
                nudge_y = 1.5) + 
      geom_text(aes(label = c(
        "Wall Insulation",            # Internal or external wall insulation
        "Energy Efficient Lighting",     # Low energy lighting for all fixed outlets
        "Solar Panels",                  # Solar photovoltaic panels, 2.5 kWp
        "Floor Insulation",              # Floor insulation
        "Solar Heating",                 # Solar water heating
        "Replace Windows",               # Replace single glazed windows
        "Condensing Boiler",             # Replace boiler with new condensing boiler
        "Loft\n Insulation",               # Increase loft insulation to 270 mm
        "Draught\n Proofing"               # Draught proofing
        
      )), 
                hjust = 1, vjust = 0.5, size = 5, 
                color = "black", nudge_y = -1) 
  })
  output$leftBarPlot <- renderPlot({
    ggplot(filter(c3, year >= 2008), aes(x = as.factor(year),
                                              y = ratio, fill = POTENTIAL_ENERGY_RATING)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = energy_colors) +
      scale_y_continuous(
        breaks = seq(0, 100, by = 25),  # Define breaks from 0% to 100% by 10%
        labels = paste0(seq(0, 100, by = 25), "%")  # Label these breaks with percentage signs
      ) +
      labs(
        title = "Potential EERs by Year", 
        x = NULL,  # No x-axis label
        y = NULL,  # No y-axis label
        fill = NULL  # No legend title
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none", 
        
        legend.title = element_blank()  # Remove legend title
      )
  })
  
  output$rightBarPlot <- renderPlot({
    ggplot(filter(c2, year >= 2008), aes(x = as.factor(year), y = ratio, fill = CURRENT_ENERGY_RATING)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = energy_colors) +
      scale_y_continuous(
        breaks = seq(0, 100, by = 25),  # Define breaks from 0% to 100% by 10%
        labels = paste0(seq(0, 100, by = 25), "%")  # Label these breaks with percentage signs
      ) +
      labs(
        title = "Current EERs by Year",  # No title
        x = NULL,  # No x-axis label
        y = NULL,  # No y-axis label
        fill = NULL  # No legend title
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12), 
        legend.title = element_blank()  # Remove legend title
      )
  })
  
  # Add code for rendering the social housing and energy ratings bar chart
  output$socialHousingEnergyPlot <- renderPlot({
    pt_long_energy <- pt %>%
      mutate(social_rental = ifelse(social_rental == "Yes", "Yes", "No/unknown")) %>%
      pivot_longer(cols = c(CURRENT_ENERGY_RATING, POTENTIAL_ENERGY_RATING), 
                   names_to = "rating_type", 
                   values_to = "energy_rating")
    
    ggplot(pt_long_energy, aes(x = energy_rating, fill = social_rental)) + 
      geom_bar(position = "stack", aes(y = ..count..)) +  
      facet_wrap(~ rating_type, labeller = as_labeller(c(CURRENT_ENERGY_RATING = "Current", 
                                                         POTENTIAL_ENERGY_RATING = "Potential"))) + 
      labs(x = "Energy Rating (A to G)", y = "Households", fill = "Social Rental Housing", 
           title = "\nCurrent & Potential EERs by Tenure", size = 7) + 
      theme_minimal() + 
      scale_fill_manual(values = c("Yes" = "#1F78B4", "No/unknown" = "grey")) + 
      theme(axis.text.x = element_text(angle = 0, size = 5, hjust = 1), 
            legend.position = "top", 
            plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
  })

  # Add code for rendering the social housing impact on EPC recommendations and LED installations
  output$socialHousingImpactPlot <- renderPlot({
    pt_long <- pt %>%
      mutate(social_rental = ifelse(social_rental == "Yes", "Yes", "No/unknown"),
             led = ifelse(led == TRUE, "LED\nRequired", "Adequate\nLighting\nEfficiency"),
             recommendations = ifelse(recommendations == TRUE, "Improvement\nRequired", "Adequate\nEfficiency")) %>%
      pivot_longer(cols = c(recommendations, led),
                   names_to = "group", 
                   values_to = "value")
    
    ggplot(pt_long, aes(x = value, fill = social_rental)) + 
      geom_bar(position = "dodge") + 
      facet_wrap(~ group, scales = "free_x", 
                 labeller = as_labeller(c(recommendations = "All Recommendations", 
                                          led = "LED Lighting Recommendations"))) + 
      labs(x = NULL, y = "Households", fill = "Social Rental Housing", 
           title = "\nEPC Recommendations by Tenure") + 
      theme_minimal() + 
      scale_fill_manual(values = c("Yes" = "#1F78B4", "No/unknown" = "lightgrey")) + 
      theme(legend.position = "bottom",
            #axis.text.x = element_blank(), 
            plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) 
  })

}


shinyApp(ui = ui, server = server)

