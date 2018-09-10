library(shiny)
library(dplyr)
#library(ggtern)
library(gridExtra)
library(tricolore)
is_theme_complete <- function(x) isTRUE(attr(x, "complete"))

# Functions ---------------------------------------------------------------

# UI ----------------------------------------------------------------------

ui <- fluidPage(

  titlePanel(title = 'Tricolore: A flexible color scale for ternary compositions'),

  sidebarLayout(

    # INPUT
    sidebarPanel(width = 3,
                 sliderInput(inputId = 'hue', label = 'Hue', ticks = FALSE,
                             min = 0, max = 1, step = 0.1, value = 0.3),
                 sliderInput(inputId = 'chroma', label = 'Chroma', ticks = FALSE,
                             min = 0, max = 1, step = 0.1, value = 0.9),
                 sliderInput(inputId = 'lightness', label = 'Lightness', ticks = FALSE,
                             min = 0, max = 1, step = 0.1, value = 0.8),
                 sliderInput(inputId = 'contrast', label = 'Contrast', ticks = FALSE,
                             min = 0, max = 1, step = 0.1, value = 0.6),
                 sliderInput(inputId = 'spread', label = 'Spread',
                             min = 0.5, max = 2, step = 0.1, value = 1, ticks = FALSE),
                 sliderInput(inputId = 'breaks', label = 'Discretization', ticks = FALSE,
                             min = 2, max = 20, step = 1, value = 5),
                 radioButtons(inputId = 'center', label = 'Mean centering',
                              choices = list(No = 'No', Yes = 'Yes'),
                              selected = 'No'),
                 radioButtons(inputId = 'show_center', label = 'Show center',
                              choices = list(No = 'No', Yes = 'Yes'),
                              selected = 'No'),
                 radioButtons(inputId = 'show_data', label = 'Show data',
                              choices = list(No = 'No', Yes = 'Yes'),
                              selected = 'No')
    ),

    # OUTPUT
    mainPanel(verbatimTextOutput(outputId = 'call'), plotOutput(outputId = 'example'))
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {

  output$call <- renderText({
    paste0(
      "Tricolore(euro_sectors, " ,
      ', breaks = ', input$breaks,
      ', hue = ', input$hue,
      ', chroma = ', input$chroma,
      ', lightness = ', input$lightness,
      ', contrast = ', input$contrast,
      ', center = ', switch(input$center, No = 'rep(1/3,3)', Yes = 'NA'),
      ', spread = ', input$spread,
      ', show_data = ', switch(input$show_data, No = FALSE, Yes = TRUE),
      ', show_center = ', switch(input$show_center, No = FALSE, Yes = TRUE),
      ', legend = TRUE)'
    )
  })

  output$example <- renderPlot(width = 800, height = 700, {

    lga_ddd_cut  %>%
      group_by( lga, usage_category ) %>%
      summarise( ddd = sum(ddd ) ) %>%
      ungroup() %>%
      filter( as.numeric(usage_category) >= 2 ) %>%
      rbind( data.frame( lga=c(11860, 89399), usage_category=c( 'short-term'), ddd =0)) %>%
      rbind( data.frame( lga=c(11860, 89399), usage_category=c( 'long-term'), ddd =0)) %>%
      rbind( data.frame( lga=c(11860, 89399), usage_category=c( 'regular'), ddd =0)) %>% 
      mutate( lga = as.factor( lga )) %>%
      complete( lga, fill = list(n=0))  %>%
      mutate( lga=as.character(lga)) %>% 
      spread( usage_category, ddd ) %>%
      { . } -> df


   mixed =  df  %>%
      Tricolore( p1 = 'short-term', p2='long-term', p3='regular' ,
                breaks = input$breaks,
                hue = input$hue, chroma = input$chroma,
                lightness = input$lightness, contrast = input$contrast,
                center = switch(input$center, No = rep(1/3,3), Yes = NA),
                spread = input$spread,
                show_data = switch(input$show_data, No = FALSE, Yes = TRUE),
                show_center = switch(input$show_center, No = FALSE, Yes = TRUE),
                legend = TRUE)
   #browser()

    # customize legend
    lgnd <- mixed[['legend']] +
      labs(x = 'short-term', y = 'long-term', z = 'regular',
           caption = paste0('Short-term, long-term, and regular users\n',
                            switch(input$center, No = 'Colors show deviations from balanced composition',
                                   input$center, Yes = 'Colors show deviation from average composition'))) +
      theme(plot.background = element_rect(fill = NA, color = NA))

      # merge data and map
      df$srgb <- mixed[['hexsrgb']]

      base_map %>%
        left_join( df, by=c("LGA_CODE11" = "lga")) %>%
      tm_shape( ) + 
      tm_polygons( title = paste("Total DDD for each user type"),
                  col='srgb', 
                  ) %>%
      {.} -> map

      df %>%
        tm_shape( xlim=get_state_capital_xlim(2),
                ylim=get_state_capital_ylim(2) ) +
        tm_polygons(  legend.show=FALSE
                    ) +
        tm_layout(frame=FALSE, bg.color="lightgreen")  %>%
        {.} -> m_melbourne

      df %>% 
        tm_shape( xlim=get_state_capital_xlim(1),
                ylim=get_state_capital_ylim(1) ) +
        tm_polygons(  legend.show=FALSE
                    ) +
        tm_layout(frame=FALSE, bg.color="lightgreen")  %>%
        {.} -> m_sydney

    print(map)	
    # print insets
    #print(m_sydney, vp=viewport(x= .55, y= .5, width= 0.2, height= 0.2))
    #print(m_melbourne, vp=viewport(x= 0.27, y= 0.15, width= 0.2, height= 0.2))
    #is_theme_complete <- function(x) isTRUE(attr(x, "complete"))
    #print( lgnd, vp=viewport(x= 0.15, y= 0.85, width= 0.4, height= 0.4))

      }
    
    )

}

shinyApp(ui, server)
