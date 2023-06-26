library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(svDialogs)
library(sp)

# Define UI ----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h3("Data"),
      fileInput("dataFileName", NULL, accept = ".csv"),

      br(),
      h3("Filter"),
      actionButton("undo", "undo last point"),
      actionButton("clear", "clear"),
      br(),
      br(),
      fileInput("filterFileName", "Load existing filter", accept = ".fil"),
      actionButton("saveFilter", "Save current filter")
                   #style="color: white; background-color: #337ab7; border-color: #2e6da4")
      
      ),
    
    mainPanel(
      textOutput("fileName"),
      
      plotOutput("plot",
                 click = clickOpts(id="plot_click")
                 ),
      p("Current:"),
      tableOutput("summaryTable"),
      actionButton("addToResBtn", "Add to results"),
      h4("Results: "),
      tableOutput("resultTable"),
      textOutput("dbg")
      
      
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  
  filt <- reactiveVal(tibble(x = c(), y = c()))
  
  observeEvent(filt(), {
    req(plotData())
    
    
    if (filt() %>% nrow() < 3){
      `mean area` <- plotData()$area %>% mean()
      `mean circ` <- plotData()$circ %>% mean()
      `% selected` <- 0
    }
    else if (filt() %>% nrow() > 2){
      plotData(
        plotData() %>% 
          mutate(selected = point.in.polygon(area, circ, filt()$x, filt()$y) %>% as.logical())
      )
    
      `mean area` <- plotData() %>% filter(selected == TRUE) %>% .$area %>% mean()
      `mean circ` <- plotData() %>% filter(selected == TRUE) %>% .$circ %>% mean()
      `% selected` <- plotData() %>% filter(selected == TRUE) %>% count() %>% as.integer()/nrow(plotData()) * 100
    }
    
    currentDataSummary(
      tibble(
        "mean area" = `mean area`,
        "mean circ" = `mean circ`,
        "% selected" = `% selected`
      )
    )
  })
  
  
  
  output$fileName <- renderText(input$dataFileName$name)
  
  plotData <- reactiveVal()
  resultData <- reactiveVal(tibble())
  currentDataSummary <- reactiveVal(tibble())
  
  
  
  observeEvent(input$dataFileName, {
    plotData({
      read_csv(input$dataFileName$datapath)# %>%
        #mutate(file = as.factor(file)) %>%
        #mutate(date = as.factor(date))%>%
        #mutate(cell_line = factor(cell_line, levels = c("238", "110"))) %>%
        #mutate(palbo = factor(palbo, levels = c("noPalbo", "Palbo"))) %>%
        #mutate(IAA = factor(IAA, levels = c("noIAA", "IAA"))) %>%
        #mutate(slide = as.factor(slide))
      
      })
  })
  
  output$summaryTable <- renderTable({
    currentDataSummary()
  })
  
  
  output$resultTable <- renderTable({
    resultData()
    })
  
  
  observeEvent(input$plot_click,{
    if (!is.null(input$plot_click)){
      filt(filt() %>% bind_rows(tibble(x = input$plot_click$x, y = input$plot_click$y)))
    }
  })
  
  
  observeEvent(input$undo, if (nrow(filt()) > 0) filt(filt() %>% .[-nrow(filt()),])
    )
  
  observeEvent(input$clear, {
    filt(tibble(x = c(), y = c()))
    })
  
  observeEvent(input$saveFilter, {
    
    if (!is.null(filt()) & filt() %>% nrow() > 2) {
        saveFilterFn <- dlg_save("filter.fil")$res
        if (length(saveFilterFn) > 0) {
          write_csv(filt(), paste0(saveFilterFn))
          }
    }
    else showModal(modalDialog(
      title = "Impossible to save the filter",
      "Incorrect filter (is empty or is just a dot or line)"
      ))
  })
  
  observeEvent(input$filterFileName, {
    filt(read_csv(input$filterFileName$datapath))
  })
  
  observeEvent(input$addToResBtn, {
    req(currentDataSummary)
    resultData({
      resultData() %>% bind_rows(currentDataSummary())
    })
  })
  
  output$plot <- renderPlot({
    req(plotData())

    p <- plotData() %>% ggplot() + theme_light(base_size = 18) + 
      geom_point(aes(x = area, y = circ), size = 0.3)
    
    
    if (filt() %>% nrow() == 1 ){
      p <- p + geom_point(data = filt(), aes(x, y), alpha = 0.3)
    } 
    else if (filt() %>% nrow() == 2 ){
      p <- p + geom_segment(aes(x = filt()$x[1], y = filt()$y[1], xend = filt()$x[2], yend = filt()$y[2]), color = "darkgrey", alpha = 0.5)
    }
    else if (filt() %>% nrow() > 2){
      
      p <- p + geom_point(aes(x = area, y = circ, color = selected), size = 0.3)+
        theme_light(base_size = 18) +
        theme(legend.position = "none") +
        scale_color_manual(values=c("black", "red")) +
        geom_polygon(data = filt(), aes(x, y), fill = "skyblue", color = "darkgrey", alpha = 0.3)
    }
    
    p
    })
}


# Run the app ----
shinyApp(ui = ui, server = server)


