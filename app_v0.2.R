# SHINY DASHBOARD - matuRity =========================================================================

## A tool to estimate date of maturity using vegetation index (VI)

## Tab package =========================================================================

### Packages library =================================================
library("shiny")
library("shinydashboard")
library("dashboardthemes")
library("shinyjs")
#library("shinyWidgets")

options(shiny.maxRequestSize=100000*1024^2)

packages_to_check <- c( "shiny", "shinydashboard", "dashboardthemes", "data.table", "DT","readr", "inspectdf", 
                          "raster", "rgdal", "ggplot2", "nadiv", "segmented", "tidyverse", "future", "future.apply", "lubridate", "reshape2")

package_tab <-     tabItem(
                      tabName = "Packages",
                           fluidPage(
                             titlePanel("Packages library"),
                           actionButton("check_button", "Check packages"),
                           actionButton("install_button", "Install and Load Packages"),
                           verbatimTextOutput("result")
                           )
)


## Tab Content =========================================================================

### Framework tab ----------------------------------------------

disclosure_tab <- tabItem(
  tabName = "Disclosure",
  img(src = "logo.png", height =200, width = 200,style="display: block; margin-left: auto; margin-right: auto;"),
  br(),
  p("matuRity is a R Shiny application developed to help researchers obtain estimates of days to plant maturity (DM) from the vegetation index."),
  p("Developed by Dr. Volpato and Dr. Francisco Gomez - Michigan State University"),
  br(),
  h4("Tutorial"),
  p("A comprehensive evaluation of the tool and a walk-through tutorial
      on how to use matuRity is ongoing", tags$a(href="https://github.com/volpatoo/matuRity","available soon")),
  br(),
  h4("Example files - available soon!"),
  p(downloadLink("sample1", "Data set sample 1"), br(),
    downloadLink("sample2", "Data set sample 2"), br(),
    downloadLink("sample3", "Data set sample 3")), br(),
  br(),
  h4("Citation"),
  p("Volpato, L., A. Dobbels, A. Borem, and A.J. Lorenz. 2021. Optimization of temporal UAS‐based imagery analysis to estimate plant maturity date for soybean breeding. Plant Phenome J. 4(1). doi: 10.1002/ppj2.20018."),
  br(),
  h4("Credits"),
  p("Leonardo Volpato (volpato1@msu.edu) and Francisco Gomez (gomezfr1@msu.edu)"),
  br(),
  h4("Disclaimer"),
  p("We welcome feedback and suggestions about the usefulness of the application and make no guarantee of the correctness, reliability, or utility 
    of the results if incorrect selections are made during the steps of MSE estimation. MaturRity is freely accessible, and the source code is hosted at https://github.com/volpatoo/matuRity")
  )


### Example files ----------------------------------------------

sample1_raw="https://github.com/volpatoo/matuRity/blob/a2c6c146bc03b6957a9cde521b3b35df2468ef44/Example_files/2020_SVREC_RGB_VIs.csv"
sample2_raw="https://github.com/volpatoo/matuRity/blob/a2c6c146bc03b6957a9cde521b3b35df2468ef44/Example_files/2021_SVREC_RGB_VIs.csv"
sample3_raw="https://github.com/volpatoo/matuRity/blob/a2c6c146bc03b6957a9cde521b3b35df2468ef44/Example_files/2022_SVREC_RGB_VIs.csv"

#### Vegetation index list ----------------------------------------------

vi_list <- list("GLI",
                "TGI",
                "HI")

### Upload file tab ----------------------------------------------

upload_tab <-     tabItem(tabName = "FileUpload",
                          fluidPage(
                            titlePanel("Data inspection"),
                            
                            sidebarLayout(
                              sidebarPanel(
                              fileInput(inputId = 'file', 'Choose file to upload:',
                                        accept = c('text/csv',
                                                   'text/comma-separated-values',
                                                   'text/tab-separated-values',
                                                   'text/plain','.csv','.tsv')),
                              checkboxInput("header", "Header", TRUE),
                              radioButtons("sep", "Separator",
                                           choices = c(Comma = ",",
                                                       Semicolon = ";",
                                                       Tab = "\t"),
                                           selected = ","),
                              radioButtons("quote", "Quote",
                                           choices = c(None = "",
                                                       "Double Quote" = '"',
                                                       "Single Quote" = "'"),
                                           selected = '"')),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Dataset", DT::dataTableOutput('contents')),
                                  tabPanel("Variable types", 
                                           verbatimTextOutput('tipos')),
                                  tabPanel("Missings", tableOutput('tbl_missings')),
                                  tabPanel("Var. categorical", tableOutput('tbl_cat')),
                                  tabPanel("Var. quantitative", tableOutput('tbl_quant')),
                                  tabPanel("Boxplot", plotOutput('box'))
                                ))),
                                box(title = strong("Warning:"), status = "warning", 
                                    solidHeader = TRUE,collapsible = TRUE,
                                    width = 4,
                                    h4("Please upload a file using the format:"),
                                    h4("First column:" , strong("Flight date - MM_DD_YYYY")),
                                    h4("Second column:", strong("Plot ID - 00000")),
                                    h4("VIs variable as:", strong("GLI_mean, TGI_median, etc,."))
                                )
                              
                            
                          )
)



### Maturity date ----------------------------------------------

mat_tab <-     tabItem(tabName = "maturity_date",
                          fluidPage(

                            titlePanel("Date to maturity estimation"),
                            
                               mainPanel(
                                tabsetPanel(
                                  tabPanel("Estimation", 
                                           br(),
                                      radioButtons("method_mod",'Method',choices = c("LOESS","SEG")), selected = "LOESS",
                                      selectInput("rgb_idx",'Vegetaion index',choices = vi_list),
                                      radioButtons("vi_method", "VI method",
                                                   choices = c(Mean = "mean",
                                                               Median = "median")),
                                      sliderInput('threshold',"Threshold ",0.01, min = 0.01, max = 1),
                                      dateInput("planting_date", "Select a planting date:",
                                                value = Sys.Date(), min = "2015-01-01", max = "2050-12-31"), 
                                      dateInput("AdjJul_31", "Select a adjusted date:",
                                                value = Sys.Date(), min = "2015-01-01", max = "2050-12-31"), 

                                      actionButton("go_button_par", "Parameters"),
                                                
                                    br(),
                                    br(),
                                    actionButton("go_button", "Estimate"),
                                    br(),
                                    br(),
                                    br(),
                                    
                                    # box(title = strong("Warning:"), status = "warning", 
                                    #     solidHeader = TRUE,collapsible = TRUE,
                                    #     width = 5,
                                    #     h4("Please run the command", strong('Parameters'), "first"),
                                    #     h4("Check the dataset at", strong('Dataset_Ajd'), "tap"),
                                    #     h4("Then run" , strong("Estimate")),
                                    #     
                                    # )
                                    ),
                                    
                                  tabPanel("Results", 
                                           
                                           conditionalPanel(condition = "input.method_mod == 'LOESS'",
                                               DT::dataTableOutput('mat_loess_cal'),
                                               conditionalPanel(condition = "output.mat_loess_info != ''",
                                                                h4("NA values check"),
                                                                verbatimTextOutput("mat_loess_info")),
                                               downloadButton("downloadData1", "Download Output")),
                                           
                                           conditionalPanel(condition = "input.method_mod == 'SEG'",
                                                            DT::dataTableOutput('mat_seg_cal'),
                                                            conditionalPanel(condition = "output.mat_seg_info != ''",
                                                                             h4("NA values check"),
                                                                             verbatimTextOutput("mat_seg_info")),
                                                            downloadButton("downloadData2", "Download Output")  )),
                                          
                                  tabPanel("Dataset_Ajd", 
                                           verbatimTextOutput("var_name"),
                                  DT::dataTableOutput('df2'),
                                  DT::dataTableOutput('df3'),
                                  uiOutput("message_na_data")
                                  ), 
                                  
                                  tabPanel("Parameters", 
                                           h4("Planting date"),
                                           verbatimTextOutput("planting_date"),
                                           h4("Adjusted date"),
                                           verbatimTextOutput("AdjJul_31"),
                                           # h4("Date list"),
                                           # verbatimTextOutput("date_list"),
                                           h4("IDplots:"),
                                           verbatimTextOutput("IDplots"),
                                           h4("Flights:"),
                                           verbatimTextOutput("flights"),
                                           # h4("Initial Flight"),
                                           # verbatimTextOutput("flight_ini"),
                                           # h4("Final Flight:"),
                                           # verbatimTextOutput("flight_end"),
                                           # h4("nrow:"),
                                           # verbatimTextOutput("nrow_Dates"),
                                           h4("Lengh days"),
                                           verbatimTextOutput("days")),
                                  
                                  tabPanel("Plots", 
                                           h4("Data distribution"),
                                           plotOutput("plot1"),
                                           h4("LOESS - Data distribution"),
                                           plotOutput("plot_loess"))
                                  
                                           )
                                )
                                  
                                )
                                

)
                              
                          
                          


### SideBar content =========================================================================

sideBar_content <- dashboardSidebar(
  shinyDashboardThemes(
    theme = "poor_mans_flatly"
  ),
  sidebarMenu(
    menuItem("Disclosure", tabName = "Disclosure"),
    menuItem("Install and Load Packages", tabName = "Packages"),
    menuItem("Upload file", tabName = "FileUpload"),
    menuItem("Maturity estimation", tabName = "maturity_date")
  )
)

### BODY content ------------------------------------------------------------------------------

body_content <- dashboardBody(
  tabItems(
    disclosure_tab,
    package_tab,
    upload_tab,
    mat_tab
  )
)

## UI =========================================================================

ui <-  dashboardPage(
  
  dashboardHeader(title = "matuRity"),
  ## Sidebar content
  sideBar_content,
  ## Body content
  body_content
)

## Aux Functions =========================================================================

## Maturity LOESS
mat_loess_est <- function(PlotID, data, nflights, ndays, vi_met, rgb_index, thresh) {
  
  setProgress(message = "Running LOESS analysis", detail = "", value = 0)
  p=0
  
  results <- data.frame()

  for (k in PlotID[-1]) {
    data_x <- as.numeric(unlist(data[,k]))
    nge_loess_loop <- loess(data_x ~ nflights)
    fitted.nge_loop <- predict(nge_loess_loop, ndays)
    list_date_pred <- approx(fitted.nge_loop, ndays, xout = thresh) 
    results <- rbind(results, data.frame(Plots_ID = k, Mat_LOESS = round(list_date_pred$y)))
    
    p= p+1
    setProgress(message = "Running LOESS analysis", detail = paste0("Processing plot ", k), value = as.numeric(p)/length(PlotID[-1]))
    
  }
  
  setProgress(message = "Analysis Complete", detail = "", value = 1)
  colnames(results) <- c("Plots_ID", paste0("Mat_LOESS",'_',rgb_index,'_',vi_met, '_', thresh))
  
  return(results)
}

## Maturity SEG
mat_seg_est <- function(PlotID, data, nflights, ndays, vi_met, rgb_index, thresh) {
  
  setProgress(message = "Running SEG analysis", detail = "", value = 0)
  
  lm_all<-list()
  p = 0
  
  for (j in PlotID[-1]){
    
    data_x <- as.numeric(unlist(data[,j]))
    mod<-lm(data_x ~ nflights) 
    attempts = 0
    if.false <- F
    
    while(if.false == F){
      attempts <- attempts + 1
        if(nrow(data) > 7 && attempts < 100){
          seg_loop<-try(segmented(mod, seg.Z = ~ nflights, npsi = 2, control = seg.control(n.boot = 50, random=T, tol=0.01)), silent = T)
          
          if("try-error" %in% class(seg_loop)) {
            seg_loop<-lm(data_x ~ nflights)
            slps <- (seg_loop$coefficients)[2]
            ncpt <- (seg_loop$coefficients)[1]
            DPM <- round((thresh - ncpt) / slps)
            lm_all[[j]] <- DPM
            
          } else if (!is.null(seg_loop$psi)) {
            
            slps <- slope(seg_loop)$nflights
            ncpt <- intercept(seg_loop)$nflights
            
            if ( rgb_index == "GLI" || rgb_index == "TGI" ) {
              slope <- min(slps[,1])
            } else if (rgb_index == "HI") {
              slope <- max(slps[,1])
            } else {
              print("Vegetation index not present in the formula")
            }
            slope_interc <- which(slps[,1] == slope)
            B1_interc <- ncpt[slope_interc,1]
            
            DPM <- round((thresh - B1_interc) / slope)
            lm_all[[j]] <- DPM
            
          } else { 
            seg_loop<-lm(data_x ~ nflights)
            slps <- (seg_loop$coefficients)[2]
            ncpt <- (seg_loop$coefficients)[1]
            DPM <- round((thresh - ncpt) / slps)
            lm_all[[j]] <- DPM
          }
          
          
        } else {
          
          seg_loop<-try(segmented(mod, seg.Z = ~ nflights, npsi = 1,  control = seg.control(n.boot = 50, random=T, tol=0.01)), silent = T)
          
          if("try-error" %in% class(seg_loop)) {
            seg_loop<-lm(data_x ~ nflights)
            slps <- (seg_loop$coefficients)[2]
            ncpt <- (seg_loop$coefficients)[1]
            DPM <- round((thresh - ncpt) / slps)
            lm_all[[j]] <- DPM
            
          } else if (!is.null(seg_loop$psi)) {
            
            slps <- slope(seg_loop)$nflights
            ncpt <- intercept(seg_loop)$nflights
            
            if ( rgb_index == "GLI" || rgb_index == "TGI" ) {
              slope <- min(slps[,1])
            } else if (rgb_index == "HI") {
              slope <- max(slps[,1])
            } else {
              print("Vegetation index not present in the formula")
            }
            slope_interc <- which(slps[,1] == slope)
            B1_interc <- ncpt[slope_interc,1]
            
            DPM <- round((thresh - B1_interc) / slope)
            lm_all[[j]] <- DPM
            
          } else { 
            seg_loop<-lm(data_x ~ nflights)
            slps <- (seg_loop$coefficients)[2]
            ncpt <- (seg_loop$coefficients)[1]
            DPM <- round((thresh - ncpt) / slps)
            lm_all[[j]] <- DPM
          }
        }
      
      
      if.false <- T	
     
    }
    p= p+1
    setProgress(message = "Running SEG analysis", detail = paste0("Processing plot ", j), value = as.numeric(p)/length(PlotID[-1]))
  }
    
  lm_all<-melt(lm_all)
  lm_all$PlotID <- PlotID[-1]
  
  lm_all <- lm_all %>%
    dplyr::select(-L1) %>% 
    dplyr::relocate(PlotID)
  
  colnames(lm_all)<- c("Plots_ID", paste0("Mat_SEG",'_',rgb_index,'_',vi_met, '_', thresh))
  
  setProgress(message = "Analysis Complete", detail = "", value = 1)

  return(lm_all)
  
}

## Plot figures 1
Plot_data_dist<- function(data, flights, threshold){
  data <- bind_cols(data, flights2 = flights)
  data <- data.frame(data)

  data<- data[,-1]
  data<- data %>% 
  pivot_longer(cols = -flights2,
               names_to = "PlotID",
               values_to = "VIs")

ggplot(data, aes(x=flights2, y = VIs, color = PlotID)) +
  geom_line() +
  theme_classic() + 
  labs(x = 'Days', y = 'VIs value') +
  theme(legend.position = "none") +
  geom_abline(intercept = threshold, slope = 0, linetype = "dashed") +
  scale_colour_grey(start = 0.1, end = 0.9)
}

## Plot figures 2 - LOESS
Plot_data_loess<- function(PlotID, data, nflights, ndays, thresh) {

  results_plot <- data.frame()
  
  for (k in PlotID[-1]) {
    data_x <- as.numeric(unlist(data[,k]))
    nge_loess_loop <- loess(data_x ~ nflights)
    fitted.nge_loop <- predict(nge_loess_loop, ndays)
    data_plot_loop <- cbind(ndays,k,fitted.nge_loop)
    results_plot <- rbind(results_plot, data.frame(Mat_LOESS = data_plot_loop))
  }
  
  results_plot$Mat_LOESS.fitted.nge_loop <- as.numeric(results_plot$Mat_LOESS.fitted.nge_loop)
  results_plot$Mat_LOESS.k <- as.character(results_plot$Mat_LOESS.k )
  results_plot$Mat_LOESS.ndays <- as.numeric(results_plot$Mat_LOESS.ndays )

ggplot(results_plot, aes(x=Mat_LOESS.ndays, y = Mat_LOESS.fitted.nge_loop, color = Mat_LOESS.k)) +
  geom_line() +
  theme_classic() + 
  labs(x = 'Days', y = 'VIs value') +
  theme(legend.position = "none") +
  geom_abline(intercept = thresh, slope = 0, linetype = "dashed") +
  scale_colour_grey(start = 0.1, end = 0.9)

}

## Check the dates
Flight_dates_fun <- function(data){
  data %>%
    dplyr::select(Flight_date) %>% 
    dplyr::mutate(Flight_date  = str_sub(Flight_date , 1, 10)) %>%
    dplyr::filter(nzchar(Flight_date))
}

## Variable names
var_name_fun <- function(rgb_idx, vi_method) {
  paste(rgb_idx, vi_method, sep = "_")
}

## Check the data set
data_ajd1 <- function(data, var_name) {
  if(colnames(data)[1] != "Flight_date"){
    data <- rename(data, Flight_date = colnames(data)[2])
  }
  if(colnames(data)[2] != "Plot_ID"){
    data <- rename(data, Plot_ID = colnames(data)[3])
  }
  
  data %>%
    dplyr::select(1:2, var_name) %>%
    pivot_wider(names_from = Plot_ID, values_from = var_name)
}


## date_list
date_list_fun <- function(nrow_dates, date, jul_date_pl, AdjJul_31){
  
  date_list2 <- list()
  for (i in 1:nrow_dates) {
    
    new_date <- as.Date(mdy(date[i,1]))
    
    flight_date <- yday(new_date)
    
    MAT_ADJ1 <- flight_date - jul_date_pl
    
    MAT_ADJ2 <- AdjJul_31 - jul_date_pl
    
    MAT_ADJ3 <- MAT_ADJ1 - MAT_ADJ2
    
    date_list2[[i]] <- MAT_ADJ3
    
    if (i == 1) {
      flight_ini <-MAT_ADJ3
      
    } else if ( i == nrow_dates) {
      flight_end <- MAT_ADJ3
    }
    
  }
  return(list(date_list2,flight_ini, flight_end))
}

## Server =========================================================================

server <- function(input, output, session) {
  
  mat_data_na <- reactive({
    
    if(is.null(data())) return("No data set found")
    
    if(is.numeric(data()[,var_name()])) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  
  # check if the packages are installed
  check_packages <- eventReactive(input$check_button, {
    missing_packages <- c()
    for (package in packages_to_check) {
      if (!require(package, character.only = TRUE)) {
        missing_packages <- c(missing_packages, package)
      }
    }
    if(length(missing_packages)>0) {
      return(paste("The following packages are not installed:", missing_packages))
    } else {
      return("All packages are already installed.")
    }
  })
  
  # install and load packages
  install_and_load_packages <- eventReactive(input$install_button, {
    missing_packages <- c()
    for (package in packages_to_check) {
      if (!require(package, character.only = TRUE)) {
        install.packages(package)
        library(package, character.only = TRUE)
        missing_packages <- c(missing_packages, package)
      }
    }
    if(length(missing_packages)>0) {
      return(paste("The following packages were installed and loaded:", missing_packages))
    } else {
      return("There are no packages to install. The packages were loaded")
    }
  })
  
## Check the data set
data <- reactive({
  if(is.null(input$file)){return()}
  read.csv(input$file$datapath, header = input$header, sep = input$sep, quote = input$quote)
})

## Variable names
var_name <- reactive({
  if(is.null(input$rgb_idx)){return()}
  if(is.null(input$vi_method)){return()}
  var_name_fun(input$rgb_idx, input$vi_method)
})


## Check the data set
data2 <- reactive({
  if(is.null(input$file)){return()}
  data_ajd1(data = read.csv(input$file$datapath, header = input$header, sep = input$sep, quote = input$quote),
            var_name = var_name()) 
})

## Check the dates
Dates <- reactive({
  if(is.null(data2())) return("No data set found")
  Flight_dates_fun(data = data2())
})

## jul_date_pl
jul_date_pl <- reactive({
  if(is.null(data2())) return("No data set found")
  #yday(input$planting_date)
  yday(as.Date(input$planting_date, format = "%Y-%m-%d"))
})

## AdjJul_31
AdjJul_31 <- reactive({
  if(is.null(data2())) return("No data set found")
  yday(as.Date(input$AdjJul_31, format = "%Y-%m-%d"))
})

## nrow Dates
nrow_Dates <- reactive({
  if(is.null(data2())) return("No data set found")
  nrow(Dates())
})

## date_list
date_list <- reactive({
  if(is.null(data2())) return("No data set found")
  
  date_list_fun(nrow_dates = nrow_Dates(), date = Dates(),
                jul_date_pl(), AdjJul_31())
})

## Initial flight
flight_ini <- reactive({
  if(is.null(data2())) return("No data set found")
  flight_ini()
})

## End flight 
flight_end <- reactive({
  if(is.null(data2())) return("No data set found")
  flight_end()
})

## Plot ID
IDplots <- reactive({
  if(is.null(data2())) return("No data set found")
  as.character(colnames(data2()))
})

## Flights
flights <- reactive({
  if(is.null(data2())) return("No data set found")
  as.numeric(unlist(date_list()[[1]]))
})

## days
days <- reactive({
  if(is.null(data2())) return("No data set found")
  seq(date_list()[[2]],date_list()[[3]],by=1)
})

## ncol data2
ncol_data <- reactive({
  if(is.null(data2())) return("No data set found")
  ncol(data2())
})


## Maturity using LOESS
mat_loess <- reactive({
  if(is.null(data2())) return("No data set found")
  
  withProgress(message = "Running LOESS method", {
    setProgress(message = "Initializing...")
    
    mat_loess_est(PlotID = IDplots(), 
                  data = data2(), 
                  nflights = flights(), 
                  ndays = days(), 
                  thresh = input$threshold,
                  rgb_index = input$rgb_idx,
                  vi_met = input$vi_method)
  })
  
})

## Plot figures 1
plot1 <- reactive({
  if(is.null(data2())) return("No data set found")
Plot_data_dist(data = data2(),
               flights = flights(),
               threshold = input$threshold)
})

## Plot figures 2
plot2 <- reactive({
  if(is.null(data2())) return("No data set found")
  Plot_data_loess(PlotID = IDplots(), 
                  data = data2(), 
                  nflights = flights(), 
                  ndays = days(), 
                  thresh = input$threshold)
  
})


mat_loess_info <- reactive({
  if(is.null(data2())) return("No data set found")
  if ( NA %in% mat_loess()[,2]) {
    #message("Consider changing the threshold - NAs found:")
    print(sum(is.na(mat_loess())))
  }
  })

## Maturity using SEG
mat_seg <- reactive({
  if(is.null(data2())) return("No data set found")
  
  withProgress(message = "Running SEG method", {
    setProgress(message = "Initializing...")
  
  mat_seg_est(PlotID = IDplots(), 
                data = data2(), 
                nflights = flights(), 
                ndays = days(), 
                thresh = input$threshold,
                rgb_index = input$rgb_idx,
                vi_met = input$vi_method)
  })
  
})

mat_seg_info <- reactive({
  
  if ( NA %in% mat_seg()[,2]) {
    #message("Consider changing the threshold - NAs found:")
    print(sum(is.na(mat_seg())))
  }
})


mat_data_na <- reactive({
  
  if(is.numeric(data()[,var_name()])) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})
## Outputs =======================================================================

output$message_na_data <- renderUI({
  if (!mat_data_na())
    shiny::showModal(modalDialog(easyClose = TRUE, 
      title = "Warning",
      "The column you selected contains non-numeric values or data set has not been uploaded. 
      Please select another vegetation index or upload a file using the correct format."
    ))
  else
    HTML("")
})



    # Check the R packages 
  observeEvent(input$check_button, {
    output$result <- renderPrint(check_packages())
  })
  
  observeEvent(input$install_button, {
    output$result <- renderPrint(install_and_load_packages())
  })


   # Check the data outputs
    output$contents <- DT::renderDataTable({
    DT::datatable(data())
  })

  observeEvent(input$file, {
    
    df<-read.csv(input$file$datapath)
    
    output$tipos<-renderPrint({
      str(df)
    })
    
    output$tbl_missings<-renderTable({
      df.missings<-inspect_na(df)
      colnames(df.missings)<-c("Variable", "Qtd.", "%")
      df.missings
    })
    
    output$tbl_cat<-renderTable({
      df.cat<-inspect_cat(df)
      df.cat<-df.cat[,1:4]
      colnames(df.cat)<-c("Variable", "N. de categories",
                          "Most freq. Cat", "%Most freq. Cat")
      df.cat
    })
    
    output$tbl_quant<-renderTable({
      df.quant<-inspect_num(df)
      df.quant<-df.quant[,1:8]
      colnames(df.quant)<-c("Variable","Min", "Q1", "Mediana",
                            "Mean", "Q3", "Máx", "SD")
      df.quant
    })
    
    output$box<-renderPlot({
      df.box<-df[,sapply(df, is.numeric)]
      df.box<-scale(df.box)
      boxplot(df.box)
    })
    
  })

  # Download example files
  output$sample1 <- downloadHandler(
    filename = function() {
      paste("2020_SVREC_RGB_VIs.csv", sep="")
    },
    content = function(file) {
      write.csv(read_csv(url(sample1_raw)), file)
    }
  )

  output$sample12 <- downloadHandler(
    filename = function() {
      paste("2021_SVREC_RGB_VIs.csv", sep="")
    },
    content = function(file) {
      write.csv(read_csv(url(sample2_raw)), file)
    }
  )

  output$sample3 <- downloadHandler(
    filename = function() {
      paste("2022_SVREC_RGB_VIs.csv", sep="")
    },
    content = function(file) {
      write.csv(read_csv(url(sample3_raw)), file)
    }
  )
  
    ## Variable names
    output$var_name <- renderPrint({
      var_name()
    })
    
    observeEvent(input$print_var, {
      print(var_name())
    })
  
    output$df2 <- DT::renderDataTable({
      if(is.null(input$file)){return()}
      DT::datatable(data2())
    })

    # Create reactive value to keep track of whether "Parameters" button has been clicked
    parameters_clicked <- reactiveVal(FALSE)    

    ## Parameters
   observeEvent(input$go_button_par, {
     
     parameters_clicked(TRUE)
     
     output$df3 <- DT::renderDataTable({
       if(is.null(input$file)){return()}
       DT::datatable(Dates())
     })
     
     ## jul_date_pl
     output$planting_date <- renderPrint({
       jul_date_pl()
     })
     
     ## AdjJul_31
     output$AdjJul_31 <- renderPrint({
       AdjJul_31()
     })
     
     # ## nrow Dates
     # output$nrow_Dates <- reactive({
     #   nrow_Dates()
     # })
  
   output$date_list <- renderPrint({
     date_list()[[1]]
   })
   
   # ## Initial flight
   # output$flight_ini <- renderPrint({
   #   if(is.null(data2())) return("No data set found")
   #   date_list()[[2]]
   # })
   # 
   # ## End flight 
   # output$flight_end <- renderPrint({
   #   if(is.null(data2())) return("No data set found")
   #   date_list()[[3]]
   # })
   
   ## Plot ID
   output$IDplots <- renderPrint({
     head(IDplots())
   })
   
   ## Flights
   output$flights <- renderPrint({
     flights()
    })
   
   ## days
   output$days <- renderPrint({
     days()
   })
   
   output$plot1 <- renderPlot({
     plot1()
   })
   
   })
   
##### Maturity estimation LOESS ######

  
  ## Maturity estimation
observeEvent(input$go_button, {
  
  # Check if "Parameters" button has been clicked
  if (!parameters_clicked()) {
    
    # Display error message if "Parameters" button has not been clicked
    tryCatch(
      stop("Please click on 'Parameters' button first."),
      error = function(e) {
        showNotification(paste("Run Parameter first and check the Dataset_Adj tab"), type = "error")
      }
    )
    
  } else {
    
  if(is.null(input$file)){return()}
  
    if(input$method_mod == "LOESS") {
  
     output$mat_loess_cal <- DT::renderDataTable({
     DT::datatable(mat_loess()) 
       })
     
     output$plot_loess <- renderPlot({
       plot2()
     })

     output$mat_loess_info <- renderPrint({ mat_loess_info() })

     output$downloadData1 <- downloadHandler(
       filename = function() { paste0("mat_loess", "_",
                                      rgb_index = input$rgb_idx,"_",
                                      vi_met = input$vi_method, "_",
                                      input$threshold,"_",
                                      ".csv") },
       content = function(file) { write.csv(mat_loess(), file, row.names = FALSE) }
       
     )
     
     # # If no error occurs, display a message to the user
     # showNotification("LOESS method ran successfully!", type = "message")
     
         }
     
    else {
      if(input$method_mod == "SEG") {
        
        output$mat_seg_cal <- DT::renderDataTable({
          DT::datatable(mat_seg()) 
        })
        
        output$mat_seg_info <- renderPrint({ mat_seg_info() })
        
        output$downloadData2 <- downloadHandler(
          filename = function() { paste0("mat_seg", "_",
                                         rgb_index = input$rgb_idx,"_",
                                         vi_met = input$vi_method, "_",
                                         input$threshold,"_",
                                         ".csv") },
          content = function(file) { write.csv(mat_seg(), file, row.names = FALSE) }
        )
          
          # # If no error occurs, display a message to the user
          # showNotification("SEG method ran successfully!", type = "message")
        
    
      }
    }
   
    # Display output
    showNotification("Estimation Running at Results tab.", type = "message")
    
  } 
  
  })

}  
 
   

# Run shiny app ---------------------------------------------------------------------------
shinyApp(ui, server)


