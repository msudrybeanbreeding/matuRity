# SHINY DASHBOARD - matuRity =========================================================================

## ChatGPT commands
# # install.packages("pak")
# pak::pak("JamesHWade/gpttools")
# # Enable repository from jameshwade
# options(repos = c(
#   jameshwade = "https://jameshwade.r-universe.dev",
#   CRAN = "https://cloud.r-project.org"
# ))
# # Download and install gpttools in R
# install.packages("gpttools")
# # Browse the gpttools manual pages
# help(package = "gpttools")
#Sys.setenv(OPENAI_API_KEY = "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")
## A tool to estimate date of maturity using vegetation index (VI)

## Tab package =========================================================================

### Packages library =================================================
#load the shiny library
library("shiny")
#load the shinydashboard library
library("shinydashboard")
#load the dashboardthemes library
library("dashboardthemes")
#load the shinyjs library
library("shinyjs")
library(data.table)
library(DT)
library(readr)
library(inspectdf)
library(raster)
library(rgdal)
library(ggplot2)
library(nadiv)
library(segmented)
library(tidyverse)
library(lubridate)
library(reshape2)


#library("shinyWidgets")

# set the max size of the file that can be uploaded to the server
options(shiny.maxRequestSize=100000*1024^2)

# list of packages that will be installed if they are not already installed
packages_to_check <- c("data.table", "DT","readr", "inspectdf", 
                          "raster", "rgdal", "ggplot2", "nadiv", "segmented", "tidyverse", "lubridate", "reshape2")

package_tab <-     tabItem( #create a tab item
                      tabName = "Packages", #name the tab item
                           fluidPage( #create a fluid page
                             titlePanel("Packages library"), #create a title panel
                           actionButton("check_button", "Check packages"), #create a button to check packages
                           actionButton("install_button", "Install and Load Packages"), #create a button to install and load packages
                           verbatimTextOutput("result") #create a text output
                           ) #close fluid page
)

#create a server function to check packages, the function will be called when the check_button is clicked
server <- function(input, output) {
  observeEvent(input$check_button, {
    output$result <- renderPrint({
      installed.packages()
    })
  })
}

#create a server function to install and load packages, the function will be called when the install_button is clicked
server <- function(input, output) {
  observeEvent(input$install_button, {
    output$result <- renderPrint({
      install.packages("ggplot2")
      library(ggplot2)
    })
  })
}



## Tab Content =========================================================================

### Framework tab ----------------------------------------------

disclosure_tab <- tabItem(
  tabName = "Disclosure",
  img(src = "logo.png", height =200, width = 200,style="display: block; margin-left: auto; margin-right: auto;"),
  br(),
  p("matuRity is a R Shiny application developed to help researchers obtain estimates of days to plant maturity (DM) from the vegetation index."),
  p("Developed and maintained by", tags$a(href="https://github.com/volpatoo/resume/blob/main/LV_resume2.pdf","Dr. Volpato")),
  br(),
  h4("Tutorial"),
  p("A comprehensive evaluation of the tool and a walk-through tutorial
      on how to use matuRity is ongoing", tags$a(href="https://github.com/msudrybeanbreeding?tab=repositories","available soon")),
  br(),
  h4("Example files - available soon!"),
  p(downloadLink("sample1", "Data set sample 1"), br(),
    downloadLink("sample2", "Data set sample 2"), br(),
    downloadLink("sample3", "Data set sample 3")), br(),
  br(),
  h4("Citation"),
  p("Volpato, L., A. Dobbels, A. Borem, and A.J. Lorenz. 2021. Optimization of temporal UAS‐based imagery analysis to estimate plant maturity date for soybean breeding. Plant Phenome J. 4(1). doi: 10.1002/ppj2.20018."),
  br(),
  h5("Dataset avalaible at:"),
  p("The datasets included were collected from the UMN soybean breeding project sites in planting seasons 2018 and 2019.", tags$a(href="https://doi.org/10.25739/bc4f-x382","Soybean dataset")),         
  br(),
  h4("Credits"),
  p("Leonardo Volpato (volpato1@msu.edu) and Francisco Gomez (gomezfr1@msu.edu) - Michigan State University (MSU)"),
  br(),
  h4("Disclaimer"),
  p("We welcome feedback and suggestions about the usefulness of the application and make no guarantee of the correctness, reliability, or utility of 
    the results if incorrect selections are made during the steps of DM estimation. MaturRity is freely accessible, and the source code will be hosted soon at", 
    tags$a(href="https://github.com/msudrybeanbreeding?tab=repositories","MSU Dry Bean Breeding Program GitHub page")),
    
  )


### Example files ----------------------------------------------

sample1_raw="https://raw.githubusercontent.com/msudrybeanbreeding/Suppl_files/main/Example_files-matuRity/2020_SVREC_RGB_VIs.csv"
sample2_raw="https://raw.githubusercontent.com/msudrybeanbreeding/Suppl_files/main/Example_files-matuRity/2021_SVREC_RGB_VIs.csv"
sample3_raw="https://raw.githubusercontent.com/msudrybeanbreeding/Suppl_files/main/Example_files-matuRity/2022_SVREC_RGB_VIs.csv"

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
  # function to calculate the date of maturity using LOESS analysis
  # PlotID: vector of plot IDs
  # data: data frame of the data
  # nflights: vector of number of flights
  # ndays: vector of number of days
  # vi_met: name of the vegetation index method
  # rgb_index: name of the RGB index
  # thresh: threshold of the vegetation index
  
  # set progress bar
  setProgress(message = "Running LOESS analysis", detail = "", value = 0)
  
  # initialize progress bar
  p=0
  
  # initialize results data frame
  results <- data.frame()
  
  # loop through each plot
  results <- data.frame()
    
    # extract the data for the current plot

    # run LOESS analysis
  for (k in PlotID[-1]) {
    
    # predict the date of maturity
    data_x <- as.numeric(unlist(data[,k]))
    
    # extract the date of maturity
    nge_loess_loop <- loess(data_x ~ nflights)
    
    # add the date of maturity to the results data frame
    fitted.nge_loop <- predict(nge_loess_loop, ndays)
    list_date_pred <- approx(fitted.nge_loop, ndays, xout = thresh) 
    # update progress bar
    results <- rbind(results, data.frame(Plots_ID = k, Mat_LOESS = round(list_date_pred$y)))
    
    p= p+1
    setProgress(message = "Running LOESS analysis", detail = paste0("Processing plot ", k), value = as.numeric(p)/length(PlotID[-1]))
    
  # set progress bar to complete
  }
  
  # set column names of the results data frame
  
  setProgress(message = "Analysis Complete", detail = "", value = 1)
  colnames(results) <- c("Plots_ID", paste0("Mat_LOESS",'_',rgb_index,'_',vi_met, '_', thresh))
  
  # return the results data frame
  return(results)
}


## Maturity SEG
mat_seg_est <- function(PlotID, data, nflights, ndays, vi_met, rgb_index, thresh) {
  
  # set progress bar
  setProgress(message = "Running SEG analysis", detail = "", value = 0)
  
  # create empty list
  lm_all<-list()
  
  # set progress bar to 0
  p = 0
  
  # loop through each plot
  for (j in PlotID[-1]){
    
    # extract data for each plot
    data_x <- as.numeric(unlist(data[,j]))
    
    # create linear model
    mod<-lm(data_x ~ nflights) 
    
    # set attempts to 0
    attempts = 0
    
    # set if.false to false
    if.false <- F
    
    # while if.false is false
    while(if.false == F){
      attempts <- attempts + 1
 #if the number of rows in the data is greater than 7 and the number of attempts is less than 100, then run the segmented function
       if(nrow(data) > 7 && attempts < 100){
          #run the segmented function with the following parameters
          seg_loop<-try(segmented(mod, seg.Z = ~ nflights, npsi = 2, control = seg.control(n.boot = 50, random=T, tol=0.01)), silent = T)
          
          #if the segmented function returns an error, then run the lm function
          if("try-error" %in% class(seg_loop)) {
            #run the lm function with the following parameters
            seg_loop<-lm(data_x ~ nflights)
            #create a variable called slps that is equal to the second coefficient of the lm function
            slps <- (seg_loop$coefficients)[2]
            #create a variable called ncpt that is equal to the first coefficient of the lm function
            ncpt <- (seg_loop$coefficients)[1]
            #create a variable called DPM that is equal to the difference between the threshold and the intercept divided by the slope
            DPM <- round((thresh - ncpt) / slps)
            #add the DPM variable to the lm_all list
            lm_all[[j]] <- DPM
            
          #if the segmented function does not return an error and the psi variable is not null, then run the following code
          } else if (!is.null(seg_loop$psi)) {
            
            #create a variable called slps that is equal to the slope of the segmented function
            slps <- slope(seg_loop)$nflights
            #create a variable called ncpt that is equal to the intercept of the segmented function
            ncpt <- intercept(seg_loop)$nflights
            
            #if the vegetation index is GLI or TGI, then set the slope variable equal to the minimum slope
            if ( rgb_index == "GLI" || rgb_index == "TGI" ) {
              slope <- min(slps[,1])
            #if the vegetation index is HI, then set the slope variable equal to the maximum slope
            } else if (rgb_index == "HI") {
              slope <- max(slps[,1])
            #if the vegetation index is not GLI, TGI, or HI, then print an error message
            } else {
              print("Vegetation index not present in the formula")
            }
            #create a variable called slope_interc that is equal to the index of the slope variable
            slope_interc <- which(slps[,1] == slope)
            #create a variable called B1_interc that is equal to the intercept at the slope_interc index
            B1_interc <- ncpt[slope_interc,1]
            
            #create a variable called DPM that is equal to the difference between the threshold and the intercept divided by the slope
            DPM <- round((thresh - B1_interc) / slope)
            #add the DPM variable to the lm_all list
            lm_all[[j]] <- DPM
            
          #if the segmented function does not return an error and the psi variable is null, then run the following code
          } else { 
            #run the lm function with the following parameters
            seg_loop<-lm(data_x ~ nflights)
            #create a variable called slps that is equal to the second coefficient of the lm function
            slps <- (seg_loop$coefficients)[2]
            #create a variable called ncpt that is equal to the first coefficient of the lm function
            ncpt <- (seg_loop$coefficients)[1]
            #create a variable called DPM that is equal to the difference between the threshold and the intercept divided by the slope
            DPM <- round((thresh - ncpt) / slps)
            #add the DPM variable to the lm_all list
            lm_all[[j]] <- DPM
          }
          
        } else {
          
          seg_loop<-try(segmented(mod, seg.Z = ~ nflights, npsi = 1,  control = seg.control(n.boot = 50, random=T, tol=0.01)), silent = T) # try to run the segmented function
          
          if("try-error" %in% class(seg_loop)) { # if the segmented function fails, run a linear model
            seg_loop<-lm(data_x ~ nflights) # run a linear model
            slps <- (seg_loop$coefficients)[2] # get the slope of the linear model
            ncpt <- (seg_loop$coefficients)[1] # get the intercept of the linear model
            DPM <- round((thresh - ncpt) / slps) # calculate the DPM
            lm_all[[j]] <- DPM # save the DPM
            
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
  #convert the lm_all list to a dataframe
  lm_all<-melt(lm_all)
  #add the PlotID column to the lm_all dataframe
  lm_all$PlotID <- PlotID[-1]
  #select the PlotID column and move it to the first column
  lm_all <- lm_all %>%
    dplyr::select(-L1) %>% 
    dplyr::relocate(PlotID)
  
  colnames(lm_all)<- c("Plots_ID", paste0("Mat_SEG",'_',rgb_index,'_',vi_met, '_', thresh))
  
  setProgress(message = "Analysis Complete", detail = "", value = 1)

  return(lm_all)
  
}


## Plot figures 1
Plot_data_dist<- function(data, flights, threshold){
  #bind the data and flights together
  data <- bind_cols(data, flights2 = flights)
  #convert the data to a dataframe
  data <- data.frame(data)
  #remove the first column

  #pivot the data to get the VIs values in one column
  data<- data[,-1]
  data<- data %>% 
  pivot_longer(cols = -flights2,
               names_to = "PlotID",
               values_to = "VIs")
#plot the data
ggplot(data, aes(x=flights2, y = VIs, color = PlotID)) +
  geom_line() +
  theme_classic() + 
  labs(x = 'Days', y = 'VI value') +
  theme(legend.position = "none") +
  geom_abline(intercept = threshold, slope = 0, linetype = "dashed") +
  scale_colour_grey(start = 0.1, end = 0.9)
}


## Plot figures 2 - LOESS
# function to plot the loess curve
Plot_data_loess<- function(PlotID, data, nflights, ndays, thresh) {
  # create an empty data frame

  results_plot <- data.frame()
  # loop through the plot ID
  
    # extract the data for each plot
  for (k in PlotID[-1]) {
    # fit the loess curve
    data_x <- as.numeric(unlist(data[,k]))
    # predict the loess curve
    nge_loess_loop <- loess(data_x ~ nflights)
    # create a data frame with the predicted values
    fitted.nge_loop <- predict(nge_loess_loop, ndays)
    # add the data frame to the empty data frame
    data_plot_loop <- cbind(ndays,k,fitted.nge_loop)
    results_plot <- rbind(results_plot, data.frame(Mat_LOESS = data_plot_loop))
  }
  # convert the data frame to numeric
  
  results_plot$Mat_LOESS.fitted.nge_loop <- as.numeric(results_plot$Mat_LOESS.fitted.nge_loop)
  results_plot$Mat_LOESS.k <- as.character(results_plot$Mat_LOESS.k )
  results_plot$Mat_LOESS.ndays <- as.numeric(results_plot$Mat_LOESS.ndays )

ggplot(results_plot, aes(x=Mat_LOESS.ndays, y = Mat_LOESS.fitted.nge_loop, color = Mat_LOESS.k)) +
  geom_line() +
  theme_classic() + 
  labs(x = 'Days', y = 'VI value') +
  theme(legend.position = "none") +
  geom_abline(intercept = thresh, slope = 0, linetype = "dashed") +
  scale_colour_grey(start = 0.1, end = 0.9)

}


## Check the dates
Flight_dates_fun <- function(data){
  # select the column Flight_date
  data %>%
    dplyr::select(Flight_date) %>% 
    # remove the time from the date
    dplyr::mutate(Flight_date  = str_sub(Flight_date , 1, 10)) %>%
    # remove the NA values
    dplyr::filter(nzchar(Flight_date))
}

## Variable names
var_name_fun <- function(rgb_idx, vi_method) {
  # paste the two strings together
  paste(rgb_idx, vi_method, sep = "_")
}


## Check the data set
data_ajd1 <- function(data, var_name) {
  # check if the first column is "Flight_date"
  if(colnames(data)[1] != "Flight_date"){
    # if not, rename the first column to "Flight_date"
    data <- rename(data, Flight_date = colnames(data)[2])
  }
  # check if the second column is "Plot_ID"
  if(colnames(data)[2] != "Plot_ID"){
    # if not, rename the second column to "Plot_ID"
    data <- rename(data, Plot_ID = colnames(data)[3])
  }
  
  # select the first two columns and the variable of interest
  data %>%
    dplyr::select(1:2, var_name) %>%
    # pivot the data frame
    pivot_wider(names_from = Plot_ID, values_from = var_name)
}


## date_list
date_list_fun <- function(nrow_dates, date, jul_date_pl, AdjJul_31){
  
  #create a list to store the dates
  date_list2 <- list()
  
  #loop through the dates
  for (i in 1:nrow_dates) {
    
    #convert the date to a date format
    new_date <- as.Date(mdy(date[i,1]))
    
    #get the day of the year
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
      data <- read_csv(url(sample1_raw))
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$sample2 <- downloadHandler(
    filename = function() {
      paste("2021_SVREC_RGB_VIs.csv", sep="")
    },
    content = function(file) {
      data <- read_csv(url(sample2_raw))
      write.csv(data, file, row.names = FALSE)
    }
  )

  output$sample3 <- downloadHandler(
    filename = function() {
      paste("2022_SVREC_RGB_VIs.csv", sep="")
    },
    content = function(file) {
      data <- read_csv(url(sample3_raw))
      write.csv(data, file, row.names = FALSE)
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


