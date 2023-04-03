#
# This is a Shiny web application for COVID-19 epidemiology analysis.
# Data are provided by ECDC.
#
# Author: Charles NUTTENS
#

#setwd("C:/Users/NUTTEC/OneDrive - Pfizer/10 - R & SHINY/Covid-19/COVID19_EPIDEMIOLOGY")

# Libraries
library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(zoo)

##########################################################################################################################

# Read datasets (i.e. ECDC database, lockdown list)
tibble.full.ecdc <- read_excel("data/COVID-19-geographic-disbtribution-worldwide-2022-07-22.xlsx")
tibble.full.ld <- read_excel("data/lockdown_25032020.xlsx")

# Remove unnecessary columns (i.e. "Day", "Month", "Year")
tibble.clean.ecdc <- select(tibble.full.ecdc,-c(2:4, 8))

# Reorder date
tibble.order.ecdc <- arrange(tibble.clean.ecdc, countriesAndTerritories, dateRep)

# Convert into 'data.frame'
df.ecdc <- as.data.frame(tibble.order.ecdc)
df.ld <- as.data.frame(tibble.full.ld)

# Calculate data World
world.split <- split(df.ecdc, df.ecdc$dateRep)
#world.df <- data.frame(t(sapply(world.split, function(i) {apply(i[c(2, 3, 5)], 2, function(j) {sum(j, na.rm = TRUE)})})))

# Re-organize World 'data.frame'
#world.df <- rownames_to_column(world.df, "dateRep")
#world.df[,1] <- as.POSIXct(world.df[,1], tz = "GMT")
#world.df["countriesAndTerritories"] <- "World"
#world.df["countryterritoryCode"] <- "WLD"
#world.df <- select(world.df, dateRep, cases, deaths, countriesAndTerritories, popData2020, countryterritoryCode)
#world.df$dateRep <- as.character(world.df$dateRep)

df.ecdc <- select(df.ecdc, -continentExp)

# Add World 'data.frame' to master 'data.frame'
#df.ecdc <- bind_rows(df.ecdc, world.df)

##########################################################################################################################

# Define 'navbarPage' page
ui <- navbarPage("COVID-19 Epidemiology",
                 
                 # 1st 'tabPanel': Analysis of single country
                 tabPanel("Country Analysis",
                          
                           # 'sideBar' layout with a left 'sidebarPanel' and a 'mainPanel'
                           sidebarLayout(
                              
                               # 'sidebar Panel' with selectors
                               sidebarPanel(
                                   width = 2,
                                   
                                   # Start of sidePanel for 1st tabPanel
                                   conditionalPanel(condition = "input.tabselected == 1",
                                       
                                       # Selection of country
                                       selectInput(
                                           "tab1.country",
                                           label = "Country",
                                           choices = unique(df.ecdc$countriesAndTerritories),
                                           selected = "Italy",
                                           multiple = FALSE,
                                           selectize = FALSE
                                       ),
                                       
                                       
                                       # # Selection of date range
                                       # dateRangeInput("tab1.range",
                                       #     label = "Date Range",
                                       #     start = min(df.ecdc$dateRep),
                                       #     end = max(df.ecdc$dateRep),
                                       #     min = min(df.ecdc$dateRep),
                                       #     max = max(df.ecdc$dateRep),
                                       #     format = "yyyy-mm-dd",
                                       #     startview = "month",
                                       #     weekstart = 1,
                                       #     language = "en",
                                       #     separator = " to "),
                                       
                                       br(),
                                       
                                       # Selection of cases/incidence
                                       radioButtons(
                                           "tab1.unit",
                                           label = "Y axis unit",
                                           choices = list("Number of cases" = 1,"Incidence (/100,000)" = 2),
                                           selected = 1
                                       ),
                                       
                                       br(),
                                       
                                       # Y scale adjustment
                                       strong("Y scale adjustment"),
                                       
                                       # Indicate of cumulative data
                                       checkboxInput("tab1.cumulative",
                                                     label = "Cumulative data",
                                                     value = FALSE),
                                       
                                       # Indicate of logarithmic Y scale
                                       checkboxInput("tab1.log",
                                                     label = "Logarithmic scale",
                                                     value = FALSE),
                                       
                                       br(),
                                       
                                       # Graphical elements
                                       strong("Graphical elements"),
                                       
                                       # Plot data points
                                       checkboxInput("tab1.points",
                                                     label = "Data points",
                                                     value = TRUE),
                                       
                                       # Plot lockdown graphical elements
                                       checkboxInput("tab1.lockdown",
                                                     label = "Lockdown information",
                                                     value = FALSE),
                                       
                                       br(),
                                       
                                       # Smoothing
                                       strong("Smooting"),
                                       
                                       # LOESS regression
                                       checkboxInput("tab1.loess",
                                                     label = "Local polynomial regression",
                                                     value = TRUE),
                                       
                                       # Control of LOESS smoothing span
                                       sliderInput(
                                           "tab1.span",
                                           label = h5("Smoothing coefficient"),
                                           min = 0.1,
                                           max = 1,
                                           step = 0.1,
                                           value = 0.3
                                       ),
                                       
                                       br(),
                                       
                                       # LM regression
                                       checkboxInput("tab1.lm",
                                                     label = "Linear regression",
                                                     value = FALSE),
                                       
                                       # Control of LM number of days
                                       sliderInput(
                                           "tab1.lmdays",
                                           label = h5("Number of days"),
                                           min = 3,
                                           max = 7,
                                           step = 1,
                                           value = 5
                                       ),
                                       
                                   # End of sidePanel for 1st tabPanel
                                   ),
                                   
                                   # Start of sidePanel for 2nd tabPanel
                                   conditionalPanel(condition = "input.tabselected == 2",
                                       
                                       # Display the name of the selected country 
                                       strong("Country"),
                                       verbatimTextOutput("tab1.country.print2"),
                                       
                                       br(),
                                       
                                       # Selection of daily cases/relatige change
                                       radioButtons(
                                           "tab1.unit2",
                                           label = "Y axis unit",
                                           choices = list("Number of cases" = 1, "Percentage" = 2),
                                           selected = 1
                                       ),
                                       
                                       br(),
                                       
                                       # Selection of daily cases/relatige change
                                       radioButtons(
                                           "tab1.source2",
                                           label = "Source of data",
                                           choices = list("Reported data" = 1, "Fitting curve" = 2),
                                           selected = 2
                                       ),
                                       
                                       verbatimTextOutput("tab1.fitcurve2"),
                                       
                                       # Graphical elements
                                       strong("Graphical elements"),
                                       
                                       # Plot lockdown graphical elements
                                       checkboxInput("tab1.lockdown2",
                                                     label = "Lockdown information",
                                                     value = FALSE),
                                       
                                   # End of sidePanel for 2nd tabPanel
                                   ),
                                   
                                   # Start of sidePanel for 3rd tabPanel
                                   conditionalPanel(condition = "input.tabselected == 3",

                                       # Display the name of the selected country
                                       strong("Country"),
                                       verbatimTextOutput("tab1.country.print3"),

                                       br(),

                                       # Y scale adjustment
                                       strong("Y scale adjustment"),

                                       # Indicate of cumulative data
                                       checkboxInput("tab1.cumulative3",
                                                     label = "Cumulative data",
                                                     value = FALSE),


                                       # Indicate of logarithmic Y scale
                                       checkboxInput("tab1.log3",
                                                     label = "Logarithmic scale",
                                                     value = FALSE),

                                       br(),

                                       # Graphical elements
                                       strong("Graphical elements"),

                                       # Plot lockdown graphical elements
                                       checkboxInput("tab1.lockdown3",
                                                     label = "Lockdown information",
                                                     value = FALSE),
                                       
                                       # Smoothing
                                       strong("Smooting"),
                                       
                                       # LOESS regression
                                       checkboxInput("tab1.loess3",
                                                     label = "Local polynomial regression",
                                                     value = TRUE),
                                       
                                       # Control of LOESS smoothing span
                                       sliderInput(
                                           "tab1.span3",
                                           label = h5("Degree of smoothing"),
                                           min = 0.1,
                                           max = 3,
                                           step = 0.1,
                                           value = 0.2
                                       )

                                   # End of sidePanel for 3rd tabPanel
                                   ),
                                   
                                   # # Start of sidePanel for 4th tabPanel
                                   # conditionalPanel(condition = "input.tabselected==4",
                                   # 
                                   #      # Display the name of the selected country
                                   #      strong("Country"),
                                   #      verbatimTextOutput("tab1.country.print")
                                   # 
                                   # # End of sidePanel for 4th tabPanel
                                   # ),
                                   # 
                                   # # Start of sidePanel for 5th tabPanel
                                   # conditionalPanel(condition = "input.tabselected==5",
                                   # 
                                   #      # Display the name of the selected country
                                   #      strong("Country"),
                                   #      verbatimTextOutput("tab1.country.print")
                                   # 
                                   # # End of sidePanel for 5th tabPanel
                                   # )

                               # End of 'sidebar Panel'
                               ), 
                              
                              # 'mainPanel' with 5 'tabPanel'
                              mainPanel(width = 10, tabsetPanel(
                                  
                                  # 1st 'tabPanel'
                                  tabPanel("Epidemic evolution", value=1, 
                                      plotOutput("tab1.epidemic",
                                                 width = "100%",
                                                 height = "800px")),
                                  
                                  # 2nd 'tabPanel'
                                  tabPanel("Daily variation", value=2,
                                      plotOutput("tab1.daily",
                                                 width = "100%",
                                                 height = "800px")),
                                  
                                  # 3rd 'tabPanel'
                                  tabPanel("Case fatality rate", value=3,
                                           plotOutput("tab1.cfr",
                                                      width = "100%",
                                                      height = "800px")),
                                  
                                  # 4th'tabPanel'
                                  tabPanel("Information", value=4,
                                      verbatimTextOutput("tab1.info")),
                                  
                                  # 5th 'tabPanel'
                                  tabPanel("Data", value=5,
                                       verbatimTextOutput("tab1.data")),
                                  id = "tabselected"
                              ))
                          )), 
                 
                 ############################################################################################
                 
                 tabPanel("Country Comparison")
)

##########################################################################################################################

# Define graphics
server <- function(input, output) {

    # List of colos (x10)
    color <- c("#E31A1C",    # red
               "green4",     # green
               "dodgerblue2",# blue
               "#FF7F00",    # orange
               "#6A3D9A",    # purple
               "black",      # black
               "gold1",      # yellow
               "hotpink",    # pink
               "cyan",       # cyan
               "aquamarine"  # blue/green
               )
    
    #Print name of country in country selector
    output$tab1.country.print2 <- renderText(input$tab1.country)
    output$tab1.country.print3 <- renderText(input$tab1.country)
    output$tab1.fitcurve2 <- renderText(paste("Smoothing coefficient:", input$tab1.span))
    
    # Tab1 graphic: Epidemique evolution
    output$tab1.epidemic <- renderPlot({
        
        # Vector of country
        vector.country <- input$tab1.country
        
        # Data.frame of selected countries
        df.country <- filter(df.ecdc,
                             countriesAndTerritories %in% vector.country)
                             #,dateRep %in% seq(as.POSIXct(input$tab1.range[1]), as.POSIXct(input$tab1.range[2]), by = "day"))
        
        # List of selected countries
        list.country <- split(df.country, df.country$countriesAndTerritories)
        
        # Recalculate data according to selected parameters
        if(input$tab1.cumulative == TRUE) {
            y.cases <- cumsum(list.country[[vector.country]]$cases)
            y.deaths <- cumsum(list.country[[vector.country]]$deaths)
        } else {
            y.cases <- list.country[[vector.country]]$cases
            y.deaths <- list.country[[vector.country]]$deaths
        }
        
        if(input$tab1.unit == 2) {
            y.cases <- (y.cases/list.country[[vector.country]]$popData2018)*100000
            y.deaths <- (y.deaths/list.country[[vector.country]]$popData2018)*100000
        }
        
        # Define x and y
        x <- list.country[[vector.country]]$dateRep
        y <- y.cases
        
        # Define graphic area
        par(mar=c(4.1, 4.1, 2.1, 4.1))
        
        # Plot empty graphic
        plot(x,y,
             log = ifelse(input$tab1.log == TRUE, "y", ""),
             type = "n",
             ylab = NA,
             xlab = NA,
             xaxt="n",
             yaxt="n")
        
        # Add x axis (bottom)
        ticks <- seq(x[1], x[length(x)], length = 20)
        axis.POSIXct(side = 1, at = ticks, format = format(ticks, "%b-%d"), las=2)
        
        # Add y cases axis (left)
        axis(2)
        mtext(ifelse(input$tab1.unit == 1,
                     ifelse(input$tab1.cumulative == FALSE, "Daily number of cases", "Cumulative number of cases"),
                     ifelse(input$tab1.cumulative == FALSE, "Incidence rate (/100,000/day)", "Cumulative incidence rate (/100,000)")
                     ),
              side = 2, line = 3, font = 2)
        
        # Add y deaths axis (right)
        axis(4, col.axis = "red")
        mtext(ifelse(input$tab1.unit == 1,
                     ifelse(input$tab1.cumulative == FALSE, "Daily number of deaths", "Cumulative number of deaths"),
                     ifelse(input$tab1.cumulative == FALSE, "Mortality rate (/100,000/day)", "Cumulative mortality rate (/100,000)")
        ),
        side = 4, line = 3, font = 2, col = "red")
        
        # Add lockdown information
        if(input$tab1.lockdown == TRUE) {
            df.ld.country <- filter(df.ld, Country %in% vector.country)
            abline(v=df.ld.country[[2]], lty=4, col = "grey")
            text(df.ld.country[[2]], max(y.cases, na.rm = T), paste(df.ld.country[[3]], "lockdown"), col = "grey", pos = 2, srt = 90)
        }
        
        # Add cases and deaths data points
        if(input$tab1.points == TRUE) {
            points(x, y.cases, pch = 3)
            points(x, y.deaths, pch = 4, col = "red")
        }
        
        # LOESS smoothing span
        span.factor <- input$tab1.span
        
        # Calculate LOESS
        cases.loess <- loess(y.cases~as.numeric(x), span = span.factor)
        cases.loess.pred <- predict(cases.loess, se=TRUE)
        
        deaths.loess <- loess(y.deaths~as.numeric(x), span = span.factor)
        deaths.loess.pred <- predict(deaths.loess, se=TRUE)

        # Add cases and deaths LOESS fitted curve and 95% Confidence interval
        if(input$tab1.loess == TRUE) {
            lines(x, cases.loess.pred$fit, col="grey50")
            lines(x, deaths.loess.pred$fit, col="pink")
            
            lines(x, cases.loess.pred$fit-1.96*cases.loess.pred$se, lty = 2, col = "grey50")
            lines(x, cases.loess.pred$fit+1.96*cases.loess.pred$se, lty = 2, col = "grey50")
            
            lines(x, deaths.loess.pred$fit-1.96*deaths.loess.pred$se, lty = 2, col = "pink")
            lines(x, deaths.loess.pred$fit+1.96*deaths.loess.pred$se, lty = 2, col = "pink")
        }

        # Add cases and deaths LM
        if(input$tab1.lm == TRUE) {
            
            # LM parameter
            days <- input$tab1.lmdays
            
            # LM cases
            cases.lm <- lm(tail(y.cases, days) ~ tail(x, days))
            cases.lm.pred <- predict(cases.lm, interval="confidence")
            lines(tail(x, days), cases.lm.pred[,1], col = "grey50")
            lines(tail(x, days), cases.lm.pred[,2], lty = 2, col = "grey50")
            lines(tail(x, days), cases.lm.pred[,3], lty = 2, col = "grey50")
            
            # LM deaths
            deaths.lm <- lm(tail(y.deaths, days) ~ tail(x, days))
            deaths.lm.pred <- predict(deaths.lm, interval="confidence")
            lines(tail(x, days), deaths.lm.pred[,1], col = "pink")
            lines(tail(x, days), deaths.lm.pred[,2], lty = 2, col = "pink")
            lines(tail(x, days), deaths.lm.pred[,3], lty = 2, col = "pink")
            
            # Add legend (86400 second in 1 day)
            if(input$tab1.cumulative == FALSE) {
                legend("left",
                       bty = "n",
                       title = "Linear regression estimate",
                       lty = c(1, 1),
                       col = c("grey50", "pink"),
                       legend = if(input$tab1.unit == 1) {
                                    c(paste(round(cases.lm$coefficients[2]*86400, 1), "cases/day"), paste(round(deaths.lm$coefficients[2]*86400, 1), "deaths/day"))
                                } else {
                                    c(paste(round(cases.lm$coefficients[2]*86400, 2), "cases/100,000/day"), paste(round(deaths.lm$coefficients[2]*86400, 2), "deaths/100,000/day"))
                                }
                )
            }
        }
        
        # Legend
        legend("topleft",
            bty = "n",
            pch = c(3, NA, NA, NA, 4, NA, NA, NA, NA),
            lty = c(NA, 1, 2, NA, NA, 1, 2, NA, 4),
            col = c("black", "grey50", "grey50", NA, "red", "pink", "pink", NA, "grey"),
            legend = c(
                "Reported case",
                "Non-linear/Linear regression fitting",
                "Confidence interval",
                "",
                "Reported deaths",
                "Non-linear/Linear regression fitting",
                " Confidence interval",
                "",
                "Lockdown information"
                ),
            text.col = c("black", "grey50", "grey50", NA, "red", "pink", "pink", NA, "grey")
        )
        
    })
    
    ############################################################################################
    
    # Tab1 graphic: Daily Evolution
    output$tab1.daily <- renderPlot({
        
        # Vector of country
        vector.country <- input$tab1.country
        
        # Data.frame of selected countries
        df.country <- filter(df.ecdc, countriesAndTerritories %in% vector.country)
        
        # List of selected countries
        list.country <- split(df.country, df.country$countriesAndTerritories)
        
        # Recalculate data according to selected parameters
        y.cases <- list.country[[vector.country]]$cases
        y.deaths <- list.country[[vector.country]]$deaths
        
        x <- list.country[[vector.country]]$dateRep
        
        # LOESS smoothing span
        span.factor <- input$tab1.span
        
        
        if(input$tab1.source2 == 2) {
        # Calculate LOESS
            cases.loess <- loess(y.cases~as.numeric(x), span = span.factor)
            y.cases2 <- predict(cases.loess, se=TRUE)$fit
            
            deaths.loess <- loess(y.deaths~as.numeric(x), span = span.factor)
            y.deaths2 <- predict(deaths.loess, se=TRUE)$fit
        } else {
            y.cases2 <- y.cases
            y.deaths2 <- y.deaths
        }
        
        if(input$tab1.unit2 == 1) {
        # Define x and y for number of cases and deaths
            y.cases.n <- c(NA, rollapply(y.cases2, 2, by = 1, function(i) {i[2]-i[1]}))
            y.deaths.n <- c(NA, rollapply(y.deaths2, 2, by = 1, function(i) {i[2]-i[1]}))
            table <- matrix(c(y.cases.n, y.deaths.n), nc = length(y.deaths.n), nr = 2, byrow = T)
        } else {
        # Define x and y for % of cases  and deaths
            y.cases.p <- c(NA, rollapply(y.cases2, 2, by = 1, function(i) {((i[2]-i[1])/i[1])*100}))
            y.deaths.p <- c(NA, rollapply(y.deaths2, 2, by = 1, function(i) {((i[2]-i[1])/i[1])*100}))
            table <- matrix(c(y.cases.p, y.deaths.p), nc = length(y.deaths.p), nr = 2, byrow = T)
            table[is.infinite(table)] <- NA
            
            table[1,1:which(y.cases2 >=10)[2]] <- NA
            table[2,1:which(y.deaths2 >=3)[2]] <- NA
        }
    
        # Define graphic area
        par(mar=c(4.1, 4.1, 2.1, 4.1))
        
        # x axis (bottom)
        x.axis <- x
        at <- seq(x[1], x[length(x)], length = 20)
        select <- format(x, "%b-%d") %in% format(at, "%b-%d")    
        x.axis[!select] <- NA
        
        # Barplot
        barplot(table,
                xlab= NA,
                ylab = NA,
                yaxt="n",
                col=c("grey50","pink"),
                legend = rownames(table),
                beside=TRUE,
                names.arg = format(x.axis, "%b-%d"),
                las=2)
        
        box()
        
        # Add y cases axis (left)
        axis(2)
        axis(4, col.axis = "red")
        mtext("Daily evolution of number of cases accoring to fitted curve", side = 2, line = 3, font = 2)
        mtext("Daily evolution of number of deaths accoring to fitted curve", side = 4, line = 3, font = 2, col = "red")
        
        # Add lockdown information
        if(input$tab1.lockdown2 == TRUE) {
            df.ld.country <- filter(df.ld, Country %in% vector.country)
            abline(v = which(x %in% df.ld.country[[2]])*3, lty=4, col = "grey")
            text(which(x %in% df.ld.country[[2]]), max(table, na.rm  = TRUE) , paste(df.ld.country[[3]], "lockdown"), col = "grey", pos = 2, srt = 90)
        }
    })
    
    ############################################################################################
    
    # Tab1 graphic: CFR
    output$tab1.cfr <- renderPlot({
        
        # Vector of country
        vector.country <- input$tab1.country
        
        # Data.frame of selected countries
        df.country <- filter(df.ecdc, countriesAndTerritories %in% vector.country)
        
        # List of selected countries
        list.country <- split(df.country, df.country$countriesAndTerritories)
        
        # Recalculate data according to selected parameters
        if(input$tab1.cumulative3 == TRUE) {
            y.cases <- cumsum(list.country[[vector.country]]$cases)
            y.deaths <- cumsum(list.country[[vector.country]]$deaths)
        } else {
            y.cases <- list.country[[vector.country]]$cases
            y.deaths <- list.country[[vector.country]]$deaths
        }
        
        # Define x and y
        x <- list.country[[vector.country]]$dateRep
        y.raw <- (y.deaths/y.cases)*100
        
        # Ajuste x and y on cases not = 0
        y <- y.raw
        y[!is.finite(y)] <- NA
        
        # Define graphic area
        par(mar=c(4.1, 4.1, 2.1, 4.1))
        
        # Plot empty graphic
        plot(x,y,
             log = ifelse(input$tab1.log3 == TRUE, "y", ""),
             type = "n",
             ylab = NA,
             xlab = NA,
             xaxt="n")
        
        # Add x axis (bottom)
        ticks <- seq(x[1], x[length(x)], length = 20)
        axis.POSIXct(side = 1, at = ticks, format = format(ticks, "%b-%d"), las=2)

        # Add y cases axis (left)
        axis(2, col.axis = "red")
        axis(4, col.axis = "red")
        mtext(ifelse(input$tab1.cumulative3 == FALSE,
                     "Daily case fatality rate (%)",
                     "Evolution of case fatality rate (%)"
                     ),
              side = c(2,4), line = 3, font = 2, col = "red")

        # Add lockdown information
        if(input$tab1.lockdown3 == TRUE) {
            df.ld.country <- filter(df.ld, Country %in% vector.country)
            abline(v = df.ld.country[[2]], lty=4, col = "grey")
            text(df.ld.country[[2]], max(y, na.rm  = TRUE) , paste(df.ld.country[[3]], "lockdown"), col = "grey", pos = 2, srt = 90)
        }

        # Add CFR data points
            points(x, y, pch = 1, col = "red")
            points(x[is.nan(y.raw)],rep(0, length(x[is.nan(y.raw)])) , pch = "#", col = "grey50")
            text(x[is.infinite(y.raw)], 0 , bquote("\U221E"), col = "red", cex = 1.4)

            # Add a filled circle for the last point
            if(input$tab1.cumulative3 == TRUE) {
                points(x[length(x)], y[length(y)], pch = 19, col = "red")
            }
        
        # LOESS smoothing span
        span.factor <- input$tab1.span3

        # Calculate LOESS
            cfr.loess <- loess(y~as.numeric(x), span = span.factor)
            cfr.loess.pred <- predict(cfr.loess, se=TRUE)
            x.adj <- x[!is.na(y)]

        # Add CFR LOESS fitted curve and 95% confidence interval
        if(input$tab1.loess3 == TRUE) {
            lines(x.adj, cfr.loess.pred$fit, col="pink")

            lines(x.adj, cfr.loess.pred$fit-1.96*cfr.loess.pred$se, lty = 2, col = "pink")
            lines(x.adj, cfr.loess.pred$fit+1.96*cfr.loess.pred$se, lty = 2, col = "pink")
        }

        # # Add linear model
        # if(input$tab1.lm3 == TRUE) {
        # 
        #     # lm() parameter
        #     days <- input$tab1.lmdays3
        # 
        #     # lm() CFR
        #     deaths.lm <- lm(tail(y, days) ~ tail(x, days))
        #     deaths.lm.pred <- predict(deaths.lm, interval="confidence")
        #     lines(tail(x, days), deaths.lm.pred[,1], col = "pink")
        #     lines(tail(x, days), deaths.lm.pred[,2], lty = 2, col = "pink")
        #     lines(tail(x, days), deaths.lm.pred[,3], lty = 2, col = "pink")
        # }

        # Legend
        legend("topleft",
               bty = "n",
               pch = c("o", bquote("\U221E"), "#", NA, NA, NA, NA, NA),
               lty = c(NA, NA, NA, NA, 1, 2, NA, 4),
               col = c("red", "red", "grey50", NA, "pink", "pink", NA, "grey"),
               legend = c(
                   "Case fatality rate (CFR)",
                   "No case reported ; death(s) reported (x/0 = Inf)",
                   "Neither case nor death reported (i.e. 0/0)",
                   "",
                   "Non-linear/Linear regression fitting",
                   "Confidence interval",
                   "",
                   "Lockdown information"
               ),
               text.col = c("red", "red", "grey50", NA, "pink", "pink", NA, "grey")
        )
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
