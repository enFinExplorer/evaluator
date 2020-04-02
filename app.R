library(shiny)
library(shinymaterial)
library(shinyWidgets)
library(tidyverse)
#library(billboarder)
library(tableHTML)
library(scales)
library(shinyjs)
library(rvest)
library(httr)
library(jsonlite)
library(zoo)
library(highcharter)
#library(plotly)
library(aRpsDCA)
library(lubridate)
library(DT)
library(caret)
library(kernlab)

options(stringsAsFactors = FALSE)
options(scipen = 999)

opList <- readRDS('./data/opList.rds')# %>% filter(operator != 'Artex Oil')
wellData <- readRDS('./data/assetSummary.rds')
costData <- readRDS('./data/costData.rds')
prodData <- readRDS('./data/prodData.rds')
propUplift <- readRDS('./data/propUplift.rds')


perfRisk <- data.frame(perf = c(0, 2500, 5000, 7500, 10000, 12500, 15000), risk = c(0, 0.55, 1, 1.45, 1.85, 2.2, 2.5))

perfUplift <- lm(perfRisk$risk ~ perfRisk$perf + I(perfRisk$perf**2))

prodData$oil[is.na(prodData$oil)] <- 0
prodData$gas[is.na(prodData$gas)] <- 0
IRRcalc <- function(cf, months){
  if(sum(cf) > 0){
    IRR1 <- 3
    loop <- 1
    while(sum(cf/((1+IRR1)^(months/12)))
          < 0){
      IRR1 <- IRR1 - 0.01
      loop = loop + 1
    }
    
  }else {
    IRR1 <- 0
  }
  return(IRR1)
}



ui <- material_page(
  title = tags$img(src = 'wmac.png'),
  nav_bar_color = "white",
  nav_bar_fixed = TRUE,
  background_color = 'white',
  # include_fonts = TRUE,
  # Place side-nav in the beginning of the UI
  material_side_nav(
    fixed = FALSE, 
    #image_source = "wmac.png",
    background_color = '#64b5f6',
    # Place side-nav tabs within side-nav
    material_side_nav_tabs(
      side_nav_tabs = c(
        "Operator Inventory" = "summary"#,
        #"Location Map" = "map"
      ),
      icons = c("insert_chart")
    )
  ),
  
  material_parallax(
    image_source =
      "rigPic.jpg"
  ),
  
  material_side_nav_tab_content(
    side_nav_tab_id = "summary",
    useShinyjs(),
    tags$br(),
    material_row(
      material_column(
        width = 4,
        offset = 1,
        material_text_box(
          input_id = "wmacUser",
          label = "Woodmac Login", 
          icon = 'recent_actors',
          color = "blue lighten-1"
        ),
        actionButton('login', 'Login', icon = icon('sign-in-alt'))
      ),
      material_column(
        width = 4,
        offset = 0,
        material_password_box(
          input_id = "wmacPass",
          label = "Woodmac Password", 
          color = "blue lighten-1"
        )
      )
    ),
    hidden(tags$div(id = 'element',
                    material_row(
                      material_column(
                        width = 4,
                        offset = 1,
                        material_dropdown(
                          input_id = "operator",
                          label = "Operator", 
                          choices =  sort(unique(opList$operator)),
                          color = "blue lighten-1"
                        )
                      ),
                      material_column(
                        width = 4,
                        offset = 0,
                        material_dropdown(
                          input_id = "plays",
                          label = "Active Areas", 
                          choices =  sort(unique(opList$id)),
                          color = "blue lighten-1"
                        )
                      )
                    ),
                    material_row(
                      material_column(
                        width = 12, offset =0,
                        highchartOutput('acres_plot')
                      )
                    ),
                    material_row(
                      
                      material_column(
                        width = 5,
                        offset = 1,
                        h6('Active Sub-Plays'),
                        br(),
                        radioButtons(
                          inputId = 'subPlay',
                          label = "",
                          #size = 'sm',
                          choices = ''
                        ),
                        br(),
                        highchartOutput('percentDev')
                        
                      ),
                      
                      
                      # numericInput('wiS', 'WI, %', 100, min = 0),
                      
                      
                      
                      
                      
                      material_column(
                        width = 6,
                        
                        material_card(
                          title = 'Sub-Play Assumptions',
                          material_row(
                            material_column(
                              width = 3,
                              numericInput('acres', 'Acreage Estimate', value = 0),
                              numericInput('wellSpacing', 'Wells Per Section', value = 0),
                              numericInput('pna', 'P&A Per Well, $', value = 0),
                              numericInput('wiPDP', 'PDP Working Interest, %', value = 100, min = 1, max = 100),
                              numericInput('nri', 'Royalty (to 100% WI), %', value = 80, min = 1, max = 100)
                            ),
                            material_column(
                              width = 3,
                              numericInput('shrink', 'Shrink (Fraction Retained)', value = 0),
                              numericInput('nglYield', 'NGL Yield (BBL/MMCF)', value = 0),
                              numericInput('btu', 'BTU Uplift', value = 0)
                            ),
                            material_column(
                              width = 3,
                              numericInput('fixedCost', 'Fixed Cost, $/mo', value = 0),
                              numericInput('varExpGas', 'Variable Cost, $/mcf', value = 0),
                              numericInput('varExpBOE', 'Variable Cost, $/boe', value = 0)
                            ),
                            material_column(
                              width = 3,
                              numericInput('stxOil', 'Oil Severance, %', value = 4.6, min = 0),
                              numericInput('oilSTX', 'Oil Severance, $/bbl', value = 0, min = 0),
                              numericInput('stxGas', 'Gas Severance, %', value = 7.5, min = 0),
                              numericInput('gasSTX', 'Gas Severance, $/mcf', value = 0, min = 0),
                              numericInput('atx', 'Ad Valorem Tax, %', value = 2.5, min = 0)
                            )
                          )
                          
                        )
                      )
                    ),
                    
                    material_row(
                      material_column(
                      width = 11, offset = 0,
                      br(),
                      highchartOutput('offsets_plot')
                      )
                    ),
                    
                    material_row(
                      material_column(
                        width = 5, offset =1,
                        selectizeInput('wellFactor', '', choices = c('perf', 'ppf', 'fpf'), selected = 'perf'),
                        highchartOutput('wellDesign'),
                        br(),
                        highchartOutput('costPerFt')
                      ),
                      material_column(
                        width =6, offset = 0,
                        material_card(
                          title = 'Well Design',
                          material_row(
                            
                            
                            material_column(width = 4,
                                            numericInput('perfSelect', 'Average Lateral Length, ft', value = 7500)),
                            
                            material_column(width =4,
                                            numericInput('ppfSelect', 'Proppant Loading, lb/ft', value = 2000)),
                            
                            material_column(width =4,
                                            numericInput('fpfSelect', 'Fluid Loading, gal/ft', value = 2000))
                            
                          )
                        ),
                        material_card(
                          
                          title = "Capex Calculator (Cost CoE)",
                          
                          material_row(
                            tableHTML_output('capexCalcs')),
                          material_row(
                            material_column(width = 3,
                                            numericInput('capexStressor', 'Capex Stress', value = 1, min = 0.1))
                          ),
                          material_row(
                            
                            
                            material_column(width = 4,
                                            numericInput('tvdSelect', 'TVD', value = 7500, min = 1000, max = 20000),
                                            numericInput('proppantPerStage', 'Proppant Per Stage', value = 440000, min = 1),
                                            numericInput('drillingFuelPerDay', 'Drilling Fuel, $/Day', value = 4000, min = 1)),
                            material_column(width =4,
                                            numericInput('drillSpeed', 'Drill Speed, ft/day', value = 100, min = 1),
                                            numericInput('padConstruction', 'Pad Construction Per Well', value = 180000, min = 1),
                                            numericInput('ancDrillPerDay', 'Ancillary Drill Services, $/Day', value = 50000, min = 1)),
                            material_column(width =4,
                                            numericInput('stagesPerDay', 'Frac Stages per Day', value = 10, min = 1),
                                            numericInput('drillingFluidsPerDay', 'Drilling Fluids, $/Day', value = 4100, min = 1),
                                            numericInput('chemsPerStage', 'Chemical Cost, $/Stage', value = 6500, min = 1))
                          ),
                          
                          material_row(
                            
                            material_column(width = 6,
                                            numericInput('pumpFuel', 'Pumping Fuel, $/Stage', value = 5000, min = 1)),
                            material_column(width = 6,
                                            numericInput('ancCompPerDay', 'Ancillary Completion Services, $/Day', value = 50000, min = 1))
                          ),
                          h6('Sand Fractions: Should Add to 1'),
                          material_row(
                            
                            material_column(width = 3,
                                            numericInput('brownSelect', 'Brown Sand', value = 1, min = 0, max = 1)),
                            material_column(width = 3,
                                            numericInput('ceramicSelect', 'Ceramic', value = 0, min = 0, max = 1)),
                            material_column(width = 3,
                                            numericInput('rcsSelect', 'Resin-Coated', value = 0, min = 0, max= 1)),
                            material_column(width = 3,
                                            numericInput('northernSelect', 'Northern White', value = 0, min = 0, max = 1))
                          )
                        )
                      )
                    ),
                    material_row(
                      material_column(
                        width = 5, offset = 1,
                        highchartOutput('stripPrice')
                      ),
                      material_column(
                        width = 6,
                        material_card(
                          title = 'Price Assumptions',
                          material_row(
                            material_column(
                              width = 6,
                              radioButtons('priceType', '', choices = c('Strip', 'Flat'), selected = 'Strip'),
                              numericInput('wti', 'WTI, $/BBL', value = 55, min = 1),
                              numericInput('hh', 'HH, $/MCF', value = 2, min = 0.1),
                              awesomeRadio('econAbanS',
                                           'Abandon at Economic Limit?',
                                           choices = c('Yes', 'No'),
                                           selected = 'Yes',
                                           status = 'primary')),
                            
                            material_column(
                              width = 6,
                              numericInput('gasDiff', 'Gas Differential, $/MCF', value = 0),
                              numericInput('oilDiff', 'Oil Differential, $/BBL', value = 0),
                              numericInput('nglDiff', 'NGL Pricing, %WTI', value = 20)
                            )
                          )
                        )
                        
                      )
                    ),
                    br(),
                    material_row(
                      material_column(
                        width = 5, offset = 1,
                        highchartOutput('spPlot1')
                      ),
                      material_column(
                        width = 6,
                        material_card(
                          title = 'Type Curve Design',
                          material_row(
                            material_column(
                              width = 6,
                              material_dropdown(input_id = 'operatorSelect',label = 'Select Operator(s)', choices = sort(unique(opList$operator)), 
                                                multiple = TRUE),
                              material_dropdown(input_id ='selectYr', label = 'Select Year(s)', choices = '2019',  multiple = TRUE),
                              selectizeInput('productSelect', 'Selected Product', choices = c('Oil', 'Gas'), multiple = FALSE),
                              numericInput('cutoff', 'IRR Cutoff, %', value = 20, min = 0),
                              
                              numericInput('spudToProd', 'Spud to First Production (Months)', value =3 , min = 1),
                              numericInput('prbRisking', 'Probable Risking (0 to 1)', value = 0.75, min = 0, max = 1 ),
                              
                              numericInput('possRisking', 'Possible Risking (0 to 1)', value = 0.5, min = 0, max = 1 )
                              
                            ),
                            
                            material_column(
                              width = 6,
                              
                              
                              numericInput('wellLifeS', 'Well Life (Years)', value = 25, min = 5, max = 50),
                              
                              numericInput('qiOilS', 'Oil IP, bbls/d', value = 1000, min = 0),
                              
                              numericInput('bOilS', 'Oil B-Factor', value = 1, min = 0, max = 2.5),
                              
                              numericInput('DiOilS', 'Di Oil', value = 0.9, min = 0.20000001, max = 0.999999),
                              
                              numericInput('DfOilS', 'Df Oil', value = 0.1, min = 0.01, max = 0.2),
                              
                              numericInput('curtailOilS', 'Oil Curtailment (months)', value = 0, min = 0),
                              
                              
                              numericInput('qiGasS', 'Gas IP, mcf/d',  value = 1000, min = 0),
                              
                              numericInput('bGasS', 'Gas B-Factor',  value = 1, min = 0, max = 2.5),
                              
                              numericInput('DiGasS', 'Di Gas', value = 0.9, min = 0.20000001, max = 0.999999),
                              
                              numericInput('DfGasS', 'Df Gas',  value = 0.08, min = 0.01, max = 0.2),
                              
                              numericInput('curtailGasS', 'Gas Curtailment (months)', value = 0, min = 0),
                              
                              actionButton('calcInv', 'Calculate Inventory')
                            )
                            
                          )
                        )
                        
                      )
                    ),
                    material_row(
                      material_column(
                        width = 5, offset = 1,
                        material_card(
                          title = 'Inventory Summary',
                          depth = 2,
                          DT::dataTableOutput('wellCalcs')
                        )
                      ),
                      material_column(
                        width =5 ,
                        material_card(
                          title = 'GEM Decline Factors',
                          depth = 2,
                          DT::dataTableOutput('wellCalcs1')
                        )
                      )
                    )
    )
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues()
  shinyjs::hide('operator')
  shinyjs::hide('plays')
  
  observe({
    if(is.null(values$qiGas)|is.null(values$qiOil)){
      shinyjs::disable('calcInv')
    } else {
      shinyjs::enable('calcInv')
    }
  })
  
  
  
  observeEvent(input$login, {
    updateActionButton(session, "login",
                       label = 'Confirming...')
    shinyjs::disable('login')
    #shinyjs::hide('plays')
    #shinyjs::hide('operator')
    values$authorization <- NULL
    values$clientID <- input$wmacUser
    values$clientSecret <- input$wmacPass
    clientID <- input$wmacUser
    clientSecret <- input$wmacPass
    authorization <- authenticate(clientID, clientSecret)
    
    
    values$authorization <- authorization
    url1 <- 'https://data.woodmac.com/query/l48/all/odata/attributes_horizontal_wells'
    basin <-  'Austin Chalk'
    
    basin1 <- paste("play_name eq '", basin, "'", sep='')
    df <- data.frame()
    
    while(!is.null(url1)) {
      wellInfo <- GET(url1, values$authorization,
                      add_headers(c('Accept' = 'application/json')),
                      query = list('$filter' = dput(basin1)))
      
      
      
      
      df1 <- content(wellInfo, as='text', encoding = 'UTF-8')[1]
      df1 <- fromJSON(df1, flatten = TRUE) %>% data.frame()
      if(nrow(df) == 0) {
        df <- df1
      } else {
        df <- df[,names(df1)]
        df <- rbind(df, df1)
      }
      rm(df1)
      url1 <- NULL
      #df <- df %>% filter(value.basin_name == basin)
      
      
    }
    
    if(nrow(df) <= 1){
      updateActionButton(session, "login",
                         label = 'Failed!')
      
    } else {
      updateActionButton(session, "login",
                         label = 'Success!')
      # shinyjs::show('playHide')
      # shinyjs::show('spPlotX')
      # shinyjs::show('welleconTableX')
      shinyjs::enable('login')
      #shinyjs::show('plays')
      #shinyjs::show('operator')
      shinyjs::show('element')
    }
    updateActionButton(session, "login",
                       label = 'Login')
    shinyjs::enable('login')
  })
  
  observeEvent(input$operator, {
    if(is.null(input$operator)){
      NULL
    } else {
      df1 <- opList %>% filter(operator %in% input$operator)
      update_material_dropdown(session, 'plays',  choices = sort(unique(df1$id)), value = sort(unique(df1$id))[1])
    }
  })
  
  df <- reactive({
    if(is.null(input$plays)||input$plays == ''){
      NULL
    } else {
      as.data.frame(wellData[[input$plays]]$df)
    }
  })
  
  ogip <- reactive({
    if(is.null(input$plays)||input$plays == ''){
      NULL
    } else {
      as.data.frame(wellData[[input$plays]]$ogip)
    }
  })
  
  
  
  leases <- reactive({
    if(is.null(input$plays)||input$plays == ''){
      NULL
    } else {
      as.data.frame(wellData[[input$plays]]$leases)
    }
  })
  
  
  df2 <- reactive(
    df() %>% filter(operator %in% input$operator)
  )
  
  observeEvent(input$plays, {
    if(is.null(input$plays)||input$plays == ''){
      NULL
    } else {
      df1 <-leases() %>% filter(operator %in% input$operator)
      
      updateRadioButtons(session, 'subPlay', '', choices = sort(unique(df1$subPlay)))
    }
  })
  
  observeEvent(input$subPlay, {
    df2 <- leases() %>% filter(operator %in% input$operator) %>% 
      filter(subPlay %in% input$subPlay) %>% filter(!duplicated(oneSecLocation))
    #print(head(df1))
    updateNumericInput(session, 'acres', value = nrow(df2)*640)
    df1 <- df() %>% filter(subPlay %in% input$subPlay)
    #print(head(df1))
    updateNumericInput(session, 'wellSpacing', value = as.integer(mean(df1$wellsPerSection)))
    updateNumericInput(session, 'shrink', value = (mean(df1$shrink)))
    updateNumericInput(session, 'nglYield', value = (mean(df1$nglYield)))
    updateNumericInput(session, 'fixedCost', value = as.integer(mean(df1$finalFixed)))
    updateNumericInput(session, 'varExpGas', value = (mean(df1$varExpGas)))
    updateNumericInput(session, 'varExpBOE', value = (mean(df1$varExpBOE)))
    updateNumericInput(session, 'gasDiff', value = (mean(df1$gasDiff)))
    updateNumericInput(session, 'oilDiff', value = (mean(df1$oilDiff)))
    updateNumericInput(session, 'pna', value = as.integer(mean(df1$pna)))
    updateNumericInput(session, 'btu', value = (mean(df1$btu)))
    
    updateNumericInput(session, 'tvdSelect', value = as.integer(mean(df2$tvd, na.rm=TRUE)))
    df3 <- df1 %>% filter(operator %in% input$operator)
    df3 <- df3 %>%  filter(fp.year == max(df3$fp.year))
    if(nrow(df3) > 0){
      updateNumericInput(session, 'perfSelect', value = as.integer(mean(df3$perf, na.rm=TRUE)))
      updateNumericInput(session, 'ppfSelect', value = as.integer(mean(df3$ppf, na.rm=TRUE)))
      updateNumericInput(session, 'fpfSelect', value = as.integer(mean(df3$fpf, na.rm=TRUE)))
    }
    
    shinyjs::disable('calcInv')
    values$qiGas <- NULL
    values$qiOil <- NULL
    
  })
  
  output$acres_plot <- renderHighchart({
    
    df <- leases() %>% filter(operator %in% input$operator) %>% filter(!duplicated(oneSecLocation)) %>%
      group_by(subPlay) %>% summarise(acres = n()*640) %>% ungroup() %>% select(subPlay, acres) %>% filter(!is.na(subPlay))
    
    totalAcres <- sum(df$acres)
   df <- df %>%
      mutate(acres = round(acres/sum(acres)*100,1))
    #print(head(df))
    cols <- c( '#00a4e3', '#adafb2','#0D1540','#06357a',  '#a31c37', '#d26400', '#eaa814',
               '#5c1848', '#786592', '#ff4e50', '#027971', '#008542', '#5c6d00')
    highchart() %>%
      hc_add_series(df, 'pie', hcaes(name = subPlay, y = acres), name = 'Percent')  %>%
      hc_plotOptions(
        series = list(
          showInLegend = FALSE,
          pointFormat = "{point.y}%"
        )) %>%
      hc_title(
        text = paste0("Percent of Total Acreage By Subplay, Total Acres: <span style=\"color:#00a4e3\">",totalAcres,"</span>"),
        useHTML = TRUE) %>%
      hc_subtitle(text = '<a href="https://www.woodmac.com/research/lens/lens-direct/">Source: P2</a>', useHTML=TRUE, align = 'right') %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      #hc_xAxis(title = list(text = '<b>First Production Year</b>')) %>%
      #hc_yAxis(title = list(text = '<b>Six Month BOE Per Ft (20:1 Gas, 25% NGL)</b>')) %>%
      hc_credits(
        enabled = TRUE,
        text = "Powered by Highcharts",
        href = "https://www.highcharts.com/") %>%
      hc_colors(cols) %>%
      hc_exporting(enabled = TRUE, filename = 'acresBySubplay')
  })
  
  
  capexValues <- reactive({
    data.frame(
      Component = c('capexStressor', 'tvdSelect', 'drillSpeed', 'stagesPerDay', 'proppantPerStage', 'padConstruction',
                    'drillingFluidsPerDay', 'drillingFuelPerDay', 'ancDrillPerDay', 'chemsPerStage', 'pumpFuel', 'ancCompPerDay',
                    'brownSelect', 'ceramicSelect', 'rcsSelect', 'northernSelect', 'perfSelect', 'ppfSelect', 'fpfSelect', 'wti', 'hh'),
      Value = c(input$capexStressor, input$tvdSelect, input$drillSpeed, input$stagesPerDay, input$proppantPerStage, input$padConstruction,
                input$drillingFluidsPerDay, input$drillingFuelPerDay, input$ancDrillPerDay, input$chemsPerStage, input$pumpFuel, input$ancCompPerDay,
                input$brownSelect, input$ceramicSelect, input$rcsSelect, input$northernSelect, input$perfSelect, input$ppfSelect, input$fpfSelect,
                input$wti, input$hh),
      stringsAsFactors = FALSE) %>% spread(Component, Value)
  })
  
  expenseValues <- reactive({
    data.frame(
      Component = c('acres', 'wellSpacing', 'pna', 'wiPDP', 'nri', 'shrink', 
                    'nglYield', 'btu', 'fixedCost', 'varExpGas', 'varExpBOE', 'stxOil',
                    'stxGas', 'oilSTX', 'gasSTX', 'atx', 'oilDiff', 'gasDiff', 'nglDiff'),
      
      Value = c(input$acres, input$wellSpacing, input$pna, input$wiPDP, input$nri, input$shrink,
                input$nglYield, input$btu, input$fixedCost, input$varExpGas, input$varExpBOE,
                input$stxOil, input$stxGas, input$oilSTX, input$gasSTX, input$atx,
                input$oilDiff, input$gasDiff, input$nglDiff),
      stringsAsFactors = FALSE) %>% spread(Component, Value)
    
  })
  observe({
    #values$dd2 <- NULL
    if(is.null(input$subPlay)){
      
      NULL
    } else {
      
      df <- df() %>% filter(subPlay %in% input$subPlay)
      df <- as.data.frame(costData)%>%
        filter(subBasin %in% df$subBasin)
      
      x <- (as.integer(mean(df$drillSpeed, na.rm=TRUE)))
      y <- (as.integer(mean(df$stagesPerDay, na.rm=TRUE)))
      
      
      
      #names(df) <- 'x'
      
      # Can use character(0) to remove all choices
      if (is.null(x))
        x <- 620
      if (is.null(y))
        y <- 5
      
      
      
      # Can also set the label and select items
      updateNumericInput(session, "drillSpeed", label = 'Drill Speed, ft/day',
                         x
      )
      
      updateNumericInput(session, "stagesPerDay", label = 'Frac Stages per Day',
                         y
      )
      
      df1 <- df() %>% filter(subPlay %in% input$subPlay)
      update_material_dropdown(session, 'selectYr',  choices = sort(unique(as.character(df1$fp.year))), value = sort(unique(as.character(df1$fp.year)))[1])
      update_material_dropdown(session, 'operatorSelect',  choices = sort(unique(df1$operator)), value  = sort(unique(df1$operator))[1])
      
    }
    
    
    
    
  })
  
  output$capexCalcs <- render_tableHTML({
    if(is.null(input$subPlay)){
      
      
      NULL
    } else {
      #capexValues <- capexValues()
      #print(head(capexValues))
      df <- df() %>% filter(subPlay %in% input$subPlay)
      df <- as.data.frame(costData)%>%
        filter(subBasin %in% df$subBasin)
      
      working1 <- df
      
      working1$md <- as.numeric(capexValues()$tvdSelect) + as.numeric(capexValues()$perfSelect)
      
      
      working1$cementing <- working1$md*8
      working1$proppant <- as.numeric(capexValues()$ppfSelect)*as.numeric(capexValues()$perfSelect)
      working1$fluid <- as.numeric(capexValues()$fpfSelect)*as.numeric(capexValues()$perfSelect)
      working1$proppantPerStage <- as.numeric(capexValues()$proppantPerStage)
      working1$stages <- working1$proppant/working1$proppantPerStage
      working1$compDays <- working1$stages/(as.numeric(capexValues()$stagesPerDay))
      working1$drillDays <- working1$md/as.numeric(capexValues()$drillSpeed)
      
      working1 <- working1 %>% 
        mutate(drillCost = as.numeric(capexValues()$tvdSelect)*sixteenCasingTVD*sixteenRate+
                 as.numeric(capexValues()$tvdSelect)*thirteenCasingTVD*thirteenRate+
                 as.numeric(capexValues()$tvdSelect)*nineCasingTVD*nineRate +
                 as.numeric(capexValues()$tvdSelect)*sevenCasingTVD*sevenRate+
                 as.numeric(capexValues()$tvdSelect)*fiveCasingTVD*fiveRate + 
                 md*fiveCasingTMD*fiveRate+md*fourCasingTMD*fourRate +
                 as.numeric(capexValues()$tvdSelect)*tubingTVD*tubingRate+
                 as.numeric(capexValues()$padConstruction)+
                 as.numeric(capexValues()$drillingFluidsPerDay)*drillDays+
                 as.numeric(capexValues()$drillingFuelPerDay)*drillDays+
                 as.numeric(capexValues()$ancDrillPerDay)*drillDays+cementing+
                 rigRate*drillDays)
      
      working1$brownPercent <- as.numeric(capexValues()$brownSelect)
      working1$ceramicPercent <- as.numeric(capexValues()$ceramicSelect)
      working1$rcsPercent <- as.numeric(capexValues()$rcsSelect)
      working1$northernPercent <- as.numeric(capexValues()$northernSelect)
      
      working1 <- working1 %>%
        mutate(compCost = fleetRateComp*fleetRatePerHP*compDays+fluid*waterAcqRate/42 +
                 fluid*waterHaulingToRate/42+
                 fluid*waterTreatmentRate/42+
                 fluid*waterHaulingFromRate/42*waterFlowbackPercent +
                 fluid*waterDispRate*waterFlowbackPercent/42+
                 (brownPercent*sandRate+ceramicPercent*ceramicRate +
                    rcsPercent*rcsRate+northernPercent*northernRate)*proppant +
                 (brownPercent*sandTranspRate +ceramicPercent*ceramicTranspRate +
                    rcsPercent*rcsTranspRate+northernPercent*northernTranspRate)*proppant +
                 stages*as.numeric(capexValues()$chemsPerStage)+
                 stages*as.numeric(capexValues()$pumpFuel)+compDays*as.numeric(capexValues()$ancCompPerDay))
      
      working1 <- working1 %>% mutate(facilities = (drillCost+compCost)/(1-eNt)*facil/2,
                                      equipment = (drillCost+compCost)/(1-eNt)*facil/2,
                                      pipeline = (drillCost+compCost)/(1-eNt)*tieIn)
      
      working1 <- working1 %>% mutate(dNc = drillCost+compCost, dcet = dNc + facilities + equipment + pipeline)
      working1$dNc <- working1$dNc*as.numeric(capexValues()$capexStressor)
      working1$dcet <- working1$dcet*as.numeric(capexValues()$capexStressor)
      working1 <- working1 %>% select(dNc, dcet)
      working1 <- as.data.frame(working1)
      working1$dNcPerFt <- dollar(as.integer(working1$dNc/as.numeric(capexValues()$perfSelect)))
      working1$dcetPerFt <- dollar(as.integer(working1$dcet/as.numeric(capexValues()$perfSelect)))
      working1$dNc <- dollar(as.integer(working1$dNc))
      working1$dcet <- dollar(as.integer(working1$dcet))
      
      names(working1) <- c('Drill & Complete, $', 'Drill, Complete, & Equip, $', 'D&C Per Ft, $/Ft', 'DC&E Per Ft, $/Ft')
      #print(head(working1))
      #working1
      table1 <- tableHTML(working1, rownames = FALSE)
      
      #datatable((working1), rownames=FALSE, 
      #            escape = FALSE,extensions = c('Buttons', 'ColReorder', 'FixedHeader','KeyTable',  'Scroller'),
      #            options = list(dom = 'Bt', fixedHeader=TRUE, keys=TRUE,
      #                           deferRender = FALSE, scroller=TRUE,
      #                           scrollX='400px',
      #                           colReorder = TRUE,
      #                           buttons = c('copy')))
      
      table1
    }
  })
  
  output$wellDesign <- renderHighchart({
    df <- df2() %>% filter(subPlay %in% input$subPlay)%>% 
      group_by(fp.year) %>% summarise(perf = mean(perf), ppf= mean(ppf), fpf=mean(fpf)) %>%
      ungroup() %>% gather(Component, Value, -c(fp.year)) %>% filter(Component == input$wellFactor) %>%
      arrange(fp.year) %>% select(fp.year, Value) %>% mutate(Value = as.integer(Value))
    if(nrow(df) == 0){
      NULL
    } else {
      if(input$wellFactor == 'perf'){
        # billboarder() %>%
        #   bb_barchart(data = df, color = "#00a4e3") %>%
        #   bb_y_grid(show = TRUE) %>%
        #   bb_y_axis(tick = list(format = suffix("ft")),
        #             label = list(text = "Lateral Length in Ft", position = "outer-top")) %>% 
        #   bb_legend(show = FALSE) %>% 
        #   bb_labs(title = "Average Lateral Length by Year",
        #           caption = "Data source: Lens Direct")
        cols <- c("#00a4e3")
        hchart(df, "column", hcaes(x = fp.year, y = Value), name = 'Lateral Length') %>%
          hc_title(
            text = "Average Lateral Length by Year",
            useHTML = TRUE) %>%
          hc_subtitle(text = '<a href="https://www.woodmac.com/research/lens/lens-direct/">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
          hc_tooltip(table = TRUE, sort = TRUE) %>%
          hc_xAxis(title = list(text = '<b>First Production Year</b>')) %>%
          hc_yAxis(title = list(text = '<b>Lateral Length, ft</b>')) %>%
          hc_credits(
            enabled = TRUE,
            text = "Powered by Highcharts",
            href = "https://www.highcharts.com/") %>%
          hc_colors(cols)%>%
          hc_exporting(enabled = TRUE, filename = 'perfData')
        
      } else if(input$wellFactor == 'ppf'){
        # billboarder() %>%
        #   bb_barchart(data = df, color = "#00a4e3") %>%
        #   bb_y_grid(show = TRUE) %>%
        #   bb_y_axis(tick = list(format = suffix("lb/ft")),
        #             label = list(text = "Proppant Loading, lb/ft", position = "outer-top")) %>% 
        #   bb_legend(show = FALSE) %>% 
        #   bb_labs(title = "Average Proppant Loading by Year",
        #           caption = "Data source: Lens Direct")
        
        cols <- c("#adafb2")
        hchart(df, "column", hcaes(x = fp.year, y = Value), name = 'Proppant Loading') %>%
          hc_title(
            text = "Average Proppant Loading by Year",
            useHTML = TRUE) %>%
          hc_subtitle(text = '<a href="https://www.woodmac.com/research/lens/lens-direct/">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
          hc_tooltip(table = TRUE, sort = TRUE) %>%
          hc_xAxis(title = list(text = '<b>First Production Year</b>')) %>%
          hc_yAxis(title = list(text = '<b>Proppant Loading, lb/ft</b>')) %>%
          hc_credits(
            enabled = TRUE,
            text = "Powered by Highcharts",
            href = "https://www.highcharts.com/") %>%
          hc_colors(cols)%>%
          hc_exporting(enabled = TRUE, filename = 'ppfData')
      } else {
        # billboarder() %>%
        #   bb_barchart(data = df, color = "#00a4e3") %>%
        #   bb_y_grid(show = TRUE) %>%
        #   bb_y_axis(tick = list(format = suffix("gal/ft")),
        #             label = list(text = "Fluid Loading, gal/ft", position = "outer-top")) %>% 
        #   bb_legend(show = FALSE) %>% 
        #   bb_labs(title = "Average Fluid Loading by Year",
        #           caption = "Data source: Lens Direct")
        
        cols <- c("#0D1540")
        hchart(df, "column", hcaes(x = fp.year, y = Value), name = 'Fluid Loading') %>%
          hc_title(
            text = "Average Fluid Loading by Year",
            useHTML = TRUE) %>%
          hc_subtitle(text = '<a href="https://www.woodmac.com/research/lens/lens-direct/">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
          hc_tooltip(table = TRUE, sort = TRUE) %>%
          hc_xAxis(title = list(text = '<b>First Production Year</b>')) %>%
          hc_yAxis(title = list(text = '<b>Fluid Loading, gal/ft</b>')) %>%
          hc_credits(
            enabled = TRUE,
            text = "Powered by Highcharts",
            href = "https://www.highcharts.com/") %>%
          hc_colors(cols)%>%
          hc_exporting(enabled = TRUE, filename = 'fpfData')
      }
    }
    
    
  })
  
  output$costPerFt <- renderHighchart({
    if(is.null(input$subPlay)){
      
      
      NULL
    } else {
      #capexValues <- capexValues()
      #print(head(capexValues))
      df <- df() %>% filter(subPlay %in% input$subPlay)
      df <- as.data.frame(costData)%>%
        filter(subBasin %in% df$subBasin)
      
      working1 <- df
      
      costCalc <- data.frame(perf = c(capexValues()$perfSelect, 2500, 4500, 7500, 9000, 12500, 15000))
      costCalc <- costCalc %>% filter(!duplicated(perf))
      costCalc$count <- 1
      
      econSummary <- lapply(split(costCalc, costCalc[,'perf']), function (well) tryCatch({
        working1$md <- as.numeric(capexValues()$tvdSelect) + as.numeric(well$perf)
        
        
        working1$cementing <- working1$md*8
        working1$proppant <- as.numeric(capexValues()$ppfSelect)*as.numeric(well$perf)
        working1$fluid <- as.numeric(capexValues()$fpfSelect)*as.numeric(well$perf)
        working1$proppantPerStage <- as.numeric(capexValues()$proppantPerStage)
        working1$stages <- working1$proppant/working1$proppantPerStage
        working1$compDays <- working1$stages/(as.numeric(capexValues()$stagesPerDay))
        working1$drillDays <- working1$md/as.numeric(capexValues()$drillSpeed)
        
        working1 <- working1 %>% 
          mutate(drillCost = as.numeric(capexValues()$tvdSelect)*sixteenCasingTVD*sixteenRate+
                   as.numeric(capexValues()$tvdSelect)*thirteenCasingTVD*thirteenRate+
                   as.numeric(capexValues()$tvdSelect)*nineCasingTVD*nineRate +
                   as.numeric(capexValues()$tvdSelect)*sevenCasingTVD*sevenRate+
                   as.numeric(capexValues()$tvdSelect)*fiveCasingTVD*fiveRate + 
                   md*fiveCasingTMD*fiveRate+md*fourCasingTMD*fourRate +
                   as.numeric(capexValues()$tvdSelect)*tubingTVD*tubingRate+
                   as.numeric(capexValues()$padConstruction)+
                   as.numeric(capexValues()$drillingFluidsPerDay)*drillDays+
                   as.numeric(capexValues()$drillingFuelPerDay)*drillDays+
                   as.numeric(capexValues()$ancDrillPerDay)*drillDays+cementing+
                   rigRate*drillDays)
        
        working1$brownPercent <- as.numeric(capexValues()$brownSelect)
        working1$ceramicPercent <- as.numeric(capexValues()$ceramicSelect)
        working1$rcsPercent <- as.numeric(capexValues()$rcsSelect)
        working1$northernPercent <- as.numeric(capexValues()$northernSelect)
        
        working1 <- working1 %>%
          mutate(compCost = fleetRateComp*fleetRatePerHP*compDays+fluid*waterAcqRate/42 +
                   fluid*waterHaulingToRate/42+
                   fluid*waterTreatmentRate/42+
                   fluid*waterHaulingFromRate/42*waterFlowbackPercent +
                   fluid*waterDispRate*waterFlowbackPercent/42+
                   (brownPercent*sandRate+ceramicPercent*ceramicRate +
                      rcsPercent*rcsRate+northernPercent*northernRate)*proppant +
                   (brownPercent*sandTranspRate +ceramicPercent*ceramicTranspRate +
                      rcsPercent*rcsTranspRate+northernPercent*northernTranspRate)*proppant +
                   stages*as.numeric(capexValues()$chemsPerStage)+
                   stages*as.numeric(capexValues()$pumpFuel)+compDays*as.numeric(capexValues()$ancCompPerDay))
        
        working1 <- working1 %>% mutate(facilities = (drillCost+compCost)/(1-eNt)*facil/2,
                                        equipment = (drillCost+compCost)/(1-eNt)*facil/2,
                                        pipeline = (drillCost+compCost)/(1-eNt)*tieIn)
        
        working1 <- working1 %>% mutate(dNc = drillCost+compCost, dcet = dNc + facilities + equipment + pipeline)
        working1$dNc <- working1$dNc*as.numeric(capexValues()$capexStressor)
        working1$dcet <- working1$dcet*as.numeric(capexValues()$capexStressor)
        working1 <- working1 %>% select(dNc, dcet)
        working1 <- as.data.frame(working1)
        #working1$dNcPerFt <- dollar(as.integer(working1$dNc/as.numeric(capexValues()$perfSelect)))
        working1$dcetPerFt <- (as.integer(working1$dcet/as.numeric(well$perf)))
        #working1$dNc <- dollar(as.integer(working1$dNc))
        #working1$dcet <- dollar(as.integer(working1$dcet))
        well$dcetPerFt <- working1$dcetPerFt
        well
      },
      error = function(e) {
        e
        NULL
      }))
      
      costCalc <- dplyr::bind_rows(econSummary)
      costCalc <- costCalc[,c('perf', 'dcetPerFt')]
      names(costCalc) <- c('Lateral', 'CapexPerFt')
      cols <- c('#06357a')
      hchart(costCalc, "line", hcaes(x = Lateral, y = CapexPerFt), name = 'Capex Per Ft') %>%
        hc_title(
          text = "Cost Per Ft vs Lateral Length",
          useHTML = TRUE) %>%
        hc_subtitle(text = '<a href="https://www.woodmac.com/">Source: Wood Mackenzie Cost Center of Excellence</a>', useHTML=TRUE, align = 'right') %>%
        hc_tooltip(table = TRUE, sort = TRUE) %>%
        hc_xAxis(title = list(text = '<b>Lateral Length, ft</b>')) %>%
        hc_yAxis(title = list(text = '<b>Capex/Ft, $/Ft</b>')) %>%
        hc_credits(
          enabled = TRUE,
          text = "Powered by Highcharts",
          href = "https://www.highcharts.com/") %>%
        hc_colors(cols)%>%
        hc_exporting(enabled = TRUE, filename = 'wellCostTrend')
      
      # billboarder() %>%
      #   bb_linechart(data = costCalc, type = 'spline', width =3, color = "#00a4e3") %>%
      #   bb_y_grid(show = TRUE) %>%
      #   bb_y_axis(tick = list(format = suffix("$/ft")),
      #             label = list(text = "Capex Per Ft, $/Ft", position = "outer-top")) %>% 
      #   bb_x_axis(tick = list(format = suffix("ft")),
      #             label = list(text = "Lateral Length, ft", position = "outer-top")) %>% 
      #   bb_legend(show = FALSE) %>% 
      #   bb_labs(title = "Cost Per Lateral Ft",
      #           caption = "Data source: Cost Center of Excellence")
    }
  })
  
  output$offsets_plot <- renderHighchart({
    df1 <- df2() %>% filter(subPlay %in% input$subPlay)
    leases1 <- leases() %>%# filter(subPlay %in% input$subPlay) %>% 
      filter(possLocation %in% df1$possLocation) %>% filter(!duplicated(paste0(operator, oneSecLocation))) %>% 
      filter(!operator %in% input$operator) %>%
      group_by(operator) %>% summarise(acres = sum(acres)) %>% ungroup() %>% arrange(desc(acres)) %>% top_n(15)
    if(nrow(leases1) == 0){
      NULL
    } else {
      cols <- c("#00a4e3")
      hchart(leases1, "column", hcaes(x = operator, y = acres), name = 'Offset Acreage') %>%
        hc_title(
          text = "Direct Offset Acreage by Operator",
          useHTML = TRUE) %>%
        hc_subtitle(text = '<a href="https://www.woodmac.com/research/lens/lens-direct/">Source: P2</a>', useHTML=TRUE, align = 'right') %>%
        hc_tooltip(table = TRUE, sort = TRUE) %>%
        hc_xAxis(title = list(text = '<b>Operator</b>')) %>%
        hc_yAxis(title = list(text = '<b>Acres</b>')) %>%
        hc_credits(
          enabled = TRUE,
          text = "Powered by Highcharts",
          href = "https://www.highcharts.com/") %>%
        hc_colors(cols)%>%
        hc_exporting(enabled = TRUE, filename = 'acreageOffsets')
    }
    
    # billboarder() %>%
    #   bb_barchart(data = leases3() %>% group_by(operator) %>% summarise(acres = sum(acres)) %>% ungroup() %>% arrange(desc(acres)) %>% top_n(15) %>% group_by(operator)) %>%
    #   bb_legend(show = FALSE) %>% 
    #   bb_x_axis(tick = list(rotate = 45)) %>%
    #   bb_labs(title = 'Acreage By Operator',
    #           caption = '')
  })
  
  
  output$percentDev <- renderHighchart({
    df1 <- df() %>% filter(subPlay %in% input$subPlay) %>% group_by(oneSecLocation) %>% summarise(wells = n()) %>% ungroup()
    if(nrow(df1) == 0){
      value1 <- 0
    } else {
      leases2 <- leases() %>% filter(operator %in% input$operator) %>% filter(subPlay %in% input$subPlay) %>% merge(df1, by='oneSecLocation', all.x=TRUE)
      #head(leases2)
      leases3 <- leases2 %>% filter(is.na(wells))
      leases2 <- leases2 %>% filter(!is.na(wells))
      
      leases3 <- leases3 %>% group_by(oneSecLocation, subPlay) %>% summarise(count=n()) %>% ungroup() %>% group_by(subPlay) %>% summarise(count = sum(count)) %>% ungroup()
      leases2 <- leases2 %>% group_by(oneSecLocation, subPlay) %>% summarise(count=n()) %>% ungroup() %>% group_by(subPlay) %>% summarise(count1 = sum(count)) %>% ungroup()
      leases2 <- merge(leases2, leases3, by='subPlay', all.x=TRUE)
      leases2$total <- leases2$count+leases2$count1
      leases2$percentDev <- leases2$count1/leases2$total*100
      value1 <- as.integer(leases2$percentDev)
      
    }
    cols <- c( '#00a4e3', '#adafb2','#0D1540','#06357a',  '#a31c37', '#d26400', '#eaa814',
               '#5c1848', '#786592', '#ff4e50', '#027971', '#008542', '#5c6d00')
    highchart() %>%
      hc_chart(type = 'solidgauge') %>%
      hc_add_series(data = value1, name = 'Percent Developed') %>%
      hc_pane(startAngle = -90,
              endAngle = 90,background= list(
                outerRadius = '100%',
                innerRadius = '60%',
                # backgroundColor = JS("Highcharts.Color('#9DFF02').setOpacity(0.1).get()"),
                shape="arc" ))%>%
      
      hc_title(
        text = "Developed Acreage Percent",
        useHTML = TRUE) %>%
      hc_subtitle(text = '<a href="https://www.woodmac.com/research/lens/lens-direct/">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
      hc_tooltip() %>%
      #hc_xAxis(title = list(text = '<b>First Production Year</b>')) %>%
      hc_yAxis(min = 0, max = 100) %>%
      hc_credits(
        enabled = TRUE,
        text = "Powered by Highcharts",
        href = "https://www.highcharts.com/") %>%
      hc_colors(cols)%>%
      hc_exporting(enabled = TRUE, filename = 'percentDeveloped')
    
    # billboarder() %>%
    #   bb_gaugechart(value = value1, color = "#00a4e3") %>%
    #   bb_legend(show = FALSE) %>% 
    #   bb_labs(title = "Percent of Acreage with at least 1 producing well",
    #           caption = "Data source: Lens and P2 Data")
  })
  
  
  observe({
    if(input$priceType == 'Strip'){
      
      shinyjs::hide('wti')
      shinyjs::hide('hh')
      
      crude <-'https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=RWTC&f=M'
      webpage <- read_html(crude)
      tbls_ls <- webpage %>%
        html_nodes('table') %>%
        .[5] %>%
        html_table(fill = TRUE)
      wti1 <- tbls_ls[[1]]
      wti1 <- wti1 %>% filter(!is.na(Jan))
      
      wti1 <- wti1 %>% gather(DATE, WTI, -c(Year))
      wti1 <- wti1 %>% mutate(DATE = paste0(DATE,'/01/', Year))
      wti1$DATE <- as.POSIXct(wti1$DATE, format = '%b/%d/%Y')
      wti1 <- wti1 %>% arrange(DATE) %>% select(DATE, WTI)
      
      crude <-'https://www.eia.gov/dnav/ng/hist/rngwhhdm.htm'
      webpage <- read_html(crude)
      tbls_ls <- webpage %>%
        html_nodes('table') %>%
        .[5] %>%
        html_table(fill = TRUE)
      hh1 <- tbls_ls[[1]]
      hh1 <- hh1 %>% filter(!is.na(Jan))
      
      hh1 <- hh1 %>% gather(DATE, HH, -c(Year))
      hh1 <- hh1 %>% mutate(DATE = paste0(DATE,'/01/', Year))
      hh1$DATE <- as.POSIXct(hh1$DATE, format = '%b/%d/%Y')
      hh1 <- hh1 %>% arrange(DATE) %>% select(DATE, HH)
      wti1 <- wti1 %>% filter(DATE >= min(hh1$DATE))
      
      crude = 'https://www.wsj.com/market-data/quotes/futures/CRUDE%20OIL%20-%20ELECTRONIC/contracts'
      webpage <- read_html(crude)
      #tbls <- html_nodes(webpage, 'table')
      
      tbls_ls <- webpage %>%
        html_nodes('table') %>%
        .[1] %>%
        html_table(fill = TRUE)
      
      wti <- tbls_ls[[1]]
      
      crude = 'https://www.wsj.com/market-data/quotes/futures/NATURAL%20GAS/contracts'
      webpage <- read_html(crude)
      #tbls <- html_nodes(webpage, 'table')
      
      tbls_ls <- webpage %>%
        html_nodes('table') %>%
        .[1] %>%
        html_table(fill = TRUE)
      
      hh <- tbls_ls[[1]]
      
      
      rm(crude, webpage, tbls_ls)
      wti <- wti[,c('MONTH', 'SETTLEMENT')]
      hh <- hh[,c('MONTH', 'SETTLEMENT')]
      
      wti <- wti %>% filter(MONTH != 'Front Month')
      hh <- hh %>% filter(MONTH != 'Front Month')
      
      
      
      wti$YEAR <- substr(wti$MONTH, nchar(wti$MONTH)-3, nchar(wti$MONTH))
      wti$MONTH <- substr(wti$MONTH, nchar(wti$MONTH)-7, nchar(wti$MONTH)-5)
      
      
      hh$YEAR <- substr(hh$MONTH, nchar(hh$MONTH)-3, nchar(hh$MONTH))
      hh$MONTH <- substr(hh$MONTH, nchar(hh$MONTH)-7, nchar(hh$MONTH)-5)
      
      
      wti$DATE <- paste(wti$MONTH, '/01/', wti$YEAR, sep='')
      hh$DATE <- paste(hh$MONTH, '/01/', hh$YEAR, sep='')
      
      wti$DATE <- as.POSIXct(wti$DATE, format = '%b/%d/%Y')
      hh$DATE <- as.POSIXct(hh$DATE, format = '%b/%d/%Y')
      
      
      wti <- wti[,c('DATE', 'SETTLEMENT')]
      hh <- hh[,c('DATE', 'SETTLEMENT')]
      
      
      names(wti) <- c('DATE', 'WTI')
      names(hh) <- c('DATE', 'HH')
      
      date1 <- min(wti1$DATE)
      date2 <- max(hh$DATE)
      date3 <- data.frame(DATE = seq(0, 1000, 1))
      date3$DATE <- date1 %m+% months(date3$DATE) 
      date3 <- date3 %>% filter(DATE <= date2)
      wti <- rbind(wti1, wti)
      wti <- merge(date3, wti, by='DATE', all.x=TRUE)
      
      hh <- rbind(hh1, hh)
      
      price <- merge(wti, hh, by='DATE', all.x=TRUE, all.y=TRUE)
      
      rm(wti, hh)
      
      
      if (is.na(price$WTI[1])) {
        price$WTI[1] <- price$WTI[2]
      }
      price <- price %>% group_by(DATE) %>% summarise_all(mean, na.rm=TRUE) %>% ungroup()
      
      
      price$WTI <- na.locf(price$WTI)
      price$HH <- na.locf(price$HH)
      
      
      
      values$price <- price
      
      
    } else {
      #shinyjs::hide('hidePrice')
      shinyjs::show('wti')
      shinyjs::show('hh')
      
      crude <-'https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=RWTC&f=M'
      webpage <- read_html(crude)
      tbls_ls <- webpage %>%
        html_nodes('table') %>%
        .[5] %>%
        html_table(fill = TRUE)
      wti1 <- tbls_ls[[1]]
      wti1 <- wti1 %>% filter(!is.na(Jan))
      
      wti1 <- wti1 %>% gather(DATE, WTI, -c(Year))
      wti1 <- wti1 %>% mutate(DATE = paste0(DATE,'/01/', Year))
      wti1$DATE <- as.POSIXct(wti1$DATE, format = '%b/%d/%Y')
      wti1 <- wti1 %>% arrange(DATE) %>% select(DATE, WTI)
      
      crude <-'https://www.eia.gov/dnav/ng/hist/rngwhhdm.htm'
      webpage <- read_html(crude)
      tbls_ls <- webpage %>%
        html_nodes('table') %>%
        .[5] %>%
        html_table(fill = TRUE)
      hh1 <- tbls_ls[[1]]
      hh1 <- hh1 %>% filter(!is.na(Jan))
      
      hh1 <- hh1 %>% gather(DATE, HH, -c(Year))
      hh1 <- hh1 %>% mutate(DATE = paste0(DATE,'/01/', Year))
      hh1$DATE <- as.POSIXct(hh1$DATE, format = '%b/%d/%Y')
      hh1 <- hh1 %>% arrange(DATE) %>% select(DATE, HH)
      wti1 <- wti1 %>% filter(DATE >= min(hh1$DATE))
      
      crude = 'https://www.wsj.com/market-data/quotes/futures/CRUDE%20OIL%20-%20ELECTRONIC/contracts'
      webpage <- read_html(crude)
      #tbls <- html_nodes(webpage, 'table')
      
      tbls_ls <- webpage %>%
        html_nodes('table') %>%
        .[1] %>%
        html_table(fill = TRUE)
      
      wti <- tbls_ls[[1]]
      
      crude = 'https://www.wsj.com/market-data/quotes/futures/NATURAL%20GAS/contracts'
      webpage <- read_html(crude)
      #tbls <- html_nodes(webpage, 'table')
      
      tbls_ls <- webpage %>%
        html_nodes('table') %>%
        .[1] %>%
        html_table(fill = TRUE)
      
      hh <- tbls_ls[[1]]
      
      
      rm(crude, webpage, tbls_ls)
      wti <- wti[,c('MONTH', 'SETTLEMENT')]
      hh <- hh[,c('MONTH', 'SETTLEMENT')]
      
      wti <- wti %>% filter(MONTH != 'Front Month')
      hh <- hh %>% filter(MONTH != 'Front Month')
      
      
      
      wti$YEAR <- substr(wti$MONTH, nchar(wti$MONTH)-3, nchar(wti$MONTH))
      wti$MONTH <- substr(wti$MONTH, nchar(wti$MONTH)-7, nchar(wti$MONTH)-5)
      
      
      hh$YEAR <- substr(hh$MONTH, nchar(hh$MONTH)-3, nchar(hh$MONTH))
      hh$MONTH <- substr(hh$MONTH, nchar(hh$MONTH)-7, nchar(hh$MONTH)-5)
      
      
      wti$DATE <- paste(wti$MONTH, '/01/', wti$YEAR, sep='')
      hh$DATE <- paste(hh$MONTH, '/01/', hh$YEAR, sep='')
      
      wti$DATE <- as.POSIXct(wti$DATE, format = '%b/%d/%Y')
      hh$DATE <- as.POSIXct(hh$DATE, format = '%b/%d/%Y')
      
      
      wti <- wti[,c('DATE', 'SETTLEMENT')]
      hh <- hh[,c('DATE', 'SETTLEMENT')]
      
      
      names(wti) <- c('DATE', 'WTI')
      names(hh) <- c('DATE', 'HH')
      wti$WTI <- capexValues()$wti
      hh$HH <- capexValues()$hh
      
      date1 <- min(wti1$DATE)
      date2 <- max(hh$DATE)
      date3 <- data.frame(DATE = seq(0, 1000, 1))
      date3$DATE <- date1 %m+% months(date3$DATE) 
      date3 <- date3 %>% filter(DATE <= date2)
      wti <- rbind(wti1, wti)
      wti <- merge(date3, wti, by='DATE', all.x=TRUE)
      
      hh <- rbind(hh1, hh)
      
      price <- merge(wti, hh, by='DATE', all.x=TRUE, all.y=TRUE)
      
      rm(wti, hh)
      
      
      if (is.na(price$WTI[1])) {
        price$WTI[1] <- price$WTI[2]
      }
      
      price <- price %>% group_by(DATE) %>% summarise_all(mean, na.rm=TRUE)
      
      price$WTI <- na.locf(price$WTI)
      price$HH <- na.locf(price$HH)
      values$price <- price
      
    }
  })
  
  
  
  output$stripPrice <- renderHighchart({
    df <- values$price
    #df <- df %>% gather(Fluid, Price, -c(DATE))
    df <- as.data.frame(df)
    df$DATE <- as.Date(df$DATE)
    #print(head(df))
    #df$DATE <- as.character(df$DATE)
    
    # billboarder(data = df) %>%
    #   bb_linechart(
    #     mapping = bbaes(x=DATE, y=WTI), type = 'line', width = 2
    #   ) %>%
    #   bb_linechart(
    #     mapping = bbaes(x=DATE, y=HH), type = 'line', width = 2
    #   ) %>%
    #   bb_colors_manual(WTI = 'green', HH = 'red') %>%
    #   bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>%
    #   bb_data(axes = list(
    #     "WTI" = "y",
    #     "HH" = "y2"
    #   )) %>%
    #   bb_axis(
    #     y2 = list(show = TRUE, label = "$/MCF"),
    #     y = list(label = "$/BBL")
    #   ) %>% 
    #   bb_labs(title = "Price Forecast",
    #           caption = "Data source: EIA and WSJ")
    cols <- c(  '#008542', '#ff4e50', '#5c6d00','#00a4e3', '#adafb2','#0D1540','#06357a',  '#a31c37', '#d26400', '#eaa814',
                '#5c1848', '#786592', '#027971')
    
    # hchart(df, type = 'line',
    #           hcaes(x = as.Date(DATE), y = Price, group = Fluid))%>%
    #   hc_title(
    #     text = "Srip Pricing",
    #     useHTML = FALSE) %>%
    #   hc_subtitle(text = '<a href="www.wsj.com">Source: Wall Street Journal</a>', useHTML=TRUE, align = 'right') %>%
    #   #hc_tooltip(table = TRUE, sort = TRUE) %>%
    #   hc_xAxis(title = list(text = '<b>DATE</b>')) %>%
    #   hc_yAxis(title = list(text = '<b>$/BBL</b>')) %>%
    #   hc_credits(
    #     enabled = TRUE,
    #     text = "Powered by Highcharts",
    #     href = "https://www.highcharts.com/") %>%
    #   hc_colors(cols)
      
    hchart(df[,c('DATE', 'WTI')], type = 'line', hcaes(x = 'DATE', y='WTI')) %>% 
      #hc_add_series(data = df$WTI, type = 'line') %>% 
      hc_add_series(data = df[,c('DATE', 'HH')], type = 'line', hcaes(x = 'DATE', y='HH'), yAxis = 1) %>% 
      hc_yAxis_multiples(
        list(lineWidth = 3, title=list(text="WTI, $/bbl")),
        list(lineWidth = 3,  title=list(text="Henry Hub, $/mcf"))
      )%>%
      hc_title(
        text = "Strip Pricing",
        useHTML = FALSE) %>%
      hc_subtitle(text = '<a href="www.wsj.com">Source: Wall Street Journal</a>', useHTML=TRUE, align = 'right') %>%
      #hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = '<b>DATE</b>')) %>%
      #hc_yAxis(title = list(text = '<b>$/BBL</b>')) %>%
      hc_credits(
        enabled = TRUE,
        text = "Powered by Highcharts",
        href = "https://www.highcharts.com/") %>%
      hc_colors(cols)%>%
      hc_exporting(enabled = TRUE, filename = 'stripPrice')
    
  })
  declineValues <- reactive({
    data.frame(
      Component = c('qiOilS', 'bOilS', 'DiOilS', 'DfOilS', 'curtailOilS',
                    'qiGasS', 'bGasS', 'DiGasS', 'DfGasS', 'curtailGasS',
                    'wellLifeS', 'spudToProd', 'cutoff', 'prbRisking', 'possRisking'),
      
      Value = c(input$qiOilS, input$bOilS, input$DiOilS, input$DfOilS, input$curtailOilS,
                input$qiGasS, input$bGasS, input$DiGasS, input$DfGasS, input$curtailGasS,
                input$wellLifeS, input$spudToProd, input$cutoff, input$prbRisking, input$possRisking),
      stringsAsFactors = FALSE) %>% spread(Component, Value)
    
  })
  
  prodData1 <- reactive(
    prodData %>% filter(API %in% df()$API)
  )
  
  observe({
    if(is.null(input$subPlay)){
      NULL
    } else {
      
      if(is.null(input$selectYr)||input$selectYr == ''){
        NULL
      } else {
        df <- as.data.frame(df()) %>% 
          filter(subPlay %in% input$subPlay) %>%
          filter(operator %in% input$operatorSelect) %>%
          filter(operator != 'OTHER') %>% filter(fp.year %in% as.numeric(input$selectYr))
        
        
        
        prod.data <- prodData1() %>% arrange(API, date) %>% filter(API %in% df$API)# %>% group_by(API) %>% mutate(month = cumsum(API/API))
        prod.data <- merge(prod.data, df[,c('API', 'perf', 'fp.year')], by='API', all.x=TRUE)
        print(head(prod.data))
        if(nrow(prod.data) == 0){
          NULL
        } else {
          if(input$productSelect == 'Oil'){
            shinyjs::hide('qiGasS')
            shinyjs::hide('DiGasS')
            shinyjs::hide('bGasS')
            shinyjs::hide('DfGasS')
            shinyjs::hide('curtailGasS')
            shinyjs::show('qiOilS')
            shinyjs::show('DiOilS')
            shinyjs::show('bOilS')
            shinyjs::show('DfOilS')
            shinyjs::show('curtailOilS')
            prod.data <- prod.data %>% filter(oil > 0) %>% mutate(oil = oil/perf*as.numeric(capexValues()$perfSelect)) %>%
              group_by(API) %>% mutate(month=cumsum(API/API)) %>% ungroup()
            prod.data <- prod.data[,c('API', 'fp.year', 'month', 'oil')]
            values$life <- max(prod.data$month)
            meanProd <- prod.data %>% group_by(month) %>% summarise(oil = mean(oil, na.rm=TRUE)/30.45, count=n()) #%>%
            prod.data <- prod.data %>% group_by(fp.year, month) %>% summarise(oil = mean(oil, na.rm=TRUE)/30.45) %>% ungroup()
            #filter(count >= max(count)*0.4)
            #print(head(meanProd))
            cols <- c(  '#008542', '#ff4e50', '#5c6d00','#00a4e3', '#adafb2','#0D1540','#06357a',  '#a31c37', '#d26400', '#eaa814',
                        '#5c1848', '#786592', '#027971')
            
            # hchart(df, type = 'line',
            #           hcaes(x = as.Date(DATE), y = Price, group = Fluid))%>%
            #   hc_title(
            #     text = "Srip Pricing",
            #     useHTML = FALSE) %>%
            #   hc_subtitle(text = '<a href="www.wsj.com">Source: Wall Street Journal</a>', useHTML=TRUE, align = 'right') %>%
            #   #hc_tooltip(table = TRUE, sort = TRUE) %>%
            #   hc_xAxis(title = list(text = '<b>DATE</b>')) %>%
            #   hc_yAxis(title = list(text = '<b>$/BBL</b>')) %>%
            #   hc_credits(
            #     enabled = TRUE,
            #     text = "Powered by Highcharts",
            #     href = "https://www.highcharts.com/") %>%
            #   hc_colors(cols)
            
            p <- hchart(prod.data[,c('fp.year', 'month', 'oil')], type = 'line', hcaes(x = 'month', y='oil', group = 'fp.year')) %>% 
              #hc_add_series(data = df$WTI, type = 'line') %>% 
              hc_add_series(data = meanProd[,c('month', 'oil')], type = 'line', hcaes(x = 'month', y='oil'),name = 'Mean') %>% 
              hc_title(
                text = "Oil Spaghetti Plot - Lateral Length Normalized",
                useHTML = FALSE) %>%
              hc_subtitle(text = '<a href="www.woodmac.com">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
              #hc_tooltip(table = TRUE, sort = TRUE) %>%
              hc_xAxis(title = list(text = '<b>Months Producing</b>')) %>%
              hc_yAxis(title = list(text = '<b>Average Production: BBL/D</b>')) %>%
              hc_credits(
                enabled = TRUE,
                text = "Powered by Highcharts",
                href = "https://www.highcharts.com/") %>%
              hc_colors(cols)
            
            # p <- plot_ly(prod.data, x=~month, y=~oil/30.45, group=~as.factor(API), color=I('gray'), type='scatter', mode='lines', name='Actuals')%>%
            #   #add_trace(data = fitOil, x=~month, y=~oilFCST/30.45, color=I('green'), name = 'Forecast', type='scatter', mode='lines') %>%
            #   add_trace(data = meanProd, x=~month, y=~oil/30.45, color=I('black'), name = 'Average', type='scatter', mode='lines')#%>%
            # #layout(title = title, yaxis = list(title='Daily Oil Rate, bopd', type='log'), xaxis=  list(title = 'Month'))
          } else {
            shinyjs::show('qiGasS')
            shinyjs::show('DiGasS')
            shinyjs::show('bGasS')
            shinyjs::show('DfGasS')
            shinyjs::show('curtailGasS')
            shinyjs::hide('qiOilS')
            shinyjs::hide('DiOilS')
            shinyjs::hide('bOilS')
            shinyjs::hide('DfOilS')
            shinyjs::hide('curtailOilS')
            prod.data <- prod.data %>% filter(gas > 0) %>% mutate(gas = gas/perf*as.numeric(capexValues()$perfSelect)) %>%
              group_by(API) %>% mutate(month=cumsum(API/API)) %>% ungroup()
            prod.data <- prod.data[,c('API', 'fp.year', 'month', 'gas')]
            values$life <- max(prod.data$month)
            
            meanProd <- prod.data %>% group_by(month) %>% summarise(gas = mean(gas, na.rm=TRUE)/30.45, count=n())# %>%
            prod.data <- prod.data %>% group_by(fp.year, month) %>% summarise(gas = mean(gas, na.rm=TRUE)/30.45) %>% ungroup()
            #filter(count >= max(count)*0.4)
            cols <- c(  '#008542', '#ff4e50', '#5c6d00','#00a4e3', '#adafb2','#0D1540','#06357a',  '#a31c37', '#d26400', '#eaa814',
                        '#5c1848', '#786592', '#027971')
            p <- hchart(prod.data[,c('fp.year', 'month', 'gas')], type = 'line', hcaes(x = 'month', y='gas', group = 'fp.year')) %>% 
              #hc_add_series(data = df$WTI, type = 'line') %>% 
              hc_add_series(data = meanProd[,c('month', 'gas')], type = 'line', hcaes(x = 'month', y='gas'),name = 'Mean') %>% 
              hc_subtitle(text = '<a href="www.woodmac.com">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
              #hc_tooltip(table = TRUE, sort = TRUE) %>%
              hc_xAxis(title = list(text = '<b>Months Producing</b>')) %>%
              hc_yAxis(title = list(text = '<b>Average Production: MCF/D</b>')) %>%
              hc_credits(
                enabled = TRUE,
                text = "Powered by Highcharts",
                href = "https://www.highcharts.com/") %>%
              hc_colors(cols)
            # p <- plot_ly(prod.data, x=~month, y=~gas/30.45, group=~as.factor(API), color=I('gray'), type='scatter', mode='lines', name='Actuals') %>%
            #   #add_trace(data = fitGas, x=~month, y=~gasFCST/30.45, color=I('red'), name = 'Forecast', type='scatter', mode='lines')%>%
            #   add_trace(data = meanProd, x=~month, y=~gas/30.45, color=I('black'), name = 'Average', type='scatter', mode='lines')#%>%
            # #layout(title = title, yaxis = list(title='Daily Gas Rate, mcfd', type='log'), xaxis=  list(title = 'Month'))
          }
          values$p <- p
        }
      }
    }
    
  })
  
  
  observeEvent(input$selectYr, {
    df1 <- df() %>% filter(subPlay %in% input$subPlay) %>% filter(as.character(fp.year) %in% input$selectYr)
    updateMultiInput(session, 'operatorSelect', 'Select Operator(s)', choices = sort(unique(df1$operator)), selected =  sort(unique(df1$operator))[1])
    
  })
  
  output$spPlot1 <- renderHighchart({
    if(is.null(values$p)){
      NULL
    } else {
      
      if(input$productSelect == 'Oil'){
        #print(head(prod.data))
        fitOil <- curtailed.q(arps.decline(
          as.numeric(declineValues()$qiOilS)*365, as.nominal(as.numeric(declineValues()$DiOilS)), as.numeric(declineValues()$bOilS), as.nominal(as.numeric(declineValues()$DfOilS))),
          as.numeric(declineValues()$curtailOilS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
        
        fitOil <- as.data.frame(fitOil)
        #rm(fitOil)
        names(fitOil) <- c('oilFCST')
        if(declineValues()$curtailOilS > 0){
          fitOil$oilFCST[1] <- fitOil$oilFCST[1]/2
        }
        
        fitOil <- fitOil %>% mutate(month = nrow(fitOil)) %>% mutate(month = cumsum(month/month))
        
        fcstOil <- sum(fitOil$oil)
        values$qiOil <- as.numeric(declineValues()$qiOilS)/(fcstOil/1000)
        #print(values$qiOil)
        title <- paste('Forecast Oil EUR (MBO): ', as.integer(fcstOil/1000), sep='')
        fitOil <- fitOil %>% filter(month <= values$life)
        fitOil$oilFCST <- fitOil$oilFCST/30.45
        p <- values$p %>%
          hc_add_series(data = fitOil[,c('month', 'oilFCST')], type = 'line', hcaes(x = 'month', y='oilFCST'),name = 'Forecast') %>%
          hc_title(
            text = title,
            useHTML = FALSE)%>%
          hc_exporting(enabled = TRUE, filename = 'oilTC')
        # p <- values$p %>%
        #   add_trace(data = fitOil, x=~month, y=~oilFCST/30.45, color=I('green'), name = 'Forecast', type='scatter', mode='lines') %>%
        #   #add_trace(data = meanProd, x=~month, y=~oil/30.45, color=I('black'), name = 'Average', type='scatter', mode='lines')%>%
        #   layout(title = title, yaxis = list(title='Daily Oil Rate, bopd', type='log'), xaxis=  list(title = 'Month'))
      } else {
        
        
        fitGas <- curtailed.q(arps.decline(
          as.numeric(declineValues()$qiGasS)*365, as.nominal(as.numeric(declineValues()$DiGasS)), as.numeric(declineValues()$bGasS), as.nominal(as.numeric(declineValues()$DfGasS))),
          as.numeric(declineValues()$curtailGasS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
        
        fitGas <- as.data.frame(fitGas)
        #rm(fitOil)
        names(fitGas) <- c('gasFCST')
        if(declineValues()$curtailGasS > 0){
          fitGas$gasFCST[1] <- fitGas$gasFCST[1]/2
        }
        fitGas <- fitGas %>% mutate(month = nrow(fitGas)) %>% mutate(month = cumsum(month/month))
        
        fcstGas <- sum(fitGas$gas)
        values$qiGas <- as.numeric(declineValues()$qiGasS)/(fcstGas/1000)
        #print(values$qiGas)
        title <- paste('Forecast Gas EUR (MMCF): ', as.integer(fcstGas/1000), sep='')
        fitGas <- fitGas %>% filter(month <= values$life)
        
        fitGas$gasFCST <- fitGas$gasFCST/30.45
        p <- values$p %>%
          hc_add_series(data = fitGas[,c('month', 'gasFCST')], type = 'line', hcaes(x = 'month', y='gasFCST'),name = 'Forecast') %>%
          hc_title(
            text = title,
            useHTML = FALSE)%>%
          hc_exporting(enabled = TRUE, filename = 'gasTC')
        # p <- values$p %>%
        #   add_trace(data = fitGas, x=~month, y=~gasFCST/30.45, color=I('red'), name = 'Forecast', type='scatter', mode='lines')%>%
        #   #add_trace(data = meanProd, x=~month, y=~gas/30.45, color=I('black'), name = 'Average', type='scatter', mode='lines')%>%
        #   layout(title = title, yaxis = list(title='Daily Gas Rate, mcfd', type='log'), xaxis=  list(title = 'Month'))
        # 
      }
      p
    }
  })
  
  
  observeEvent(input$calcInv, {
    if(is.null(input$subPlay)){
      NULL
    }else {
      updateActionButton(session, "calcInv",
                         label = 'Calculating....')
      shinyjs::disable('calcInv')
      
      
      df <- leases() %>% filter(operator %in% input$operator) %>% filter(subPlay %in% input$subPlay) %>% filter(!duplicated(oneSecLocation))
      
      df3 <- df %>% group_by(operator) %>% summarise(acres = n()*640) %>% ungroup()
      df3 <- as.data.frame(df3)
      
      
      df1 <- df() %>% filter(oneSecLocation %in% df$oneSecLocation) %>% 
        #filter(reservoir %in% input$reservoirSelect) %>%
        group_by(oneSecLocation) %>% summarise(wells=n(), perf = mean(perf, na.rm=TRUE),
                                               ppf =mean(ppf, na.rm=TRUE), fpf = mean(fpf, na.rm=TRUE))
      df <- merge(df, df1, by='oneSecLocation', all.x=TRUE)
      
      
      
      df$bOil <- as.numeric(declineValues()$bOilS)
      df$bGas <- as.numeric(declineValues()$bGasS)
      df$DfOil <- as.numeric(declineValues()$DfOilS)
      df$DfGas <- as.numeric(declineValues()$DfGasS)
      #rm(factors)
      df$oldPerf <- df$perf
      df$oldPPF <- df$ppf
      df$oldFPF <- df$fpf
      df$perf <- as.numeric(capexValues()$perfSelect)
      df$ppf <- as.numeric(capexValues()$ppfSelect)
      df$fpf <- as.numeric(capexValues()$fpfSelect)
      
      
      
      df$wells[is.na(df$wells)] <- 0
      #print(head(df))                                        
      
      qiOil <- as.numeric(values$qiOil)
      qiGas <- as.numeric(values$qiGas)
      
      
      
      
      df <- df %>% group_by(oneSecLocation) %>% 
        mutate(risk1 = mean(risk, na.rm=TRUE), tvd1 = mean(tvd, na.rm=TRUE), gasFrac1 = mean(gasFrac, na.rm=TRUE))
      df$risk[is.na(df$risk)] <- df$risk1[is.na(df$risk)]
      df$tvd[is.na(df$tvd)] <- df$tvd1[is.na(df$tvd)]
      df$gasFrac[is.na(df$gasFrac)] <- df$gasFrac1[is.na(df$gasFrac)]
      df <- df %>% group_by(threeSecLocation) %>%
        mutate(risk1 = mean(risk, na.rm=TRUE), tvd1 = mean(tvd, na.rm=TRUE), gasFrac1 = mean(gasFrac, na.rm=TRUE))
      df$risk[is.na(df$risk)] <- df$risk1[is.na(df$risk)]
      df$tvd[is.na(df$tvd)] <- df$tvd1[is.na(df$tvd)]
      df$gasFrac[is.na(df$gasFrac)] <- df$gasFrac1[is.na(df$gasFrac)]
      df <- df %>% group_by(possLocation) %>%
        mutate(risk1 = mean(risk, na.rm=TRUE), tvd1 = mean(tvd, na.rm=TRUE), gasFrac1 = mean(gasFrac, na.rm=TRUE))
      df$risk[is.na(df$risk)] <- df$risk1[is.na(df$risk)]
      df$tvd[is.na(df$tvd)] <- df$tvd1[is.na(df$tvd)]
      df$gasFrac[is.na(df$gasFrac)] <- df$gasFrac1[is.na(df$gasFrac)]
      df <- df %>% group_by(twnRngLocation) %>%
        mutate(risk1 = mean(risk, na.rm=TRUE), tvd1 = mean(tvd, na.rm=TRUE), gasFrac1 = mean(gasFrac, na.rm=TRUE))
      df$risk[is.na(df$risk)] <- df$risk1[is.na(df$risk)]
      df$tvd[is.na(df$tvd)] <- df$tvd1[is.na(df$tvd)]
      df$gasFrac[is.na(df$gasFrac)] <- df$gasFrac1[is.na(df$gasFrac)]
      df$logPPF <- log(df$ppf)
      df <- as.data.frame(df)
      df <- subset(df, select=-c(risk1, tvd1, gasFrac1))
      df <- df %>% filter(!is.na(risk)) %>% filter(!is.na(tvd)) %>% filter(!is.na(gasFrac))
      df$md <- df$tvd + df$perf
      #df$bOil[is.na(df$bOil)] <- (as.numeric(declineValues()$bOilS))
      #df$bGas[is.na(df$bGas)] <- (as.numeric(declineValues()$bGasS))
      #df$DfOil[is.na(df$DfOil)] <- (as.numeric(declineValues()$DfOilS))
      #df$DfGas[is.na(df$DfGas)] <- (as.numeric(declineValues()$DfGasS))
      
      
      
      if(nrow(df) == 0) {
        df <- data.frame('Summary' = 'No Economic Locations')
      } else {
        
        x <- -0.2931903
        y <- -0.4256987
        z <- 0.2750021
        perfUplift <- perfUplift
        propUplift <- propUplift
        df$x <- x
        df$y <- y
        df$z <- z
        df$EUR <- exp(predict(propUplift, df))
        df$EUR <- df$EUR*df$risk
        df$perfRisk <- perfUplift$coefficients[1] + 
          perfUplift$coefficients[2]*df$perf + 
          perfUplift$coefficients[3]*(df$perf**2)
        df$EUR <- df$EUR*df$perfRisk
        df$oilEUR <- df$EUR*(1-df$gasFrac)
        df$gasEUR <- (df$EUR-df$oilEUR)*20
        
        pdpCount <- df() %>% #filter(subBasin %in% input$subBasinSelect) %>% 
          filter(subPlay %in% input$subPlay) %>%
          group_by(oneSecLocation) %>% summarise(pdpCount=n()) %>% ungroup()
        pudCount <-  df()%>% #filter(subBasin %in% input$subBasinSelect) %>% 
          filter(subPlay %in% input$subPlay) %>%
          group_by(twoSecLocation) %>% summarise(pudCount=n())%>% ungroup()
        prbCount <-  df() %>% #filter(subBasin %in% input$subBasinSelect) %>% 
          filter(subPlay %in% input$subPlay) %>%
          group_by(threeSecLocation) %>% summarise(prbCount = n()) %>% ungroup()
        possCount <-  df() %>% #filter(subBasin %in% input$subBasinSelect) %>% 
          filter(subPlay %in% input$subPlay) %>%
          group_by(possLocation) %>% summarise(possCount=n())
        
        df <- merge(df, pudCount, by=c('twoSecLocation'), all.x=TRUE)
        df <- merge(df, prbCount, by=c('threeSecLocation'), all.x=TRUE)
        df <- merge(df, possCount, by=c('possLocation'), all.x=TRUE)
        rm(pudCount, prbCount, possCount)
        df$rsvCat <- NA_character_
        df$rsvCat[!is.na(df$pudCount)] <- 'PUD'
        df$rsvCat[is.na(df$rsvCat) & !is.na(df$prbCount)] <- 'PRB'
        df$rsvCat[is.na(df$rsvCat) & !is.na(df$possCount)] <- 'POSS'
        df <- merge(df, pdpCount, by=c('oneSecLocation'), all.x=TRUE)
        rm(pdpCount)
        df$rsvCat[!is.na(df$pdpCount)] <- 'PUD'
        totalAcreage <- nrow(df)*640
        df <- df %>% filter(!is.na(rsvCat))
        
        df$oilEUR[df$rsvCat == 'PRB'] <- df$oilEUR[df$rsvCat == 'PRB']*as.numeric(declineValues()$prbRisking)
        df$gasEUR[df$rsvCat == 'PRB'] <- df$gasEUR[df$rsvCat == 'PRB']*as.numeric(declineValues()$prbRisking)
        df$oilEUR[df$rsvCat == 'POSS'] <- df$oilEUR[df$rsvCat == 'POSS']*as.numeric(declineValues()$possRisking)
        df$gasEUR[df$rsvCat == 'POSS'] <- df$gasEUR[df$rsvCat == 'POSS']*as.numeric(declineValues()$possRisking)
        df$qiOil <- df$oilEUR * qiOil
        df$qiGas <- df$gasEUR * qiGas
        df$oilEUR <-df$oilEUR*1000
        df$gasEUR <- df$gasEUR*1000
        
        df$wellsPerSection <- as.numeric(expenseValues()$wellSpacing)
        df$remWells <- df$wellsPerSection - df$wells
        df$oldPerf[is.na(df$oldPerf)] <- as.numeric(capexValues()$perfSelect)
        df$remWells <- df$remWells*5280/df$oldPerf
        
        df$remWells[df$remWells < 0] <- 0
        df$remWells <- as.integer(df$remWells)
        df$remWells[df$remWells > as.numeric(expenseValues()$wellSpacing)] <- as.numeric(expenseValues()$wellSpacing)
        #print('Good so far!')
        #subBasins <- values$df %>% filter(subPlay %in% input$subPlaySelect)
        df$subBasin <- df()$subBasin[1]
        #df$subBasin[df$subBasin == 'DJ'] <- 'Niobrara'
        #df$subBasin[df$subBasin == 'Powder'] <- 'Niobrara'
        
        #print(head(df))
        working1 <- merge(df, costData, by='subBasin', all.x=TRUE)
        
        working1$cementing <- working1$md*8
        working1$proppant <- working1$ppf*working1$perf
        working1$fluid <- working1$fpf*working1$perf
        working1$proppantPerStage <- as.numeric(capexValues()$proppantPerStage)
        working1$stages <- working1$proppant/working1$proppantPerStage
        working1$compDays <- working1$stages/as.numeric(capexValues()$stagesPerDay)
        working1$drillDays <- working1$md/as.numeric(capexValues()$drillSpeed)
        working1 <- working1 %>% 
          mutate(drillCost = tvd*sixteenCasingTVD*sixteenRate+
                   tvd*thirteenCasingTVD*thirteenRate+tvd*nineCasingTVD*nineRate +
                   tvd*sevenCasingTVD*sevenRate+tvd*fiveCasingTVD*fiveRate +
                   md*fiveCasingTMD*fiveRate+md*fourCasingTMD*fourRate +
                   tvd*tubingTVD*tubingRate+as.numeric(capexValues()$padConstruction)+
                   as.numeric(capexValues()$drillingFluidsPerDay)*drillDays+
                   as.numeric(capexValues()$drillingFuelPerDay)*drillDays+
                   as.numeric(capexValues()$ancDrillPerDay)*drillDays+cementing+rigRate*drillDays)
        
        working1$brownPercent <- as.numeric(capexValues()$brownSelect)
        working1$ceramicPercent <- as.numeric(capexValues()$ceramicSelect)
        working1$rcsPercent <- as.numeric(capexValues()$rcsSelect)
        working1$northernPercent <- as.numeric(capexValues()$northernSelect)
        
        working1 <- working1 %>%
          mutate(compCost = fleetRateComp*fleetRatePerHP*compDays+fluid*waterAcqRate/42 +
                   fluid*waterHaulingToRate/42+fluid*waterTreatmentRate/42+
                   fluid*waterHaulingFromRate/42*waterFlowbackPercent +
                   fluid*waterDispRate*waterFlowbackPercent/42+
                   (brownPercent*sandRate+ceramicPercent*ceramicRate +
                      rcsPercent*rcsRate+northernPercent*northernRate)*proppant +
                   (brownPercent*sandTranspRate +ceramicPercent*ceramicTranspRate +
                      rcsPercent*rcsTranspRate+northernPercent*northernTranspRate)*proppant +
                   stages*as.numeric(capexValues()$chemsPerStage)+
                   stages*as.numeric(capexValues()$pumpFuel)+compDays*as.numeric(capexValues()$ancCompPerDay))
        
        working1 <- working1 %>% mutate(facilities = (drillCost+compCost)/(1-eNt)*facil/2,
                                        equipment = (drillCost+compCost)/(1-eNt)*facil/2,
                                        pipeline = (drillCost+compCost)/(1-eNt)*tieIn)
        
        working1 <- working1 %>% mutate(dNc = drillCost+compCost, dcet = dNc + facilities + equipment + pipeline)
        
        df$dNc <- working1$dNc*as.numeric(capexValues()$capexStressor)
        df$dcet <- working1$dcet*as.numeric(capexValues()$capexStressor)
        rm(working1)
        
        df <- as.data.frame(df)
        
        df$DiGas <-  declineValues()$DiGasS    
        
        df$DiOil <- declineValues()$DiOilS  
        
        
        
        
        #df <- bind_rows(econSummary)
        #df <- as.data.frame(df)
        #print(nrow(df))
        df <- df %>% filter(!duplicated(oneSecLocation))
        
        #print(nrow(df))
        #rm(econSummary)
        
        #print(head(df))
        #df <- as.data.frame(df)
        
        
        econSummary <- lapply(split(df, df[,'oneSecLocation']), function (df1) tryCatch({
          df2 <- df1
          fitOil <- curtailed.q(arps.decline(
            as.numeric(df1$qiOil)*365, (as.numeric(df1$DiOil)), as.numeric(df1$bOil), as.nominal(as.numeric(df1$DfOil))),
            as.numeric(declineValues()$curtailOilS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
          
          fit <- as.data.frame(fitOil)
          rm(fitOil)
          names(fit) <- c('oilFCST')
          #if(input$curtailOil1 > 0){
          #  fit$oilFCST[1] <- fit$oilFCST[1]/2
          #}
          
          
          fit <- fit %>% mutate(prodMonth = nrow(fit)) %>% mutate(prodMonth = cumsum(prodMonth/prodMonth))
          
          fitGas <- curtailed.q(arps.decline(
            as.numeric(df1$qiGas)*365, (as.numeric(df1$DiGas)), as.numeric(df1$bGas), as.nominal(as.numeric(df1$DfGas))),
            as.numeric(declineValues()$curtailGasS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
          names(fitGas) <- c('gasFCST')
          fit$gasFCST <- fitGas
          #if(input$curtailGas1 > 0){
          #  fit$gasFCST[1] <- fit$gasFCST[1]/2
          #}
          
          fit <- fit[,c('prodMonth', 'oilFCST', 'gasFCST')]
          names(fit) <- c('Months', 'Oil', 'Gas')
          fit$Water <- fit$Oil + fit$Gas/20
          df <- fit
          rm(fit)
          
          #print(head(df))
          #df$Oil <- df$Oil*expenseValues()$wi/100
          #df$Gas <- df$Gas*expenseValues()$wi/100
          #df$Water <- df$Water*expenseValues()$wi/100
          df$Sales_Gas <- df$Gas*expenseValues()$shrink
          df$NGL <- df$Gas*expenseValues()$nglYield/1000
          #df$wi <- expenseValues()$wi
          df1 <- data.frame(Months = seq(((declineValues()$spudToProd-1)*-1), 0, 1), Oil = 0, Gas = 0, Water = 0)
          df$capex <- 0
          df1$capex <- 0
          df1$capex[1] <- df2$dcet
          #df1$capex[nrow(df1)] <- expenseValues()$completeCost*expenseValues()$wi/100
          if(input$priceType == 'Flat'){
            df$oilPrice <- capexValues()$wti
            df$gasPrice <- capexValues()$hh
            df$nglPrice <- df$oilPrice*expenseValues()$nglDiff/100
          } else {
            prices <- values$price
            prices <- prices %>% filter(DATE >= today())
            prices <- prices[(declineValues()$spudToProd):nrow(prices),]
            if(nrow(prices) > nrow(df)){
              prices <- prices[1:nrow(df),]
            }
            df$oilPrice <- NA
            df$oilPrice[1:nrow(prices)] <- prices$WTI
            df$gasPrice <- NA
            df$gasPrice[1:nrow(prices)] <- prices$HH
            rm(prices)
            df$oilPrice <- na.locf(df$oilPrice)
            df$gasPrice <- na.locf(df$gasPrice)
            df$nglPrice <- df$oilPrice*expenseValues()$nglDiff/100
          }
          #print(head(df))
          df$nri <- expenseValues()$nri#*expenseValues()$wi/100
          df$oilRev <- (df$oilPrice-expenseValues()$oilDiff)*df$nri/100*df$Oil
          df$gasRev <- (df$gasPrice-expenseValues()$gasDiff)*df$nri/100*df$Sales_Gas*expenseValues()$btu
          df$nglRev <- (df$nglPrice)*df$nri/100*df$NGL
          df$rev <- df$oilRev+df$gasRev+df$nglRev
          #print(head(df))
          df$tax <- df$oilRev*expenseValues()$stxOil/100 + (df$gasRev+df$nglRev)*expenseValues()$stxGas/100 +
            df$Oil*df$nri/100*expenseValues()$oilSTX + df$Gas*df$nri/100*expenseValues()$gasSTX +
            df$rev*df$nri/100*expenseValues()$atx/100
          #print(head(df))
          #print(expenseValues()$fixedCost)
          #print(expenseValues()$varExpGas)
          #print(expenseValues()$varExpBOE)
          
          
          df$expense <- expenseValues()$fixedCost + df$Gas*expenseValues()$varExpGas + 
            (df$Oil + df$NGL)*expenseValues()$varExpBOE
          #print(head(df))
          df$nocf <- df$rev-df$tax-df$expense
          
          
          
          if(input$econAbanS == 'Yes'){
            df <- df[nrow(df):1,]
            df$cumNOCF <- cumsum(df$nocf)
            df$prev <- df$cumNOCF- dplyr::lag(df$cumNOCF, n = 1L)
            prev <- which(df$prev > 0)[1]
            df <- df[prev:nrow(df),]
            df <- df[nrow(df):1,]
            df <- subset(df, select = -c(cumNOCF, prev))
          }
          df <- as.data.frame(df)
          df$pna <- 0
          df$pna[nrow(df)] <- expenseValues()$pna
          
          Missing <- setdiff(dput(names(df)), names(df1))  # Find names of missing columns
          df1[Missing] <- 0                    # Add them, filled with '0's
          df1 <- df1[,names(df)]
          
          df <- rbind(df1, df)
          df$fcf <- df$nocf - df$capex - df$pna
          df$Months <- seq(0,nrow(df)-1,1)
          
          
          
          
          
          df$pv10 <- df$fcf/(1.1^(df$Months/12))
          
          df2$IRR <- IRRcalc(df$fcf, df$Months)
          
          
          df2
        },
        error=function(e) {
          e
          NULL
        })
        
        )
        
        df <- bind_rows(econSummary)
        df <- df %>% filter(IRR >= (as.numeric(declineValues()$cutoff)/100))
        #print(head(df))
        
        
        
        if(nrow(df) == 0){
          df <- data.frame('Summary' = 'No Economic Locations')
          
        } else {
          #print(head(df))
          df$oldPPF[is.na(df$oldPPF)] <- as.numeric(capexValues()$ppfSelect)
          df$oldFPF[is.na(df$oldFPF)] <- as.numeric(capexValues()$fpfSelect)
          df$wellSpacing <- 5280/(as.numeric(expenseValues()$wellSpacing))
          df <- df[,c('subPlay', 'operator', 
                      'wells', 'remWells', 'wellSpacing',
                      'qiOil', 'bOil', 'DiOil', 'DfOil', 'oilEUR',
                      'qiGas', 'bGas', 'DiGas', 'DfGas', 'gasEUR', 'dNc', 'dcet')]
          
          df$qiOilTotal <- df$qiOil*df$remWells
          df$qiGasTotal <- df$qiGas*df$remWells
          df$oilEURTotal <- df$oilEUR*df$remWells
          df$gasEURTotal <- df$gasEUR*df$remWells
          df$dNcTotal <- df$dNc*df$remWells
          df$dcetTotal <- df$dcet*df$remWells
          
          df <- df %>% filter(remWells > 0)
          viableAcreage <- nrow(df)*640/totalAcreage
          values$viableAcreage <- viableAcreage
          #print(viableAcreage)
          #print(summary(df))
          #print(head(df))
          df <- df %>% group_by(subPlay, operator) %>% 
            summarise(wells = sum(wells), remWells = sum(remWells), 
                      wellSpacing = mean(wellSpacing), qiOil = sum(qiOilTotal), 
                      bOil = mean(bOil), DiOil = mean(DiOil), DfOil = mean(DfOil),
                      oilEUR = sum(oilEURTotal), 
                      qiGas = sum(qiGasTotal), bGas = mean(bGas), DiGas = mean(DiGas),
                      DfGas = mean(DfGas), gasEUR = sum(gasEURTotal), dNc = sum(dNcTotal),
                      dcet = sum(dcetTotal)) %>% ungroup() %>%  mutate(qiOil = qiOil/remWells,
                                                                       qiGas = qiGas/remWells,
                                                                       oilEUR = oilEUR/remWells,
                                                                       gasEUR = gasEUR/remWells,
                                                                       dNc = dNc/remWells,
                                                                       dcet = dcet/remWells)
          
          #print(summary(df))
          
          df <- as.data.frame(df)
          #print(head(df))
          
          econSummary <- lapply(split(df, df[,'operator']), function (df1) tryCatch({
            df2 <- df1
            fitOil <- curtailed.q(arps.decline(
              as.numeric(df1$qiOil)*365, (as.numeric(df1$DiOil)), as.numeric(df1$bOil), as.nominal(as.numeric(df1$DfOil))),
              as.numeric(declineValues()$curtailOilS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
            
            fit <- as.data.frame(fitOil)
            rm(fitOil)
            names(fit) <- c('oilFCST')
            #if(input$curtailOil1 > 0){
            #  fit$oilFCST[1] <- fit$oilFCST[1]/2
            #}
            
            
            fit <- fit %>% mutate(prodMonth = nrow(fit)) %>% mutate(prodMonth = cumsum(prodMonth/prodMonth))
            
            fitGas <- curtailed.q(arps.decline(
              as.numeric(df1$qiGas)*365, (as.numeric(df1$DiGas)), as.numeric(df1$bGas), as.nominal(as.numeric(df1$DfGas))),
              as.numeric(declineValues()$curtailGasS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
            names(fitGas) <- c('gasFCST')
            fit$gasFCST <- fitGas
            #if(input$curtailGas1 > 0){
            #  fit$gasFCST[1] <- fit$gasFCST[1]/2
            #}
            
            fit <- fit[,c('prodMonth', 'oilFCST', 'gasFCST')]
            #names(fit) <- c('Months', 'Oil', 'Gas')
            names(fit) <- c('MONTH', 'OIL', 'GAS')
            
            
            fit3 <- fit[1:(nrow(fit)-1),]
            fit3 <- rbind(fit[1,], fit3)
            fit3$actOil <- fit$OIL
            fit3$actGas <- fit$GAS
            
            fit3$DiOil <- 1-(fit3$actOil/fit3$OIL)
            fit3$DiGas <- 1-(fit3$actGas/fit3$GAS)
            
            decl2Oil <- fit3$DiOil[2]
            decl2Gas <- fit3$DiGas[2]
            decl3Oil <- mean(fit3$DiOil[3:4])
            decl3Gas <- mean(fit3$DiGas[3:4])
            decl4Oil <- mean(fit3$DiOil[5:6])
            decl4Gas <- mean(fit3$DiGas[5:6])
            decl5Oil <- mean(fit3$DiOil[7:12])
            decl5Gas <- mean(fit3$DiGas[7:12])
            decl6Oil <- mean(fit3$DiOil[13:18])
            decl6Gas <- mean(fit3$DiGas[13:18])
            decl7Oil <- mean(fit3$DiOil[19:24])
            decl7Gas <- mean(fit3$DiGas[19:24])
            decl8Oil <- mean(fit3$DiOil[25:36])
            decl8Gas <- mean(fit3$DiGas[25:36])
            
            decl9Oil <- 1-sum(fit3$actOil[37:48])/sum(fit3$actOil[25:36])
            decl9Gas <- 1-sum(fit3$actGas[37:48])/sum(fit3$actGas[25:36])
            
            decl10Oil <- 1-sum(fit3$actOil[49:60])/sum(fit3$actOil[37:48])
            decl10Gas <- 1-sum(fit3$actGas[49:60])/sum(fit3$actGas[37:48])
            
            decl11Oil <- ((1-sum(fit3$actOil[61:72])/sum(fit3$actOil[49:60]))+
                            (1-sum(fit3$actOil[73:84])/sum(fit3$actOil[61:72]))+
                            (1-sum(fit3$actOil[85:96])/sum(fit3$actOil[73:84]))+
                            (1-sum(fit3$actOil[97:108])/sum(fit3$actOil[85:96]))+
                            (1-sum(fit3$actOil[109:120])/sum(fit3$actOil[97:108])))/5
            
            decl11Gas <- ((1-sum(fit3$actGas[61:72])/sum(fit3$actGas[49:60]))+
                            (1-sum(fit3$actGas[73:84])/sum(fit3$actGas[61:72]))+
                            (1-sum(fit3$actGas[85:96])/sum(fit3$actGas[73:84]))+
                            (1-sum(fit3$actGas[97:108])/sum(fit3$actGas[85:96]))+
                            (1-sum(fit3$actGas[109:120])/sum(fit3$actGas[97:108])))/5
            
            
            i <- 121
            j <- 132
            k <- as.integer((nrow(fit3) - j)/12)
            l <- 1
            dtx <- data.frame()
            while(l <= k){
              decl <- (1-sum(fit3$actOil[(i+12):(j+12)])/sum(fit3$actOil[(i):(j)]))
              dtx <- rbind(dtx, decl)
              l <- l+1
              i <- i+12
              j <- j+12
            }
            names(dtx) <- c('decl12Oil')
            decl12Oil <- mean(dtx$decl12Oil)
            
            i <- 121
            j <- 132
            k <- as.integer((nrow(fit3) - j)/12)
            l <- 1
            dtx <- data.frame()
            while(l <= k){
              decl <- (1-sum(fit3$actGas[(i+12):(j+12)])/sum(fit3$actGas[(i):(j)]))
              dtx <- rbind(dtx, decl)
              l <- l+1
              i <- i+12
              j <- j+12
            }
            names(dtx) <- c('decl12Gas')
            decl12Gas <- mean(dtx$decl12Gas)
            
            names(fit) <- c('Months', 'Oil', 'Gas')
            
            fit$Water <- fit$Oil + fit$Gas/20
            df <- fit
            rm(fit)
            
            
            #df$Oil <- df$Oil*expenseValues()$wi/100
            #df$Gas <- df$Gas*expenseValues()$wi/100
            #df$Water <- df$Water*expenseValues()$wi/100
            df$Sales_Gas <- df$Gas*expenseValues()$shrink
            df$NGL <- df$Gas*expenseValues()$nglYield/1000
            #df$wi <- expenseValues()$wi
            df1 <- data.frame(Months = seq(((declineValues()$spudToProd-1)*-1), 0, 1), Oil = 0, Gas = 0, Water = 0)
            df$capex <- 0
            df1$capex <- 0
            df1$capex[1] <- df2$dcet
            #df1$capex[nrow(df1)] <- expenseValues()$completeCost*expenseValues()$wi/100
            if(input$priceType == 'Flat'){
              df$oilPrice <- capexValues()$wti
              df$gasPrice <- capexValues()$hh
              df$nglPrice <- df$oilPrice*expenseValues()$nglDiff/100
            } else {
              prices <- values$price
              prices <- prices %>% filter(DATE >= today())
              prices <- prices[(declineValues()$spudToProd):nrow(prices),]
              if(nrow(prices) > nrow(df)){
                prices <- prices[1:nrow(df),]
              }
              df$oilPrice <- NA
              df$oilPrice[1:nrow(prices)] <- prices$WTI
              df$gasPrice <- NA
              df$gasPrice[1:nrow(prices)] <- prices$HH
              rm(prices)
              df$oilPrice <- na.locf(df$oilPrice)
              df$gasPrice <- na.locf(df$gasPrice)
              df$nglPrice <- df$oilPrice*expenseValues()$nglDiff/100
            }
            print(head(df))
            df$nri <- expenseValues()$nri#*expenseValues()$wi/100
            df$oilRev <- (df$oilPrice-expenseValues()$oilDiff)*df$nri/100*df$Oil
            df$gasRev <- (df$gasPrice-expenseValues()$gasDiff)*df$nri/100*df$Sales_Gas*expenseValues()$btu
            df$nglRev <- (df$nglPrice)*df$nri/100*df$NGL
            df$rev <- df$oilRev+df$gasRev+df$nglRev
            df$tax <- df$oilRev*expenseValues()$stxOil/100 + (df$gasRev+df$nglRev)*expenseValues()$stxGas/100 +
              df$Oil*df$nri/100*expenseValues()$oilSTX + df$Gas*df$nri/100*expenseValues()$gasSTX +
              df$rev*df$nri/100*expenseValues()$atx/100
            
            df$expense <- expenseValues()$fixedCost + df$Gas*expenseValues()$varExpGas + 
              (df$Oil + df$NGL)*expenseValues()$varExpBOE
            
            
            df$nocf <- df$rev-df$tax-df$expense
            
            # print(head(df))
            
            if(input$econAbanS == 'Yes'){
              df <- df[nrow(df):1,]
              df$cumNOCF <- cumsum(df$nocf)
              df$prev <- df$cumNOCF- dplyr::lag(df$cumNOCF, n = 1L)
              prev <- which(df$prev > 0)[1]
              df <- df[prev:nrow(df),]
              df <- df[nrow(df):1,]
              df <- subset(df, select = -c(cumNOCF, prev))
            }
            df <- as.data.frame(df)
            df$pna <- 0
            df$pna[nrow(df)] <- expenseValues()$pna
            
            Missing <- setdiff(dput(names(df)), names(df1))  # Find names of missing columns
            df1[Missing] <- 0                    # Add them, filled with '0's
            df1 <- df1[,names(df)]
            
            df <- rbind(df1, df)
            df$fcf <- df$nocf - df$capex - df$pna
            df$Months <- seq(0,nrow(df)-1,1)
            
            
            
            
            
            df$pv10 <- df$fcf/(1.1^(df$Months/12))
            
            df2$IRR <- IRRcalc(df$fcf, df$Months)
            
            if(is.nan(decl2Oil)){
              decl2Oil <- 0
            }
            if(is.nan(decl3Oil)){
              decl3Oil <- 0
            }
            if(is.nan(decl4Oil)){
              decl4Oil <- 0
            }
            if(is.nan(decl5Oil)){
              decl5Oil <- 0
            }
            if(is.nan(decl6Oil)){
              decl6Oil <- 0
            }
            if(is.nan(decl7Oil)){
              decl7Oil <- 0
            }
            if(is.nan(decl8Oil)){
              decl8Oil <- 0
            }
            if(is.nan(decl9Oil)){
              decl9Oil <- 0
            }
            if(is.nan(decl10Oil)){
              decl10Oil <- 0
            }
            if(is.nan(decl11Oil)){
              decl11Oil <- 0
            }
            if(is.nan(decl12Oil)){
              decl12Oil <- 0
            }
            
            if(is.nan(decl2Gas)){
              decl2Gas <- 0
            }
            if(is.nan(decl3Gas)){
              decl3Gas <- 0
            }
            if(is.nan(decl4Gas)){
              decl4Gas <- 0
            }
            if(is.nan(decl5Gas)){
              decl5Gas <- 0
            }
            if(is.nan(decl6Gas)){
              decl6Gas <- 0
            }
            if(is.nan(decl7Gas)){
              decl7Gas <- 0
            }
            if(is.nan(decl8Gas)){
              decl8Gas <- 0
            }
            if(is.nan(decl9Gas)){
              decl9Gas <- 0
            }
            if(is.nan(decl10Gas)){
              decl10Gas <- 0
            }
            if(is.nan(decl11Gas)){
              decl11Gas <- 0
            }
            if(is.nan(decl12Gas)){
              decl12Gas <- 0
            }
            
            df2$decl2Oil <- percent(decl2Oil)
            df2$decl3Oil <- percent(decl3Oil)
            df2$decl4Oil <- percent(decl4Oil)
            df2$decl5Oil <- percent(decl5Oil)
            df2$decl6Oil <- percent(decl6Oil)
            df2$decl7Oil <- percent(decl7Oil)
            df2$decl8Oil <- percent(decl8Oil)
            df2$decl9Oil <- percent(decl9Oil)
            df2$decl10Oil <- percent(decl10Oil)
            df2$decl11Oil <- percent(decl11Oil)
            df2$decl12Oil <- percent(decl12Oil)
            df2$decl2Gas <- percent(decl2Gas)
            df2$decl3Gas <- percent(decl3Gas)
            df2$decl4Gas <- percent(decl4Gas)
            df2$decl5Gas <- percent(decl5Gas)
            df2$decl6Gas <- percent(decl6Gas)
            df2$decl7Gas <- percent(decl7Gas)
            df2$decl8Gas <- percent(decl8Gas)
            df2$decl9Gas <- percent(decl9Gas)
            df2$decl10Gas <- percent(decl10Gas)
            df2$decl11Gas <- percent(decl11Gas)
            df2$decl12Gas <- percent(decl12Gas)
            df2
          },
          error=function(e) {
            e
            NULL
          })
          
          )
          
          df <- bind_rows(econSummary)
          df <- as.data.frame(df)
          
          df1 <- df[,c('decl2Oil', 'decl3Oil', 'decl4Oil',
                       'decl5Oil', 'decl6Oil', 'decl7Oil',
                       'decl8Oil', 'decl9Oil', 'decl10Oil',
                       'decl11Oil', 'decl12Oil')]
          df2 <- df[,c( 'decl2Gas', 'decl3Gas', 'decl4Gas',
                        'decl5Gas', 'decl6Gas', 'decl7Gas',
                        'decl8Gas', 'decl9Gas', 'decl10Gas',
                        'decl11Gas', 'decl12Gas')]
          
          df1 <- df1[1,]
          df2 <- df2[1,]
          names(df1) <- c('Month 2 Decl', 'Month 3-4 Decl', 
                          'Month 5-6 Decl', 'Month 7-12 Decl',
                          'Month 13-18 Decl', 'Month 19-24 Decl',
                          'Month 25-36 Decl', 'Year 4 Decl', 
                          'Year 5 Decl', 'Year 6-10 Decl', 'Year 11+ Decl')
          
          df1 <- t(df1)
          
          names(df2) <- c('Month 2 Decl', 'Month 3-4 Decl', 
                          'Month 5-6 Decl', 'Month 7-12 Decl',
                          'Month 13-18 Decl', 'Month 19-24 Decl',
                          'Month 25-36 Decl', 'Year 4 Decl', 
                          'Year 5 Decl', 'Year 6-10 Decl', 'Year 11+ Decl')
          df2 <- t(df2)
          
          df1 <- cbind(df1, df2)
          df1 <- as.data.frame(df1)
          names(df1) <- c('OIL', 'GAS')
          df1$TIME <- rownames(df1)
          df1 <- df1[,c('TIME', 'OIL', 'GAS')]
          #df1$ROW <- seq(1, 11, 1)
          values$wellDeclines <- df1
          # print((df1))
          
          df <- as.data.frame(df)
          
          df <- subset(df, select=-c(wells, decl2Oil, decl3Oil, decl4Oil,
                                     decl5Oil, decl6Oil, decl7Oil,
                                     decl8Oil, decl9Oil, decl10Oil,
                                     decl11Oil, decl12Oil, 
                                     decl2Gas, decl3Gas, decl4Gas,
                                     decl5Gas, decl6Gas, decl7Gas,
                                     decl8Gas, decl9Gas, decl10Gas,
                                     decl11Gas, decl12Gas))
          
          df$wellSpacing <- as.integer(df$wellSpacing)
          df$qiOil <- as.integer(df$qiOil)
          df$bOil <- round(df$bOil, 2)
          #df$DiOil <- percent(round(as.effective(df$DiOil), 2))
          #df$DfOil <- percent(round(df$DfOil, 2))
          df$bGas <- round(df$bGas, 2)
          #df$DiGas <- percent(round(as.effective(df$DiGas), 2))
          #df$DfGas <- percent(round(df$DfGas, 2))
          df$oilEUR <- as.integer(df$oilEUR)
          df$gasEUR <- as.integer(df$gasEUR)
          df$qiGas <- as.integer(df$qiGas)
          df$dNcPerFt <- df$dNc/as.numeric(capexValues()$perfSelect)
          df$dcetPerFt <- df$dcet/as.numeric(capexValues()$perfSelect)
          #df$dNc <- dollar(df$dNc)
          #df$dcet <- dollar(df$dcet)
          df$dNcPerFt <- dollar(df$dNcPerFt)
          df$dcetPerFt <- dollar(df$dcetPerFt)
          df$IRR <- percent(df$IRR)
          #df$NPV <- dollar(df$NPV)
          
          #acres <- as.data.frame(values$leaseData) %>% filter(subPlay %in% input$subPlaySelect2)
          #acres <- acres$acreage
          #acres <- as.numeric(input$subPlayAcres)/acres
          #df$remWells <- as.integer(df$remWells*acres)
          df$id <- input$play
          #df <- merge(df, df3, by='operator', all.x=TRUE)
          
          df <- as.data.frame(df)
          df3 <- as.data.frame(df3)
          #print(head(df3))
          
          df$operator <- as.character(df$operator)
          df3$operator <- as.character(df3$operator)
          df <- merge(df, df3, by='operator', all.x=TRUE)
          #print(head(df))
          df$remWells <- as.integer(df$remWells/sum(df3$acres)*as.numeric(expenseValues()$acres))
          
          df$acres <- as.integer(df$acres/sum(df3$acres)*as.numeric(expenseValues()$acres))
          df$viableAcres <- percent(viableAcreage)
          #print(viableAcreage)
          #values$viableAcres <- viableAcreage
          
          df1 <- df() %>%
            filter(subPlay %in% input$subPlay) %>%
            #filter(reservoir %in% input$reservoirSelect) %>% 
            #filter(county %in% input$countySelect) %>%
            filter(operator %in% input$operator) %>%
            group_by(operator) %>% summarise(oldWells=n(), oldPerf=as.integer(mean(perf)),
                                             oldPPF=as.integer(mean(ppf)), oldFPF = as.integer(mean(fpf))) %>% ungroup()
          
          
          
          df <- merge(df, df1, by='operator', all.x=TRUE)
          df$netWells <- as.integer(df$oldWells/sum(df3$acres)*as.numeric(expenseValues()$acres))
          #df <- df %>% arrange(desc(acres))
          #print(head(df))
          #print(head(df))
          df$curtailOil <- declineValues()$curtailOilS
          df$curtailGas <- declineValues()$curtailGasS
          
          #print(head(df))
          df <- df[,c('subPlay', 'operator','acres', 'viableAcres', 'oldWells',  'oldPerf', 'oldPPF', 'oldFPF',
                      'wellSpacing', 'remWells', 'oilEUR', 'gasEUR', 'dNc', 'dcet', 'dNcPerFt', 'dcetPerFt',
                      'IRR', 'qiOil', 'DiOil', 'bOil', 'DfOil', 'curtailOil', 'qiGas', 'DiGas', 'bGas', 'DfGas', 'curtailGas')]
          values$wellCalc2 <- as.data.frame(df)
          names(df) <- c('Sub-Play', 'Operator',  'Acreage Estimate, acres', 'Viable Acreage, %',
                         'Historic Gross Wells', 'Historic Lateral Length, ft', 'Historic Proppant Loading, lb/ft',
                         'Historic Fluid Loading, gal/ft', 'Inter-well Spacing, ft', 'Remaining Inventory',
                         'Oil EUR, bbls', 'Gas EUR, mcf', 'D&C, $', 'DC&E, $', 'D&C/Ft', 'DC&E/Ft',  'IRR, %',
                         'Oil IP-30, bbl/d', 'Initial Decline Oil', 'b-Factor Oil', 'Terminal Decline Oil', 'Oil Curtailment, months',
                         'Gas IP-30, mcf/d', 'Initial Decline Gas', 'b-Factor Gas', 'Terminal Decline Gas', 'Gas Curtailment, months')
          
          #names(df) <- c('Sub Basin', 'Operator',  'Remaining Economic Wells', 'Well Spacing, ft', 'Oil IP-30, bbl/d', 'b-Factor Oil',
          #               'Initial Decline Oil', 'Terminal Decline Oil', 'Oil EUR, bbls', 'Wet Gas IP-30, mcf/d', 'b-Factor Gas',
          #               'Initial Decline Gas', 'Terminal Decline Gas', 'Gas EUR, mcf', 'Drill & Complete, $', 'Drill, Complete, and Equip, $','NPV, $', 'IRR, %', 
          #               'D&C Per Ft, $', 'D&C+E Per Ft, $',
          #               'Reservoir', 'Gross Acreage Estimate, acres', 'Operator Wells in Selected Subplays', 'Operator Historical Lateral Length, ft',
          #               'Operator Historical Proppant Loading, lb/ft')
          
          #print(head(df))
        }
      }
      values$wellCalc <- (df)
      updateActionButton(session, "calcInv",
                         label = 'CALCULATE')
      shinyjs::enable('calcInv')
    }
  })
  
  output$wellCalcs <- DT::renderDataTable({
    if(is.null(values$wellCalc)) {
      NULL
    } else {
      if(ncol(values$wellCalc) == 1){
        DT::datatable(values$wellCalc, rownames=TRUE,extensions = c('Buttons'),
                      options = list(dom='B', paging = FALSE, 
                                     buttons = c('copy', 'csv', 'excel'),
                                     info = FALSE, ordering = FALSE),
                      caption = htmltools::tags$caption(
                        style = 'caption-side: bottom; text-align: center;',
                        'Table: ', htmltools::em('Remaining Economic Inventory Data')),
                      class = 'cell-border stripe')
      } else {
        declines <- values$wellCalc
        declines <- declines %>% mutate(PERF = as.numeric(capexValues()$perfSelect),
                                        PPF = as.numeric(capexValues()$ppfSelect),
                                        FPF = as.numeric(capexValues()$fpfSelect))
        declines <- as.data.frame(t(declines))
        txt1 <- paste0(input$operator,'/', input$play,'/', input$subPlay, collapse=' ')
        #print(txt1)
        names(declines) <- txt1
        
        
        expenseValues1 <- data.frame(
          Component = c('Fixed Expense/Month', 'Gas Variable Expense',
                        'Liquids Variable Expense','WTI Discount', 'Henry Hub Discount', 'NGL % WTI',
                        'Shrink',
                        'NGL Yield', 'BTU Uplift', 'Ad Val Tax Percent Revenue', 'Oil Severance Percent Revenue',
                        'Gas and NGL Severance Percent Revenue','Oil Severance Per BBL', 'Gas Severance Per MCF',
                        'Months Spud to Production', 'Net Revenue Interest', 'P & A'),
          Value = c(expenseValues()$fixedCost, 
                    expenseValues()$varExpGas, expenseValues()$varExpBOE,
                    expenseValues()$oilDiff, expenseValues()$gasDiff, expenseValues()$nglDiff,
                    expenseValues()$shrink, expenseValues()$nglYield, expenseValues()$btu,
                    expenseValues()$atx, expenseValues()$stxOil, expenseValues()$stxGas,
                    expenseValues()$oilSTX, expenseValues()$gasSTX,
                    declineValues()$spudToProd, expenseValues()$nri,
                    expenseValues()$pna))
        
        
        
        rownames(expenseValues1) <- expenseValues1$Component
        expenseValues1 <- subset(expenseValues1, select = -c(Component))
        print(expenseValues1)
        #print(head(expenseValues1))
        #print(head(declines))
        names(expenseValues1) <- names(declines)
        declines <- rbind(as.data.frame(declines), as.data.frame(expenseValues1))
        print(declines)
        
        values$declines <- declines
        
        DT::datatable(declines, rownames = TRUE,
                      extensions = c('Buttons', 'Scroller'), 
                      options = list(
                        dom = 'Bfrtip',
                        scrollX = TRUE,
                        scrollY = FALSE,
                        deferRender = TRUE,
                        paging = FALSE,
                        searching = FALSE,
                        buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print'))
                      ,
                      caption = htmltools::tags$caption(
                        style = 'caption-side: bottom; text-align: center;',
                        'Table: ', htmltools::em('Remaining Economic Inventory Data')),
                      class = 'cell-border stripe')
      }
    }
  })
  output$wellCalcs1 <- DT::renderDataTable({
    if(is.null(values$wellDeclines)) {
      NULL
    } else {
      DT::datatable(values$wellDeclines, rownames=FALSE,extensions = c('Buttons'),
                    options = list(dom='B', paging = FALSE, 
                                   buttons = c('copy', 'csv', 'excel'),
                                   info = FALSE, ordering = FALSE),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: center;',
                      'Table: ', htmltools::em('Decline Inputs for GEM Python')),
                    class = 'cell-border stripe')
    }
  })
  
}
shinyApp(ui = ui, server = server)