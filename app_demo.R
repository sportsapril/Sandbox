rm(list = objects())

##### library #####
library(flexdashboard)
library(cagr)
library(cgthemes)
library(cgdata)
library(data.table)
library(magrittr)
library(stringr)
library(shiny)
library(DT)
library(PerformanceAnalytics)
library(RSQLite)
library(shinydashboard)
library(dplyr)
library(dygraphs)
library(highcharter)
library(tidyr)
library(htmltools)
library(dbplyr)
library(purrr)
library(plotly)

setwd('C:/Users/conapx/Desktop/POC/')

##### self defined functions##### 
getSQL <- function(filepath)
{
  con = file(filepath, "r")
  sql.string <- ""
  
  while ( TRUE )
  {
    line <- readLines(con, n = 1)
    if ( length(line) == 0 ) { break }
    
    line <- gsub("\\t", " ", line)
    
    if(grepl("--",line) == TRUE)
    {
      line <- paste(sub("--","/*",line),"*/")
    }
    
    sql.string <- paste(sql.string, line)
  }
  
  close(con)
  return(sql.string)
}


##### database connections##### 

db = datasource(lasr, instance = 'im', database = 'IM_U_II_CAG_SHARED_S')
con = connect(db)

##### get funds and index info##### 
obsl = fread('C:/Users/conapx/Desktop/POC/Files/Family_Obsolete_Merge_Liquid_Ratio.csv')
mapping <- fread('Files/Fund_Index_HoldingID_Mapping.csv')

funds = get_data(type = 'firm')
indexAll = as.data.table(query(con,'SELECT Distinct IndexName, Ticker FROM [IM_U_II_CAG_SHARED_S].[dbo].[IndexReturns]'))


#####  Fetch Data ##### 
# check if files needs to be generated
sql_getLatestExpRatio = getSQL(filepath = 'C:/Users/conapx/Desktop/POC/SQL/sql_getLatestExpRatio.sql')
sql_getLatestNetAsset = getSQL(filepath = 'C:/Users/conapx/Desktop/POC/SQL/sql_getLatestNetAsset.sql')
sql_getEquityRegion = getSQL(filepath = 'C:/Users/conapx/Desktop/POC/SQL/sql_getEquityRegion.sql')

expRFilePath = 'C:/Users/conapx/Desktop/POC/Files/expR.csv'
netAssetFilePath = 'C:/Users/conapx/Desktop/POC/Files/netAsset.csv'

if (!file.exists(expRFilePath)){
  expR = query(con, sql_getLatestExpRatio)
  write.csv(expR, expRFilePath)
} else {
  expR = fread(expRFilePath)
}


if (!file.exists(netAssetFilePath)){
  netAsset = query(con, sql_getLatestNetAsset)
  write.csv(netAsset, netAssetFilePath)
} else {
  netAsset = fread(netAssetFilePath)
}

res = funds %>% left_join(netAsset, by = c('SecId' = 'SecID', 'FundId' = 'FundID')) %>% 
  left_join(expR, by = c('SecId' = 'SecID', 'FundId' = 'FundID'))

##### carve out obsoleted funds #####
res = res[is.na(funds$ObsoleteDate),]

### create category avg ###
catAvgStats <- res %>%
  group_by(MorningstarCategory) %>%
  summarise(catAvgExpR = mean(AnnualReportNetExpenseRatioYear, na.rm = T), 
            catAvgOwnership = mean(FirmManagerOwnership,na.rm = T))


### quartiles ###
res = res %>% group_by(MorningstarCategory) %>% mutate(expRQrt = ntile(AnnualReportNetExpenseRatioYear, 4))
res = res %>% group_by(MorningstarCategory) %>% mutate(ownershipQrt = ntile(FirmManagerOwnership, 4))

##### Stitch Together #####
res = res %>% left_join(catAvgStats, by = 'MorningstarCategory') %>% left_join(obsl, by = c('SecId','FundId'))
res = as.data.table(res)

##### Get Holdings ####
fund_holding = as.data.table(query(con, "SELECT * FROM [IM_U_II_CAG_SHARED_S].[dbo].[FundHoldings] WHERE Date = '2017-12-31'"))
index_holding = as.data.table(query(con, "SELECT * FROM [IM_U_II_CAG_SHARED_S].[dbo].[IndexHoldings] WHERE Date = '2017-12-31'"))



##### test on a smaller sample ##### 
# POC_Idx = !is.na(res$AnnualReportNetExpenseRatioYear) & 
#   !is.na(res$catAvgExpR) & 
#   !is.na(res$ownershipQrt) & 
#   !is.na(res$catAvgOwnership) &
#   res$PrimaryProspectusBenchmarkId %in% indexAll$Ticker
# 
# smp = sample(which(POC_Idx),10)
# fund_names = res$Name[smp]
# index_names = res$PrimaryProspectusBenchmark[smp]

fund_names = c("American Funds AMCAP A", 
               "American Funds Europacific Growth A",
               "Janus Henderson Global Inc Mgd Volatil I", 
               "Domini Impact Equity A", 
               "TCW International Small Cap N", 
               "Lazard Emerging Markets Eq Advtg Instl", 
               "American Funds Global Balanced 529C", 
               "BMO Pyrford International Stock A", 
               "Calvert Short Duration Income C", 
               "Oppenheimer Global Multi-Asset Income R"
               )

index_names = c("S&P 500 TR USD", 
                "MSCI ACWI Ex USA NR USD",
                "MSCI World NR USD", 
                "S&P 500 TR USD", 
                "MSCI ACWI Ex USA Small NR USD", 
                "MSCI EM NR USD", 
                "MSCI ACWI NR USD", 
                "MSCI EAFE NR USD", 
                "BBgBarc Credit 1-5 Yr TR USD", 
                "BBgBarc US Agg Bond TR USD"
                )

##### UI #####
##### UI:sidebar and body layout #####
sidebar <- dashboardSidebar(
  selectInput(inputId = "fund",choices = fund_names,
              label = "Please choose a fund: ",selected = fund_names[1]),
  selectInput(inputId = "index", 
              label = "Please choose an index: ",
              choices = index_names,
              selected = index_names[1]),
  sidebarMenu(
    actionLink("remove", "Remove detail tabs")
  )
)

body <- dashboardBody(
  tabsetPanel(id = 'tabs',
              tabPanel('Overview',
                       
            
                        fluidRow(
                          box(width = 6, dygraphOutput("growth_plot")),
                          box(width = 6, dygraphOutput("drawdown_plot"))
                        )
                        ,fluidRow(
                          valueBoxOutput('expense_quartile'),
                          valueBoxOutput('expense_ratio'),
                          valueBoxOutput('catAvgExpense_ratio')
                        )
                        ,fluidRow(
                          valueBoxOutput('ownership_quartile'),
                          valueBoxOutput('ownership_amount'),
                          valueBoxOutput('average_ownership_amount')
                        )
                        ,fixedRow(
                          box(width = 4,title = 'Obsolete Ratio',gaugeOutput('obsolete_ratio')),
                          box(width = 4,title = 'Merged Ratio',gaugeOutput('merged_ratio')),
                          box(width = 4,title = 'Liquidated Ratio',gaugeOutput('liquidated_ratio'))
                        )
                )
              ,tabPanel('Holdings',
                        fluidRow(
                          box(width = 12, status = 'primary', solidHeader = TRUE, 
                              title = 'Sector Alloc. Comparison',
                              plotlyOutput('comp')
                          )
                        )
                        ,fluidRow(
                          box(width = 6, status = 'primary', collapsed = T, solidHeader = T,
                              title = 'Top 50 Holdings by Fund',
                              DT::dataTableOutput("fundtop50")
                          ),
                          box(width = 6, status = 'success', collapsed = T, solidHeader = T,
                              title = 'Top 50 Holdings by Index',
                              DT::dataTableOutput("indextop50")
                          )
                        )
                )
              ,tabPanel('Equity Region',
                          fluidRow(
                            box(width = 12, status = 'primary', collapsed = T, solidHeader = T,
                                title = 'Equity Region',
                                dygraphOutput("eqregion_plot"))
                            )
                          , fluidRow(
                            box(width = 6, status = 'primary', collapsed = T, solidHeader = T,
                                title = 'Holdings by Country',
                                highchartOutput("countryEqHolding")
                                ),
                            box(width = 6, status = 'success', collapsed = T, solidHeader = T,
                                title = 'Holdings by Region',
                                highchartOutput("regionEqHolding")
                            )
                            )
                          )
                  )
)



# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Capital Advantage Dashboard"),
  sidebar,
  body
)

##### Server #####
server <- function(input, output, session){
  tab_list <- NULL
  # get id from fund list
  fundid <- reactive({
    funds[Name == input$fund]$SecId
  })
  
  # get fund abbr name from mapping
  fundAbb <- reactive({
    mapping[F_MSTARID == funds[Name == input$fund]$FundId]$NAME_ABBREV
  })
  
  # get id from index list
  indexid <- reactive({
    indexAll[IndexName == input$index]$Ticker
  })
  
  # get indexabbr name from mapping
  indexAbb <- reactive({
    mapping[I_MSTARID == indexid()]$INDEX_ABBREV
  })
  
  # get fund character
  fund_data  <-  reactive({
    as.data.table(res[Name == input$fund])
  })
  
  # grab fund return data
  fund_returns <- reactive({
    get_data(fundid(), id_type = "SecId", type = "return", source = 'morningstar')
  })
  
  # get fund holdings
  f_holding <- reactive({
    fund_holding[Fund == fundAbb()]
   })
  
  # get index holdings
  i_holding <- reactive({
    index_holding[Fund == indexAbb()]
  }) 
  
  # get equity region
  eq_region <- reactive({
    tmp = as.data.table(query(con,paste0(sql_getEquityRegion, " WHERE dev.SecId = '", fundid(), "'")))
    colnames(tmp) = c("SecId","Date","Developed","Emerging")
    xts(tmp, order.by = as.Date(tmp$Date, '%m/%d/%Y'))[,c("Developed","Emerging")]
  })
  
  
  # grab index return data
  index_returns <- reactive({
    get_data(indexid(), id_type = "Ticker", type = "return", source = 'index')
  })
  
  # combine data
  plot_data <- reactive({
    rbind(fund_returns()[,.(Name,Date,Return)],
          index_returns()[,.(Name = IndexName, Date, Return = IndexReturn)]) %>% 
      format_xts(id_col = "Name", value_column = "Return", date_column = "Date") %>% 
      subset(., complete.cases(.))
  })
  
  # plot growth of a dollar
  output$growth_plot <- renderDygraph({
    plot_growth(plot_data())
  })
  
  # plot drawdowns
  output$drawdown_plot <- renderDygraph({
    plot_drawdown(plot_data())
  })
  
  # plot equity region alloc
  output$eqregion_plot <- renderDygraph({
    dygraph(eq_region(), main = "Equity Allocation by Region", xlab = "Time", 
                      ylab = "Equity %") %>% dygraphs::dyRangeSelector(.) %>% 
      dyUnzoom %>% dyCrosshair %>% dyBrand %>% dygraphs::dyAxis(., 
                                                                name = "y", axisLabelFormatter = "function(d){return + Math.round(d*100)/100 + \"%\" }", 
                                                                valueFormatter = "function(d){return  + Math.round(d*100)/100 + \"%\"}")
    
  })

  # expense_quartile
  output$expense_quartile <- renderValueBox({
    valueBox(fund_data()$expRQrt, "Expense Quartile",icon = icon("balance-scale"),
             color = ifelse(fund_data()$expRQrt == 1,
                            "green",ifelse(fund_data()$expRQrt == 4,
                                           "red","yellow"))
             )
  })
  
  # expense_ratio
  output$expense_ratio <- renderValueBox({
    valueBox(
      paste0(floor(fund_data()$AnnualReportNetExpenseRatioYear * 100)/100,"%"), "Expense Ratio", icon = icon("percent"),
      color = ifelse(fund_data()$AnnualReportNetExpenseRatioYear < fund_data()$catAvgExpR,
                     "green","red")
    )
  })
  
  # catAvgExpense_ratio
  output$catAvgExpense_ratio <- renderValueBox({
    valueBox(
      paste0(floor(fund_data()$catAvgExpR * 100) / 100,"%"), "Category Avg. Expense Ratio", icon = icon("percent"),
      color = "light-blue"
    )
  })
  
  # ownership quartile
  output$ownership_quartile <- renderValueBox({
    valueBox(
      fund_data()$ownershipQrt, "Ownership Quartile", icon = icon("balance-scale"),
      color = ifelse(fund_data()$ownershipQrt == 4,
                     "green",ifelse(fund_data()$ownershipQrt == 1,
                                    "red","yellow"))
    )
  })
  
  # ownership level
  output$ownership_amount <- renderValueBox({
    valueBox(
      paste0("$",round(fund_data()$FirmManagerOwnership)), "Ownership Level", icon = icon("usd"),
      color = ifelse(fund_data()$ownershipQrt == 4,
                     "green",ifelse(fund_data()$ownershipQrt == 1,
                                    "red","yellow"))
    )
  })
  
  # category ownership avg level
  output$average_ownership_amount <- renderValueBox({
    valueBox(
      paste0("$", round(fund_data()$catAvgOwnership)), "Category Avg. Ownership Level", icon = icon("usd"),
      color = 'light-blue'
    )
  })
  
  # obsolete_ratio
  output$obsolete_ratio <- renderGauge({
    gauge(fund_data()$`Family Obsolete Ratio`,min = 0, max = 100,symbol = "%",sectors =  gaugeSectors(
      success = c(0, 10), warning = c(11, 50), danger = c(50, 100)
    ))
  })

  # merged_ratio
  output$merged_ratio <- renderGauge({
    gauge(fund_data()$`Family Merged Ratio`,min = 0, max = 100,symbol = "%",sectors =  gaugeSectors(
      success = c(0, 10), warning = c(11, 50), danger = c(50, 100)
    ))
  })
  
  # liquidated_ratio
  output$liquidated_ratio <- renderGauge({
    gauge(fund_data()$`Family Liquidated Ratio`,min = 0, max = 100,symbol = "%",sectors =  gaugeSectors(
      success = c(0, 10), warning = c(11, 50), danger = c(50, 100)
    ))
  })
  
  # plot sector allocation comparison
  output$comp <- renderPlotly({
    x = f_holding() %>% group_by(Sector) %>% summarise(secAlloc = sum(Weight))
    y = i_holding() %>% group_by(Sector) %>% summarise(secAlloc = sum(Weight))
  
    A = as.data.table(merge(x = x, y = y, by = "Sector", all = TRUE))
    A = A[order(-secAlloc.x, na.last = T),]
    A$Sector <- factor(A$Sector, levels = unique(A$Sector))
    A$secAlloc.x = round(A$secAlloc.x,2)
    A$secAlloc.y = round(A$secAlloc.y,2)
    plot_ly(A, x = ~Sector, y = ~secAlloc.x, type = 'bar', name = 'Fund', marker = list(color = 'blue')) %>%
      add_trace(y = ~secAlloc.y, name = 'Index', marker = list(color = 'orange')) %>%
      layout(yaxis = list(title = '% Weight'), barmode = 'group', margin = list(b=100, l=100), xaxis = list(titlefont=list(size=30)))
  })
  
  # fund top 50 holdings
  output$fundtop50 <- DT::renderDataTable({
    top10_f = f_holding() %>% arrange(desc(Weight)) %>% head(50)
    DT::datatable(top10_f[,c('Holding','Price','Sector','Return3Mo','Weight')]) %>% formatRound(column = c('Price','Return3Mo','Weight'), digits = 2)
  })

  # index top 50 holdings
  output$indextop50 <- DT::renderDataTable({
    top10_i = i_holding() %>% arrange(desc(Weight)) %>% head(50)
    DT::datatable(top10_i[,c('Holding','Price','Sector','Return3Mo','Weight')]) %>% formatRound(column = c('Price','Return3Mo','Weight'), digits = 2)
  })
  
  #### Drilldown ####

  #### Bar Click ####
  js_bar_clicked_f_c <- JS("function(event) {Shiny.onInputChange('bar_clicked_f_c', [event.point.category]);}")
  js_bar_clicked_f_r <- JS("function(event) {Shiny.onInputChange('bar_clicked_f_r', [event.point.category]);}")
  js_bar_clicked_i_c <- JS("function(event) {Shiny.onInputChange('bar_clicked_i_c', [event.point.category]);}")
  js_bar_clicked_i_r <- JS("function(event) {Shiny.onInputChange('bar_clicked_i_r', [event.point.category]);}")

  #### Plot Holdings by country ####
  output$countryEqHolding <- renderHighchart({

    result_f <- f_holding() %>% filter(Type == 'EQ') %>%
      group_by(Country) %>%
      summarise(countryAlloc = sum(Weight)) %>%
      arrange(desc(countryAlloc)) %>%
      head(10)

    result_i <- i_holding() %>% filter(Type == 'EQ') %>%
      group_by(Country) %>%
      summarise(countryAlloc = sum(Weight)) %>%
      arrange(desc(countryAlloc)) %>%
      head(10)

    
    result = merge(x = result_f, y = result_i, by = 'Country', all = T) %>% arrange(desc(countryAlloc.x, Country))
    result$countryAlloc.x = round(result$countryAlloc.x,2)
    result$countryAlloc.y = round(result$countryAlloc.y,2)
    
    highchart() %>%
      hc_add_series(
        data = result$countryAlloc.x, 
        type = "bar",
        name = paste("Fund"),
        color = 'blue',
        events = list(click = js_bar_clicked_f_c)) %>%
      hc_add_series(
        data = result$countryAlloc.y,
        type = 'bar',
        name = paste("Index"),
        color = 'orange',
        events = list(click = js_bar_clicked_i_c)) %>%
      hc_xAxis(
        categories = result$Country,
        tickmarkPlacement="on")
    
  })
  
  #### Plot Holdings by region ####
  output$regionEqHolding <- renderHighchart({
    
    result_f <- f_holding() %>% filter(Type == 'EQ') %>%
      group_by(Region) %>%
      summarise(regionAlloc = sum(Weight)) %>%
      arrange(desc(regionAlloc)) %>%
      head(10)
    
    result_i <- i_holding() %>% filter(Type == 'EQ') %>%
      group_by(Region) %>%
      summarise(regionAlloc = sum(Weight)) %>%
      arrange(desc(regionAlloc)) %>%
      head(10)
    
    
    result = merge(x = result_f, y = result_i, by = 'Region', all = T) %>% arrange(desc(regionAlloc.x), Region)
    result$regionAlloc.x = round(result$regionAlloc.x,2)
    result$regionAlloc.y = round(result$regionAlloc.y,2)
    
    highchart() %>%
      hc_add_series(
        data = result$regionAlloc.x, 
        type = "bar",
        name = paste("Fund"),
        color = 'blue',
        events = list(click = js_bar_clicked_f_r)) %>%
      hc_add_series(
        data = result$regionAlloc.y,
        type = 'bar',
        name = paste("Index"),
        color = 'orange',
        events = list(click = js_bar_clicked_i_r)) %>%
      hc_xAxis(
        categories = result$Region,
        tickmarkPlacement="on")
    
  })
  
  ##### click on fund country #####  
  observeEvent(input$bar_clicked_f_c,
               {
                 country <- input$bar_clicked_f_c[1]
                 tab_title <- paste0('Fund - ', country)
                 
                 if(tab_title %in% tab_list == FALSE){
                   details <- f_holding() %>%
                     filter(Country == country, Type == 'EQ')
                   
                   details <- details %>%
                     arrange(desc(Weight)) %>%
                     select(Holding,
                            Sector,
                            IndustryGroup,
                            MarketValue,
                            Weight) %>%
                     collect() 
                   
                   
                   appendTab(inputId = "tabs",
                             tabPanel(
                               tab_title,
                               renderDataTable(datatable(details) %>% formatRound(column = c('Weight'), digits = 2)))
                             )
                   
                   tab_list <<- c(tab_list, tab_title)
                   
                 }
                 
                 updateTabsetPanel(session, "tabs", selected = tab_title)
                 
               })
  
  ##### click on fund region #####
  observeEvent(input$bar_clicked_f_r,
               {
                 region <- input$bar_clicked_f_r[1]
                 tab_title <- paste0('Fund - ', region)
                 
                 if(tab_title %in% tab_list == FALSE){
                   details <- f_holding() %>%
                     filter(Region == region, Type == 'EQ')
                   
                   details <- details %>%
                     arrange(desc(Weight)) %>%
                     select(Holding,
                            Sector,
                            IndustryGroup,
                            MarketValue,
                            Weight) %>%
                     collect() 
                   
                   
                   appendTab(inputId = "tabs",
                             tabPanel(
                               tab_title,
                               renderDataTable(datatable(details) %>% formatRound(column = c('Weight'), digits = 2)))
                   )
                   
                   tab_list <<- c(tab_list, tab_title)
                   
                 }
                 
                 updateTabsetPanel(session, "tabs", selected = tab_title)
                 
               })

  ##### click on index country #####
  observeEvent(input$bar_clicked_i_c,
               {
                 country <- input$bar_clicked_i_c[1]
                 tab_title <- paste0('Index - ', country)
                 
                 if(tab_title %in% tab_list == FALSE){
                   details <- i_holding() %>%
                     filter(Country == country, Type == 'EQ')
                   
                   details <- details %>%
                     arrange(desc(Weight)) %>%
                     select(Holding,
                            Sector,
                            IndustryGroup,
                            MarketValue,
                            Weight) %>%
                     collect() 
                   
                   
                   appendTab(inputId = "tabs",
                             tabPanel(
                               tab_title,
                               renderDataTable(datatable(details) %>% formatRound(column = c('Weight'), digits = 2)))
                   )
                   
                   tab_list <<- c(tab_list, tab_title)
                   
                 }
                 
                 updateTabsetPanel(session, "tabs", selected = tab_title)
                 
               })
  
  ##### click on index region #####
  observeEvent(input$bar_clicked_i_r,
               {
                 region <- input$bar_clicked_i_r[1]
                 tab_title <- paste0('Index - ', region)
                 
                 if(tab_title %in% tab_list == FALSE){
                   details <- i_holding() %>%
                     filter(Region == region, Type == 'EQ')
                   
                   details <- details %>%
                     arrange(desc(Weight)) %>%
                     select(Holding,
                            Sector,
                            IndustryGroup,
                            MarketValue,
                            Weight) %>%
                     collect() 
                   
                   
                   appendTab(inputId = "tabs",
                             tabPanel(
                               tab_title,
                               renderDataTable(datatable(details) %>% formatRound(column = c('Weight'), digits = 2)))
                   )
                   
                   tab_list <<- c(tab_list, tab_title)
                   
                 }
                 
                 updateTabsetPanel(session, "tabs", selected = tab_title)
                 
               })

  
  observeEvent(input$remove,{
    # Use purrr's walk command to cycle through each
    # panel tabs and remove them
    tab_list %>%
      walk(~removeTab("tabs", .x))
    tab_list <<- NULL
  })

}


shinyApp(ui, server)