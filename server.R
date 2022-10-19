library(ggplot2)
library(dplyr)
library(tidyr)
library(shiny)
library(shinydashboard)

setwd("~/.Fall 2021 Classes/Stat 385/Stock Analysis Tool")

load("returns_long.RData")
load("performance_summary.RData")
load("sp500.RData")

server = function(input, output) {
  
  ## Candlestick
  output$candlestick = renderPlot({
  
    charting_data = returns_long %>% filter(Ticker == input$ticker_select, Date >= "2021-11-15")
    
    candlestick = ggplot(charting_data) + 
      geom_boxplot(aes(x = as.character(Date), y = Value, fill = Movement), color = "#D0D3D4", width = 0.2) +
      scale_fill_manual(values = c(Up = "#0066ff", Down = "#ffff00")) +
      xlab("Date") +
      ylab("Stock Price") +
      labs(
        title = paste0(charting_data$Name[1], " (", input$ticker_select, ")"),
        subtitle = charting_data$Sector[1],
        caption = "Source: Yahoo! Finance"
      ) +
      scale_y_continuous(labels = scales::dollar) +
      theme(
        plot.background = element_rect(fill = "#17202A"),
        panel.background = element_rect(fill = "#17202A"),
        axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = "#ffffff"),
        axis.title.x = element_text(color = "#ffffff"),
        axis.title.y = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#ffffff"),
        plot.subtitle = element_text(color = "#ffffff"),
        plot.caption = element_text(color = "#ffffff", face = "italic", size = 6),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#273746"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none"
        
      )
    
    candlestick
  })
  
  ## Price Chart
  output$price_chart = renderPlot({
  
    price_data = returns_long %>% filter(Ticker == input$ticker_select, Series == "Close")
    
    price_chart = ggplot(price_data) + 
      geom_line(aes(x = Date, y = Value), color = "#0066ff") +
      scale_fill_manual(values = c(Up = "#0066ff", Down = "#ffff00")) +
      xlab("Date") +
      ylab("Stock Price") +
      labs(
        title = paste0(price_data$Name[1], " (", input$ticker_select, ")"),
        subtitle = price_data$Sector[1],
        caption = "Source: Yahoo! Finance"
      ) +
      scale_y_continuous(labels = scales::dollar) +
      theme(
        plot.background = element_rect(fill = "#17202A"),
        panel.background = element_rect(fill = "#17202A"),
        axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = "#ffffff"),
        axis.title.x = element_text(color = "#ffffff"),
        axis.title.y = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#ffffff"),
        plot.subtitle = element_text(color = "#ffffff"),
        plot.caption = element_text(color = "#ffffff", face = "italic", size = 6),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#273746"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none"
        
      )
    
    price_chart
  })
  
  ## Industry Chart
  output$industry_chart = renderPlot({
    
    sector = sp500 %>% filter(Ticker == input$ticker_select) %>%  select(Sector) %>% as.character()
    industry = sp500 %>% filter(Ticker == input$ticker_select) %>% select(Industry) %>% as.character()
    
    ## Fix the difference in industry 
    industry_summary_data = performance_summary %>% 
      filter(Sector == sector) %>% 
      mutate(
        isIndustry = ifelse(Industry == industry, "Industry", "Non_Industry")
      )
    
    industry_chart = ggplot(industry_summary_data) + 
      geom_bar(aes(x = Industry, y = One_year, fill = isIndustry), stat = "summary", fun = "mean") +
      scale_fill_manual(values = c(Industry = "#ffff00", Non_Industry = "#0066ff")) +
      xlab("One Year Return") +
      ylab("Industry") +
      labs(
        title = "Industry Returns",
        subtitle = price_data$Sector[1],
        caption = "Source: Yahoo! Finance"
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme(
        plot.background = element_rect(fill = "#17202A"),
        panel.background = element_rect(fill = "#17202A"),
        axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = "#ffffff"),
        axis.title.x = element_text(color = "#ffffff"),
        axis.title.y = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#ffffff"),
        plot.subtitle = element_text(color = "#ffffff"),
        plot.caption = element_text(color = "#ffffff", face = "italic", size = 6),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#273746"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none"
        
      )
    
    industry_chart
  })
  
  ## Performance Chart
  output$performance_chart = renderPlot({
    
    performance_summary_data = performance_summary %>% 
      filter(Ticker == input$ticker_select) %>% 
      select(One_Month, Three_Month, One_year, Three_years, Five_years, Ten_years)
    
    performance_summary_data = performance_summary_data %>% gather("Period", "Return")
    
    performance_summary_data = performance_summary_data %>% mutate(
      Period = case_when(
        Period == "One_Month" ~ "1 Month",
        Period == "Three_Month" ~ "1 Quarter",
        Period == "One_year" ~ "1 Year",
        Period == "Three_years" ~ "3 Years",
        Period == "Five_years" ~ "5 Years",
        Period == "Ten_years" ~ "10 Years",
      )
    )
    
    performance_summary_data$Period = factor(performance_summary_data$Period, levels = c("1 Month", "1 Quarter", "1 Year", "3 Years", "5 Years", "10 Years"))
    
    performance_chart = ggplot(performance_summary_data) + 
      geom_bar(aes(x = Period, y = Return), stat = "identity", fill = "#0066ff") +
      xlab("Period") +
      ylab("Annualized Returns") +
      labs(
        title = "Returns",
        caption = "Source: Yahoo! Finance"
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme(
        plot.background = element_rect(fill = "#17202A"),
        panel.background = element_rect(fill = "#17202A"),
        axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = "#ffffff"),
        axis.title.x = element_text(color = "#ffffff"),
        axis.title.y = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#ffffff"),
        plot.subtitle = element_text(color = "#ffffff"),
        plot.caption = element_text(color = "#ffffff", face = "italic", size = 6),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#273746"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none"
        
      )
    
    performance_chart
  })
  
}

shinyApp(ui, server)
