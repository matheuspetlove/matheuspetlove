library(shiny)
library(ggplot2)
library(bigrquery) #pacote conexão bigquery
library(magrittr) #pacote para fazer ETL
library(nleqslv)
library(nloptr)
library(dplyr)
library(plotly)
library(reshape2)
library(DT)  # Biblioteca para a tabela interativa
library(readr)


campanhas <- read_csv("Área de Trabalho/Calculadora Shiny/campanhas.csv", 
                      col_types = cols(start_week = col_character()))


# Definir a interface do aplicativo
ui <- fluidPage(
  titlePanel("Análise de Campanhas de Marketing"),
  sidebarLayout(
    sidebarPanel(
      selectInput("campaign", "Selecione a campanha:", unique(campanhas$campaign_type)),
      selectInput("ajuste", "Tipo de ajuste:",
                  choices = c("Linear", "Logarítmico", "Polinomial")),
      dateRangeInput("date_range", "Escolha o intervalo de datas:",
                     start = min(campanhas$start_week), end = max(campanhas$start_week),
                     min = min(campanhas$start_week), max = max(campanhas$start_week))
    ),
    mainPanel(
      plotlyOutput("custo_novos_clientes_plot"),
      DTOutput("coeficientes_table")
    )
  )
)

# Definir a função para o cálculo do ajuste escolhido
calculate_adjustment <- function(data, type) {
  if (type == "Linear") {
    fit <- lm(novos_clientes ~ custo, data)
    data$predicted <- predict(fit, newdata = data)
    params <- format(fit$coefficients, scientific = TRUE, digits = 3)
    mape <- mean(abs((data$novos_clientes - data$predicted) / data$novos_clientes)) * 100
    rsquared <- summary(fit)$r.squared
  } else if (type == "Logarítmico") {
    fit <- lm(log(novos_clientes) ~ custo, data)
    data$predicted <- exp(predict(fit, newdata = data))
    params <- format(fit$coefficients, scientific = TRUE, digits = 3)
    mape <- mean(abs((data$novos_clientes - data$predicted) / data$novos_clientes)) * 100
    rsquared <- summary(fit)$r.squared
  } else if (type == "Polinomial") {
    fit <- lm(novos_clientes ~ poly(custo, 2), data)
    data$predicted <- predict(fit, newdata = data)
    params <- format(fit$coefficients, digits = 3)
    mape <- mean(abs((data$novos_clientes - data$predicted) / data$novos_clientes)) * 100
    rsquared <- summary(fit)$r.squared
  }
  return(list(data = data, params = params, mape = mape, rsquared = rsquared))
}

# Definir o servidor
server <- function(input, output) {
  output$custo_novos_clientes_plot <- renderPlotly({
    campanha_selecionada <- campanhas[campanhas$campaign_type == input$campaign &
                                        campanhas$start_week >= input$date_range[1] &
                                        campanhas$start_week <= input$date_range[2], ]
    tipo_ajuste <- input$ajuste
    
    result <- calculate_adjustment(campanha_selecionada, tipo_ajuste)
    plot_data <- result$data
    params <- result$params
    mape <- result$mape
    rsquared <- result$rsquared
    
    p <- ggplot(plot_data, aes(x = custo, y = novos_clientes)) +
      geom_point() +
      geom_line(aes(x = custo, y = predicted), color = "blue") +
      geom_point(aes(x = custo, y = predicted), color = "red") +
      labs(
        x = "Custo",
        y = "Novos Clientes",
        title = paste("Análise da Campanha", input$campaign, " | MAPE:", round(mape, 2), "% | R²:", round(rsquared, 2)),
        subtitle = paste("Ajuste:", tipo_ajuste)
      )
    
    ggplotly(p)
  })
  
  output$coeficientes_table <- renderDT({
    campanha_selecionada <- campanhas[campanhas$campaign_type == input$campaign &
                                        campanhas$start_week >= input$date_range[1] &
                                        campanhas$start_week <= input$date_range[2], ]
    tipo_ajuste <- input$ajuste
    
    result <- calculate_adjustment(campanha_selecionada, tipo_ajuste)
    params <- result$params
    
    df <- data.frame(
      Coeficientes = names(params),
      Valor = params,
      row.names = NULL
    )
    
    datatable(df, options = list(pageLength = 5, searching = FALSE, paging = FALSE, info = FALSE))
  })
}

# Executar o aplicativo
shinyApp(ui = ui, server = server)

