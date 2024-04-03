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


campanhas <- read_csv("/home/matheus/matheuspetlove/Calculadora Ecommerce/campanhas.csv", 
                           col_types = cols(start_week = col_character()))


# Definir a interface do aplicativo
ui <- fluidPage(
  titlePanel("Análise de Campanhas de Marketing"),
  sidebarLayout(
    sidebarPanel(
      selectInput("campaign", "Selecione a campanha:", unique(campanhas$campaign_type)),
      selectInput("ajuste", "Tipo de ajuste:",
                  choices = c("Linear", "Logarítmico", "Polinomial")),
      checkboxInput("force_intercept", "Forçar Intercepto como 0", value = FALSE),
      dateRangeInput("date_range", "Escolha o intervalo de datas:",
                     start = min(campanhas$start_week), end = max(campanhas$start_week),
                     min = min(campanhas$start_week), max = max(campanhas$start_week)),
      actionButton("save_button", "Salvar")
    ),
    mainPanel(
      plotlyOutput("custo_novos_clientes_plot"),
      DTOutput("coeficientes_table")
    )
  )
)

# Definir a função para o cálculo do ajuste escolhido
calculate_adjustment <- function(data, type, force_intercept) {
  if (type == "Linear") {
    if (force_intercept) {
      fit <- lm(novos_clientes ~ custo + 0, data)  # Força intercepto como 0
    } else {
      fit <- lm(novos_clientes ~ custo, data)
    }
    data$predicted <- predict(fit, newdata = data)
    params <- format(fit$coefficients, scientific = TRUE, digits = 3)
    mape <- mean(abs((data$novos_clientes - data$predicted) / data$novos_clientes)) * 100
    rsquared <- summary(fit)$r.squared
  } else if (type == "Logarítmico") {
    if (force_intercept) {
      fit <- lm(novos_clientes ~ log(custo) + 0, data)  # Força intercepto como 0
    } else {
      fit <- lm(novos_clientes ~ log(custo), data)
    }
    data$predicted <- predict(fit, newdata = data)
    params <- format(fit$coefficients, scientific = TRUE, digits = 3)
    mape <- mean(abs((data$novos_clientes - data$predicted) / data$novos_clientes)) * 100
    rsquared <- summary(fit)$r.squared
  } else if (type == "Polinomial") {
    if (force_intercept) {
      fit <- lm(novos_clientes ~ I(custo)+I(custo^2) + 0, data)  # Força intercepto como 0
    } else {
      fit <- lm(novos_clientes ~ I(custo)+I(custo^2), data)
    }
    data$predicted <- predict(fit, newdata = data)
    params <- format(fit$coefficients, digits = 3)
    mape <- mean(abs((data$novos_clientes - data$predicted) / data$novos_clientes)) * 100
    rsquared <- summary(fit)$r.squared
  }
  return(list(data = data, params = params, mape = mape, rsquared = rsquared))
}

# Definir o servidor
server <- function(input, output, session) {
  saved_coefficients <- data.frame(Campanha = character(),
                                   Ajuste = character(),
                                   Coeficientes = character(),
                                   stringsAsFactors = FALSE)
  
  observeEvent(input$save_button, {
    campanha_selecionada <- campanhas[campanhas$campaign_type == input$campaign &
                                        campanhas$start_week >= input$date_range[1] &
                                        campanhas$start_week <= input$date_range[2], ]
    tipo_ajuste <- input$ajuste
    
    result <- calculate_adjustment(campanha_selecionada, tipo_ajuste, input$force_intercept)
    params <- result$params
    
    if (nrow(saved_coefficients) == 0) {
      saved_coefficients <- data.frame(Campanha = input$campaign,
                                       Ajuste = tipo_ajuste,
                                       Coeficientes = params,
                                       stringsAsFactors = FALSE)
    } else {
      idx <- which(saved_coefficients$Campanha == input$campaign)
      if (length(idx) == 0) {
        new_row <- data.frame(Campanha = input$campaign,
                              Ajuste = tipo_ajuste,
                              Coeficientes = params,
                              stringsAsFactors = FALSE)
        saved_coefficients <- rbind(saved_coefficients, new_row)
      } else {
        saved_coefficients$Ajuste[idx] <- tipo_ajuste
        saved_coefficients$Coeficientes[idx] <- params
      }
    }
    
    # Imprimir a lista salva no console
    print(saved_coefficients)
  })
  
  output$custo_novos_clientes_plot <- renderPlotly({
    campanha_selecionada <- campanhas[campanhas$campaign_type == input$campaign &
                                        campanhas$start_week >= input$date_range[1] &
                                        campanhas$start_week <= input$date_range[2], ]
    tipo_ajuste <- input$ajuste
    
    result <- calculate_adjustment(campanha_selecionada, tipo_ajuste, input$force_intercept)
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
    
    result <- calculate_adjustment(campanha_selecionada, tipo_ajuste, input$force_intercept)
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
