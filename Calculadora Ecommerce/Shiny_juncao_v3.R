library(shiny)
library(shinydashboard)
library(ggplot2)
library(bigrquery)
library(magrittr)
library(nleqslv)
library(nloptr)
library(dplyr)
library(plotly)
library(reshape2)
library(DT)
library(readr)
library(caret)
library(lubridate)


petlove_colors <- c('#B788D1','#A1D188','#D19288')
campanhas <- read_csv("campanhas.csv", 
                      col_types = cols(start_week = col_character()))
df <- read_csv("dados_face_google_atualizado.csv", col_types = cols(anomes = col_date(format = "%m/%d/%Y")))


ui <- dashboardPage(
  dashboardHeader(title = "Marketing Dados"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Análise de Campanhas", tabName = "campanhas_tab", icon = icon("chart-line")),
      menuItem("Calculadora de Investimento", tabName = "calculadora_tab", icon = icon("calculator")),
      menuItem("Previsão de Novos Clientes", tabName = "novos_clientes", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "campanhas_tab",
        fluidPage(
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
      ),
      tabItem(
        tabName = "calculadora_tab",
        fluidPage(
          titlePanel("Calculadora"),
          sidebarLayout(
            sidebarPanel(
              numericInput("custo_tot", "Valor total do Investimento:", value = 3243452, min = 0),
              numericInput("custo_rest_min_aux", "Valor mínimo que o investimento pode ficar abaixo em porcentagem (80%=0.8)", value = 0.8, min = 0),
              numericInput("custo_rest_max_aux", "Valor máximo que o investimento pode ficar acima em porcentagem (120%=1.2)", value = 1.2, min = 0),
              textInput("month_rest", "Mês de referência", value = '2023-07-01'),
              actionButton("submit", "Otimizar")
            ),
            mainPanel(
              plotlyOutput("grafico_investimento_atual"),
              tableOutput("table")
            )
          )
        )
      ),
      tabItem(
        tabName = "novos_clientes",
        fluidPage(
          titlePanel("Previsão de Novos Clientes"),
          
          sidebarLayout(
            sidebarPanel(
              numericInput("investimento_google", "Investimento em Google:", value = 3468546),
              numericInput("investimento_facebook", "Investimento em Facebook:", value = 982103),
              
              dateRangeInput("intervalo_datas", "Intervalo de Datas:", start = as.Date("2022-01-01"), end = max(df$anomes)),
              
              sliderInput("proporcao_treinamento", "Proporção de Treinamento:", min = 0.5, max = 0.8, value = 0.7, step = 0.1),
              
              actionButton("atualizar", "Atualizar Previsão")
            ),
            
            mainPanel(
              plotlyOutput("grafico"),
              verbatimTextOutput("previsao_mensagem"),
              verbatimTextOutput("previsao_coefs")
            )
          )
        )
      )
    )
  )
)


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
  #Adicionando codigo da calculadora -------------------------
  projectid = "petlove-dataeng-prod-01"
  
  bq_auth(use_oob = TRUE, cache = FALSE)
  
  custo_rest_min_aux = 0.8 #20% a menos nao pode ultrapassar isso para reduzir mutio uma campanha de um mes para outro
  custo_rest_max_aux = 1.2 #20% a mais nao pode22 ultrapassar isso para aumentar mutio uma campanha de um mes para outro
  month_rest = "2023-09-01" #ultimo mes (se final mes para proximo, colocar mes corrente)
  
  sql <- "
  
  with custo_face_google as (
      --agrupando dados da seed facebook custo por mês  e acertando os tipos
  with face_custo as (SELECT
  --campaign_id,
        campaign_name as campanha,
      DATE_TRUNC(cast(date as date), MONTH) AS month,
        'face_insta_pago' as partner,
    sum(cast(replace(cast(cost as string), ',', '.') as float64)) as custo,
    sum(cast(link_clicks as int64)) as clicks,
    sum(cast(impressions as int64)) as impressions,
      date AS date
      FROM `petlove-dataeng-prod-01.ssot_seeds.seed_custo_marketing_petlove_facebook`
      where refdate =  (select max(refdate) from `petlove-dataeng-prod-01.ssot_seeds.seed_custo_marketing_petlove_facebook` )
      group by date, month, campanha, date ),
  
        --agrupando dados da seeds google custo por mês e acertando os tipos
   google_custo as (
  SELECT
  --    campaign_id ,
        campaign_name as campanha,
      DATE_TRUNC(cast(date as date), MONTH) AS month,
        'google_pago' as partner,
    sum(cast(replace(cast(cost as string), ',', '.') as float64)) as custo,
    sum(cast(clicks as int64)) as clicks,
    sum(cast(impressions as int64)) as impressions,
      date AS date
      FROM `petlove-dataeng-prod-01.ssot_seeds.seed_custo_marketing_petlove_google`
      where refdate =  (select max(refdate) from `petlove-dataeng-prod-01.ssot_seeds.seed_custo_marketing_petlove_google` ) --'2023-05-21'
      group by date, month, campanha, date
  )
  -- fazendo de-para para categorizar campanhas do google e face - query pega do time aquisição
  select
             fc.month,
                        fc.partner,
             CASE
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND LOWER(fc.campanha) LIKE '%instituc%' AND LOWER(fc.campanha) NOT LIKE '%instream%' THEN 'institucional'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE '%brandformance%' OR LOWER(fc.campanha) LIKE '%yt_assinatura%') THEN 'brandformance'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND LOWER(fc.campanha) LIKE '%branding%' AND LOWER(fc.campanha) NOT LIKE '%marcas_exclusivas%' THEN 'branding'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND LOWER(fc.campanha) LIKE '%dsa%' THEN 'DSA'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND(LOWER(fc.campanha) LIKE 's_%' OR LOWER(fc.campanha) LIKE '%paid-search%') AND (LOWER(fc.campanha) LIKE '%s_marcas_exclusivas%' OR LOWER(fc.campanha) LIKE '%marcas-exclusivas%' ) THEN 'SEM_MARCAS_EXCLUSIVAS'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 's_%' OR LOWER(fc.campanha) LIKE '%paid-search%') AND LOWER(fc.campanha) LIKE '%med%' AND LOWER(fc.campanha) NOT LIKE '%comed%' THEN 'SEM_MED'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 's_%' OR LOWER(fc.campanha) LIKE '%paid-search%') AND LOWER(fc.campanha) LIKE '%rac%' AND LOWER(fc.campanha) NOT LIKE '%racas%' THEN 'SEM_RAC'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 's_%' OR LOWER(fc.campanha) LIKE '%paid-search%') AND LOWER(fc.campanha) LIKE '%gen%' THEN 'SEM_GEN'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 's_%' OR LOWER(fc.campanha) LIKE '%paid-search%') AND (LOWER(fc.campanha) LIKE '%bri%' OR LOWER(fc.campanha) LIKE '%acessorios%' OR LOWER(fc.campanha) LIKE '%trans_nova%') OR LOWER(fc.campanha) LIKE '%ac-bri%' THEN 'SEM_AC_BRI'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 's_%' OR LOWER(fc.campanha) LIKE '%paid-search%') AND LOWER(fc.campanha) LIKE '%hl%' THEN 'SEM_HL'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 's_%' OR LOWER(fc.campanha) LIKE '%paid-search%') AND LOWER(fc.campanha) NOT LIKE 'smart_%' THEN  'SEM_outros' --fc.campanha --
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND LOWER(fc.campanha) LIKE '%rmkt%' AND (LOWER(fc.campanha) NOT LIKE '%display%' OR LOWER(fc.campanha) NOT LIKE '%yt%') THEN 'retargeting'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE '%display%' OR LOWER(fc.campanha) LIKE 'gsp%') THEN 'display'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND LOWER(fc.campanha) LIKE '%discovery%' THEN 'discovery'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE '%app%' AND LOWER(fc.campanha) LIKE '%android%') AND LOWER(fc.campanha) NOT LIKE '%ace%' THEN 'app_install_and'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE '%app%' AND LOWER(fc.campanha) LIKE '%ios%') AND LOWER(fc.campanha) NOT LIKE '%ace%' THEN 'app_install_ios'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE '%app%' AND LOWER(fc.campanha) LIKE '%ace%') THEN 'app_engagement'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND LOWER(fc.campanha) LIKE '%app%' AND (LOWER(fc.campanha) NOT LIKE ('%android%') OR LOWER(fc.campanha) NOT LIKE '%ios%') THEN 'app_install'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 'v_%' OR LOWER(fc.campanha) LIKE 'yt_%' OR LOWER(fc.campanha) LIKE '%bumper%' OR LOWER(fc.campanha) LIKE '%trueview%') AND (LOWER(fc.campanha) NOT LIKE '%branding%' OR LOWER(fc.campanha) NOT LIKE '%assinatura%') THEN 'youtube'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND LOWER(fc.campanha) LIKE '%rmkt%' AND (LOWER(fc.campanha) NOT LIKE '%display%' OR LOWER(fc.campanha) NOT LIKE '%yt%') THEN 'retargeting'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 'gs_%' OR LOWER(fc.campanha) LIKE '%paid-shopping%')AND LOWER(fc.campanha) NOT LIKE '%gsp%' AND LOWER(fc.campanha) LIKE '%med%' AND LOWER(fc.campanha) NOT LIKE '%comed%' THEN 'shopping_MED'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 'gs_%' OR LOWER(fc.campanha) LIKE '%paid-shopping%')AND LOWER(fc.campanha) NOT LIKE '%gsp%' AND (LOWER(fc.campanha) LIKE '%rac_low%' OR LOWER(fc.campanha) LIKE '%raclowltv%') THEN 'shopping_RAC_low_ltv'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 'gs_%' OR LOWER(fc.campanha) LIKE '%paid-shopping%')AND LOWER(fc.campanha) NOT LIKE '%gsp%' AND (LOWER(fc.campanha) LIKE '%rac_high%' OR LOWER(fc.campanha) LIKE '%rachgltv%') THEN 'shopping_RAC_high_ltv'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 'gs_%' OR LOWER(fc.campanha) LIKE '%paid-shopping%')AND LOWER(fc.campanha) NOT LIKE '%gsp%' AND (LOWER(fc.campanha) LIKE '%ac%' OR LOWER(fc.campanha) LIKE '%bri%') AND LOWER(fc.campanha) NOT LIKE '%gsp%' THEN 'shopping_AC_BRI'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 'gs_%' OR LOWER(fc.campanha) LIKE '%paid-shopping%')AND LOWER(fc.campanha) NOT LIKE '%gsp%' AND LOWER(fc.campanha) LIKE '%hl%' THEN 'shopping_HL'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 'gs_%' OR LOWER(fc.campanha) LIKE '%paid-shopping%')AND LOWER(fc.campanha) NOT LIKE '%gsp%' AND LOWER(fc.campanha) LIKE '%hab%' THEN 'shopping_HAB'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 'gs_%' OR LOWER(fc.campanha) LIKE '%paid-shopping%')AND LOWER(fc.campanha) NOT LIKE '%gsp%' AND (LOWER(fc.campanha) LIKE '%marcas_exclusivas%' OR LOWER(fc.campanha) LIKE '%marcas-exclusivas%') THEN 'shopping_MARCAS_EXCLUSIVAS'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 'gs_%' OR LOWER(fc.campanha) LIKE '%paid-shopping%')AND LOWER(fc.campanha) NOT LIKE '%gsp%' THEN 'shopping_outros'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND LOWER(fc.campanha) LIKE '%marcas_exclusivas%' THEN 'marcas_exclusivas'
          WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')THEN 'google_outros'
  --FACEBOOK
          WHEN lower(fc.partner) = 'face_insta_pago' AND (LOWER(fc.campanha) LIKE '%marcas_exclusivas%' OR LOWER(fc.campanha) LIKE '%futurepet%' OR LOWER(fc.campanha) LIKE  '%marcas-exclusivas%') THEN 'marcas_exclusivas'
          WHEN lower(fc.partner) = 'face_insta_pago' AND LOWER(fc.campanha) LIKE '%branding%' THEN 'branding'
          WHEN lower(fc.partner) = 'face_insta_pago' AND (LOWER(fc.campanha) LIKE '%brandformance%' OR LOWER(fc.campanha) LIKE '%compra_recorrente%'OR LOWER(fc.campanha) LIKE  '%reels%') THEN 'brandformance'
          WHEN lower(fc.partner) = 'face_insta_pago' AND LOWER(fc.campanha) LIKE '%app%' AND LOWER(fc.campanha) LIKE '%android%' THEN 'app_install_and'
          WHEN lower(fc.partner) = 'face_insta_pago' AND LOWER(fc.campanha) LIKE '%app%' AND LOWER(fc.campanha) LIKE '%ios%' THEN 'app_install_ios'
          WHEN lower(fc.partner) = 'face_insta_pago' AND LOWER(fc.campanha) LIKE '%app%' AND (LOWER(fc.campanha) NOT LIKE '%android%' OR LOWER(fc.campanha) NOT LIKE '%ios%') THEN 'app_install'
          WHEN lower(fc.partner) = 'face_insta_pago' AND (LOWER(fc.campanha) LIKE '%prospeccao%' OR LOWER(fc.campanha) LIKE'%asc%') THEN 'prospeccao'
          WHEN lower(fc.partner) = 'face_insta_pago' AND (LOWER(fc.campanha) LIKE '%retargeting_pre%' OR LOWER(fc.campanha) LIKE'%retargeting-pre%' OR LOWER(fc.campanha) LIKE'%retargeting_sem-dpa%' OR LOWER(fc.campanha) LIKE '%retargeting_dpa_ate30d%') THEN 'retargeting_pre_venda'
          WHEN lower(fc.partner) = 'face_insta_pago' AND (LOWER(fc.campanha) LIKE '%retargeting-pos%' OR LOWER(fc.campanha) LIKE '%rmktposvenda%' OR LOWER(fc.campanha) LIKE '%crossell%') THEN 'retargeting_pos_venda'
          WHEN lower(fc.partner) = 'face_insta_pago' AND LOWER(fc.campanha) LIKE '%loja%' THEN 'lojas_fisicas'
          WHEN lower(fc.partner) = 'face_insta_pago'  THEN 'outros'
          ELSE 'outros'
  END AS campaign_type,
  
             sum(fc.custo) as custo,
             sum(fc.clicks) as clicks,
             sum(fc.impressions) as impressions ,
           --  sum(fc.unique_clicks) as unique_clicks
              from  (select * from  face_custo union all select * From google_custo ) fc
           --   left join campaign_type_face_google ctfg
              group by month, campaign_type, partner
  
  
  --cruzar query bianchi
  
  
  ),
  -- colocando custo min e max para cada campanha com base no custo historico do ultimo mes e proporcionalmente nao alocar muito a mais ou a menos
  
  campanhas_modelo as (select 1 as ordem,'google_pago' as partner ,'shopping_RAC_high_ltv' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max union all
  select 2 as ordem,'google_pago' as partner , 'shopping_RAC_low_ltv' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max union all
  select 3 as ordem,'google_pago' as partner,'shopping_MED' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max union all
  select 4 as ordem,'google_pago' as partner,'shopping_AC_BRI' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max union all
  select 5 as ordem,'google_pago' as partner,'shopping_HL' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max union all
  select 6 as ordem,'google_pago' as partner,'shopping_HAB' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max union all
  select 7 as ordem,'google_pago' as partner,'shopping_MARCAS_EXCLUSIVAS' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max union all
  select 8 as ordem,'google_pago' as partner,'SEM_RAC' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max union all
  select 9 as ordem,'google_pago' as partner,'SEM_MED' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max union all
  select 10 as ordem,'google_pago' as partner,'DSA' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max union all
  select 11 as ordem,'google_pago' as partner,'SEM_GEN' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max union all
  select 12 as ordem,'google_pago' as partner,'SEM_AC_BRI' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max union all
  select 13 as ordem,'google_pago' as partner,'SEM_HL' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max union all
  select 14 as ordem,'google_pago' as partner,'SEM_MARCAS_EXCLUSIVAS' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max  union all
  select 15 as ordem,'facebook_insta_pago' as partner,'marcas_exclusivas' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max  union all
  select 16 as ordem,'facebook_insta_pago' as partner,'prospeccao' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max  union all
  select 17 as ordem,'facebook_insta_pago' as partner,'retargeting_pre_venda' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max  --union all
  --select 18 as ordem,'facebook_insta_pago' as partner,'retargeting_pos_venda' as campaign_type, custo_rest_min_aux as rest_min, custo_rest_max_aux as rest_max
  ),
  --criando a tabela final a ser utilizda pelo modelo na ordem das curvas do google sheet )
  tabela_aux as(
  select ordem,	cm.campaign_type as campanha,	month	,cm.partner,	custo,(custo  / SUM(custo) OVER ()) AS custo_perc, custo_tot*rest_min*(custo  / SUM(custo) OVER ()) as custo_rest_min ,custo_tot*rest_max*(custo  / SUM(custo) OVER ()) as custo_rest_max, 	clicks,	impressions from campanhas_modelo cm  left join (select * from custo_face_google   where month =  month_rest ) cfg on cfg.campaign_type=cm.campaign_type order by ordem
  ) -- where partner ='face_insta_pago'
  select campanha,custo_rest_min,custo_rest_max, custo from tabela_aux
  --select campaign_type, sum(custo) custo from custo_face_google where partner ='face_insta_pago' and month = custo_tot  group by campaign_type order by custo desc
  "
  options(useFancyQuotes = FALSE)
  custo_tot <- reactive({
    input$custo_tot
  })  %>% bindEvent(input$submit)
  custo_rest_min_aux <- reactive({
    input$custo_rest_min_aux
  })  %>% bindEvent(input$submit)
  custo_rest_max_aux <- reactive({
    input$custo_rest_max_aux
  })  %>% bindEvent(input$submit)
  month_rest <- reactive({
    input$month_rest
  })  %>% bindEvent(input$submit)
  
  
  df_alocacao <- reactive({
    sql = gsub("custo_rest_min_aux", custo_rest_min_aux(), sql)
    sql = gsub("custo_rest_max_aux", custo_rest_max_aux(), sql)
    sql = gsub("month_rest", sQuote(month_rest()), sql)
    tb <- bq_project_query(projectid, gsub("custo_tot", custo_tot(), sql))
    df = bq_table_download(tb) %>% data.frame()
    orçamento_campanhas = custo_tot()
    
    tot_val = ( orçamento_campanhas)/31*7 #colar linha 4
    #tot_marcas_exclusivas= tot_val*0.04022
    tot_marcas_exclusivas= 0
    max_acs=tot_val/2
    
    
    
    
    eval_g_eq <- function(x)
    {
      return ( x[1] + x[2] + x[3] + x[4] +
                 x[5]+ x[6]+ x[7]+ x[8]+ x[9]+
                 x[10]+ x[11]+ x[12]+ x[13]+
                 x[14]+ x[15]+ x[16]+ x[17] - tot_val)
    }
    
    
    eval_f <- function(x)
      #otimizando receita total = equacao  y invest = custo xaxsax.. minimizando - => maximizando
    { #x[i] --> investimento na campanha i
      # a*x[i] + b ou a*log(x[i]) +c ou a*x[i]^n + b , os coefs sai do ajuste
      # https://docs.google.com/spreadsheets/d/1004GNz-Bc3LP12S3SrBfDJXrahLXOd9UaW5U8ymRvEY/edit#gid=803232819
      return ( -(
        #(novos clientes)* LTV médio * Fator de Atribuição * Fator usado no fim do ano
        ((0.00152713506482925*x[1]+(0))*52.43*(1+(-0.0088))*(1+(0))+
           (154.442819659219*log(x[2])+
              (-1360.95034317049))*3.62*(1+(-0.0693))*(1+(0))+
           (62.3957035293728*log(x[3])+
              (-553.661631872104))*12.07*(1+(-0.0462))*(1+(0))+
           (41.8121593805383*log(x[4])+
              (-291.693781881052))*13.33*(1+(-0.0418))*(1+(0))+
           (80.3796690626354*log(x[5])+
              (-680.871911186158))*12.43*(1+(-0.055))*(1+(0))+
           (59.2339302041498*log(x[6])+
              (-564.989091041965))*32.54*(1+(-0.0638))*(1+(0))+
           (0+(0.00289605110347188)*x[7]+
              (-0.0000000114983457686385)*x[7]^2)*21*(1+(-0.0935))*(1+(0))+
           (53.3549828280935*log(x[8])+
              (-483.883322578414))*30.68*(1+(0.0693))*(1+(0))+
           (0+(0.00164658302260055)*x[9]+
              (-0.00000000744682895623575)*x[9]^2)*41*(1+(-0.0027))*(1+(0))+
           (26.9590312031208*log(x[10])+
              (-218.548483248899))*29.71*(1+(-0.0063))*(1+(0))+
           (0+(0.00217121083251638)*x[11]+
              (-0.000000026435483539321)*x[11]^2)*27*(1+(0.0018))*(1+(0))+
           (2.09459294717371*log(x[12])+(-11.1511478352375))*39*(1+(0.0063))*(1+(0))+
           (0.00181656268939222*x[13]+(0))*40*(1+(0.0252))*(1+(0))+
           (0.00187733117168608*x[14]+(0))*80*(1+(0.0693))*(1+(0))+
           (0.598274991572796*log(x[15])+(-4.12416640388016))*13*(1+(8.17))*(1+(0))+
           (0.000150641610484799*x[16]+(0))*35*(1+(18.06))*(1+(0))+
           (0.000129389343507315*x[17]+(0))*10*(1+(18.791))*(1+(0)))
        #aquiterminae
      ))
      
    }
    
    # numeros ltv -https://docs.google.com/spreadsheets/d/1ib6t8bTibtrYpSUKeEJNoxI0HMDdiaJbZBozS_0_T1w/edit#gid=483343494
    
    
    # Lower and upper bounds
    lb <- df$custo_rest_min
    ub <- df$custo_rest_max
    lb = lb /31*7
    ub=ub/31*7 # /37*7 ajuste de semanal para mensal
    #x0 <- c((tot_val-max_acs)/3,(tot_val-max_acs)/3,max_acs,(tot_val-max_acs)/3)
    
    x0 <- (lb+ub)/2 # valor de inicio para otimização
    
    # Set optimization options.
    local_opts <- list( "algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1.0e-15 )
    opts <- list( "algorithm"= "NLOPT_GN_ISRES", #Escolhido de forma aleatória pelo Felipe
                  "xtol_rel"= 1.0e-15,
                  "maxeval"= 1660000,
                  "local_opts" = local_opts,
                  "print_level" = 0 )
    res <- nloptr ( x0 = x0,
                    eval_f = eval_f,
                    lb = lb,
                    ub = ub,
                    #eval_g_ineq = eval_g_ineq,
                    eval_g_eq = eval_g_eq,
                    opts = opts
    )
    
    results <- res$solution
    df_alocacao <- data.frame(df %>% head(19) %>% data.frame() ,
                              prop_mes_referencia =df$custo/sum(df$custo),sugestao_calc=res$solution/7*31, prop_calc = res$solution/7*31/sum(res$solution/7*31),
                              var_prop = ((res$solution/7*31/sum(res$solution/7*31))/ (df$custo/sum(df$custo)) -1)*100  )
    df_alocacao
  })
  df_resultado <- reactive({
    aux <- data.frame(df_alocacao()$campanha,df_alocacao()$custo,df_alocacao()$sugestao_calc)
    colnames(aux) <- c('campanha','Sem calculadora', 'Sugestão Calculadora')
    aux <- melt(aux,id.vars='campanha')
    colnames(aux) <- c('campanha','Fonte','Valor')
    aux
  })
  output$grafico_investimento_atual <- renderPlotly({
    
    ggplotly(ggplot(df_resultado(), aes(x = campanha, y = Valor, fill = Fonte)) +
               geom_bar(stat = "identity",position='dodge') +
               theme_minimal() +
               labs(title = "Gráfico de Barras - Alocação por Campanha", x = "Campanha", y = "Alocação") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  })
  output$table <- renderTable({
    df_alocacao()
  })
  
  ###### essa parte é para novos clientes #################
  df_filtered <- reactive({
    df %>%
      filter(anomes >= input$intervalo_datas[1] & anomes <= input$intervalo_datas[2])
  })
  
  observeEvent(input$atualizar, {
    set.seed(42)  # Definir a semente (seed) para reproducibilidade
    
    # Criar e treinar o modelo de regressão linear
    df_treinamento <- df_filtered()
    x <- df_treinamento[, c("custo_total_google", "custo_total_facebook")]
    y <- df_treinamento$novos_clientes
    trainIndex <- createDataPartition(y, p = input$proporcao_treinamento, list = FALSE)
    x_treinamento <- x[trainIndex, ]
    y_treinamento <- y[trainIndex]
    modelo <- train(
      x = x_treinamento,
      y = y_treinamento,
      method = "lm"
    )
    
    # Prever o próximo mês
    X_proximo_mes <- data.frame(
      custo_total_google = input$investimento_google,
      custo_total_facebook = input$investimento_facebook
    )
    
    y_proximo_mes <- predict(modelo, X_proximo_mes)
    
    mae_teste <- mean(abs(y_treinamento - predict(modelo, x_treinamento)))
    mape_teste <- mean(abs((y_treinamento - predict(modelo, x_treinamento)) / y_treinamento)) * 100
    
    mes_proximo <- as.Date(Sys.time()) %m+% months(1)
    
    output$previsao_mensagem <- renderPrint({
      cat("Previsão para o próximo mês (", format(mes_proximo, "%Y-%m"), "):", round(y_proximo_mes, 2), "Novos Clientes\n")
    })
    output$previsao_coefs <- renderPrint({
      cat("Coeficientes da Regressão:\n",
          "Intercepto: ", coef(modelo$finalModel)[1], "\n",
          "Google: ", coef(modelo$finalModel)[2], "\n",
          "Face: ", coef(modelo$finalModel)[3])
    })
    
    
    # Gráfico comparando valores reais e previstos
    df_previsto_teste <- data.frame(anomes = df_treinamento$anomes, previsto = predict(modelo, df_treinamento))
    y_proximo_mes_arredondado <- round(y_proximo_mes, 0)
    
    output$grafico <- renderPlotly({
      p <- ggplot() +
        geom_line(data = df_treinamento, aes(x = anomes, y = novos_clientes, color = "Real"), size = 1, show.legend = TRUE) +
        geom_line(data = df_previsto_teste, aes(x = anomes, y = previsto, color = "Previsto"), linetype = "dashed", size = 1, show.legend = TRUE) +
        geom_point(data = data.frame(anomes = mes_proximo, previsto = y_proximo_mes), aes(x = anomes, y = previsto, color = "Próximo Mês"), size = 3, show.legend = TRUE) +
        geom_text(data = data.frame(anomes = mes_proximo, previsto = y_proximo_mes_arredondado), aes(x = anomes, y = previsto, label = previsto), vjust = -1, nudge_y = 900, size = 3, show.legend = FALSE, check_overlap = TRUE) +
        xlab("Ano/Mês") +
        ylab("Novos Clientes") +
        labs(
          title = paste("Comparação entre Valores Reais e Previstos\nMAE:", round(mae_teste, 2), ", MAPE:", round(mape_teste, 2), "%"),
          color = ""
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray"),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "gray"),
          axis.title.x = element_text(color = "gray"),
          axis.title.y = element_text(color = "gray"),
          axis.text.x = element_text(color = "gray"),
          axis.text.y = element_text(color = "gray")
        ) +
        scale_color_manual(values = petlove_colors)
      
      ggplotly(p)
    })
  })
}

shinyApp(ui = ui, server = server)

