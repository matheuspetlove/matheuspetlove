# Calculadora Saúde
library(bigrquery)
library(magrittr) 
library(nleqslv) 
library(nloptr)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyverse)


# A Calculadora é dividida nas seguintes etapas:

#1. Curva de novos Pets para cada campanha
#2. Cálculo do valor agregado por Pet para cada campanha
#3. Constante de atribuição das campanhas ( Opcional)

# A calculadora deve otimizar a função
# F([x]) = Curva(xi)*Valor Agregado(xi)* Atribuição (xi)
# onde xi representa o nome da campanha.


#1. Curvas de Novos Pets

# Importando os dados do BigQuery
projectid = "petlove-dataeng-prod-01"
query_curvas <- "-- Selecionar os novos pets com detalhes de transação
WITH novo_pet AS (
    SELECT
        id_pet,
        date_trunc(dt_inclusao_pet, week) as data_inclusao,
        transaction_id,
        chv_canais_marketing,
        chv_plataforma_marketing,
        tipo_transacao,
        tipo_checkout
    FROM
        `petlove-dataeng-prod-01.curated_ga.itd_transactions_ga4` a
    RIGHT JOIN
        `op_health.beneficiarios` b 
    ON
        CAST(a.transaction_id AS STRING) = CAST(b.vindi_subscription_id AS STRING)
    WHERE
        fl_pet_sem_inclusao = 0
        AND fl_contrato_migrado = 0
), 
-- Mapear as campanhas de marketing
depara_campanhas AS (
    SELECT DISTINCT
        tipo_campanha,
        a.chv_dim_canais_marketing,
        b.chv_dim_campanhas_custos
    FROM
        dw_corporativo.ft_custos_mkt_regioes a
    LEFT JOIN
        dw_corporativo.dim_campanhas_custos b 
    ON
        a.chv_dim_campanhas_custos = b.chv_dim_campanhas_custos

   LEFT JOIN
     dw_corporativo.dim_canais_marketing_spk a2
    ON    a2.chv_canais_marketing = a.chv_dim_canais_marketing
    WHERE
        bu = 'saude'
),
-- Selecionar novos pets com informações de campanha de marketing
novos_pets AS ( 
    SELECT DISTINCT
        a.*,
        tipo_campanha
    FROM
        novo_pet a
    LEFT JOIN
        depara_campanhas b 
    ON
        a.chv_canais_marketing = b.chv_dim_canais_marketing
    WHERE
        tipo_campanha IS NOT NULL
), 
-- Contar a quantidade de novos pets por semana e campanha
main_conversao AS ( 
    SELECT
        data_inclusao,
        tipo_campanha,
        COUNT(DISTINCT id_pet) AS Quantidade
    FROM
        novos_pets 
    GROUP BY
        1,2
    ORDER BY
        1 DESC
),
-- Selecionar os custos de campanhas de marketing
custos_aux AS (
    SELECT
        date_trunc(dt_campanha, week) AS data,
        tipo_campanha,
        a.chv_dim_canais_marketing,
        dt_campanha,
        vlr_custo
    FROM
        dw_corporativo.ft_custos_mkt_regioes a
    LEFT JOIN
        dw_corporativo.dim_campanhas_custos b 
    ON
        a.chv_dim_campanhas_custos = b.chv_dim_campanhas_custos
    LEFT JOIN
     dw_corporativo.dim_canais_marketing_spk a2
    ON    a2.chv_canais_marketing = a.chv_dim_canais_marketing
    
    WHERE
        bu = 'saude'
),
-- Somar os custos de campanha por semana e campanha
custos_main AS (
    SELECT
        data,
        tipo_campanha,
        SUM(vlr_custo) AS Custo
    FROM
        custos_aux
    GROUP BY
        1,2
    ORDER BY
        3 DESC
)
-- Selecionar os resultados finais combinando dados de conversão e custos
SELECT
    a.data,
    data_inclusao,
    a.tipo_campanha,
    Custo,
    Quantidade
FROM
    custos_main a 
LEFT JOIN
    main_conversao b 
ON
    a.tipo_campanha = b.tipo_campanha
    AND a.data = b.data_inclusao
WHERE
    data_inclusao >= '2023-01-01'" 

tb_curvas <- bq_project_query(projectid,query_curvas)
df_curvas = bq_table_download(tb_curvas) %>% data.frame()

#Visualizando as curvas em scatterplot
p <- ggplot(df_curvas, aes(x = Custo, y = Quantidade, color = tipo_campanha)) +
  geom_point() + 
  facet_wrap(~tipo_campanha)

p_plotly <- ggplotly(p)
p_plotly
######################################


#Agora vamos entender o comportamento linear/não linear

# Cria uma função para calcular as correlações de Pearson e Spearman
calc_correlations <- function(df) {
  pearson_corr <- cor(df$Custo, df$Quantidade, method = "pearson")
  spearman_corr <- cor(df$Custo, df$Quantidade, method = "spearman")
  return(c(pearson_corr, spearman_corr))
}

# Aplicando a função a cada subconjunto de dados para cada campanha

cor_table <- by(df_curvas, df_curvas$tipo_campanha, FUN = calc_correlations)

#transformando em df

cor_table <- do.call(rbind, cor_table)

#Renomeando as colunas
colnames(cor_table) <- c("Correlação_Pearson", "Correlação_Spearman")


##### Definição das curvas
# Função para ajuste linear (exemplo apenas)

## Função para ajuste linear
ajuste_linear <- function(df) {
  coeficientes <- coef(lm(Quantidade ~ Custo, data = df))
  return(coeficientes)
}

# Aplicando a função para cada tipo de campanha
ajustes_por_campanha <- by(df_curvas, df_curvas$tipo_campanha, FUN = ajuste_linear)

# Mostrando os ajustes para cada campanha
for (i in 1:length(ajustes_por_campanha)) {
  a <- ajustes_por_campanha[[i]][2]
  b <- ajustes_por_campanha[[i]][1]
  a_formatted <- format(a, scientific = TRUE)
  b_formatted <- format(b, scientific = TRUE)
  print(paste("Ajuste para", names(ajustes_por_campanha)[i], ":", paste0(a_formatted, "*x[", i, "] + ", b_formatted)))
}
######### Montagem da Tabela Para Otimização
# A tabela deve conter o investimento do mês anterior
# onde colocaremos a estrição de não poder
# ultrapassar 20% do mês passado

#Truncando a data
df_curvas$data_mes <- floor_date(df_curvas$data, unit = "month")

#Filtrando o ultimo mês fechado
df_investimento <- df_curvas %>% filter(data_mes == '2024-05-01')

### Pegando o investimento e definindo  80-120% pra cada campanha

df_investimento <- df_investimento %>%
  group_by(tipo_campanha) %>%
  summarise(Investimento = sum(Custo)) %>% mutate('Inf' = Investimento*0.8,
                                                  'Sup' = Investimento*1.2)


#Criando uma tabela com o "LTV" de cada campanha (ficticios até aqui)

# Definindo as campanhas
campanhas <- c(
  "google_(not set)",
  "google_display_e_discovery",
  "google_pmax",
  "google_search_institucional",
  "google_search_longtail",
  "google_search_outros",
  "google_search_prioritarias"
)

# Gerando valores aleatórios para LTV
set.seed(42) # Define uma semente para reproducibilidade
LTV <- runif(length(campanhas), min = 50, max = 300)

# Criando o dataframe
df <- data.frame(Campanha = campanhas, LTV = LTV)


#Criando uma função que busca o LTV da campanha na tabela acima

ltv_campanha <- function(nome_campanha) {
  valor_ltv <- df$LTV[df$Campanha == nome_campanha]
  if (length(valor_ltv) == 0) {
    return("Campanha não encontrada")
  } else {
    return(valor_ltv)
  }
}


#Definindo funções de Otimização
# 1 google_(not set)                
# 2 google_display_e_discovery        
# 3 google_pmax                     
# 4 google_search_institucional      
# 5 google_search_longtail         
# 6 google_search_outros             
# 7 google_search_prioritarias

funcao_condicao_1 <- function(x){
  return( x[1] + x[2] + x[3] + x[4] +
            x[5]+ x[6]+ x[7] - Investimento )
}

funcao_objetiva <- function(x){ return ( - (
    (-1.513305e-04*x[1] + 3.337461e+00) * ltv_campanha('google_(not set)') +
    (4.900751e-04*x[2]  + 6.293656e+00) * ltv_campanha('google_display_e_discovery') +
    (1.259749e-03*x[3]  + 1.171868e+01)  *  ltv_campanha('google_pmax') +
    (1.72813e-02*x[4]   + -2.960562e+01)  * ltv_campanha('google_search_institucional') +
    (1.20775e-03*x[5]   + 1.246943e+01)   * ltv_campanha('google_search_longtail') +
    (5.875933e-04*x[6]  + 4.65942e+00)   *  ltv_campanha('google_search_outros') +
    (-1.209453e-03*x[7] + 1.275086e+02) * ltv_campanha('google_search_prioritarias') 
))
}

# Definindo os argumentos da função de otimização
limite_superior <- df_investimento$Sup
limite_inferior <- df_investimento$`Inf`

x0 <- (limite_superior + limite_inferior)/2 # Chute Inicial

local_opts <- list( "algorithm" = "NLOPT_GN_ISRES", "xtol_rel" = 1.0e-4 )
opts <- list( "algorithm"= "NLOPT_GN_ISRES", #Escolhido de forma aleatória pelo Felipe
              "xtol_rel"= 1.0e-4,
              "maxeval"= 1660000,
              "local_opts" = local_opts,
              "print_level" = 0 )
Investimento <- 1300000
nloptr(x0,
       funcao_objetiva,
       lb = limite_inferior,
       ub = limite_superior,
       eval_g_ineq = funcao_condicao_1,
       opts = opts)