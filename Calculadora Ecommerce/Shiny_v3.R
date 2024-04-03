#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(bigrquery) #pacote conexão bigquery
library(magrittr) #pacote para fazer ETL
library(nleqslv)
library(nloptr)
library(dplyr)
library(plotly)
library(reshape2)


ui <- fluidPage(
  titlePanel("Central limit theorem"),
  sidebarLayout(
    sidebarPanel(
      numericInput("custo_tot", "Valor total do Investimento:", value = 3243452, min = 0),
      actionButton("submit", "Otimizar")
    ),
    mainPanel(
      plotlyOutput("grafico_investimento_atual"),
      tableOutput("table")
    )
  )
)
server <- function(input, output, session) {
  projectid = "petlove-dataeng-prod-01"
  
  bq_auth(use_oob = TRUE, cache = FALSE)
  
  custo_rest_min_aux = 0.8 #20% a menos nao pode ultrapassar isso para reduzir mutio uma campanha de um mes para outro
  custo_rest_max_aux = 1.2 #20% a mais nao pode22 ultrapassar isso para aumentar mutio uma campanha de um mes para outro
  month_rest = "2023-07-01" #ultimo mes (se final mes para proximo, colocar mes corrente)
  
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
  sql = gsub("custo_rest_min_aux", custo_rest_min_aux, sql)
  sql = gsub("custo_rest_max_aux", custo_rest_max_aux, sql)
  sql = gsub("month_rest", sQuote(month_rest), sql)
  df_alocacao <- reactive({ 
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
                              prop_abril =df$custo/sum(df$custo),sugestao_calc=res$solution/7*31, prop_calc = res$solution/7*31/sum(res$solution/7*31),
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
}

# Run the application 
shinyApp(ui = ui, server = server)