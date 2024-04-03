library("bigrquery") #pacote conexão bigquery
library(magrittr) #pacote para fazer ETL
#pacotes para fazer otimização não linear
library(nleqslv)
library(nloptr)
library(dplyr)
library(googlesheets4)

## Parte 2 --> Pegar investimento por campanha + curvas

sql2 <- "      --agrupando dados da seed facebook custo por mês  e acertando os tipos
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
      where refdate =  (select max(refdate)-3 from `petlove-dataeng-prod-01.ssot_seeds.seed_custo_marketing_petlove_facebook` )
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
           --where month >= DATE_TRUNC(current_date() -60,MONTH)
              group by month, campaign_type, partner

  "

library("bigrquery") #pacote conexão bigquery
library(magrittr) #pacote para fazer ETL
#pacotes para fazer otimização não linear
library(nleqslv)
library(nloptr)
library(dplyr)
library(googlesheets4)
library(ggplot2)

projectid = "petlove-dataeng-prod-01"
tb2 <- bq_project_query(projectid, sql2)
df2 =bq_table_download(tb2) %>% data.frame()
df2 <- df2 %>% mutate(campanha = case_when(campaign_type %in% c('shopping_MARCAS_EXCLUSIVAS','marcas_exclusivas','SEM_MARCAS_EXCLUSIVAS') ~'marcas_exclusivas', TRUE ~ campaign_type))

result <- df2 %>%
  group_by(month, campanha) %>%
  summarise(custo = sum(custo))
library(plotly)
result <- result %>% group_by(month) %>% mutate(custop = custo/sum(custo))

result <- result %>% ungroup() %>% group_by(month) %>% mutate(custot = sum(custo))

lista_campanhas <- c('shopping_MARCAS_EXCLUSIVAS',
                     'SEM_MARCAS_EXCLUSIVAS',
                     'marcas_exclusivas','shopping_RAC_low_ltv','shopping_RAC_high_ltv')

#result <- result %>% mutate(campanha = case_when(campaign_type %in% c('shopping_MARCAS_EXCLUSIVAS','marcas_exclusivas','SEM_MARCAS_EXCLUSIVAS') ~'marcas_exclusivas', TRUE ~ campaign_type))
ggplotly(result %>% filter(campanha %in% lista_campanhas,month  != '2024-03-01') %>%
  ggplot(aes(x=month,y=custop*100,colour = campanha)) + labs(x = '', y = 'Percentual', title = 'Percentual do Custo em relação ao Budget Total') +
    geom_line())

result %>% group_by(month) %>% summarise(custot=sum(custo)) %>% filter(month != '2024-03-01') %>%
  ggplot(aes(x = month, y= custot))  + geom_line() + labs( y = 'Budget em R$', x = '', title = 'Evolução Temporal do Budget')





# Substitua 'result' pelo nome real do seu dataframe

# Criar um gráfico com linhas coloridas
plot1 <- result %>%
  filter(campanha %in% lista_campanhas, month != '2024-03-01') %>%
  ggplot(aes(x = month, y = custop * 100, colour = campanha)) +
  geom_line() +
  labs(x = '', y = 'Percentual', title = 'Percentual do Custo em relação ao Budget Total')

# Criar um gráfico de linhas para o Budget
plot2 <- result %>%
  group_by(month) %>%
  summarise(custot = sum(custo)) %>%
  filter(month != '2024-03-01') %>%
  ggplot(aes(x = month, y = custot)) +
  geom_line() +
  labs(y = 'Budget em R$', x = '', title = 'Evolução Temporal do Budget')

# Juntar os dois gráficos usando subplot
subplot(plot1, plot2, nrows = 2, titleX = FALSE, titleY = FALSE, shareX = TRUE, shareY = FALSE)

            