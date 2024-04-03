library("bigrquery") #pacote conexão bigquery
library(magrittr) #pacote para fazer ETL
#pacotes para fazer otimização não linear
library(nleqslv)
library(nloptr)
library(dplyr)
library(googlesheets4)

projectid = "petlove-dataeng-prod-01"

sql <- "
WITH
  primeiro_pedido AS ( -- Pega informações do primeiro pedido do cliente ( origem/midia/campanha)
  SELECT
    DISTINCT cpf,
    DATE_TRUNC(mc.data_primeiro_pedido_cpf,MONTH) safra,
    -- Para cohort
    CASE 
--GOOGLE
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND LOWER(campanha) LIKE '%instituc%' AND LOWER(campanha) NOT LIKE '%instream%' THEN 'institucional'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE '%brandformance%' OR LOWER(campanha) LIKE '%yt_assinatura%') THEN 'brandformance'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND LOWER(campanha) LIKE '%branding%' AND LOWER(campanha) NOT LIKE '%marcas_exclusivas%' THEN 'branding'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND LOWER(campanha) LIKE '%dsa%' THEN 'DSA'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 's_%' OR LOWER(campanha) LIKE '%paid-search%') AND  (LOWER(campanha) LIKE '%s_marcas_exclusivas%' OR LOWER(campanha) LIKE '%marcas-exclusivas%') THEN 'SEM_MARCAS_EXCLUSIVAS'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 's_%' OR LOWER(campanha) LIKE '%paid-search%') AND LOWER(campanha) LIKE '%med%' AND LOWER(campanha) NOT LIKE '%comed%' THEN 'SEM_MED'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 's_%' OR LOWER(campanha) LIKE '%paid-search%') AND LOWER(campanha) LIKE '%rac%' AND LOWER(campanha) NOT LIKE '%racas%' THEN 'SEM_RAC'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 's_%' OR LOWER(campanha) LIKE '%paid-search%') AND LOWER(campanha) LIKE '%gen%' THEN 'SEM_GEN'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 's_%' OR LOWER(campanha) LIKE '%paid-search%') AND (LOWER(campanha) LIKE '%bri%' OR LOWER(campanha) LIKE '%acessorios%' OR LOWER(campanha) LIKE '%trans_nova%') OR LOWER(campanha) LIKE '%ac-bri%' THEN 'SEM_AC_BRI'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 's_%' OR LOWER(campanha) LIKE '%paid-search%') AND LOWER(campanha) LIKE '%hl%' THEN 'SEM_HL'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 's_%' OR LOWER(campanha) LIKE '%paid-search%') AND LOWER(campanha) NOT LIKE 'smart_%' THEN 'SEM_outros'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND LOWER(campanha) LIKE '%rmkt%' AND (LOWER(campanha) NOT LIKE '%display%' OR LOWER(campanha) NOT LIKE '%yt%') THEN 'retargeting'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE '%display%' OR LOWER(campanha) LIKE 'gsp%') THEN 'display'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND LOWER(campanha) LIKE '%discovery%' THEN 'discovery'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE '%app%' AND LOWER(campanha) LIKE '%android%') AND LOWER(campanha) NOT LIKE '%ace%' THEN 'app_install_and'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE '%app%' AND LOWER(campanha) LIKE '%ios%') AND LOWER(campanha) NOT LIKE '%ace%' THEN 'app_install_ios'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE '%app%' AND LOWER(campanha) LIKE '%ace%') THEN 'app_engagement'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND LOWER(campanha) LIKE '%app%' AND (LOWER(campanha) NOT LIKE ('%android%') OR LOWER(campanha) NOT LIKE '%ios%') THEN 'app_install'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 'v_%' OR LOWER(campanha) LIKE 'yt_%' OR LOWER(campanha) LIKE '%bumper%' OR LOWER(campanha) LIKE '%trueview%') AND (LOWER(campanha) NOT LIKE '%branding%' OR LOWER(campanha) NOT LIKE '%assinatura%') THEN 'youtube'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND LOWER(campanha) LIKE '%rmkt%' AND (LOWER(campanha) NOT LIKE '%display%' OR LOWER(campanha) NOT LIKE '%yt%') THEN 'retargeting'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 'gs_%' OR LOWER(campanha) LIKE '%paid-shopping%')AND LOWER(campanha) NOT LIKE '%gsp%' AND LOWER(campanha) LIKE '%med%' AND LOWER(campanha) NOT LIKE '%comed%' THEN 'shopping_MED'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 'gs_%' OR LOWER(campanha) LIKE '%paid-shopping%')AND LOWER(campanha) NOT LIKE '%gsp%' AND (LOWER(campanha) LIKE '%rac_low%' OR LOWER(campanha) LIKE '%raclowltv%') THEN 'shopping_RAC_low_ltv' 
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 'gs_%' OR LOWER(campanha) LIKE '%paid-shopping%')AND LOWER(campanha) NOT LIKE '%gsp%' AND (LOWER(campanha) LIKE '%rac_high%' OR LOWER(campanha) LIKE '%rachgltv%') THEN 'shopping_RAC_high_ltv'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 'gs_%' OR LOWER(campanha) LIKE '%paid-shopping%')AND LOWER(campanha) NOT LIKE '%gsp%' AND (LOWER(campanha) LIKE '%ac%' OR LOWER(campanha) LIKE '%bri%') AND LOWER(campanha) NOT LIKE '%gsp%' THEN 'shopping_AC_BRI'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 'gs_%' OR LOWER(campanha) LIKE '%paid-shopping%')AND LOWER(campanha) NOT LIKE '%gsp%' AND LOWER(campanha) LIKE '%hl%' THEN 'shopping_HL'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 'gs_%' OR LOWER(campanha) LIKE '%paid-shopping%')AND LOWER(campanha) NOT LIKE '%gsp%' AND LOWER(campanha) LIKE '%hab%' THEN 'shopping_HAB'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 'gs_%' OR LOWER(campanha) LIKE '%paid-shopping%')AND LOWER(campanha) NOT LIKE '%gsp%' AND (LOWER(campanha) LIKE '%marcas_exclusivas%' OR LOWER(campanha) LIKE '%marcas-exclusivas%') THEN 'shopping_MARCAS_EXCLUSIVAS'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND (LOWER(campanha) LIKE 'gs_%' OR LOWER(campanha) LIKE '%paid-shopping%')AND LOWER(campanha) NOT LIKE '%gsp%' THEN 'shopping_outros'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' AND LOWER(campanha) LIKE '%marcas_exclusivas%' THEN 'marcas_exclusivas'
        WHEN LOWER(origem) IN('google', 'bing') AND LOWER(midia) LIKE '%cpc%' THEN 'google_outros'
--FACEBOOK
        WHEN LOWER(midia) LIKE '%social_ads%' AND (LOWER(campanha) LIKE '%marcas_exclusivas%' OR LOWER(campanha) LIKE '%futurepet%' OR LOWER(campanha) LIKE  '%marcas-exclusivas%') THEN 'marcas_exclusivas'
        WHEN LOWER(midia) LIKE '%social_ads%' AND LOWER(campanha) LIKE '%branding%' THEN 'branding'
        WHEN LOWER(midia) LIKE '%social_ads%' AND (LOWER(campanha) LIKE '%brandformance%' OR LOWER(campanha) LIKE '%compra_recorrente%'OR LOWER(campanha) LIKE  '%reels%') THEN 'brandformance'
        WHEN LOWER(midia) LIKE '%social_ads%' AND LOWER(campanha) LIKE '%app%' AND LOWER(campanha) LIKE '%android%' THEN 'app_install_and'
        WHEN LOWER(midia) LIKE '%social_ads%' AND LOWER(campanha) LIKE '%app%' AND LOWER(campanha) LIKE '%ios%' THEN 'app_install_ios'
        WHEN LOWER(midia) LIKE '%social_ads%' AND LOWER(campanha) LIKE '%app%' AND (LOWER(campanha) NOT LIKE '%android%' OR LOWER(campanha) NOT LIKE '%ios%') THEN 'app_install'
        WHEN LOWER(midia) LIKE '%social_ads%' AND (LOWER(campanha) LIKE '%prospeccao%' OR LOWER(campanha) LIKE'%aquisicao%') THEN 'prospeccao'
        WHEN LOWER(midia) LIKE '%social_ads%' AND (LOWER(campanha) LIKE '%retargeting_pre%' OR LOWER(campanha) LIKE'%retargeting-pre%' OR LOWER(campanha) LIKE'%retargeting_sem-dpa%' OR LOWER(campanha) LIKE '%retargeting_dpa_ate30d%') THEN 'retargeting_pre_venda'
        WHEN LOWER(midia) LIKE '%social_ads%' AND (LOWER(campanha) LIKE '%retargeting-pos%' OR LOWER(campanha) LIKE '%rmktposvenda%' OR LOWER(campanha) LIKE '%crossell%') THEN 'retargeting_pos_venda'
        WHEN LOWER(midia) LIKE '%social_ads%' AND LOWER(campanha) LIKE '%loja%' THEN 'lojas_fisicas'
        WHEN LOWER(midia) LIKE '%social_ads%'  THEN 'outros'
        ELSE 'outros'
END AS  origem_midia_campanha
  FROM
    `dw_corporativo.dts_margem_conciliacao` mc
  LEFT JOIN
    `dw_corporativo.dim_canais_marketing` dcm
  ON
    mc.chv_canais_marketing = dcm.chv_canais_marketing
  WHERE
    data_pedido = data_primeiro_pedido_cpf
    AND data_primeiro_pedido_cpf >= '2020-01-01'),
  clientes_cohort AS (
  SELECT
    mc.cpf,
    pp.safra,
    DATE_DIFF(mc.data_pedido,mc.data_primeiro_pedido_cpf,MONTH) periodo,
    origem_midia_campanha,
    COUNT(DISTINCT mc.cpf) clientes,
    MAX(COUNT(DISTINCT mc.cpf)) OVER (PARTITION BY safra, origem_midia_campanha) AS maximo_clientes_safra,
    COUNT(DISTINCT id_pedido) pedidos,
    SUM(receita_liquida_operacional) receita_liquida_total_cliente,
    COUNT(DISTINCT id_pedido) AS pedidos_cliente,
    SUM(margem_1) + sum(receita_liquida_operacional)*0.045 AS margem_1_cliente,
    SUM(margem_2) + sum(receita_liquida_operacional)*0.045 AS margem_2_cliente
  FROM
    `dw_corporativo.dts_margem_conciliacao` mc
  JOIN
    primeiro_pedido pp
  ON
    mc.cpf = pp.cpf
  GROUP BY
    1,
    2,
    3,
    4),
  features_agregadas AS ( -- agrego as features por safra,periodo e campanha
  SELECT
    cc.safra,
    periodo,
    cc.origem_midia_campanha,
    SUM(clientes) total_clientes,
    MAX(clientes_cohort) total_clientes_cohort,
    SUM(pedidos) total_pedidos,
    SUM(sum(pedidos)) OVER (PARTITION BY cc.safra,cc.origem_midia_campanha ORDER BY periodo ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) pedidos_acumulados,
    SUM(receita_liquida_total_cliente) receita_liquida_total_periodo,
    SUM(margem_1_cliente) AS margem_1_total,
    SUM(margem_2_cliente) AS margem_2_total,
    SUM(sum(receita_liquida_total_cliente)) OVER (PARTITION BY cc.safra,cc.origem_midia_campanha ORDER BY periodo ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) receita_liquida_acumulada_safra,
    SUM(sum(margem_1_cliente)) OVER (PARTITION BY cc.safra,cc.origem_midia_campanha ORDER BY periodo ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) margem_1_acumulada,
    SUM(sum(margem_2_cliente)) OVER (PARTITION BY cc.safra,cc.origem_midia_campanha ORDER BY periodo ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) margem_2_acumulada,
  FROM
    clientes_cohort cc
  LEFT JOIN (
    SELECT
      safra,
      clientes_cohort.origem_midia_campanha,
      SUM(clientes) clientes_cohort
    FROM
      clientes_cohort
    WHERE
       periodo = 0
    GROUP BY
      1,
      2) mcs
  ON
    mcs.safra = cc.safra
    AND cc.origem_midia_campanha = mcs.origem_midia_campanha
  where periodo is not null
  GROUP BY
    1,
    2,
    3
  ORDER BY
    2 ASC)
SELECT -- faz os calculos finais
  safra,
  periodo,
  origem_midia_campanha,
  total_clientes,
  total_clientes_cohort,
  total_pedidos,
  pedidos_acumulados,
  round(1.0*pedidos_acumulados/total_clientes_cohort,2) as frequencia_acumulada,
  round(receita_liquida_acumulada_safra/pedidos_acumulados,2) as ticket_medio_acumulado,
--  receita_liquida_acumulada_safra,
--  margem_1_acumulada,
--  margem_2_acumulada,
  margem_1_acumulada/total_clientes_cohort as margem_1_cliente,
  margem_2_acumulada/total_clientes_cohort as margem_2_cliente,
  avg(margem_2_acumulada/total_clientes_cohort) over (partition by origem_midia_campanha order by safra rows between 5 PRECEDING and current row) media_margem_2,
  round(margem_1_acumulada/receita_liquida_acumulada_safra*100,2) as margem_1_percentual,
  round(margem_2_acumulada/receita_liquida_acumulada_safra*100,2) as margem_2_percentual,
  round(receita_liquida_acumulada_safra/total_clientes_cohort,2) as receita_liquida_cliente
FROM
  features_agregadas
  WHERE periodo = 11
  and safra between  date_add(date_trunc(current_date(),MONTH),interval -17 MONTH) and date_add(date_trunc(current_date(),MONTH),interval -12 MONTH)
"



tb <- bq_project_query(projectid, sql)
df =bq_table_download(tb) %>% data.frame()



investimentos_calculadora <- read_sheet("https://docs.google.com/spreadsheets/d/1Nt17VXN2iC9axE7H0PLKB7e83etYOy1LXiUyPcaVSE8/edit#gid=1595172887")
#investimentos_calculadora %>% filter(Referência >= '2024-01-01') %>% View()

library(lubridate)
### (1) Pega o PCV M-1 e M
cohort_atual <- floor_date(Sys.Date()-365, "month")
cohort_anterior <- floor_date(Sys.Date()-365 -30, "month")

df %>% filter(safra >= cohort_anterior) %>% 
  group_by(safra,origem_midia_campanha) %>% summarise( media_margem_2 = sum(media_margem_2)) %>%
  ggplot(aes(x = media_margem_2, y= origem_midia_campanha,fill = as.factor(safra))) +
  geom_col(position='dodge')


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
           where month >= DATE_TRUNC(current_date() -60,MONTH)
              group by month, campaign_type, partner

  "


tb2 <- bq_project_query(projectid, sql2)
df2 =bq_table_download(tb2) %>% data.frame()



#### Abaixo, função com as curvas para plot
gerar_valor_estimado <- function(campanha, x) {
  
  # Selecionando a função da campanha
  funcao_curva <- curva_campanhas[[campanha]]
  
  # Calculando o valor de y para a curva
  y <- funcao_curva(x)
  
  # Retornando o valor de y
  return(y)
}

# Dicionário que armazena as funções de cada campanha
curva_campanhas <- list(
  "shopping_RAC_high_ltv" = function(x) { 4.62e-03 * x - 9.15e-09 * x^2 },
  "shopping_RAC_low_ltv" = function(x) { 4.83e-03 * x },
  "shopping_MED" = function(x) { 1.67e02 * log(x) - 553.661631872104 },
  "shopping_AC_BRI" = function(x) { (-4.92e-03 * x + 1.03e-07 * x^2) + 2.36e02 },
  "shopping_HL" = function(x) { 6.81e-03 * x + 59.2339302041498 * x^2 },
  "shopping_HAB" = function(x) { 3.22e-03 * x + 1.59e01 },
  "shopping_MARCAS_EXCLUSIVAS" = function(x) { (6.04e-03 * x - 7.52e-08 * x^2) },
  "SEM_RAC" = function(x) { 5.40e-03 * x - 5.04e-08 * x^2 },
  "SEM_MED" = function(x) { 2.6e-03 * x },
  "DSA" = function(x) { 4.14e-03 * x - 4.84e-08 * x^2 },
  "SEM_GEN" = function(x) { (1.61e01 + 2.14e-03 * x - 3.49e-08 * x^2) },
  "SEM_AC_BRI" = function(x) { 9.47 * log(x) - 6.42e01 },
  "SEM_HL" = function(x) { 2.29e01 * x - 1.78e02 },
  "SEM_MARCAS_EXCLUSIVAS" = function(x) { 3.47e-03 * x },
  "marcas_exclusivas" = function(x) { 0.598274991572796 * log(x) - 4.12416640388016 },
  "prospeccao" = function(x) { 0.000150641610484799 * x },
  "retargeting_pre_venda" = function(x) { 0.000129389343507315 * x }
)

df2 <- df2 %>%
  mutate(nova_coluna = case_when(
    month == '2024-01-01' ~ 'Mês Anterior',
    month == '2024-02-01' ~ 'Sugestão Calculadora',
    TRUE ~ 'Outro Valor'  # Substitua por outro valor padrão se necessário
  ))

df2 <- df2 %>% mutate(custo2=custo*7/31)
campanhas_disponiveis <- names(curva_campanhas)

df2 <- df2 %>%
  filter(campaign_type %in% campanhas_disponiveis) %>%
  mutate(NovosClientes = case_when(
    campaign_type == 'shopping_RAC_high_ltv' ~ gerar_valor_estimado('shopping_RAC_high_ltv', custo2),
    campaign_type == 'shopping_RAC_low_ltv' ~ gerar_valor_estimado('shopping_RAC_low_ltv', custo2),
    campaign_type == 'shopping_MED' ~ gerar_valor_estimado('shopping_MED', custo2),
    campaign_type == 'shopping_AC_BRI' ~ gerar_valor_estimado('shopping_AC_BRI', custo2),
    campaign_type == 'shopping_HL' ~ gerar_valor_estimado('shopping_HL', custo2),
    campaign_type == 'shopping_HAB' ~ gerar_valor_estimado('shopping_HAB', custo2),
    campaign_type == 'shopping_MARCAS_EXCLUSIVAS' ~ gerar_valor_estimado('shopping_MARCAS_EXCLUSIVAS', custo2),
    campaign_type == 'SEM_RAC' ~ gerar_valor_estimado('SEM_RAC', custo2),
    campaign_type == 'SEM_MED' ~ gerar_valor_estimado('SEM_MED', custo2),
    campaign_type == 'DSA' ~ gerar_valor_estimado('DSA', custo2),
    campaign_type == 'SEM_GEN' ~ gerar_valor_estimado('SEM_GEN', custo2),
    campaign_type == 'SEM_AC_BRI' ~ gerar_valor_estimado('SEM_AC_BRI', custo2),
    campaign_type == 'SEM_HL' ~ gerar_valor_estimado('SEM_HL', custo2),
    campaign_type == 'SEM_MARCAS_EXCLUSIVAS' ~ gerar_valor_estimado('SEM_MARCAS_EXCLUSIVAS', custo2),
    campaign_type == 'marcas_exclusivas' ~ gerar_valor_estimado('marcas_exclusivas', custo2),
    campaign_type == 'prospeccao' ~ gerar_valor_estimado('prospeccao', custo2),
    campaign_type == 'retargeting_pre_venda' ~ gerar_valor_estimado('retargeting_pre_venda', custo2),
    TRUE ~ NA_real_  # Se nenhuma condição for atendida, coloca NA
  ))

data_subset <- df2 %>% filter(campaign_type=='shopping_RAC_high_ltv',nova_coluna != 'Outro Valor')
# Criar uma sequência de valores de x para representar a curva
x_values_curve <- seq(min(data_subset$custo2), max(data_subset$custo2), length.out = 100)
y_values_curve <- gerar_valor_estimado("shopping_RAC_high_ltv", x_values_curve)

# Identificar os pontos para os meses anterior e atual
points <- data_subset %>%
  arrange(month) %>%
  group_by(month) %>%
  slice_head(n = 2)

# Criar uma sequência de valores de x dentro dos limites
limits <- points %>% ungroup() %>%
  summarise(min_custo2 = min(custo2), max_custo2 = max(custo2))

x_values_curve <- seq(limits$min_custo2 - 1000, limits$max_custo2 + 1000, length.out = 100)
y_values_curve <- gerar_valor_estimado("shopping_RAC_high_ltv", x_values_curve)

# Plotar os pontos reais
ggplot() +
  geom_point(data = points, aes(x = custo2, y = NovosClientes, color = as.factor(month)), size = 3) +
  geom_line(data = data.frame(custo2 = x_values_curve, NovosClientes = y_values_curve),
            aes(x = custo2, y = NovosClientes),
            color = "blue") +
  geom_segment(data = points,
               aes(x = lag(custo2), y = lag(NovosClientes), xend = custo2, yend = NovosClientes),
               arrow = arrow(length = unit(0.03, "npc")),
               color = "black") +
  labs(title = "Pontos e Curva para shopping_AC_BRI")

