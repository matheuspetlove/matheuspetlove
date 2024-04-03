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

projectid = "petlove-dataeng-prod-01"

petlove_colors <- c('#B788D1','#A1D188','#D19288')
# campanhas <- read_csv("campanhas.csv", 
#                       col_types = cols(start_week = col_character()))

sql_campanhas <- "--falta trocar tabela face pra correta 
with custo_face_google as (with 
face_custo as(SELECT 
--campaign_id,
      campaign_name as campanha,
    DATE_TRUNC(cast(date as date), WEEK(MONDAY)) AS start_week,
      'face_insta_pago' as partner,
  sum(cast(replace(cast(cost as string), ',', '.') as float64)) as custo, 
  sum(cast(link_clicks as int64)) as clicks, 
  sum(cast(impressions as int64)) as impressions,
    date AS date
    FROM `petlove-dataeng-prod-01.ssot_seeds.seed_custo_marketing_petlove_facebook`
    where refdate = (  select max(refdate)-3 from  `petlove-dataeng-prod-01.ssot_seeds.seed_custo_marketing_petlove_google`)
    group by date, start_week, campanha, date
)
,
google_custo as(--    campaign_id ,
      SELECT campaign_name as campanha,
    DATE_TRUNC(cast(date as date),  WEEK(MONDAY)) AS start_week,
      'google_pago' as partner,
  sum(cast(replace(cast(cost as string), ',', '.') as float64)) as custo, 
  sum(cast(clicks as int64)) as clicks, 
  sum(cast(impressions as int64)) as impressions,
    date AS date
    FROM `petlove-dataeng-prod-01.ssot_seeds.seed_custo_marketing_petlove_google`
      where refdate = (  select max(refdate) from  `petlove-dataeng-prod-01.ssot_seeds.seed_custo_marketing_petlove_google`)
    group by date, start_week, campanha, date)
  
select    
           fc.start_week,
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
        WHEN (lower(fc.partner)='google_pago' or lower(fc.partner)='bing_pago')AND (LOWER(fc.campanha) LIKE 's_%' OR LOWER(fc.campanha) LIKE '%paid-search%') AND LOWER(fc.campanha) NOT LIKE 'smart_%' THEN 'SEM_outros'
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
        WHEN lower(fc.partner) = 'face_insta_pago' AND (LOWER(fc.campanha) LIKE '%prospeccao%' OR LOWER(fc.campanha) LIKE'%aquisicao%') THEN 'prospeccao'
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
            group by start_week, campaign_type, partner


--cruzar query bianchi


)
,
conv_ads as (SELECT 
--dmc.data_pedido AS date
DATE_TRUNC(data_pedido, WEEK(MONDAY)) AS start_week
--,dmc.canal 
--,dmc.subcanal

,CASE 
  WHEN LOWER(dcm.canal_marketing) IN ('shopping', 'search') THEN 'google_pago'
  WHEN LOWER(dcm.canal_marketing) IN ('social_ads') THEN 'face_insta_pago'
END AS partner
,dcm.campaign_type
--,dcm.parceiro
,COUNT(DISTINCT CASE WHEN dmc.data_pedido = dmc.data_primeiro_pedido_cpf THEN cpf ELSE NULL END) AS novos_clientes
,COUNT(DISTINCT dmc.id_pedido) AS quant_pedidos 
,SUM(dmc.receita_bruta_total) AS receita_bruta_total
,SUM(dmc.receita_liquida_operacional) AS receita_liquida
,SUM(dmc.custo_produto) AS custo_produto
,SUM(dmc.margem_1) AS margem_bruta
FROM dw_corporativo.dts_margem_conciliacao dmc
--LEFT JOIN dw_corporativo.ft_pedido_faturado fpf 
  --ON dmc.id_pedido = fpf.pedido_beagle
LEFT JOIN (SELECT *
,CASE 
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
END AS campaign_type
    FROM dw_corporativo.dim_canais_marketing) dcm      
      ON dmc.chv_canais_marketing = dcm.chv_canais_marketing
WHERE 1=1
AND LOWER(canal_marketing) IN ('search', 'shopping', 'social_ads')
AND data_pedido >= '2022-01-01'
GROUP BY 1,2,3
ORDER BY start_week ASC)

select * from   (select * From conv_ads where partner ='google_pago' or partner ='face_insta_pago' ) ca
full outer join 
(select * From custo_face_google
where (partner ='google_pago' or partner ='face_insta_pago') and start_week >='2022-01-01') cf 
on cf.start_week =  ca.start_week and
cf.campaign_type = ca.campaign_type and cf.partner = ca.partner
where ca.start_week is not null and custo is not null
order by ca.start_week


"

tbcampanhas <- bq_project_query(projectid, sql_campanhas)
campanhas = bq_table_download(tbcampanhas) %>% data.frame()
campanhas$start_week <- as.Date(campanhas$start_week)


df <- read_csv("dados_face_google_atualizado.csv",
               col_types = cols(anomes = col_date(format = "%m/%d/%Y")))


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
              selectInput("campaign", "Selecione a campanha:",
                          choices = c(
                            'shopping_RAC_high_ltv',
                            'shopping_RAC_low_ltv',
                            'shopping_MED',
                            'shopping_AC_BRI',
                            'shopping_HL',
                            'shopping_HAB',
                            'shopping_MARCAS_EXCLUSIVAS',
                            'SEM_RAC',
                            'SEM_MED',
                            'DSA',
                            'SEM_GEN',
                            'SEM_AC_BRI',
                            'SEM_HL',
                            'SEM_MARCAS_EXCLUSIVAS',
                            'marcas_exclusivas',
                            'prospeccao',
                            'retargeting_pre_venda'
                          )),
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
              DTOutput("coeficientes_table"),
              textOutput("formula_coeficientes")
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
              textInput("month_rest", "Mês de referência", value = '2023-10-01'),
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

  output$formula_coeficientes <- renderText({ #######################################
    
    campanha_selecionada <- campanhas[campanhas$campaign_type == input$campaign, ]
    tipo_ajuste <- input$ajuste
    force_intercept <- input$force_intercept
    
    result <- calculate_adjustment(campanha_selecionada, tipo_ajuste, force_intercept)
    params <- result$params
    saved_coefficients <- data.frame(Campanha = input$campaign,
                                     Ajuste = tipo_ajuste,
                                     Coeficientes = params,
                                     stringsAsFactors = FALSE)

    
    
    df_indices_campanhas <- data.frame(campanha = c('shopping_RAC_high_ltv', 'shopping_RAC_low_ltv', 'shopping_MED', 
                                                    'shopping_AC_BRI', 'shopping_HL', 'shopping_HAB', 
                                                    'shopping_MARCAS_EXCLUSIVAS', 'SEM_RAC', 'SEM_MED', 'DSA', 
                                                    'SEM_GEN', 'SEM_AC_BRI', 'SEM_HL', 'SEM_MARCAS_EXCLUSIVAS', 
                                                    'marcas_exclusivas', 'prospeccao', 'retargeting_pre_venda'), 
                                       indice = c('x[1]', 'x[2]', 'x[3]', 'x[4]', 'x[5]', 'x[6]', 'x[7]', 'x[8]', 'x[9]',
                                                  'x[10]', 'x[11]', 'x[12]', 'x[13]', 'x[14]', 'x[15]', 'x[16]', 'x[17]'))
    indice_x <- df_indices_campanhas$indice[df_indices_campanhas$campanha == input$campaign]
    coeficientes <- saved_coefficients$Coeficientes
    
    
    if (tipo_ajuste == "Polinomial") {
      coef_intercept <- coeficientes[1]  # Índice 1 corresponde ao coeficiente do Intercept
      coef_custos <- coeficientes[2]  # Índice 2 corresponde ao coeficiente do termo linear
      coef_custos_quadrado <- coeficientes[3]  
      if (force_intercept == TRUE) {
        coef_custos <- coeficientes[1]  # Índice 2 corresponde ao coeficiente do termo linear
        coef_custos_quadrado <- coeficientes[2] 
        print(paste("0 + (",coef_custos, ") *",indice_x, "+ (", coef_custos_quadrado, ")*",indice_x,"**2"))
      } else {
        print(paste("(",coef_intercept, ") + (", coef_custos, "*",indice_x, "+ (", coef_custos_quadrado, ") *",indice_x,"**2"))
      }
    } else if (tipo_ajuste == "Logarítmico") {
      coef_intercept <- coeficientes[1]  # Índice 1 corresponde ao coeficiente do Intercept
      coef_log_custo <- coeficientes[2]  # Índice 2 corresponde ao coeficiente do termo logarítmico
      if (force_intercept == TRUE) {
        coef_log_custo <- coeficientes[1] 
        print(paste("0 + log(", indice_x, ")* (",coef_log_custo,")"))
      } else {
        print(paste(coef_intercept, "+ log(", indice_x, ")*",coef_log_custo))
      }
    } else if (tipo_ajuste == "Linear") {
      coef_intercept <- coeficientes[1]  
      coef_custo <- coeficientes[2]  
      if (force_intercept == TRUE) {
        coef_custo <- coeficientes[1] 
        print(paste("0 + (", coef_custo,")*",indice_x))
      } else {
        print(paste("(",coef_intercept, ") + (", coef_custo,") *",indice_x))
      }
    } else {
      print("Tipo de ajuste não reconhecido.")
    }
    
  })
  #Adicionando codigo da calculadora -------------------------
  projectid = "petlove-dataeng-prod-01"
  
  bq_auth(use_oob = TRUE, cache = FALSE)
  
  # custo_rest_min_aux = 0.8 #20% a menos nao pode ultrapassar isso para reduzir mutio uma campanha de um mes para outro
  # custo_rest_max_aux = 1.2 #20% a mais nao pode22 ultrapassar isso para aumentar mutio uma campanha de um mes para outro
  # month_rest = "2023-09-01" #ultimo mes (se final mes para proximo, colocar mes corrente)
  
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
select ordem, cm.campaign_type as campanha, month ,cm.partner,
custo,(custo  / SUM(custo) OVER ()) AS custo_perc,custo_tot*(custo/SUM(custo) OVER ())
as custo_perc_new ,custo_tot*rest_min*(custo  / SUM(custo) OVER ()) as custo_rest_min ,
custo_tot*rest_max*(custo / SUM(custo) OVER ()) as custo_rest_max,clicks,
impressions from campanhas_modelo cm  left join (select * from custo_face_google   where month =  month_rest ) cfg 
on cfg.campaign_type=cm.campaign_type 
order by ordem
) 
select campanha,custo_rest_min,custo_rest_max, custo, custo_perc_new from tabela_aux
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
    
    
#De para das Campanhas 
    # 1 'shopping_RAC_high_ltv'
    # 2 'shopping_RAC_low_ltv'
    # 3 'shopping_MED'
    # 4 'shopping_AC_BRI'
    # 5 'shopping_HL'
    # 6 'shopping_HAB'
    # 7 'shopping_MARCAS_EXCLUSIVAS'
    # 8 'SEM_RAC'
    # 9 'SEM_MED'
    # 10 'DSA'
    # 11 'SEM_GEN'
    # 12 'SEM_AC_BRI'
    # 13 'SEM_HL'
    # 14 'SEM_MARCAS_EXCLUSIVAS' 
    # 15 'marcas_exclusivas' 
    # 16 'prospeccao' 
    # 17 'retargeting_pre_venda'  
    
    eval_g_eq <- function(x)
    {
      return ( x[1] + x[2] + x[3] + x[4] +
                 x[5]+ x[6]+ x[7]+ x[8]+ x[9]+
                 x[10]+ x[11]+ x[12]+ x[13]+
                 x[14]+ x[15]+ x[16]+ x[17] - tot_val)
    }
    
    
    
    #### Vamos pegar os pcvs
    sql_pcv <- "WITH
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
    2 ASC), main as ( 
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
  avg(margem_2_acumulada/total_clientes_cohort) over (partition by origem_midia_campanha order by safra rows between 3 PRECEDING and current row) media_margem_2,
  round(margem_1_acumulada/receita_liquida_acumulada_safra*100,2) as margem_1_percentual,
  round(margem_2_acumulada/receita_liquida_acumulada_safra*100,2) as margem_2_percentual,
  round(receita_liquida_acumulada_safra/total_clientes_cohort,2) as receita_liquida_cliente
FROM
  features_agregadas
  WHERE periodo = 11
  and safra between  date_add(date_trunc(current_date(),MONTH),interval -17 MONTH) and date_add(date_trunc(current_date(),MONTH),interval -12 MONTH)) -- pega as ultimas 5 safras completas
select  origem_midia_campanha,
 avg(margem_1_cliente) as margem_1_cliente,
 avg(margem_2_cliente) as margem_2_cliente
from main
group by 1
"
    tbpcv <- bq_project_query(projectid, sql_pcv)
    pcv = bq_table_download(tbpcv) %>% data.frame()
    
  
    valor_pcv <- function(campanha){
      pcv$margem_2_cliente[pcv$`origem_midia_campanha` == campanha]
    }
    eval_f <- function(x)
      #otimizando receita total = equacao  y invest = custo xaxsax.. minimizando - => maximizando
    { #x[i] --> investimento na campanha i
      # a*x[i] + b ou a*log(x[i]) +c ou a*x[i]^n + b , os coefs sai do ajuste
      # https://docs.google.com/spreadsheets/d/1004GNz-Bc3LP12S3SrBfDJXrahLXOd9UaW5U8ymRvEY/edit#gid=803232819
      return ( -(
   
        #(novos clientes)* LTV médio * Fator de Atribuição * Fator usado no fim do ano
        ((4.62e-03*x[1]  -9.15e-09*x[1]**2 +(0))*valor_pcv('shopping_RAC_high_ltv')*(1+(-0.0088))*(1+(0))+
           (4.83e-03*x[2]+(0))*valor_pcv('shopping_RAC_low_ltv')*(1+(-0.0693))*(1+(0))+
           (1.67e+02*log(x[3]))+(-553.661631872104))*valor_pcv('shopping_MED')*(1+(-0.0462))*(1+(0))+
           ((-4.92e-03*(x[4])+ (1.03e-07*x[4]**2)+(2.36e+02))*valor_pcv('shopping_AC_BRI')*(1+(-0.0418))*(1+(0))+
           (6.81e-03*x[5] -3.11e-08*x[5]**2 +(0))*valor_pcv('shopping_HL')*(1+(-0.055))*(1+(0))+
           (3.22e-03*x[6] +(1.59e+01))*valor_pcv('shopping_HAB')*(1+(-0.0638))*(1+(0))+
           (0  +(6.04e-03)*x[7]+(-7.52e-08)*x[7]**2)*valor_pcv('shopping_MARCAS_EXCLUSIVAS')*(1+(-0.0935))*(1+(0))+
           (5.40e-03*x[8]+ -5.04e-08*x[8]**2 +(0))*valor_pcv('SEM_RAC')*(1+(0.0693))*(1+(0))+
           (2.6e-03*x[9]+(0))*valor_pcv('SEM_MED')*(1+(-0.0027))*(1+(0))+
           (4.14e-03*x[10]	- 4.84e-08*x[10]**2 +(0))*valor_pcv('DSA')*(1+(-0.0063))*(1+(0))+
           (1.61e+01+(2.14e-03)*x[11]+(-3.49e-08)*x[11]**2)*valor_pcv('SEM_GEN')*(1+(0.0018))*(1+(0))+
           (9.47*log(x[12])+(-6.42e+01))*valor_pcv('SEM_AC_BRI')*(1+(0.0063))*(1+(0))+
           (2.29e+01*x[13]+(-1.78e+02))*valor_pcv('SEM_HL')*(1+(0.0252))*(1+(0))+
           (3.47e-03*x[14]+(0))*valor_pcv('SEM_MARCAS_EXCLUSIVAS')*(1+(0.0693))*(1+(0))+
           (0.598274991572796*log(x[15])+(-4.12416640388016))*valor_pcv('marcas_exclusivas')*(1+(8.17))*(1+(0))+
           (0.000150641610484799*x[16]+(0))*valor_pcv('prospeccao')*(1+(18.06))*(1+(0))+
           (0.000129389343507315*x[17]+(0))*valor_pcv('retargeting_pre_venda')*(1+(18.791))*(1+(0)))
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
                              prop_mes_anterior =df$custo/sum(df$custo),
                              sugestao_calc=res$solution/7*31,
                              prop_calc = res$solution/7*31/sum(res$solution/7*31),
                              #var_prop = ((res$solution/7*31/sum(res$solution/7*31))/ (df$custo/sum(df$custo)) -1)*100,
                              var_custo = ((res$solution/7*31))/ (df$custo_perc_new) -1 )
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
