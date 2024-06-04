library("bigrquery") #pacote conexão bigquery
library(magrittr) #pacote para fazer ETL
#pacotes para fazer otimização não linear
library(nleqslv)
library(nloptr)
library(dplyr)

projectid = "petlove-dataeng-prod-01"

bq_auth(use_oob = TRUE, cache = FALSE)

custo_rest_min_aux = 0.8 #20% a menos nao pode ultrapassar isso para reduzir mutio uma campanha de um mes para outro
custo_rest_max_aux = 1.2 #20% a mais nao pode22 ultrapassar isso para aumentar mutio uma campanha de um mes para outro
custo_tot= 3382494 # investimento de face + google  sem app do ultimo mes
month_rest = "2024-04-01" #ultimo mes (se final mes para proximo, colocar mes corrente)

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
sql=gsub("custo_rest_min_aux",custo_rest_min_aux,sql)
sql=gsub("custo_rest_max_aux",custo_rest_max_aux,sql)
sql=gsub("month_rest",sQuote(month_rest),sql)
sql=gsub("custo_tot",custo_tot,sql)


tb <- bq_project_query(projectid, sql)
df =bq_table_download(tb) %>% data.frame()

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
#definições

orçamento_campanhas = custo_tot

tot_val = (   orçamento_campanhas)/31*7 # olar linha 4
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
           ((3.45e-03*x[1] -4.08e-09*x[1]**2 +(0))*valor_pcv('shopping_RAC_high_ltv')*(1+(-0.0088))*(1+(0))+
           (4.38e-03*x[2]+(0))*valor_pcv('shopping_RAC_low_ltv')*(1+(-0.0693))*(1+(0))+
           (5.49e-03*x[3])+(0))*valor_pcv('shopping_MED')*(1+(-0.0462))*(1+(0))+
           ((-5.85e-04*(x[4])+ (4.24e-08*x[4]**2)+(1.48e+02))*valor_pcv('shopping_AC_BRI')*(1+(-0.0418))*(1+(0))+
           (6.21e-03*x[5] -3.61e-08*x[5]**2 +(0))*valor_pcv('shopping_HL')*(1+(-0.055))*(1+(0))+
           (3.97e-03*x[6] +(2.97e+01))*valor_pcv('shopping_HAB')*(1+(-0.0638))*(1+(0))+
           (0  +(1.04e-02)*x[7]+(-1.16e-07)*x[7]**2)*valor_pcv('shopping_MARCAS_EXCLUSIVAS')*(1+(-0.0935))*(1+(0))+
           (5.67e-03*x[8]+ -2.89e-08*x[8]**2 +(0))*valor_pcv('SEM_RAC')*(1+(0.0693))*(1+(0))+
           (1.92e-03*x[9] +2.16e-08*x[9]**2+(0))*valor_pcv('SEM_MED')*(1+(-0.0027))*(1+(0))+
           (4.13e-03*x[10]	-1.85e-08*x[10]**2 +(0))*valor_pcv('DSA')*(1+(-0.0063))*(1+(0))+
           (3.85e+01+(-7.95e-04)*x[11]+(1.52e-07)*x[11]**2)*valor_pcv('SEM_GEN')*(1+(0.0018))*(1+(0))+
           (9.47*log(x[12])+(-6.42e+01))*valor_pcv('SEM_AC_BRI')*(1+(0.0063))*(1+(0))+
           (3.16e-03*x[13]+(1.90e+00))*valor_pcv('SEM_HL')*(1+(0.0252))*(1+(0))+
           (5.04e-04*x[14]+(3.25e+01))*valor_pcv('SEM_MARCAS_EXCLUSIVAS')*(1+(0.0693))*(1+(0))+
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
ub=ub/31*7 # /37*7 ajuste de mensal para semanal
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
df_alocacao <- data.frame(Campanha = df$campanha, Alocacao = results)
gg_bar <- ggplot(df_alocacao, aes(x = Campanha, y = Alocacao, fill = Campanha)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Gráfico de Barras - Alocação por Campanha", x = "Campanha", y = "Alocação") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Tornar o gráfico interativo com plotly
plotly_bar <- ggplotly(gg_bar, tooltip = c("x", "y", "fill"))
print(plotly_bar)

# Imprimir o gráfico interativo
print(plotly_bar)
print(res)
print(sum(res$solution/7*31))
print(res$solution/7*31, dec='.',sep='\t', file='clipboard',row.names = FALSE)
print(res$solution/7*31, dec='.',sep='\t', file='clipboard',row.names = FALSE)

data.frame(df %>% head(19) %>% data.frame() ,
           prop_abril =df$custo/sum(df$custo),sugestao_calc=res$solution/7*31, prop_calc = res$solution/7*31/sum(res$solution/7*31),
           var_prop = ((res$solution/7*31/sum(res$solution/7*31))/ (df$custo/sum(df$custo)) -1)*100  ) %>% View()

-eval_f(res$solution/7*31)


df2=df %>% mutate(custo=replace(custo, is.na(custo), 0.0001))

-eval_f(custo_tot * df2$custo/sum(df2$custo)) #prop_abril  df$custo/sum(df$custo)

#ganho
-eval_f(res$solution/7*31)+  eval_f(custo_tot * df2$custo/sum(df2$custo))

