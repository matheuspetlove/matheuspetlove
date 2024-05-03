library(dplyr)
library(bigrquery)
library(ggplot2)
library(tidyr)
library(shiny)
library(plotly)
projectid = "petlove-dataeng-prod-01"


query <- "DECLARE min_data DATE DEFAULT '2023-06-01'; 
DECLARE max_data DATE DEFAULT '2023-06-30'; 

WITH bonificacoes AS (
  SELECT
    DATE(ano_mes)                                     AS mes
    , CASE 
      WHEN fornecedor IN (
        'ZOETIS INDUSTRIA DE PRODUTOS VETERINARIO', 
        'ZOETIS COMERCIO E DISTRIBUICAO LTDA', 
        'ZOETIS IND PROD VETERINARIOS LTDA', 
        'ZOETIS IND PROD VETERINARIOS LTDA') 
      THEN 'ZOETIS COMERCIO E DISTRIBUICAO LTDA'
      WHEN fornecedor IN (
        'CEVA SAUDE ANIMAL LTDA.', 
        'CEVA SAÚDE ANIMAL LTDA', 
        'CEVA VETERINÁRIA S/A') 
      THEN 'CEVA SAUDE ANIMAL LTDA.' 
      WHEN fornecedor IN (
        'NEOVIA NUTRIÇÃO E SAÚDE ANIMAL LTDA', 
        'NEOVIA NUTRIÇÃO E SAÚDE ANIMAL LTDA.') 
      THEN 'NEOVIA NUTRIÇÃO E SAÚDE ANIMAL LTDA'
      WHEN fornecedor IN (
        'BENTONISA – BENTONITA DO NORDESTE S', 
        'BENTONIT UNIÃO NORDESTE INDÚSTRIA E') 
      THEN 'BENTONIT UNIÃO NORDESTE INDÚSTRIA E'
      WHEN fornecedor IN (
        'GRANDFOOD INDÚSTRIA E COMERCIO LTDA',
        'GRANDFOOD INDUSTRIA E COMERCIO LTDA') 
      THEN 'GRANDFOOD INDÚSTRIA E COMERCIO LTDA'
      WHEN fornecedor IN (
        'VECTOR CHEMICAL IND PRODUCTS LTDA', 
        'DNAMAZON ANIMALS INDUSTRIA, COMERCIO, IM', 
        'DNAMAZON ANIMALS INDUSTRIA, COMERCI') 
      THEN 'DNAMAZON ANIMALS INDUSTRIA, COMERCIO, IM'
      WHEN fornecedor IN (
        'RECRIAR SOLUÇÕES SUSTENTÁVEIS LTDA', 
        'ISALU SOLUÇÕES SUSTENTÁVEIS LTDA')
      THEN 'ISALU SOLUÇÕES SUSTENTÁVEIS LTDA'
      WHEN fornecedor IN (
        'F.G. INDUSTRIA DE ACESSORIOS PETS L')
      THEN 'F.G. INDUSTRIA DE ACESSORIOS PETS LTDA'
      WHEN fornecedor IN (
        'NESTLE BRASIL LTDA', 
        'Nestle Brasil Ltda')
      THEN 'NESTLE BRASIL LTDA.'
      WHEN fornecedor IN (
        'NUTRISANTOS ALIMENTAÇÃO ANIMAL', 
        'Nutrisantos Alimentação Animal')
      THEN 'NUTRISANTOS ALIMENTACAO ANIMAL LTDA'
      WHEN fornecedor IN (
        'OURO FINO AGRONEGOCIO LTDA')
      THEN 'OURO FINO AGRONEGOCIO LTDA.'
      WHEN fornecedor IN (
        'PETIX IND.E COM. IMP E EXP LTDA')
      THEN 'PETIX IND., COM., IMP. E EXP. DE PROD. G'
      WHEN fornecedor IN (
        'F. H. M. ADAMI-ME')
      THEN 'F. H. M. ADAMI'
      WHEN fornecedor IN (
        'JORGE LUIS DA SILVA LEME')
      THEN 'JORGE LUIS DA SILVA LEME 32077870885'
      WHEN fornecedor IN (
        'LEONARDO NERI COMERCIAL TÊXTIL EIRE')
      THEN 'LEONARDO NERI COMERCIAL TEXTIL LTDA'
      WHEN fornecedor IN (
        'WILLIAN EDUARDO LARGUEZA JUNIOR S.A')
      THEN '31.570.994 WILLIAN EDUARDO LARGUEZA JUNI'
      WHEN fornecedor IN (
        'COMPANHIA FRANCESA DE DISTRIBUIÇAO  DE P')
      THEN 'COMPANHIA FRANCESA DE DISTRIBUIÇAO  DE P'
      WHEN fornecedor IN (
        'GENERAL TREATS INDUSTRIA E COMERCIO')
      THEN 'GENERAL TREATS INDUSTRIA E COMERCIO LTDA'
      WHEN fornecedor IN (
        'J N DA SILVEIRA NUTRICAO E SUPLEMENTACAO ANIMAL')
      THEN 'JN DA SILVEIRA NUTRIÇÃO E SUPLEMENT'
      WHEN fornecedor IN (
        'NUTROPICA NUTRICAO ESPECIALIZADA LTDA')
      THEN 'NUTRÓPICA NUTRIÇÃO ESPECIALIZADA LT'
      WHEN fornecedor IN (
        'Petitos.Ind.Com.de.Alim.Animais.Ltda-EPP')
      THEN 'PETITOS.IND.COM.DE.ALIM.ANIMAIS.LTD'
      WHEN fornecedor IN (
        'RACOES REIS DISTRIBUIDORA DE ARTIGO')
      THEN 'RACOES REIS DISTRIBUIDORA DE ARTIGOS PET'
      WHEN fornecedor IN (
        'VITAPET FAB.PROD.ALIMENTÍCIOSANIMAI', 
        'Vitapet Fab.Prod.Alimentíciosanimai')
      THEN 'VITA PET FABRICACAO DE PRODUTOS ALIMENTI'
      WHEN fornecedor IN (
        'VETOQUINOL SAUDE ANIMAL')
      THEN 'VETOQUINOL SAUDE ANIMAL LTDA.'
      WHEN fornecedor IN (
        'UNIÃO QUÍMICA FARMACÊUTICA NACIONAL S A')
      THEN 'UNIAO QUIMICA FARMACEUTICA NACIONAL S A'
      WHEN fornecedor IN (
        'PET MED INDÚSTRIA E COMÉRCIO LTDA')
      THEN 'PET MED INDÚSTRIA E COMÉRCIO LTDA'
      WHEN fornecedor IN (
        'CHEMITEC AGRO-VETERINÁRIA LTDA - FI')
      THEN 'CHEMITEC AGRO-VETERINARIA LTDA'
      WHEN fornecedor IN (
        'CHAMPION FARMOQUÍMICO LTDA')
      THEN 'CHAMPION FARMOQUÍMICO LTDA'
      WHEN fornecedor IN (
        'Alivet Saúde Animal LTDA')
      THEN 'ALIVET  SAÚDE ANIMAL LTDA'
      WHEN fornecedor IN (
        'VETYS DO BRASIL PROD VETERINÁRIOS L')
      THEN 'VETYS DO BRASIL PROD VETERINÁRIOS L'
      WHEN fornecedor IN (
        'SOLUTION DISTRIBUIDORA DE PRODUTOS')
      THEN 'SOLUTION DISTRIBUIDORA DE PRODUTOS VETER'
      WHEN fornecedor IN (
        'Ecoenergia Industria E Comercio Ltd', 
        'ECOENERGIA INDUSTRIA E COMERCIO LTD')
      THEN 'ECOENERGIA INDUSTRIA E COMERCIO LTDA'
      WHEN fornecedor IN (
        'ALLERLESS ATAC E VAR DE PROD P ANIM')
      THEN 'ALLERLESS ATAC E VAR DE PROD P ANIM'
      WHEN fornecedor IN (
        'ALESS KANAAN QUÍMICA E INDÚSTRIA E COMÉR')
      THEN 'ALLES PET LTDA'
      WHEN fornecedor IN (
        'ZOE PETFOOD COMERCIO DE ALIMENTOS PARA')
      THEN 'ZOE PETFOOD COMERCIO DE ALIMENTOS PARA A'
      WHEN fornecedor IN (
        'Upper Dog Comercial - LTDA')
      THEN 'UPPER DOG COMERCIAL LTDA'
      WHEN fornecedor IN (
        'Selecta Pet Care Indústria Comércio Ltda')
      THEN 'SELECTA PET CARE INDÚSTRIA COMÉRCIO'    
      WHEN fornecedor IN (
        'JORGE LUIS DA SILVA LEME')
      THEN 'JORGE LUIS DA SILVA LEME 32077870885' 
      WHEN fornecedor IN (
        'LEONARDO NERI COMERCIAL TÊXTIL EIRE')
      THEN 'LEONARDO NERI COMERCIAL TEXTIL LTDA'   
      WHEN fornecedor IN (
        'GENERAL TREATS INDUSTRIA E COMERCIO')
      THEN 'GENERAL TREATS INDUSTRIA E COMERCIO LTDA'  
      WHEN fornecedor IN (
        'SUL MINEIRA ALIMENTOS LTDA - VARGIN')
      THEN 'NEOVIA NUTRIÇÃO E SAÚDE ANIMAL LTDA'                                                     
      ELSE TRIM(UPPER(fornecedor))    
    END                                                       AS fornecedor
    , setor
    , SUM(net)                                                AS net_fornecedor
  FROM `petlove-dataeng-prod-01.op_bonificacao.dts_bonificacao_financeiro`
  WHERE 1=1
  GROUP BY 1, 2, 3
)
, performance_fornecedor_setor AS (
  SELECT
    DATE_TRUNC(data_emissao_nota, MONTH)            AS mes
    , CASE 
      WHEN margem.nm_fornecedor IN (
        'ZOETIS INDUSTRIA DE PRODUTOS VETERINARIO', 
        'ZOETIS COMERCIO E DISTRIBUICAO LTDA', 
        'ZOETIS IND PROD VETERINARIOS LTDA', 
        'ZOETIS IND PROD VETERINARIOS LTDA') 
      THEN 'ZOETIS COMERCIO E DISTRIBUICAO LTDA'
      WHEN margem.nm_fornecedor IN (
        'CEVA SAUDE ANIMAL LTDA.', 
        'CEVA SAÚDE ANIMAL LTDA', 
        'CEVA VETERINÁRIA S/A') 
      THEN 'CEVA SAUDE ANIMAL LTDA.' 
      WHEN margem.nm_fornecedor IN (
        'NEOVIA NUTRIÇÃO E SAÚDE ANIMAL LTDA', 
        'NEOVIA NUTRIÇÃO E SAÚDE ANIMAL LTDA.') 
      THEN 'NEOVIA NUTRIÇÃO E SAÚDE ANIMAL LTDA'
      WHEN margem.nm_fornecedor IN (
        'BENTONISA – BENTONITA DO NORDESTE S', 
        'BENTONIT UNIÃO NORDESTE INDÚSTRIA E') 
      THEN 'BENTONIT UNIÃO NORDESTE INDÚSTRIA E'
      WHEN margem.nm_fornecedor IN (
        'GRANDFOOD INDÚSTRIA E COMERCIO LTDA', 
        'GRANDFOOD INDUSTRIA E COMERCIO LTDA') 
      THEN 'GRANDFOOD INDÚSTRIA E COMERCIO LTDA'
      WHEN margem.nm_fornecedor IN (
        'VECTOR CHEMICAL IND PRODUCTS LTDA', 
        'DNAMAZON ANIMALS INDUSTRIA, COMERCIO, IM', 
        'DNAMAZON ANIMALS INDUSTRIA, COMERCI') 
      THEN 'DNAMAZON ANIMALS INDUSTRIA, COMERCIO, IM'
      WHEN margem.nm_fornecedor IN (
        'RECRIAR SOLUÇÕES SUSTENTÁVEIS LTDA', 
        'ISALU SOLUÇÕES SUSTENTÁVEIS LTDA')
      THEN 'ISALU SOLUÇÕES SUSTENTÁVEIS LTDA'
      WHEN margem.nm_fornecedor IN (
        'F.G. INDUSTRIA DE ACESSORIOS PETS L')
      THEN 'F.G. INDUSTRIA DE ACESSORIOS PETS LTDA'
      WHEN margem.nm_fornecedor IN (
        'NESTLE BRASIL LTDA', 
        'Nestle Brasil Ltda')
      THEN 'NESTLE BRASIL LTDA.'
      WHEN margem.nm_fornecedor IN (
        'NUTRISANTOS ALIMENTAÇÃO ANIMAL', 
        'Nutrisantos Alimentação Animal')
      THEN 'NUTRISANTOS ALIMENTACAO ANIMAL LTDA'
      WHEN margem.nm_fornecedor IN (
        'OURO FINO AGRONEGOCIO LTDA')
      THEN 'OURO FINO AGRONEGOCIO LTDA.'
      WHEN margem.nm_fornecedor IN (
        'PETIX IND.E COM. IMP E EXP LTDA')
      THEN 'PETIX IND., COM., IMP. E EXP. DE PROD. G'
      WHEN margem.nm_fornecedor IN (
        'F. H. M. ADAMI-ME')
      THEN 'F. H. M. ADAMI'
      WHEN margem.nm_fornecedor IN (
        'JORGE LUIS DA SILVA LEME')
      THEN 'JORGE LUIS DA SILVA LEME 32077870885'
      WHEN margem.nm_fornecedor IN (
        'LEONARDO NERI COMERCIAL TÊXTIL EIRE')
      THEN 'LEONARDO NERI COMERCIAL TEXTIL LTDA'
      WHEN margem.nm_fornecedor IN (
        'WILLIAN EDUARDO LARGUEZA JUNIOR S.A')
      THEN '31.570.994 WILLIAN EDUARDO LARGUEZA JUNI'
      WHEN margem.nm_fornecedor IN (
        'COMPANHIA FRANCESA DE DISTRIBUIÇAO  DE P')
      THEN 'COMPANHIA FRANCESA DE DISTRIBUIÇAO  DE P'
      WHEN margem.nm_fornecedor IN (
        'GENERAL TREATS INDUSTRIA E COMERCIO')
      THEN 'GENERAL TREATS INDUSTRIA E COMERCIO LTDA'
      WHEN margem.nm_fornecedor IN (
        'J N DA SILVEIRA NUTRICAO E SUPLEMENTACAO ANIMAL')
      THEN 'JN DA SILVEIRA NUTRIÇÃO E SUPLEMENT'
      WHEN margem.nm_fornecedor IN (
        'NUTROPICA NUTRICAO ESPECIALIZADA LTDA')
      THEN 'NUTRÓPICA NUTRIÇÃO ESPECIALIZADA LT'
      WHEN margem.nm_fornecedor IN (
        'Petitos.Ind.Com.de.Alim.Animais.Ltda-EPP')
      THEN 'PETITOS.IND.COM.DE.ALIM.ANIMAIS.LTD'
      WHEN margem.nm_fornecedor IN (
        'RACOES REIS DISTRIBUIDORA DE ARTIGO')
      THEN 'RACOES REIS DISTRIBUIDORA DE ARTIGOS PET'
      WHEN margem.nm_fornecedor IN (
        'VITAPET FAB.PROD.ALIMENTÍCIOSANIMAI', 
        'Vitapet Fab.Prod.Alimentíciosanimai')
      THEN 'VITA PET FABRICACAO DE PRODUTOS ALIMENTI'
      WHEN margem.nm_fornecedor IN (
        'VETOQUINOL SAUDE ANIMAL')
      THEN 'VETOQUINOL SAUDE ANIMAL LTDA.'
      WHEN margem.nm_fornecedor IN (
        'UNIÃO QUÍMICA FARMACÊUTICA NACIONAL S A')
      THEN 'UNIAO QUIMICA FARMACEUTICA NACIONAL S A'
      WHEN margem.nm_fornecedor IN (
        'PET MED INDÚSTRIA E COMÉRCIO LTDA')
      THEN 'PET MED INDÚSTRIA E COMÉRCIO LTDA'
      WHEN margem.nm_fornecedor IN (
        'CHEMITEC AGRO-VETERINÁRIA LTDA - FI')
      THEN 'CHEMITEC AGRO-VETERINARIA LTDA'
      WHEN margem.nm_fornecedor IN (
        'CHAMPION FARMOQUÍMICO LTDA')
      THEN 'CHAMPION FARMOQUÍMICO LTDA'
      WHEN margem.nm_fornecedor IN (
        'Alivet Saúde Animal LTDA')
      THEN 'ALIVET  SAÚDE ANIMAL LTDA'
      WHEN margem.nm_fornecedor IN (
        'VETYS DO BRASIL PROD VETERINÁRIOS L')
      THEN 'VETYS DO BRASIL PROD VETERINÁRIOS L'
      WHEN margem.nm_fornecedor IN (
        'SOLUTION DISTRIBUIDORA DE PRODUTOS')
      THEN 'SOLUTION DISTRIBUIDORA DE PRODUTOS VETER'
      WHEN margem.nm_fornecedor IN (
        'Ecoenergia Industria E Comercio Ltd', 
        'ECOENERGIA INDUSTRIA E COMERCIO LTD')
      THEN 'ECOENERGIA INDUSTRIA E COMERCIO LTDA'
      WHEN margem.nm_fornecedor IN (
        'ALLERLESS ATAC E VAR DE PROD P ANIM')
      THEN 'ALLERLESS ATAC E VAR DE PROD P ANIM'
      WHEN margem.nm_fornecedor IN (
        'ALESS KANAAN QUÍMICA E INDÚSTRIA E COMÉR')
      THEN 'ALLES PET LTDA'
      WHEN margem.nm_fornecedor IN (
        'ZOE PETFOOD COMERCIO DE ALIMENTOS PARA')
      THEN 'ZOE PETFOOD COMERCIO DE ALIMENTOS PARA A'
      WHEN margem.nm_fornecedor IN (
        'Upper Dog Comercial - LTDA')
      THEN 'UPPER DOG COMERCIAL LTDA'
      WHEN margem.nm_fornecedor IN (
        'Selecta Pet Care Indústria Comércio Ltda')
      THEN 'SELECTA PET CARE INDÚSTRIA COMÉRCIO'    
      WHEN margem.nm_fornecedor IN (
        'JORGE LUIS DA SILVA LEME')
      THEN 'JORGE LUIS DA SILVA LEME 32077870885' 
      WHEN margem.nm_fornecedor IN (
        'LEONARDO NERI COMERCIAL TÊXTIL EIRE')
      THEN 'LEONARDO NERI COMERCIAL TEXTIL LTDA'   
      WHEN margem.nm_fornecedor IN (
        'GENERAL TREATS INDUSTRIA E COMERCIO')
      THEN 'GENERAL TREATS INDUSTRIA E COMERCIO LTDA'                                            
      WHEN margem.nm_fornecedor IN (
        'SUL MINEIRA ALIMENTOS LTDA - VARGIN')
      THEN 'NEOVIA NUTRIÇÃO E SAÚDE ANIMAL LTDA'
      ELSE nm_fornecedor
    END                                             AS nm_fornecedor
    , setor
    , SUM(receita_bruta_produto)                    AS receita_fornecedor_setor
  FROM `petlove-dataeng-prod-01.dw_corporativo.dts_margem_conciliacao` margem
  WHERE 1=1
    AND data_emissao_nota BETWEEN min_data AND max_data
    AND marca_exclusiva = 'Não'
  GROUP BY 1, 2, 3
)
, desempenho_pedido AS (
  SELECT
    DATE_TRUNC(margem.data_emissao_nota, MONTH)       AS mes
    , margem.id_pedido
    , CASE 
      WHEN margem.nm_fornecedor IN (
        'ZOETIS INDUSTRIA DE PRODUTOS VETERINARIO', 
        'ZOETIS COMERCIO E DISTRIBUICAO LTDA', 
        'ZOETIS IND PROD VETERINARIOS LTDA', 
        'ZOETIS IND PROD VETERINARIOS LTDA') 
      THEN 'ZOETIS COMERCIO E DISTRIBUICAO LTDA'
      WHEN margem.nm_fornecedor IN (
        'CEVA SAUDE ANIMAL LTDA.', 'CEVA SAÚDE ANIMAL LTDA', 
        'CEVA VETERINÁRIA S/A') 
      THEN 'CEVA SAUDE ANIMAL LTDA.' 
      WHEN margem.nm_fornecedor IN (
        'NEOVIA NUTRIÇÃO E SAÚDE ANIMAL LTDA', 
        'NEOVIA NUTRIÇÃO E SAÚDE ANIMAL LTDA.') 
      THEN 'NEOVIA NUTRIÇÃO E SAÚDE ANIMAL LTDA'
      WHEN margem.nm_fornecedor IN (
        'BENTONISA – BENTONITA DO NORDESTE S', 
        'BENTONIT UNIÃO NORDESTE INDÚSTRIA E') 
      THEN 'BENTONIT UNIÃO NORDESTE INDÚSTRIA E'
      WHEN margem.nm_fornecedor IN (
        'GRANDFOOD INDÚSTRIA E COMERCIO LTDA', 
        'GRANDFOOD INDUSTRIA E COMERCIO LTDA') 
      THEN 'GRANDFOOD INDÚSTRIA E COMERCIO LTDA'
      WHEN margem.nm_fornecedor IN (
        'VECTOR CHEMICAL IND PRODUCTS LTDA', 
        'DNAMAZON ANIMALS INDUSTRIA, COMERCIO, IM', 
        'DNAMAZON ANIMALS INDUSTRIA, COMERCI') 
      THEN 'DNAMAZON ANIMALS INDUSTRIA, COMERCIO, IM'
      WHEN margem.nm_fornecedor IN (
        'RECRIAR SOLUÇÕES SUSTENTÁVEIS LTDA', 
        'ISALU SOLUÇÕES SUSTENTÁVEIS LTDA')
      THEN 'ISALU SOLUÇÕES SUSTENTÁVEIS LTDA'
      WHEN margem.nm_fornecedor IN (
        'F.G. INDUSTRIA DE ACESSORIOS PETS L')
      THEN 'F.G. INDUSTRIA DE ACESSORIOS PETS LTDA'
      WHEN margem.nm_fornecedor IN (
        'NESTLE BRASIL LTDA', 
        'Nestle Brasil Ltda')
      THEN 'NESTLE BRASIL LTDA.'
      WHEN margem.nm_fornecedor IN (
        'NUTRISANTOS ALIMENTAÇÃO ANIMAL', 
        'Nutrisantos Alimentação Animal')
      THEN 'NUTRISANTOS ALIMENTACAO ANIMAL LTDA'
      WHEN margem.nm_fornecedor IN (
        'OURO FINO AGRONEGOCIO LTDA')
      THEN 'OURO FINO AGRONEGOCIO LTDA.'
      WHEN margem.nm_fornecedor IN (
        'PETIX IND.E COM. IMP E EXP LTDA')
      THEN 'PETIX IND., COM., IMP. E EXP. DE PROD. G'
      WHEN margem.nm_fornecedor IN (
        'F. H. M. ADAMI-ME')
      THEN 'F. H. M. ADAMI'
      WHEN margem.nm_fornecedor IN (
        'JORGE LUIS DA SILVA LEME')
      THEN 'JORGE LUIS DA SILVA LEME 32077870885'
      WHEN margem.nm_fornecedor IN (
        'LEONARDO NERI COMERCIAL TÊXTIL EIRE')
      THEN 'LEONARDO NERI COMERCIAL TEXTIL LTDA'
      WHEN margem.nm_fornecedor IN (
        'WILLIAN EDUARDO LARGUEZA JUNIOR S.A')
      THEN '31.570.994 WILLIAN EDUARDO LARGUEZA JUNI'
      WHEN margem.nm_fornecedor IN (
        'COMPANHIA FRANCESA DE DISTRIBUIÇAO  DE P')
      THEN 'COMPANHIA FRANCESA DE DISTRIBUIÇAO  DE P'
      WHEN margem.nm_fornecedor IN (
        'GENERAL TREATS INDUSTRIA E COMERCIO')
      THEN 'GENERAL TREATS INDUSTRIA E COMERCIO LTDA'
      WHEN margem.nm_fornecedor IN (
        'J N DA SILVEIRA NUTRICAO E SUPLEMENTACAO ANIMAL')
      THEN 'JN DA SILVEIRA NUTRIÇÃO E SUPLEMENT'
      WHEN margem.nm_fornecedor IN (
        'NUTROPICA NUTRICAO ESPECIALIZADA LTDA')
      THEN 'NUTRÓPICA NUTRIÇÃO ESPECIALIZADA LT'
      WHEN margem.nm_fornecedor IN (
        'Petitos.Ind.Com.de.Alim.Animais.Ltda-EPP')
      THEN 'PETITOS.IND.COM.DE.ALIM.ANIMAIS.LTD'
      WHEN margem.nm_fornecedor IN (
        'RACOES REIS DISTRIBUIDORA DE ARTIGO')
      THEN 'RACOES REIS DISTRIBUIDORA DE ARTIGOS PET'
      WHEN margem.nm_fornecedor IN (
        'VITAPET FAB.PROD.ALIMENTÍCIOSANIMAI', 
        'Vitapet Fab.Prod.Alimentíciosanimai')
      THEN 'VITA PET FABRICACAO DE PRODUTOS ALIMENTI'
      WHEN margem.nm_fornecedor IN (
        'VETOQUINOL SAUDE ANIMAL')
      THEN 'VETOQUINOL SAUDE ANIMAL LTDA.'
      WHEN margem.nm_fornecedor IN (
        'UNIÃO QUÍMICA FARMACÊUTICA NACIONAL S A')
      THEN 'UNIAO QUIMICA FARMACEUTICA NACIONAL S A'
      WHEN margem.nm_fornecedor IN (
        'PET MED INDÚSTRIA E COMÉRCIO LTDA')
      THEN 'PET MED INDÚSTRIA E COMÉRCIO LTDA'
      WHEN margem.nm_fornecedor IN (
        'CHEMITEC AGRO-VETERINÁRIA LTDA - FI')
      THEN 'CHEMITEC AGRO-VETERINARIA LTDA'
      WHEN margem.nm_fornecedor IN (
        'CHAMPION FARMOQUÍMICO LTDA')
      THEN 'CHAMPION FARMOQUÍMICO LTDA'
      WHEN margem.nm_fornecedor IN (
        'Alivet Saúde Animal LTDA')
      THEN 'ALIVET  SAÚDE ANIMAL LTDA'
      WHEN margem.nm_fornecedor IN (
        'VETYS DO BRASIL PROD VETERINÁRIOS L')
      THEN 'VETYS DO BRASIL PROD VETERINÁRIOS L'
      WHEN margem.nm_fornecedor IN (
        'SOLUTION DISTRIBUIDORA DE PRODUTOS')
      THEN 'SOLUTION DISTRIBUIDORA DE PRODUTOS VETER'
      WHEN margem.nm_fornecedor IN (
        'Ecoenergia Industria E Comercio Ltd', 
        'ECOENERGIA INDUSTRIA E COMERCIO LTD')
      THEN 'ECOENERGIA INDUSTRIA E COMERCIO LTDA'
      WHEN margem.nm_fornecedor IN (
        'ALLERLESS ATAC E VAR DE PROD P ANIM')
      THEN 'ALLERLESS ATAC E VAR DE PROD P ANIM'
      WHEN margem.nm_fornecedor IN (
        'ALESS KANAAN QUÍMICA E INDÚSTRIA E COMÉR')
      THEN 'ALLES PET LTDA'
      WHEN margem.nm_fornecedor IN (
        'ZOE PETFOOD COMERCIO DE ALIMENTOS PARA')
      THEN 'ZOE PETFOOD COMERCIO DE ALIMENTOS PARA A'
      WHEN margem.nm_fornecedor IN (
        'Upper Dog Comercial - LTDA')
      THEN 'UPPER DOG COMERCIAL LTDA'
      WHEN margem.nm_fornecedor IN (
        'Selecta Pet Care Indústria Comércio Ltda')
      THEN 'SELECTA PET CARE INDÚSTRIA COMÉRCIO'    
      WHEN margem.nm_fornecedor IN (
        'JORGE LUIS DA SILVA LEME')
      THEN 'JORGE LUIS DA SILVA LEME 32077870885' 
      WHEN margem.nm_fornecedor IN (
        'LEONARDO NERI COMERCIAL TÊXTIL EIRE')
      THEN 'LEONARDO NERI COMERCIAL TEXTIL LTDA'   
      WHEN margem.nm_fornecedor IN (
        'GENERAL TREATS INDUSTRIA E COMERCIO')
      THEN 'GENERAL TREATS INDUSTRIA E COMERCIO LTDA'                                            
      WHEN margem.nm_fornecedor IN (
        'SUL MINEIRA ALIMENTOS LTDA - VARGIN')
      THEN 'NEOVIA NUTRIÇÃO E SAÚDE ANIMAL LTDA'
      ELSE margem.nm_fornecedor
    END                                               AS nm_fornecedor
    , margem.setor
    , SUM(margem.receita_bruta_produto)
      / receita_fornecedor_setor                      AS contribuicao_pedido
    , SUM(margem.receita_bruta_produto)               AS receita_fornecedor_pedido
  FROM `petlove-dataeng-prod-01.dw_corporativo.dts_margem_conciliacao` margem
  LEFT JOIN performance_fornecedor_setor performance
    ON margem.nm_fornecedor = performance.nm_fornecedor
    AND margem.setor = performance.setor
    AND DATE_TRUNC(margem.data_emissao_nota, MONTH) = performance.mes
  WHERE 1=1
    AND margem.data_emissao_nota BETWEEN min_data AND max_data
    AND margem.marca_exclusiva = 'Não'
  GROUP BY 1, 2, 3, 4, receita_fornecedor_setor
)
, contribuicao_setor_pedido AS (
  SELECT
    desempenho_pedido.*
    , (contribuicao_pedido * net_fornecedor) AS contribuicao_ponderada --adicionar isso na margem2
  FROM desempenho_pedido
  LEFT JOIN bonificacoes
    ON desempenho_pedido.mes = bonificacoes.mes
    AND desempenho_pedido.nm_fornecedor = bonificacoes.fornecedor
    AND desempenho_pedido.setor = bonificacoes.setor
  WHERE 1=1
    AND net_fornecedor IS NOT NULL
)
, recomposicao_da_margem AS (
  SELECT
    id_pedido
    , SUM(contribuicao_ponderada) AS contribuicao_a_recompor
  FROM contribuicao_setor_pedido
  GROUP BY 1
)
, margem AS (
  SELECT
    margem.id_pedido
    , (
      SUM(margem.lucro_bruto_operacional) 
      - SUM(margem.despesas_vendas) 
      - SUM(margem.despesas_operacao)
    )                                           AS margem_2_ajustada
    , SUM(margem.receita_liquida_operacional)   AS receita_liquida_operacional
  FROM `petlove-dataeng-prod-01.dw_corporativo.dts_margem_conciliacao` margem
  WHERE 1=1
    AND data_emissao_nota BETWEEN min_data AND max_data
  GROUP BY 1
), main_margem as (
--, margem_recomposta AS (
  SELECT
    margem.*
    , margem.margem_2_ajustada + COALESCE(contribuicao_a_recompor, 0) AS margem_recomposta
  FROM margem
  LEFT JOIN recomposicao_da_margem
    ON margem.id_pedido = recomposicao_da_margem.id_pedido),
    dados_margem_conciliacao as (
    select id_pedido, sum(receita_liquida_operacional) as receita_liquida_operacional_conciliacao,sum(receita_bruta_produto) as receita_bruta_produto, sum(margem_2) as margem_2
    from dw_corporativo.dts_margem_conciliacao
    group by 1),
    marcador_pedido_aux as(select distinct pedido_beagle, max(fpf.chv_entrega) chv_entrega, max(fpf.chv_canal_venda) chv_canal_venda, max(fpf.chv_setor_censitario) chv_setor_censitario, max(fpf.chv_municipio_entrega) chv_municipio_entrega from petlove-dataeng-prod-01.dw_corporativo.ft_pedido_faturado fpf
group by 1
)
,marcador_pedido as(
  select pedido_beagle, nm_municipio, uf, nm_regiao, localidade, cep_inicial, cep_final , canal_venda, subcanal_venda from marcador_pedido_aux fpf
left join petlove-dataeng-prod-01.dw_corporativo.dim_municipio dm on dm.chv_municipio = fpf.chv_municipio_entrega
left join petlove-dataeng-prod-01.dw_corporativo.dim_canal_venda dcv on dcv.chv_canal_venda = fpf.chv_canal_venda
left join petlove-dataeng-prod-01.dw_corporativo.dim_setor_censitario dsc on dsc.chv_setor_censitario= fpf.chv_setor_censitario
)
    select distinct 
    date_trunc(c.data_pedido,MONTH) anomes,
    a.id_pedido,
    c.uf_entrega,
    c.regiao_entrega,
    d.localidade,
    receita_liquida_operacional_conciliacao,
    b.receita_liquida_operacional,
    a.receita_bruta_produto,
    a.margem_2,
    b.margem_2_ajustada,
    b.margem_recomposta
    from dados_margem_conciliacao a
    join main_margem b
    on a.id_pedido = b.id_pedido
    join dw_corporativo.dts_margem_conciliacao c
    on a.id_pedido = c.id_pedido
    left join marcador_pedido d
    on a.id_pedido = d.pedido_beagle"


tb <- bq_project_query(projectid,query)
df = bq_table_download(tb) %>% data.frame()

df <- na.omit(df)



# Função para criar intervalos de 10 em 10 na variável receita_bruta_produto
create_intervals <- function(data) {
  min_value <- min(data)
  max_value <- max(data)
  seq(min_value, max_value, by = 10)
}

# Definir os intervalos
intervals <- create_intervals(df$receita_bruta_produto)
df <- df %>% filter(anomes >= '2023-04-01',receita_bruta_produto <=1000)

ui <- fluidPage(
  titlePanel("Distribuição de Pedidos por Receita Líquida Operacional"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("date_range", "Selecione o período:",
                  min = min(df$anomes), max = max(df$anomes),
                  value = c(min(df$anomes), max(df$anomes)),
                  step = 1),
      sliderInput("revenue_range", "Selecione a receita líquida operacional:",
                  min = min(df$receita_bruta_produto), max = max(df$receita_bruta_produto),
                  value = c(min(df$receita_bruta_produto), max(df$receita_bruta_produto)),
                  step = 10),
      selectInput("uf_filter", "Selecione o UF:",
                  choices = unique(df$uf_entrega)),
      selectInput("localidade_filter", "Selecione a Localidade:",
                  choices = unique(df$localidade))
    ),
    mainPanel(
      plotlyOutput("distribution_plot")
    )
  )
)

# Definir server
server <- function(input, output) {
  
  filtered_data <- reactive({
    df %>%
      filter(anomes >= input$date_range[1] & anomes <= input$date_range[2] &
               receita_bruta_produto >= input$revenue_range[1] &
               receita_bruta_produto <= input$revenue_range[2] &
               uf_entrega == input$uf_filter &
               localidade == input$localidade_filter)
  })
  
  output$distribution_plot <- renderPlotly({
    ggplotly(
      ggplot(filtered_data(), aes(x = receita_bruta_produto,fill=margem_2)) +
        geom_histogram(binwidth = 10) +
        labs(x = "Receita Líquida Operacional", y = "Quantidade de Pedidos",
             title = "Distribuição de Pedidos por Receita Líquida Operacional") +
        theme_minimal()
    )
  })
}

# Rodar o aplicativo
shinyApp(ui = ui, server = server)