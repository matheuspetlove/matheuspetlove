library(dplyr)
library(bigrquery)
library(ggplot2)
library(tidyr)
library(flextable)
library(shiny)
library(plotly)
projectid = "petlove-dataeng-prod-01"


query <- "DECLARE min_data DATE DEFAULT '2023-01-01'; 
DECLARE max_data DATE DEFAULT CURRENT_DATE - 1; 
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
    , margem.data_pedido
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
  GROUP BY 1, 2, 3, 4, 5, receita_fornecedor_setor
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
    , margem.data_pedido
    , margem.regiao_entrega
    , margem.uf_entrega
    , margem.cidade_entrega
    , margem.bairro_entrega
    , margem.cep_entrega
    , (
      SUM(margem.lucro_bruto_operacional) 
      - SUM(margem.despesas_vendas) 
      - SUM(margem.despesas_operacao)
    )                                           AS margem_2_ajustada
    , SUM(margem.receita_liquida_operacional)   AS receita_liquida_operacional
    , SUM(margem.receita_bruta_produto)         AS receita_bruta_pedido
  FROM `petlove-dataeng-prod-01.dw_corporativo.dts_margem_conciliacao` margem
  WHERE 1=1
    AND data_emissao_nota BETWEEN min_data AND max_data
  GROUP BY 1,2,3,4,5,6,7
)
, margem_recomposta AS (
  SELECT
    margem.*
    , margem.margem_2_ajustada + COALESCE(contribuicao_a_recompor, 0) AS margem_recomposta
  FROM margem
  LEFT JOIN recomposicao_da_margem
    ON margem.id_pedido = recomposicao_da_margem.id_pedido
)
, thresholds AS (
  SELECT DISTINCT
    TRIM(number)                                      AS pedido_beagle
    , free_shipping                                   AS fl_frete_gratis
    , exceptional_free_shipping_min_value             AS exceptional_value
    , JSON_VALUE(free_shipping_data, '$.min_value')   AS threshold_frete_gratis
    , JSON_VALUE(free_shipping_data, '$.updated_at')  AS json_updated_at
    , ROW_NUMBER() OVER (
        PARTITION BY
          number
        ORDER BY 
          CAST(
            JSON_VALUE(
              free_shipping_data, 
              '$.updated_at') 
            AS TIMESTAMP) 
          DESC)                                       AS rn
  FROM `petlove-dataeng-prod-01.curated_bulldog.combinations_sources` sources
  WHERE 1=1
    AND sources.refdate <= CURRENT_DATE - 1
    AND TRIM(REPLACE(number, ' ', '')) IS NOT NULL
    AND TRIM(REPLACE(number, ' ', '')) <> ''
    AND TRIM(REPLACE(number, ' ', '')) <> '-dm'
    AND JSON_VALUE(free_shipping_data, '$.min_value') IS NOT NULL
)
, base_analitica AS (
  SELECT 
    margem_recomposta.*
    , thresholds.threshold_frete_gratis
    , thresholds.fl_frete_gratis
    , thresholds.exceptional_value
  FROM margem_recomposta
  LEFT JOIN thresholds
    ON margem_recomposta.id_pedido = thresholds.pedido_beagle
    AND thresholds.rn = 1
  WHERE 1=1
    --AND threshold_frete_gratis IS NOT NULL
)
, regras_canais AS (
  SELECT DISTINCT
    rule_channels.free_shipping_rule_id
    , rule_channels.channel_id
    , channels.channel
    , channels.subchannel
  FROM `petlove-dataeng-prod-01.curated_bulldog.free_shipping_rule_channels` rule_channels
  JOIN `petlove-dataeng-prod-01.curated_bulldog.channels` channels
    ON channels.refdate <= CURRENT_DATE
    AND rule_channels.channel_id = channels.id
  WHERE 1=1
    AND rule_channels.refdate <= CURRENT_DATE
) 
, ajuste_regras AS (
  SELECT DISTINCT
    rules.from_zipcode        AS inicio_cep
    , rules.to_zipcode        AS fim_cep
    --, UPPER(rules.state)      AS uf
    --, LOWER(rules.city)       AS faixa_de_cep
    , DATE(rules.created_at)  AS dia_criacao
    , MIN(rules.min_value)    AS threshold_da_regra
  FROM `petlove-dataeng-prod-01.curated_bulldog.free_shipping_rules` rules
  LEFT JOIN regras_canais
    ON rules.id = regras_canais.free_shipping_rule_id
    AND regras_canais.channel IS NOT NULL
    AND LOWER(regras_canais.channel) <> 'white_label'
  WHERE 1=1
    AND rules.refdate <= CURRENT_DATE
    AND rules.state IS NOT NULL
    AND rules.city IS NOT NULL 
    AND TRIM(rules.city) <> ''
    AND rules.max_weight IS NULL
    --AND DATE(rules.created_at) < '2023-09-27'
  GROUP BY 1,2,3--,4,5
)
, base_thresholds_estimados AS (
  SELECT
    *
    , COALESCE(
      DATE_SUB(
        DATE(LEAD(dia_criacao, 1) OVER (
        PARTITION BY 
          inicio_cep
          , fim_cep
          --, uf
          --, faixa_de_cep
        ORDER BY DATE(dia_criacao)
      )), INTERVAL 1 DAY)
      , CURRENT_DATE)                      AS valido_ate --inicio da bulldog
  FROM ajuste_regras
  WHERE 1=1
)
, faturados AS (
  SELECT DISTINCT
    id_pedido
    , CAST(cep_entrega AS INT64)  AS cep_entrega
    , data_pedido
    , SUM(receita_bruta_frete)    AS bruta_frete
    , SUM(receita_liquida_frete)  AS liquida_frete
  FROM `petlove-dataeng-prod-01.dw_corporativo.dts_margem_conciliacao`
  WHERE 1=1
    AND data_emissao_nota BETWEEN 
      min_data
        AND
      '2023-09-26'
    AND data_pedido BETWEEN 
      min_data
        AND
      '2023-09-26'
  GROUP BY 1,2,3
)
, base_thresholds_calculados AS (
  SELECT
    faturados.id_pedido
    , CASE 
      WHEN bruta_frete <= 0.01
        THEN 1
        ELSE 0
      END                     AS fl_frete_gratis
    , MIN(threshold_da_regra) AS menor_threshold_calculado
    , MAX(threshold_da_regra) AS maior_threshold_calculado
  FROM faturados
  LEFT JOIN base_thresholds_estimados thresholds
    ON faturados.cep_entrega BETWEEN 
      thresholds.inicio_cep
        AND thresholds.fim_cep
    AND faturados.data_pedido BETWEEN 
      thresholds.dia_criacao
        AND thresholds.valido_ate
  WHERE 1=1
  GROUP BY 1,2
  HAVING menor_threshold_calculado IS NOT NULL --só a partir de quando temos regras disponíveis
)
--, base_final AS (
SELECT
  base_analitica.id_pedido
  , base_analitica.data_pedido
  , base_analitica.regiao_entrega
  , base_analitica.uf_entrega
  , base_analitica.cidade_entrega
  , base_analitica.bairro_entrega
  , base_analitica.cep_entrega
  , base_analitica.margem_2_ajustada
  , base_analitica.receita_bruta_pedido
  , base_analitica.receita_liquida_operacional
  , base_analitica.margem_recomposta
  , COALESCE(
      base_analitica.fl_frete_gratis
      , threshold_calculado.fl_frete_gratis,0)      AS fl_frete_gratis
  , base_analitica.threshold_frete_gratis
  , base_analitica.exceptional_value
  , COALESCE(
    CAST(threshold_frete_gratis AS FLOAT64)
    , CAST(exceptional_value AS FLOAT64)
    , CAST(maior_threshold_calculado AS FLOAT64)
    , CAST(menor_threshold_calculado AS FLOAT64)
  )                                                 AS threshold_final
  , CASE
    WHEN threshold_frete_gratis IS NOT NULL
      THEN 'bulldog threshold '
    WHEN exceptional_value IS NOT NULL
      THEN 'bulldog exceptional_value'
    WHEN 
      menor_threshold_calculado 
        = 
      maior_threshold_calculado
      THEN 'calculado - valor único'
    WHEN 
      menor_threshold_calculado 
        <> 
      maior_threshold_calculado
      THEN 'calculado - maximo entre valores'
    ELSE NULL
    END                                            AS cluster_threshold
FROM base_analitica
LEFT JOIN base_thresholds_calculados threshold_calculado
  ON base_analitica.id_pedido = threshold_calculado.id_pedido
WHERE 1=1
order by rand()
limit 3000000"


tb <- bq_project_query(projectid,query)
df = bq_table_download(tb) %>% data.frame()


create_intervals <- function(data) {
  min_value <- 0
  max_value <- 300
  cut(data, breaks = c(seq(min_value, max_value, by = 10), Inf), include.lowest = TRUE, right = FALSE)
}


# Aplicando a função mutate com case_when
df <- df %>%
  mutate(Faixa = case_when(
    receita_bruta_pedido <= 300 ~ as.character(create_intervals(receita_bruta_pedido)),
    TRUE ~ '+300'
  ))

df_margem <- df %>% group_by(uf_entrega,regiao_entrega,Faixa) %>%
  summarise(Margem = sum(margem_2_ajustada)/sum(receita_liquida_operacional)*100)

df_tabela <- df %>% filter(data_pedido >= '2023-08-01') %>% group_by(uf_entrega,regiao_entrega) %>%
  summarise("Vendas" = n_distinct(id_pedido),
  "Ticket Médio" = mean(receita_bruta_pedido),
  "Ticket Mediano" = median(receita_bruta_pedido),
  "Threshold_min" = min(threshold_final,na.rm = TRUE),
  "Threshold_max" = max(threshold_final,na.rm = TRUE),
  "Threshold_medio" = mean(threshold_final,na.rm = TRUE),
  "Threshold_mediano" = median(threshold_final,na.rm = TRUE))

df_tabela <- df_tabela %>%
  mutate(Faixa = case_when(
    `Ticket Mediano` <= 300 ~ as.character(create_intervals(`Ticket Mediano`)),
    TRUE ~ '+300'
  ))

merged_df <- merge(df_tabela, df_margem, by = c("uf_entrega", "regiao_entrega", "Faixa"), all.x = TRUE)

# Criando a coluna "Margem" no dataframe "merged_df"
merged_df <- merged_df %>%
  mutate("Margem Tier" = case_when(
    Margem > 10 ~ "Tier 1",
    Margem > 5 & Margem <= 10 ~ "Tier 2",
    Margem > 0 & Margem <= 5 ~ "Tier 3",
    Margem <= 0 ~ "Tier 4",
    TRUE ~ NA_character_ # Se nenhuma condição for atendida, atribui NA
  ))

library(readr)
write_csv(merged_df,"theshold.csv")
