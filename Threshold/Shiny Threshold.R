library(dplyr)
library(bigrquery)
library(ggplot2)
library(tidyr)
library(shiny)
library(plotly)
library(readr)
projectid = "petlove-dataeng-prod-01"

#query <- readChar("/home/matheus/matheuspetlove/Threshold/query_threshold.txt", file.info("/home/matheus/matheuspetlove/Threshold/query_threshold.txt")$size)
query <- "DECLARE min_data DATE DEFAULT '2023-06-01'; 
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
),marcador_pedido_aux as(select distinct pedido_beagle, max(fpf.chv_entrega) chv_entrega, max(fpf.chv_canal_venda) chv_canal_venda, max(fpf.chv_setor_censitario) chv_setor_censitario, max(fpf.chv_municipio_entrega) chv_municipio_entrega from petlove-dataeng-prod-01.dw_corporativo.ft_pedido_faturado fpf
group by 1
)
,marcador_pedido as(
  select pedido_beagle, nm_municipio, uf, nm_regiao, localidade, cep_inicial, cep_final , canal_venda, subcanal_venda from marcador_pedido_aux fpf
left join petlove-dataeng-prod-01.dw_corporativo.dim_municipio dm on dm.chv_municipio = fpf.chv_municipio_entrega
left join petlove-dataeng-prod-01.dw_corporativo.dim_canal_venda dcv on dcv.chv_canal_venda = fpf.chv_canal_venda
left join petlove-dataeng-prod-01.dw_corporativo.dim_setor_censitario dsc on dsc.chv_setor_censitario= fpf.chv_setor_censitario
)
SELECT
  base_analitica.id_pedido
  , base_analitica.data_pedido
  , base_analitica.regiao_entrega
  , base_analitica.uf_entrega
  , b.localidade
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
left join marcador_pedido b
on base_analitica.id_pedido = b.pedido_beagle
WHERE 1=1
order by rand()
limit 200000"

tb <- bq_project_query(projectid,query)
df = bq_table_download(tb) %>% data.frame()

# Definindo a função para classificar a receita em segmentos
classificar_numero <- function(numero) {
  if (numero <= 10) {
    return(10)
  } else if (numero > 10 & numero <= 20) {
    return(20)
  } else if (numero > 20 & numero <= 30) {
    return(30)
  } else if (numero > 30 & numero <= 40) {
    return(40)
  } else if (numero > 40 & numero <= 50) {
    return(50)
  } else if (numero > 50 & numero <= 60) {
    return(60)
  } else if (numero > 60 & numero <= 70) {
    return(70)
  } else if (numero > 70 & numero <= 80) {
    return(80)
  } else if (numero > 80 & numero <= 90) {
    return(90)
  } else if (numero > 90 & numero <= 100) {
    return(100)
  } else if (numero > 100 & numero <= 110) {
    return(110)
  } else if (numero > 110 & numero <= 120) {
    return(120)
  } else if (numero > 120 & numero <= 130) {
    return(130)
  } else if (numero > 130 & numero <= 140) {
    return(140)
  } else if (numero > 140 & numero <= 150) {
    return(150)
  } else if (numero > 150 & numero <= 160) {
    return(160)
  } else if (numero > 160 & numero <= 170) {
    return(170)
  } else if (numero > 170 & numero <= 180) {
    return(180)
  } else if (numero > 180 & numero <= 190) {
    return(190)
  } else if (numero > 190 & numero <= 200) {
    return(200)
  } else if (numero > 200 & numero <= 210) {
    return(210)
  } else if (numero > 210 & numero <= 220) {
    return(220)
  } else if (numero > 220 & numero <= 230) {
    return(230)
  } else if (numero > 230 & numero <= 240) {
    return(240)
  } else if (numero > 240 & numero <= 250) {
    return(250)
  } else if (numero > 250 & numero <= 260) {
    return(260)
  } else if (numero > 260 & numero <= 270) {
    return(270)
  } else if (numero > 270 & numero <= 280) {
    return(280)
  } else if (numero > 280 & numero <= 290) {
    return(290)
  } else if (numero > 290 & numero <= 300) {
    return(300)
  }
}


  df <- df %>%
    mutate(segmento_receita = case_when(
      receita_bruta_pedido <= 10 ~ 10,
      receita_bruta_pedido > 10 & receita_bruta_pedido <= 20 ~ 20,
      receita_bruta_pedido > 20 & receita_bruta_pedido <= 30 ~ 30,
      receita_bruta_pedido > 30 & receita_bruta_pedido <= 40 ~ 40,
      receita_bruta_pedido > 40 & receita_bruta_pedido <= 50 ~ 50,
      receita_bruta_pedido > 50 & receita_bruta_pedido <= 60 ~ 60,
      receita_bruta_pedido > 60 & receita_bruta_pedido <= 70 ~ 70,
      receita_bruta_pedido > 70 & receita_bruta_pedido <= 80 ~ 80,
      receita_bruta_pedido > 80 & receita_bruta_pedido <= 90 ~ 90,
      receita_bruta_pedido > 90 & receita_bruta_pedido <= 100 ~ 100,
      receita_bruta_pedido > 100 & receita_bruta_pedido <= 110 ~ 110,
      receita_bruta_pedido > 110 & receita_bruta_pedido <= 120 ~ 120,
      receita_bruta_pedido > 120 & receita_bruta_pedido <= 130 ~ 130,
      receita_bruta_pedido > 130 & receita_bruta_pedido <= 140 ~ 140,
      receita_bruta_pedido > 140 & receita_bruta_pedido <= 150 ~ 150,
      receita_bruta_pedido > 150 & receita_bruta_pedido <= 160 ~ 160,
      receita_bruta_pedido > 160 & receita_bruta_pedido <= 170 ~ 170,
      receita_bruta_pedido > 170 & receita_bruta_pedido <= 180 ~ 180,
      receita_bruta_pedido > 180 & receita_bruta_pedido <= 190 ~ 190,
      receita_bruta_pedido > 190 & receita_bruta_pedido <= 200 ~ 200,
      receita_bruta_pedido > 200 & receita_bruta_pedido <= 210 ~ 210,
      receita_bruta_pedido > 210 & receita_bruta_pedido <= 220 ~ 220,
      receita_bruta_pedido > 220 & receita_bruta_pedido <= 230 ~ 230,
      receita_bruta_pedido > 230 & receita_bruta_pedido <= 240 ~ 240,
      receita_bruta_pedido > 240 & receita_bruta_pedido <= 250 ~ 250,
      receita_bruta_pedido > 250 & receita_bruta_pedido <= 260 ~ 260,
      receita_bruta_pedido > 260 & receita_bruta_pedido <= 270 ~ 270,
      receita_bruta_pedido > 270 & receita_bruta_pedido <= 280 ~ 280,
      receita_bruta_pedido > 280 & receita_bruta_pedido <= 290 ~ 290,
      receita_bruta_pedido > 290 & receita_bruta_pedido <= 300 ~ 300
    ))
  
  ### Tratando localidade NA
  df <- df %>%
    mutate(localidade = coalesce(localidade, "Não Encontrada"))
  df <- df %>%
    mutate(localidade = case_when(localidade=='' ~ "Não Encontrada", TRUE ~ localidade))
  ### Juntando Todas as uf do norte
  
  df<- df %>%
    mutate(uf_nova = case_when(regiao_entrega == 'Norte' ~ 'Norte',TRUE~uf_entrega))
  
  #Criando um dataframe que pega a o total de tudo  uf e localidade
  df_total <- df %>% filter(data_pedido >= '2023-08-01') %>% group_by(uf_nova,localidade) %>%
    summarise("Vendas" = n_distinct(id_pedido),
              'Margem Ajustada' = sum(margem_2_ajustada,na.rm = TRUE),
              'Margem Recomposta' = sum(margem_recomposta,na.rm = TRUE),
              "Receita Bruta Total" = sum(receita_bruta_pedido,na.rm = TRUE),
              "Receita Liquida Operacional" = sum(receita_liquida_operacional,na.rm = TRUE),
              "Ticket Médio" = mean(receita_bruta_pedido,na.rm = TRUE),
              "Ticket Mediano" = median(receita_bruta_pedido,na.rm = TRUE),
              "Threshold_min" = min(threshold_final,na.rm = TRUE),
              "Threshold_max" = max(threshold_final,na.rm = TRUE),
              "Threshold_medio" = mean(threshold_final,na.rm = TRUE),
              "Threshold_mediano" = median(threshold_final,na.rm = TRUE))
  
  df_total <- df_total %>%
    mutate(segmento_receita = case_when(
      Threshold_mediano <= 10 ~ 10,
      Threshold_mediano > 10 & Threshold_mediano <= 20 ~ 20,
      Threshold_mediano > 20 & Threshold_mediano <= 30 ~ 30,
      Threshold_mediano > 30 & Threshold_mediano <= 40 ~ 40,
      Threshold_mediano > 40 & Threshold_mediano <= 50 ~ 50,
      Threshold_mediano > 50 & Threshold_mediano <= 60 ~ 60,
      Threshold_mediano > 60 & Threshold_mediano <= 70 ~ 70,
      Threshold_mediano > 70 & Threshold_mediano <= 80 ~ 80,
      Threshold_mediano > 80 & Threshold_mediano <= 90 ~ 90,
      Threshold_mediano > 90 & Threshold_mediano <= 100 ~ 100,
      Threshold_mediano > 100 & Threshold_mediano <= 110 ~ 110,
      Threshold_mediano > 110 & Threshold_mediano <= 120 ~ 120,
      Threshold_mediano > 120 & Threshold_mediano <= 130 ~ 130,
      Threshold_mediano > 130 & Threshold_mediano <= 140 ~ 140,
      Threshold_mediano > 140 & Threshold_mediano <= 150 ~ 150,
      Threshold_mediano > 150 & Threshold_mediano <= 160 ~ 160,
      Threshold_mediano > 160 & Threshold_mediano <= 170 ~ 170,
      Threshold_mediano > 170 & Threshold_mediano <= 180 ~ 180,
      Threshold_mediano > 180 & Threshold_mediano <= 190 ~ 190,
      Threshold_mediano > 190 & Threshold_mediano <= 200 ~ 200,
      Threshold_mediano > 200 & Threshold_mediano <= 210 ~ 210,
      Threshold_mediano > 210 & Threshold_mediano <= 220 ~ 220,
      Threshold_mediano > 220 & Threshold_mediano <= 230 ~ 230,
      Threshold_mediano > 230 & Threshold_mediano <= 240 ~ 240,
      Threshold_mediano > 240 & Threshold_mediano <= 250 ~ 250,
      Threshold_mediano > 250 & Threshold_mediano <= 260 ~ 260,
      Threshold_mediano > 260 & Threshold_mediano <= 270 ~ 270,
      Threshold_mediano > 270 & Threshold_mediano <= 280 ~ 280,
      Threshold_mediano > 280 & Threshold_mediano <= 290 ~ 290,
      Threshold_mediano > 290 & Threshold_mediano <= 300 ~ 300
    ))
  
# Define a UI
ui <- fluidPage(
  titlePanel("Análise de Receita e Margem"),
  sidebarLayout(
    sidebarPanel(
      selectInput("uf_nova", "Selecione a UF:",
                  choices = unique(df$uf_nova)),
      selectInput("localidade", "Selecione a Localidade:",
                  choices = unique(df$localidade)),
      sliderInput("churn", "Percentual de Churn:",
                  min = 0, max = 1, value = 0.7, step = 0.05),
      sliderInput("novo_threshold", "Novo Threshold:",
                  min = 10, max = 300, value = 100, step = 10)
    ),
    mainPanel(
      plotlyOutput("histograma"),
      tableOutput("simulacao")
    )
  )
)

# Define a server logic
server <- function(input, output){

  
  output$histograma <- renderPlotly({
    ###define altura da barra
    novo_threshold <- input$novo_threshold
    
    uf_nova_selecionada <- input$uf_nova
    
    localidade_selecionada <- input$localidade
    
    threshold_praticado <- df_total %>%
      filter(uf_nova == uf_nova_selecionada, localidade == localidade_selecionada) %>%
      pull('Threshold_mediano')
    ticket_mediano_regiao <- df_total %>%
      filter(uf_nova == uf_nova_selecionada, localidade == localidade_selecionada) %>%
      pull('Ticket Mediano')
    ticket_medio_regiao <- df_total %>%
      filter(uf_nova == uf_nova_selecionada, localidade == localidade_selecionada) %>%
      pull('Ticket Médio')
    
    ###define altura da barra
    df2 <- df %>% filter(receita_bruta_pedido <=300,
                         data_pedido >= '2023-08-01',uf_nova == uf_nova_selecionada,localidade == localidade_selecionada)
    #altura barra
    y_barra <- max(table(cut(df2$receita_bruta_pedido,seq(min(df2$receita_bruta_pedido),max(df2$receita_bruta_pedido),dist(range(df2$receita_bruta_pedido))/30)))) + 100
    
      ggplotly(
      df %>%
        filter(data_pedido >= '2023-08-01', receita_bruta_pedido <= 300, uf_nova == uf_nova_selecionada, localidade == localidade_selecionada) %>%
        ggplot(aes(x = receita_bruta_pedido)) +
        geom_histogram(binwidth = 10, fill = 'purple',color = "black") +
        geom_vline(aes(xintercept = threshold_praticado, color = "Threshold Praticado"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = novo_threshold, color = "Novo Threshold"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = ticket_mediano_regiao, color = "Ticket Mediano Região"), linetype = "solid", size = 1) +
        geom_vline(aes(xintercept = ticket_medio_regiao, color = "Ticket Médio Região"), linetype = "solid", size = 1) +
        labs(title = "Histograma com Linhas Verticais para Thresholds",
             x = "Valor",
             y = "Contagem",
             color = "Legenda") + geom_segment(aes(x=threshold_praticado,xend=input$novo_threshold,y=y_barra,yend=y_barra))+
        scale_color_manual(name = "Legenda",
                           values = c("Threshold Praticado" = "red",
                                      "Novo Threshold" = "green",
                                      "Ticket Mediano Região" = "yellow",
                                      "Ticket Médio Região" = "blue"),
                           labels = c("Threshold Praticado" = "Threshold Praticado",
                                      "Novo Threshold" = "Novo Threshold",
                                      "Ticket Mediano Região" = "Ticket Mediano Região",
                                      "Ticket Médio Região" = "Ticket Médio Região")) +
        
        theme(legend.position = "bottom"))
  })
  
  output$simulacao <- renderTable({ 
    novo_threshold <- input$novo_threshold
    
    uf_nova_selecionada <- input$uf_nova
    
    localidade_selecionada <- input$localidade
    
    margem_faixa <- df %>%  filter(data_pedido >= '2023-08-01',uf_nova == uf_nova_selecionada,
                                   localidade == localidade_selecionada,
                                   receita_bruta_pedido <=300) %>%
      group_by(uf_nova,localidade,segmento_receita) %>%
      summarise("Vendas" = n_distinct(id_pedido),
                'Margem Ajustada' = sum(margem_2_ajustada,na.rm = TRUE),
                'Margem Recomposta' = sum(margem_recomposta,na.rm = TRUE),
                "Margem Percentual" = round(sum(margem_2_ajustada,na.rm = TRUE)/sum(receita_liquida_operacional,na.rm = TRUE),2),
                "Receita Bruta Total" = sum(receita_bruta_pedido,na.rm = TRUE),
                "Receita Liquida Operacional" = sum(receita_liquida_operacional,na.rm = TRUE),
                "Ticket Médio" = mean(receita_bruta_pedido,na.rm = TRUE),
                "Ticket Mediano" = median(receita_bruta_pedido,na.rm = TRUE))
    receita_total <- df_total %>%
      filter(uf_nova == uf_nova_selecionada, localidade == localidade_selecionada) %>%
      pull('Receita Bruta Total')
    cash_margin_total <- df_total %>%
      filter(uf_nova == uf_nova_selecionada, localidade == localidade_selecionada) %>%
      pull('Margem Ajustada')

    receita_total <- df_total %>%
      filter(uf_nova == uf_nova_selecionada, localidade == localidade_selecionada) %>%
      pull('Receita Bruta Total')
    total_pedidos <- df_total %>%
      filter(uf_nova == uf_nova_selecionada, localidade == localidade_selecionada) %>%
      pull('Vendas')
    threshold_praticado <- df_total %>%
      filter(uf_nova == uf_nova_selecionada, localidade == localidade_selecionada) %>%
      pull('Threshold_mediano')
    pedidos_abaixo_threshold <- df %>%
      filter(data_pedido >= '2023-08-01',uf_nova == uf_nova_selecionada,
             localidade == localidade_selecionada,
             receita_bruta_pedido<threshold_praticado) %>%
      summarise(pedidos = n_distinct(id_pedido))  %>% pull(pedidos)
    #
    pedidos_entre_thresholds = df %>% filter(receita_bruta_pedido >threshold_praticado,
                                             receita_bruta_pedido<novo_threshold,
                                             data_pedido >= '2023-08-01',
                                             uf_nova == uf_nova_selecionada,
                                             localidade == localidade_selecionada) %>%
      summarise(pedidos = n_distinct(id_pedido)) %>% pull(pedidos)
    #
    #
    ticket_mediano_regiao <- df_total %>%
      filter(uf_nova == uf_nova_selecionada, localidade == localidade_selecionada) %>%
      pull('Ticket Mediano')
    #
    ticket_medio_regiao <- df_total %>%
      filter(uf_nova == uf_nova_selecionada, localidade == localidade_selecionada) %>%
      pull('Ticket Médio')
    #
    receita_entre_thresholds = df %>%
      filter(receita_bruta_pedido >threshold_praticado,receita_bruta_pedido<novo_threshold,
             data_pedido >= '2023-08-01',uf_nova == uf_nova_selecionada,localidade == localidade_selecionada) %>%
      summarise(receita = sum(receita_bruta_pedido)) %>% pull(receita)
    #
    percentual_receita_entre_thresholds <- round(receita_entre_thresholds/receita_total*100,2)
    #
    # #### Resultado Simulação
    # #total de pedidos abaixo do threshold atual
    percentual_fica <- pedidos_abaixo_threshold/total_pedidos
    #
    # #Pessoas que compram mesmo com frete ( numero fixo baseado nos dados atuais)
    pedidos_fica <- round(percentual_fica*pedidos_entre_thresholds,0)
    #
    # #percentual de churn : pessoas que vao embora ( o maximo é 1 - percentual_fica)
    # ## carrinho não converte
    churn <-  input$churn ## usuário da de input
    #
    pedidos_saem <- round(pedidos_entre_thresholds*churn,0)
    #
    # ### Percentual aumenta ticket medio para conseguir frete
    aumentam_ticket <- 1 - percentual_fica - churn
    pedidos_aumenta <- pedidos_entre_thresholds- pedidos_saem - pedidos_fica
    #

    ## Receita e Margem que saem:
    ## O total de clientes churn indica a receita e margem que perdemos
    ## pois os mesmos não vao aumentar o ticket médio



    ## Receita e Margem que aumenta:
    ## Os pedidos que que aumentam, consequentemente
    ## vão para ticket medio = threshold_mediano
    ## e margem_percentual do ticket_mediano
    ## Logo, vai ter um aumento na receita de  ticket_novo - ticket_antigo
    ## e aumento no cash margin de
    ## %percentual antiga * ticket antigo - margem nova * ticket novo



    # # Cria grupos aleatórios com base nos tamanhos calculados
    df_entre_thresholds <- df %>% filter(receita_bruta_pedido >threshold_praticado,
                                         receita_bruta_pedido<novo_threshold,
                                         data_pedido >= '2023-08-01',
                                         uf_nova == uf_nova_selecionada,
                                         localidade == localidade_selecionada)
    #
    grupo_churn <- rep("Churn", pedidos_saem)
    grupo_fica <- rep("Fica", pedidos_fica)
    grupo_aumenta_ticket <- rep("Aumenta Ticket", pedidos_aumenta)
    
    
    # Junta os grupos em um único vetor
    grupos <- c(grupo_churn, grupo_fica, grupo_aumenta_ticket)
    
    # Embaralha o vetor para obter grupos aleatórios
    grupos_aleatorios <- sample(grupos)
    
    # Adiciona a marcação de grupo ao dataframe
    df_entre_thresholds$grupo <- grupos_aleatorios
    #Nova margem para os que aumentam ticket:
    margem_aumenta <-  margem_faixa %>%
      filter(segmento_receita ==classificar_numero(novo_threshold)) %>%
      pull(`Margem Percentual`)
    df_entre_thresholds <- df_entre_thresholds %>%
      mutate("Nova Receita" = case_when(grupo == 'Churn' ~ 0,
                                        grupo == 'Fica' ~ receita_bruta_pedido,
                                        TRUE~novo_threshold),
             "Novo Cash Margin" =case_when(grupo == 'Churn' ~ 0,
                                           grupo == 'Fica' ~ margem_2_ajustada,
                                           TRUE~ margem_2_ajustada + (novo_threshold - receita_bruta_pedido)*margem_aumenta) ## não sei se essa é a melhor forma
      )
    df_entre_thresholds %>%
      select(id_pedido,segmento_receita,receita_bruta_pedido,`Nova Receita`,margem_2_ajustada,`Novo Cash Margin`,grupo) 
    
    
    ##Vamos calculadora os aumentos e diminuições de receita na tabela final
    df_final <- df_entre_thresholds %>% group_by(uf_nova,localidade) %>%
      summarise('Total Receita do Intervalo Antes'  = sum(receita_bruta_pedido),
                'Total Receita do Intervalo Depois' = sum(`Nova Receita`),
                'Diferença Total da Receita' = sum(`Nova Receita`) -sum(receita_bruta_pedido),
                'Total Cash Margin do Intervalo Antes'  = sum(margem_2_ajustada),
                'Total Cash Margin do Intervalo Depois' = sum(`Novo Cash Margin`),
                'Diferença Cash Margin' = sum(`Novo Cash Margin`) - sum(margem_2_ajustada)) %>% 
      mutate('Diferença Receita do Intervalo (%)' = round(`Diferença Total da Receita`/`Total Receita do Intervalo Antes` * 100,2),
             'Diferença Receita Geral (%)' = round(`Diferença Total da Receita`/receita_total * 100,2),
             'Diferença Cash Margin do Intervalo (%)' = round(`Diferença Cash Margin`/abs(`Total Cash Margin do Intervalo Antes`) * 100,2),
             'Diferença Cash Margin Geral (%)' = round((`Diferença Cash Margin`)/abs(cash_margin_total) * 100,2))
    df_final %>% mutate('Pedidos Mantidos (%)' = round(100*percentual_fica,2)) %>% 
      select(uf_nova,localidade,`Pedidos Mantidos (%)`,`Diferença Total da Receita`,`Diferença Cash Margin`,`Diferença Receita do Intervalo (%)`,`Diferença Receita Geral (%)`,`Diferença Cash Margin do Intervalo (%)`,`Diferença Cash Margin Geral (%)`)
    
    })
}
# Roda o app Shiny
shinyApp(ui = ui, server = server)


