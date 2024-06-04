library(dplyr)
library(bigrquery)
library(ggplot2)
library(tidyr)
library(shiny)
library(gridExtra)
library(plotly)
projectid = "petlove-dataeng-prod-01"
query <- "WITH teste AS (
  SELECT
     abt.alternative
    , abt.order_number
    , abt.changed_outcome
    , abt.created_at
    , order_total
    , chosen_shipping_method
    , completed_at
    , ROW_NUMBER() OVER (PARTITION BY order_number ORDER BY CASE WHEN completed_at IS NOT NULL THEN 0 ELSE 1 END, combination_id DESC) AS rk
  FROM `petlove-dataeng-prod-01.curated_bulldog.ab_test_outcomes` abt
  WHERE 1=1
    AND abt.refdate >= '2024-01-01'
    AND abt.name = 'teste_dryrun_curtiba_v3_CMP'
    AND order_total > 0),   

margem AS (
  SELECT
    data_pedido,
    id_pedido,
    SUM(receita_bruta_total)   AS receita_bruta_total,
    SUM(margem_2)              AS margem_2,
    SUM(receita_bruta_produto) AS receita_bruta_produto,
    SUM(receita_bruta_frete)   AS receita_bruta_frete
  FROM `petlove-dataeng-prod-01.dw_corporativo.dts_margem_conciliacao`
  WHERE data_emissao_nota >= '2024-03-01'
  GROUP BY 1,2
)

SELECT
  DATE_TRUNC(created_at, DAY)      AS dia,
  order_number,
  alternative,
  id_pedido,
  completed_at,
  margem.receita_bruta_frete,
  margem.receita_bruta_produto,
  margem.receita_bruta_total,
  order_total,
  chosen_shipping_method
FROM teste
LEFT JOIN margem
  ON teste.order_number = margem.id_pedido
WHERE rk = 1"

tb <- bq_project_query(projectid,query)
df = bq_table_download(tb) %>% data.frame()

df$Faturado <- ifelse(is.na(df$id_pedido), "Não Faturado", "Faturado")


# Análise qualidade dos dados

# Metricas / Grupo

df %>% group_by(alternative) %>%
  summarise("Total Pedidos" = n_distinct(order_number),
            "Total Faturado" = sum(Faturado=='Faturado'),
            "Percentual Faturado" = round(sum(Faturado=='Faturado')/n()*100,2),
            "Ticket Médio" = round(mean(receita_bruta_produto,na.rm = T),2),
            "Ticket Mediano" = round(median(receita_bruta_produto,na.rm = T),2)) %>% View()


# Gráficos de Distribuição
# Criando o gráfico de densidade
ggplot(df, aes(x = receita_bruta_produto, fill = alternative)) +
  geom_density(alpha = 0.5) +
  labs(x = "Receita Bruta do Produto", y = "Densidade") +
  theme_minimal()




# Criando o gráfico conjunto
p_conjunto <- ggplot(df, aes(x = receita_bruta_produto, fill = alternative)) +
  geom_density(alpha = 0.5) +
  labs(x = "Receita Bruta do Produto", y = "Densidade") +
  theme_minimal()

# Criando gráficos separados para cada grupo de alternative
p_grupos <- ggplot(df, aes(x = receita_bruta_produto, fill = alternative)) +
  geom_density(alpha = 0.5) +
  labs(x = "Receita Bruta do Produto", y = "Densidade") +
  theme_minimal() +
  facet_wrap(~alternative)

# Combinando os gráficos em um painel
grid.arrange(p_conjunto, p_grupos, nrow = 2)


# Teste T
df_clean <- df[!is.na(df$receita_bruta_produto), ]

# Dividindo o dataframe limpo em subconjuntos para cada grupo
grupo_controle_1 <- df_clean[df_clean$alternative == "teste_dryrun_curtiba_v3_79", "receita_bruta_produto"]
grupo_controle_2 <- df_clean[df_clean$alternative == "teste_dryrun_curtiba_v3_controle_79", "receita_bruta_produto"]
grupo_teste <- df_clean[df_clean$alternative == "teste_dryrun_curtiba_v3_80", "receita_bruta_produto"]

# Teste t assumindo distribuição normal
t_test_normal <- t.test(grupo_controle_1, grupo_controle_2, paired = FALSE)
t_test_normal2 <- t.test(grupo_controle_1, grupo_teste, paired = FALSE)
t_test_normal3 <- t.test(grupo_controle_2, grupo_teste, paired = FALSE)

# Teste t sem assumir distribuição normal



# Realizado os testes
t_test_not_normal <- t.test(grupo_controle_1, grupo_controle_2, paired = FALSE, var.equal = FALSE)
t_test_not_normal2 <- t.test(grupo_controle_1, grupo_teste, paired = FALSE, var.equal = FALSE)
t_test_not_normal3 <- t.test(grupo_controle_2, grupo_teste, paired = FALSE, var.equal = FALSE)

# Exibindo os resultados
print("Teste t assumindo distribuição normal:")
print(t_test_normal)
print(t_test_normal2)
print(t_test_normal3)
print("")

print("Teste t sem assumir distribuição normal:")
print(t_test_not_normal)
print(t_test_not_normal2)
print(t_test_not_normal3)



# Ordenando as amostras:
grupo_controle_1 <- sort(grupo_controle_1)
grupo_controle_2 <- sort(grupo_controle_2)
grupo_teste <- sort(grupo_teste)

# Realizando o teste U de Mann-Whitney para cada par de grupos
test_group1_group2 <- wilcox.test(grupo_controle_1, grupo_controle_2)
test_group1_group3 <- wilcox.test(grupo_controle_1, grupo_teste)
test_group2_group3 <- wilcox.test(grupo_controle_2, grupo_teste)

# Exibindo os resultados
print("Teste U de Mann-Whitney para cada par de grupos:")
print(test_group1_group2)
print(test_group1_group3)
print(test_group2_group3)
