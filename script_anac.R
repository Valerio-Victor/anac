# PACOTES: ----------------------------------------------------------------
library(magrittr)
library(ggplot2)


# IMPORTAÇÃO DOS DADOS: ---------------------------------------------------
url <- 'https://sistemas.anac.gov.br/dadosabertos/Voos%20e%20opera%C3%A7%C3%B5es%20a%C3%A9reas/Dados%20Estat%C3%ADsticos%20do%20Transporte%20A%C3%A9reo/Dados_Estatisticos.csv'


base <- readr::read_delim(url,
                          delim = ";",
                          escape_double = FALSE,
                          trim_ws = TRUE,
                          skip = 1)


# PARÂMETROS: -------------------------------------------------------------
cor_azul <- '#367fa9'
cor_vermelho <- '#B95D56'
cor_cinza <- '#9CADBC'
intensidade_carbono_qav <- 3.08

# ARRUMAÇÃO DOS DADOS: ----------------------------------------------------
dados_anac <- base %>%
  janitor::clean_names() %>%
  dplyr::filter(empresa_nacionalidade == 'BRASILEIRA') %>%
  dplyr::select(ano,
                mes,
                empresa_nome,
                aeroporto_de_origem_nome,
                aeroporto_de_origem_uf,
                aeroporto_de_origem_regiao,
                combustivel_litros,
                passageiros_pagos,
                passageiros_gratis,
                carga_paga_kg,
                carga_gratis_kg) %>%
  dplyr::transmute(ano = ano,
                   mes = mes,
                   empresa_nome = empresa_nome,
                   aeroporto_de_origem_nome = aeroporto_de_origem_nome,
                   aeroporto_de_origem_uf = aeroporto_de_origem_uf,
                   aeroporto_de_origem_regiao = aeroporto_de_origem_regiao,
                   combustivel_litros = combustivel_litros,
                   passageiro = passageiros_pagos + passageiros_gratis,
                   carga = carga_paga_kg + carga_gratis_kg) %>%
  dplyr::mutate(voo = 1) %>%
  tidyr::drop_na()


# TRATAMENTO DOS DADOS: ---------------------------------------------------
total_ts <- dados_anac %>%
  dplyr::group_by(ano, mes) %>%
  dplyr::summarise(voo = sum(voo),
                   combustivel_litros = sum(combustivel_litros),
                   passageiro = sum(passageiro)) %>%
  dplyr::mutate(data = as.Date(paste0(ano, "-", mes, "-01"))) %>%
  dplyr::ungroup() %>%
  dplyr::select(-ano, -mes) %>%
  dplyr::arrange(data) %>%
  dplyr::mutate(voo_acum = cumsum(voo),
                combustivel_litros_acum = cumsum(combustivel_litros),
                passageiro_acum = cumsum(passageiro)) %>%
  dplyr::mutate(emissao_carbono = combustivel_litros * intensidade_carbono_qav,
                emissao_carbono_acum = combustivel_litros_acum * intensidade_carbono_qav)


aeroporto_ts <- dados_anac %>%
  dplyr::group_by(ano, mes, aeroporto_de_origem_nome) %>%
  dplyr::summarise(voo = sum(voo),
                   combustivel_litros = sum(combustivel_litros),
                   passageiro = sum(passageiro)) %>%
  dplyr::mutate(data = as.Date(paste0(ano, "-", mes, "-01"))) %>%
  dplyr::ungroup() %>%
  dplyr::select(-ano, -mes) %>%
  dplyr::arrange(data) %>%
  dplyr::mutate(voo_acum = cumsum(voo),
                combustivel_litros_acum = cumsum(combustivel_litros),
                passageiro_acum = cumsum(passageiro)) %>%
  dplyr::mutate(emissao_carbono = combustivel_litros * intensidade_carbono_qav,
                emissao_carbono_acum = combustivel_litros_acum * intensidade_carbono_qav)


estado_ts <- dados_anac %>%
  dplyr::group_by(ano, mes, aeroporto_de_origem_uf) %>%
  dplyr::summarise(voo = sum(voo),
                   combustivel_litros = sum(combustivel_litros),
                   passageiro = sum(passageiro)) %>%
  dplyr::mutate(data = as.Date(paste0(ano, "-", mes, "-01"))) %>%
  dplyr::ungroup() %>%
  dplyr::select(-ano, -mes) %>%
  dplyr::arrange(data) %>%
  dplyr::mutate(voo_acum = cumsum(voo),
                combustivel_litros_acum = cumsum(combustivel_litros),
                passageiro_acum = cumsum(passageiro)) %>%
  dplyr::mutate(emissao_carbono = combustivel_litros * intensidade_carbono_qav,
                emissao_carbono_acum = combustivel_litros_acum * intensidade_carbono_qav)


regiao_ts <- dados_anac %>%
  dplyr::group_by(ano, mes, aeroporto_de_origem_regiao) %>%
  dplyr::summarise(voo = sum(voo),
                   combustivel_litros = sum(combustivel_litros),
                   passageiro = sum(passageiro)) %>%
  dplyr::mutate(data = as.Date(paste0(ano, "-", mes, "-01"))) %>%
  dplyr::ungroup() %>%
  dplyr::select(-ano, -mes) %>%
  dplyr::arrange(data) %>%
  dplyr::mutate(voo_acum = cumsum(voo),
                combustivel_litros_acum = cumsum(combustivel_litros),
                passageiro_acum = cumsum(passageiro)) %>%
  dplyr::mutate(emissao_carbono = combustivel_litros * intensidade_carbono_qav,
                emissao_carbono_acum = combustivel_litros_acum * intensidade_carbono_qav)


empresa_ts <- dados_anac %>%
  dplyr::group_by(ano, mes, empresa_nome) %>%
  dplyr::summarise(voo = sum(voo),
                   combustivel_litros = sum(combustivel_litros),
                   passageiro = sum(passageiro)) %>%
  dplyr::mutate(data = as.Date(paste0(ano, "-", mes, "-01"))) %>%
  dplyr::ungroup() %>%
  dplyr::select(-ano, -mes) %>%
  dplyr::arrange(data) %>%
  dplyr::mutate(voo_acum = cumsum(voo),
                combustivel_litros_acum = cumsum(combustivel_litros),
                passageiro_acum = cumsum(passageiro)) %>%
  dplyr::mutate(emissao_carbono = combustivel_litros * intensidade_carbono_qav,
                emissao_carbono_acum = combustivel_litros_acum * intensidade_carbono_qav)


# VISUALIZAÇÃO DOS DADOS: -------------------------------------------------



