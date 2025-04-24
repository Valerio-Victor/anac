'# PACOTES: ----------------------------------------------------------------
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
filtro_aeroporto <- 'GUARULHOS'
filtro_estado <- 'SP'
filtro_empresa <- 'GOL LINHAS AÉREAS S.A. (EX- VRG LINHAS AÉREAS S.A.)'

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
                   passageiro = sum(passageiro),
                   carga = sum(carga)) %>%
  dplyr::mutate(data = as.Date(paste0(ano, "-", mes, "-01"))) %>%
  dplyr::ungroup() %>%
  dplyr::select(-ano, -mes) %>%
  dplyr::arrange(data) %>%
  dplyr::mutate(voo_acum = cumsum(voo),
                combustivel_litros_acum = cumsum(combustivel_litros)) %>%
  dplyr::mutate(emissao_carbono = combustivel_litros * intensidade_carbono_qav,
                emissao_carbono_acum = combustivel_litros_acum/1000 * intensidade_carbono_qav)


total_ts_simples <- total_ts %>%
  dplyr::select(data, voo_acum, emissao_carbono_acum) %>%
  dplyr::rename('voo_acum_total' = voo_acum,
                'emissao_carbono_acum_total' = emissao_carbono_acum)


aeroporto_ts <- dados_anac %>%
  dplyr::mutate(data = as.Date(paste0(ano, "-", mes, "-01"))) %>%
  dplyr::group_by(data, aeroporto_de_origem_nome) %>%
  dplyr::summarise(voo = sum(voo),
                   combustivel_litros = sum(combustivel_litros)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(aeroporto_de_origem_nome) %>%
  dplyr::mutate(combustivel_litros_acum = cumsum(combustivel_litros),
                voo_acum = cumsum(voo)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(data) %>%
  dplyr::mutate(emissao_carbono_acum = combustivel_litros_acum/1000 * intensidade_carbono_qav) %>%
  dplyr::select(data, aeroporto_de_origem_nome, voo_acum, emissao_carbono_acum) %>%
  dplyr::left_join(total_ts_simples, by = 'data') %>%
  dplyr::mutate(prop_voo = round(voo_acum/voo_acum_total,4),
                prop_emissao = round(emissao_carbono_acum/emissao_carbono_acum_total,4))


estado_ts <- dados_anac %>%
  dplyr::mutate(data = as.Date(paste0(ano, "-", mes, "-01"))) %>%
  dplyr::group_by(data, aeroporto_de_origem_uf) %>%
  dplyr::summarise(voo = sum(voo),
                   combustivel_litros = sum(combustivel_litros)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(aeroporto_de_origem_uf) %>%
  dplyr::mutate(combustivel_litros_acum = cumsum(combustivel_litros),
                voo_acum = cumsum(voo)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(data) %>%
  dplyr::mutate(emissao_carbono_acum = combustivel_litros_acum/1000 * intensidade_carbono_qav) %>%
  dplyr::select(data, aeroporto_de_origem_uf, voo_acum, emissao_carbono_acum) %>%
  dplyr::left_join(total_ts_simples, by = 'data') %>%
  dplyr::mutate(prop_voo = round(voo_acum/voo_acum_total,4),
                prop_emissao = round(emissao_carbono_acum/emissao_carbono_acum_total,4))


empresa_ts <- dados_anac %>%
  dplyr::mutate(data = as.Date(paste0(ano, "-", mes, "-01"))) %>%
  dplyr::group_by(data, empresa_nome) %>%
  dplyr::summarise(voo = sum(voo),
                   combustivel_litros = sum(combustivel_litros)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(empresa_nome) %>%
  dplyr::mutate(combustivel_litros_acum = cumsum(combustivel_litros),
                voo_acum = cumsum(voo)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(data) %>%
  dplyr::mutate(emissao_carbono_acum = combustivel_litros_acum/1000 * intensidade_carbono_qav) %>%
  dplyr::select(data, empresa_nome, voo_acum, emissao_carbono_acum) %>%
  dplyr::left_join(total_ts_simples, by = 'data') %>%
  dplyr::mutate(prop_voo = round(voo_acum/voo_acum_total,4),
                prop_emissao = round(emissao_carbono_acum/emissao_carbono_acum_total,4))

calc_aeroportos <- base %>%
  dplyr::filter(NATUREZA == 'DOMÉSTICA' & ANO == dplyr::last(ANO)) %>%
  dplyr::select(MES, AEROPORTO_DE_ORIGEM_NOME, COMBUSTIVEL_LITROS) %>%
  tidyr::drop_na() %>%
  dplyr::group_by(MES, AEROPORTO_DE_ORIGEM_NOME) %>%
  dplyr::summarise(consumo = sum(COMBUSTIVEL_LITROS)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(AEROPORTO_DE_ORIGEM_NOME) %>%
  dplyr::summarise(consumo = mean(consumo)) %>%
  dplyr::ungroup() %>%
  dplyr::rename('consumo' = consumo,
                'aeroporto' = AEROPORTO_DE_ORIGEM_NOME)


# VISUALIZAÇÃO DOS DADOS: -------------------------------------------------
# TOTAL: ------------------------------------------------------------------
#----
voo_total_graf <- total_ts %>%
  ggplot() +
  geom_line(aes(x = data,
                y = voo_acum,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Total de Voos: ',
                              scales::number(voo_acum,
                                big.mark = '.',
                                decimal.mark = ',',
                                accuracy = 1))),
            color = cor_azul) +
  geom_point(aes(x = data,
                 y = voo_acum,
                 group = 1,
                 text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                               '<br>Total de Voos: ',
                               scales::number(voo_acum,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 1))),
             color = cor_azul,
             size = 0.5) +
  scale_y_continuous(labels = scales::number_format(big.mark = '.',
                                                    decimal.mark = ',',
                                                    accuracy = 1)) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = 'Total de Voos Realizados no Brasil (Empresas Brasileiras)',
       x = '',
       y = 'Total de Voos (Acumulado)') +
  theme_minimal()


plotly::ggplotly(voo_total_graf, tooltip = 'text')


#----
emissao_total_graf <- total_ts %>%
  ggplot() +
  geom_line(aes(x = data,
                y = emissao_carbono_acum,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Emissão de CO2: ',
                              scales::number(emissao_carbono_acum,
                                big.mark = '.',
                                decimal.mark = ',',
                                accuracy = 1), ' Toneladas')),
            color = cor_vermelho) +
  geom_point(aes(x = data,
                y = emissao_carbono_acum,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Emissão de CO2: ',
                              scales::number(emissao_carbono_acum,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1), ' Toneladas')),
            color = cor_vermelho,
            size = 0.5) +
  scale_y_continuous(labels = scales::number_format(big.mark = '.',
                                                    decimal.mark = ',',
                                                    accuracy = 1)) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = 'Total de Emissões de CO2 no Brasil (Empresas Brasileiras)',
       x = '',
       y = 'Toneladas de CO2 (Acumulado)') +
  theme_minimal()

plotly::ggplotly(emissao_total_graf, tooltip = 'text')


#----
passageiro_total_graf <- total_ts %>%
  ggplot() +
  geom_line(aes(x = data,
                y = passageiro,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Total de Passageiros: ',
                              scales::number(passageiro,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1))),
            color = cor_azul) +
  geom_point(aes(x = data,
                y = passageiro,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Total de Passageiros: ',
                              scales::number(passageiro,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1))),
            color = cor_azul,
            size = 0.5) +
  scale_y_continuous(labels = scales::number_format(big.mark = '.',
                                                    decimal.mark = ',',
                                                    accuracy = 1)) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = 'Total de Passageiros Transportados no Brasil (Empresas Brasileiras)',
       x = '',
       y = 'Pessoas (Passageiros Pagos e Grátis)') +
  theme_minimal()

plotly::ggplotly(passageiro_total_graf, tooltip = 'text')


#----
carga_total_graf <- total_ts %>%
  ggplot() +
  geom_line(aes(x = data,
                y = carga/1000,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Carga Paga: ',
                              scales::number(carga/1000,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1), ' Tonelada')),
            color = cor_azul) +
  geom_point(aes(x = data,
                y = carga/1000,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Carga Paga: ',
                              scales::number(carga/1000,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1), ' Tonelada')),
            color = cor_azul,
            size = 0.5) +
  scale_y_continuous(labels = scales::number_format(big.mark = '.',
                                                    decimal.mark = ',',
                                                    accuracy = 1)) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = 'Total de Carga Transportada no Brasil (Empresas Brasileiras)',
       x = '',
       y = 'Tonelada (Carga Paga e Grátis)') +
  theme_minimal()

plotly::ggplotly(carga_total_graf, tooltip = 'text')



# ESTADOS: ----------------------------------------------------------------
#----
voo_estado_graf <- estado_ts %>%
  dplyr::filter(aeroporto_de_origem_uf == filtro_estado) %>%
  ggplot() +
  geom_line(aes(x = data,
                y = voo_acum,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Total de Voos: ',
                              scales::number(voo_acum,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1))),
            color = cor_azul) +
  geom_point(aes(x = data,
                 y = voo_acum,
                 group = 1,
                 text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                               '<br>Total de Voos: ',
                               scales::number(voo_acum,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 1))),
             color = cor_azul,
             size = 0.5) +
  scale_y_continuous(labels = scales::number_format(big.mark = '.',
                                                    decimal.mark = ',',
                                                    accuracy = 1)) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = paste0('Total de Voos Realizados no Estado de ', filtro_estado),
       x = '',
       y = 'Total de Voos (Acumulado)') +
  theme_minimal()

plotly::ggplotly(voo_estado_graf, tooltip = 'text')


#----
voo_estado_prop_graf <- estado_ts %>%
  dplyr::filter(aeroporto_de_origem_uf == filtro_estado) %>%
  ggplot() +
  geom_line(aes(x = data,
                y = prop_voo,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Proporção de Voos: ',
                              scales::percent(prop_voo))),
            color = cor_azul) +
  geom_point(aes(x = data,
                y = prop_voo,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Proporção de Voos: ',
                              scales::percent(prop_voo))),
            color = cor_azul,
            size = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = paste0('Proporção de Voos Realizados no Estado de ', filtro_estado),
       x = '',
       y = 'Proporção de Voos') +
  theme_minimal()

plotly::ggplotly(voo_estado_prop_graf, tooltip = 'text')


#----
emissao_estado_graf <- estado_ts %>%
  dplyr::filter(aeroporto_de_origem_uf == filtro_estado) %>%
  ggplot() +
  geom_line(aes(x = data,
                y = emissao_carbono_acum,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Emissão de CO2: ',
                              scales::number(emissao_carbono_acum,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1), ' Toneladas')),
            color = cor_vermelho) +
  geom_point(aes(x = data,
                y = emissao_carbono_acum,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Emissão de CO2: ',
                              scales::number(emissao_carbono_acum,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1), ' Toneladas')),
            color = cor_vermelho,
            size = 0.5) +
  scale_y_continuous(labels = scales::number_format(big.mark = '.',
                                                    decimal.mark = ',',
                                                    accuracy = 1)) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = paste0('Total de Emissões de CO2 no Estado de ', filtro_estado),
       x = '',
       y = 'Toneladas de CO2 (Acumulado)') +
  theme_minimal()

plotly::ggplotly(emissao_estado_graf, tooltip = 'text')


#----
emissao_estado_prop_graf <- estado_ts %>%
  dplyr::filter(aeroporto_de_origem_uf == filtro_estado) %>%
  ggplot() +
  geom_line(aes(x = data,
                y = prop_emissao,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Proporção de Emissão de CO2: ',
                              scales::percent(prop_emissao))),
            color = cor_vermelho) +
  geom_point(aes(x = data,
                y = prop_emissao,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Proporção de Emissão de CO2: ',
                              scales::percent(prop_emissao))),
            color = cor_vermelho,
            size = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = paste0('Proporção de Emissões de CO2 no Estado de ', filtro_estado),
       x = '',
       y = 'Proporção de Emissão de CO2') +
  theme_minimal()

plotly::ggplotly(emissao_estado_prop_graf, tooltip = 'text')


# AEROPORTOS: -------------------------------------------------------------
#----
voo_aeroporto_graf <- aeroporto_ts %>%
  dplyr::filter(aeroporto_de_origem_nome == filtro_aeroporto) %>%
  ggplot() +
  geom_line(aes(x = data,
                y = voo_acum,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Total de Voos: ',
                              scales::number(voo_acum,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1))),
            color = cor_azul) +
  geom_point(aes(x = data,
                y = voo_acum,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Total de Voos: ',
                              scales::number(voo_acum,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1))),
            color = cor_azul,
            size = 0.5) +
  scale_y_continuous(labels = scales::number_format(big.mark = '.',
                                                    decimal.mark = ',',
                                                    accuracy = 1)) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = paste0('Total de Voos Realizados no Aeroporto de ', filtro_aeroporto),
       x = '',
       y = 'Total de Voos (Acumulado)') +
  theme_minimal()

plotly::ggplotly(voo_aeroporto_graf, tooltip = 'text')


#----
voo_aeroporto_prop_graf <- aeroporto_ts %>%
  dplyr::filter(aeroporto_de_origem_nome == filtro_aeroporto) %>%
  ggplot() +
  geom_line(aes(x = data,
                y = prop_voo,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Proporção de Voos: ',
                              scales::percent(prop_voo))),
            color = cor_azul) +
  geom_point(aes(x = data,
                y = prop_voo,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Proporção de Voos: ',
                              scales::percent(prop_voo))),
            color = cor_azul,
            size = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = paste0('Proporção de Voos Realizados no Aeroporto de ', filtro_aeroporto),
       x = '',
       y = 'Proporção de Voos') +
  theme_minimal()

plotly::ggplotly(voo_aeroporto_prop_graf, tooltip = 'text')


#----
emissao_aeroporto_graf <- aeroporto_ts %>%
  dplyr::filter(aeroporto_de_origem_nome == filtro_aeroporto) %>%
  ggplot() +
  geom_line(aes(x = data,
                y = emissao_carbono_acum,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Emissão de CO2: ',
                              scales::number(emissao_carbono_acum,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1), ' Toneladas')),
            color = cor_vermelho) +
  geom_point(aes(x = data,
                 y = emissao_carbono_acum,
                 group = 1,
                 text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                               '<br>Emissão de CO2: ',
                               scales::number(emissao_carbono_acum,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 1), ' Toneladas')),
             color = cor_vermelho,
             size = 0.5) +
  scale_y_continuous(labels = scales::number_format(big.mark = '.',
                                                    decimal.mark = ',',
                                                    accuracy = 1)) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = paste0('Total de Emissões de CO2 no Aeroporto de ', filtro_aeroporto),
       x = '',
       y = 'Toneladas de CO2 (Acumulado)') +
  theme_minimal()

plotly::ggplotly(emissao_aeroporto_graf, tooltip = 'text')


#----
emissao_aeroporto_prop_graf <- aeroporto_ts %>%
  dplyr::filter(aeroporto_de_origem_nome == filtro_aeroporto) %>%
  ggplot() +
  geom_line(aes(x = data,
                y = prop_emissao,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Proporção de Emissão de CO2: ',
                              scales::percent(prop_emissao))),
            color = cor_vermelho) +
  geom_point(aes(x = data,
                 y = prop_emissao,
                 group = 1,
                 text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                               '<br>Proporção de Emissão de CO2: ',
                               scales::percent(prop_emissao))),
             color = cor_vermelho,
             size = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = paste0('Proporção de Emissões de CO2 no Aeroporto de ', filtro_aeroporto),
       x = '',
       y = 'Proporção de Emissão de CO2') +
  theme_minimal()

plotly::ggplotly(emissao_aeroporto_prop_graf, tooltip = 'text')


# COMPANHIAS: -------------------------------------------------------------
#----
voo_empresa_graf <- empresa_ts %>%
  dplyr::filter(empresa_nome == filtro_empresa) %>%
  ggplot() +
  geom_line(aes(x = data,
                y = voo_acum,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Total de Voos: ',
                              scales::number(voo_acum,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1))),
            color = cor_azul) +
  geom_point(aes(x = data,
                y = voo_acum,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Total de Voos: ',
                              scales::number(voo_acum,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1))),
            color = cor_azul,
            size = 0.5) +
  scale_y_continuous(labels = scales::number_format(big.mark = '.',
                                                    decimal.mark = ',',
                                                    accuracy = 1)) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = paste0('Total de Voos Realizados pela Companhia<br>', filtro_empresa),
       x = '',
       y = 'Total de Voos (Acumulado)') +
  theme_minimal()

plotly::ggplotly(voo_empresa_graf, tooltip = 'text')


#----
voo_empresa_prop_graf <- empresa_ts %>%
  dplyr::filter(empresa_nome == filtro_empresa) %>%
  ggplot() +
  geom_line(aes(x = data,
                y = prop_voo,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Proporção de Voos: ',
                              scales::percent(prop_voo))),
            color = cor_azul) +
  geom_point(aes(x = data,
                y = prop_voo,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Proporção de Voos: ',
                              scales::percent(prop_voo))),
            color = cor_azul,
            size = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = paste0('Proporção de Voos Realizados pela Companhia<br>', filtro_empresa),
       x = '',
       y = 'Proporção de Voos') +
  theme_minimal()

plotly::ggplotly(voo_empresa_prop_graf, tooltip = 'text')


#----
emissao_empresa_graf <- empresa_ts %>%
  dplyr::filter(empresa_nome == filtro_empresa) %>%
  ggplot() +
  geom_line(aes(x = data,
                y = emissao_carbono_acum,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Emissão de CO2: ',
                              scales::number(emissao_carbono_acum,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1), ' Toneladas')),
            color = cor_vermelho) +
  geom_point(aes(x = data,
                y = emissao_carbono_acum,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Emissão de CO2: ',
                              scales::number(emissao_carbono_acum,
                                             big.mark = '.',
                                             decimal.mark = ',',
                                             accuracy = 1), ' Toneladas')),
            color = cor_vermelho,
            size = 0.5) +
  scale_y_continuous(labels = scales::number_format(big.mark = '.',
                                                    decimal.mark = ',',
                                                    accuracy = 1)) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = paste0('Total de Emissões de CO2 pela Companhia<br>', filtro_empresa),
       x = '',
       y = 'Toneladas de CO2 (Acumulado)') +
  theme_minimal()

plotly::ggplotly(emissao_empresa_graf, tooltip = 'text')


#----
emissao_empresa_prop_graf <- empresa_ts %>%
  dplyr::filter(empresa_nome == filtro_empresa) %>%
  ggplot() +
  geom_line(aes(x = data,
                y = prop_emissao,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Proporção de Emissão de CO2: ',
                              scales::percent(prop_emissao))),
            color = cor_vermelho) +
  geom_point(aes(x = data,
                y = prop_emissao,
                group = 1,
                text = paste0('Data: ', scales::date_format(format = '%m/%Y')(data),
                              '<br>Proporção de Emissão de CO2: ',
                              scales::percent(prop_emissao))),
            color = cor_vermelho,
            size = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(date_labels = '%Y', date_breaks = '2 years') +
  labs(title = paste0('Proporção de Emissões de CO2 pela Companhia<br>', filtro_empresa),
       x = '',
       y = 'Proporção de Emissão de CO2') +
  theme_minimal()

plotly::ggplotly(emissao_empresa_prop_graf, tooltip = 'text')


# EXPORTAÇÃO DOS DADOS: ---------------------------------------------------
tabela_anac <- list(total_ts = total_ts,
                    aeroporto_ts = aeroporto_ts,
                    estado_ts = estado_ts,
                    empresa_ts = empresa_ts)


saveRDS(tabela_anac, 'tabela_anac.rds')

saveRDS(calc_aeroportos, 'calc_aeroportos.rds')

