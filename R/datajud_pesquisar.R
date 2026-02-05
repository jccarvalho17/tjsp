#' Pesquisa processos na API Publica do Datajud (CNJ)
#'
#' @description
#' Realiza consultas na API Publica do Datajud, a base nacional de dados do
#' Poder Judiciario brasileiro. A API utiliza Elasticsearch para as buscas.
#'
#' @param tribunal Sigla do tribunal (ex: "tjsp", "tjrj", "trf1", "stj").
#'     Use \code{datajud_tribunais()} para ver a lista completa.
#' @param numero_processo Numero do processo no formato CNJ (com ou sem pontuacao)
#' @param classe_codigo Codigo numerico da classe processual
#' @param orgao_julgador Nome ou codigo do orgao julgador
#' @param data_inicio Data inicial para filtro de ajuizamento (formato "YYYY-MM-DD")
#' @param data_fim Data final para filtro de ajuizamento (formato "YYYY-MM-DD")
#' @param pagina Numero da pagina (default: 1)
#' @param tamanho Quantidade de resultados por pagina (default: 10, max: 10000)
#' @param api_key Chave de API. Se NULL, usa a chave publica do CNJ.
#' @param search_after Vetor para paginacao via search_after (retornado na resposta anterior)
#'
#' @return Um tibble com os metadados dos processos encontrados
#' @export
#'
#' @examples
#' \dontrun{
#' # Pesquisa por numero de processo no TJSP
#' datajud_pesquisar(tribunal = "tjsp", numero_processo = "1000001-00.2020.8.26.0001")
#'
#' # Pesquisa por classe processual
#' datajud_pesquisar(tribunal = "tjsp", classe_codigo = 7)
#'
#' # Pesquisa com filtro de data
#' datajud_pesquisar(
#'   tribunal = "tjsp",
#'   classe_codigo = 7,
#'   data_inicio = "2023-01-01",
#'   data_fim = "2023-12-31"
#' )
#' }
#'
datajud_pesquisar <- function(tribunal,
                              numero_processo = NULL,
                              classe_codigo = NULL,
                              orgao_julgador = NULL,
                              data_inicio = NULL,
                              data_fim = NULL,
                              pagina = 1,
                              tamanho = 10,
                              api_key = NULL,
                              search_after = NULL) {

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("O pacote 'jsonlite' e necessario. Instale com: install.packages('jsonlite')")
  }

  tribunal <- tolower(tribunal)

  tribunais_validos <- datajud_tribunais()$alias

  if (!tribunal %in% tribunais_validos) {
    stop(
      "Tribunal '", tribunal, "' nao encontrado. ",
      "Use datajud_tribunais() para ver a lista de tribunais disponiveis."
    )
  }

  if (is.null(api_key)) {
    api_key <- "cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw=="
  }

  url <- paste0("https://api-publica.datajud.cnj.jus.br/api_publica_", tribunal, "/_search")

  query <- datajud_construir_query(
    numero_processo = numero_processo,
    classe_codigo = classe_codigo,
    orgao_julgador = orgao_julgador,
    data_inicio = data_inicio,
    data_fim = data_fim,
    pagina = pagina,
    tamanho = tamanho,
    search_after = search_after
  )

  body_json <- jsonlite::toJSON(query, auto_unbox = TRUE)

  response <- httr::POST(
    url,
    httr::add_headers(
      "Authorization" = paste("APIKey", api_key),
      "Content-Type" = "application/json"
    ),
    body = body_json,
    encode = "raw",
    httr::timeout(30)
  )

  if (httr::status_code(response) != 200) {
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop(
      "Erro na requisicao. Status: ", httr::status_code(response),
      "\nMensagem: ", content
    )
  }

  content <- httr::content(response, as = "text", encoding = "UTF-8")
  resultado <- jsonlite::fromJSON(content, flatten = TRUE)

  datajud_processar_resposta(resultado)
}


#' Constroi a query Elasticsearch para a API do Datajud
#'
#' @inheritParams datajud_pesquisar
#'
#' @return Uma lista com a estrutura da query Elasticsearch
#' @keywords internal
#'
datajud_construir_query <- function(numero_processo = NULL,
                                    classe_codigo = NULL,
                                    orgao_julgador = NULL,
                                    data_inicio = NULL,
                                    data_fim = NULL,
                                    pagina = 1,
                                    tamanho = 10,
                                    search_after = NULL) {

  query <- list()

  must_clauses <- list()

  if (!is.null(numero_processo)) {
    numero_limpo <- stringr::str_remove_all(numero_processo, "\\D")
    must_clauses <- c(must_clauses, list(list(match = list(numeroProcesso = numero_limpo))))
  }

  if (!is.null(classe_codigo)) {
    must_clauses <- c(must_clauses, list(list(match = list("classe.codigo" = as.integer(classe_codigo)))))
  }

  if (!is.null(orgao_julgador)) {
    must_clauses <- c(must_clauses, list(list(match = list("orgaoJulgador.nome" = orgao_julgador))))
  }

  if (!is.null(data_inicio) || !is.null(data_fim)) {
    range_filter <- list(dataAjuizamento = list())
    if (!is.null(data_inicio)) {
      range_filter$dataAjuizamento$gte <- data_inicio
    }
    if (!is.null(data_fim)) {
      range_filter$dataAjuizamento$lte <- data_fim
    }
    must_clauses <- c(must_clauses, list(list(range = range_filter)))
  }

  if (length(must_clauses) == 0) {
    query$query <- list(match_all = list())
  } else if (length(must_clauses) == 1) {
    query$query <- must_clauses[[1]]
  } else {
    query$query <- list(bool = list(must = must_clauses))
  }

  query$size <- min(tamanho, 10000)

  if (!is.null(search_after)) {
    query$search_after <- search_after
    query$sort <- list(list("@timestamp" = "asc"))
  } else {
    from_value <- (pagina - 1) * tamanho
    if (from_value > 0) {
      query$from <- from_value
    }
  }

  return(query)
}


#' Processa a resposta da API do Datajud
#'
#' @param resultado Lista com a resposta da API
#'
#' @return Um tibble com os dados processados
#' @keywords internal
#'
datajud_processar_resposta <- function(resultado) {

  hits <- resultado$hits$hits

  if (is.null(hits) || length(hits) == 0) {
    message("Nenhum processo encontrado.")
    return(tibble::tibble())
  }

  total <- resultado$hits$total$value
  message("Total de processos encontrados: ", total)

  df <- hits$`_source`

  if (!is.data.frame(df)) {
    df <- purrr::map_dfr(df, ~{
      as.data.frame(t(unlist(.x)), stringsAsFactors = FALSE)
    })
  }

  df <- tibble::as_tibble(df)

  if ("sort" %in% names(hits)) {
    ultimo_sort <- hits$sort[[length(hits$sort)]]
    attr(df, "search_after") <- ultimo_sort
    attr(df, "total") <- total
  }

  return(df)
}


#' Lista os tribunais disponiveis na API do Datajud
#'
#' @return Um tibble com os alias e nomes dos tribunais
#' @export
#'
#' @examples
#' datajud_tribunais()
#'
datajud_tribunais <- function() {

  tribunais <- tibble::tribble(
    ~alias, ~nome, ~tipo,
    # Tribunais Superiores
    "stf", "Supremo Tribunal Federal", "Superior",
    "stj", "Superior Tribunal de Justica", "Superior",
    "tst", "Tribunal Superior do Trabalho", "Superior",
    "tse", "Tribunal Superior Eleitoral", "Superior",
    "stm", "Superior Tribunal Militar", "Superior",
    # Justica Federal
    "trf1", "Tribunal Regional Federal da 1a Regiao", "Federal",
    "trf2", "Tribunal Regional Federal da 2a Regiao", "Federal",
    "trf3", "Tribunal Regional Federal da 3a Regiao", "Federal",
    "trf4", "Tribunal Regional Federal da 4a Regiao", "Federal",
    "trf5", "Tribunal Regional Federal da 5a Regiao", "Federal",
    "trf6", "Tribunal Regional Federal da 6a Regiao", "Federal",
    # Justica Estadual
    "tjac", "Tribunal de Justica do Acre", "Estadual",
    "tjal", "Tribunal de Justica de Alagoas", "Estadual",
    "tjam", "Tribunal de Justica do Amazonas", "Estadual",
    "tjap", "Tribunal de Justica do Amapa", "Estadual",
    "tjba", "Tribunal de Justica da Bahia", "Estadual",
    "tjce", "Tribunal de Justica do Ceara", "Estadual",
    "tjdft", "Tribunal de Justica do Distrito Federal", "Estadual",
    "tjes", "Tribunal de Justica do Espirito Santo", "Estadual",
    "tjgo", "Tribunal de Justica de Goias", "Estadual",
    "tjma", "Tribunal de Justica do Maranhao", "Estadual",
    "tjmg", "Tribunal de Justica de Minas Gerais", "Estadual",
    "tjms", "Tribunal de Justica do Mato Grosso do Sul", "Estadual",
    "tjmt", "Tribunal de Justica do Mato Grosso", "Estadual",
    "tjpa", "Tribunal de Justica do Para", "Estadual",
    "tjpb", "Tribunal de Justica da Paraiba", "Estadual",
    "tjpe", "Tribunal de Justica de Pernambuco", "Estadual",
    "tjpi", "Tribunal de Justica do Piaui", "Estadual",
    "tjpr", "Tribunal de Justica do Parana", "Estadual",
    "tjrj", "Tribunal de Justica do Rio de Janeiro", "Estadual",
    "tjrn", "Tribunal de Justica do Rio Grande do Norte", "Estadual",
    "tjro", "Tribunal de Justica de Rondonia", "Estadual",
    "tjrr", "Tribunal de Justica de Roraima", "Estadual",
    "tjrs", "Tribunal de Justica do Rio Grande do Sul", "Estadual",
    "tjsc", "Tribunal de Justica de Santa Catarina", "Estadual",
    "tjse", "Tribunal de Justica de Sergipe", "Estadual",
    "tjsp", "Tribunal de Justica de Sao Paulo", "Estadual",
    "tjto", "Tribunal de Justica do Tocantins", "Estadual",
    # Justica do Trabalho
    "trt1", "Tribunal Regional do Trabalho da 1a Regiao (RJ)", "Trabalho",
    "trt2", "Tribunal Regional do Trabalho da 2a Regiao (SP)", "Trabalho",
    "trt3", "Tribunal Regional do Trabalho da 3a Regiao (MG)", "Trabalho",
    "trt4", "Tribunal Regional do Trabalho da 4a Regiao (RS)", "Trabalho",
    "trt5", "Tribunal Regional do Trabalho da 5a Regiao (BA)", "Trabalho",
    "trt6", "Tribunal Regional do Trabalho da 6a Regiao (PE)", "Trabalho",
    "trt7", "Tribunal Regional do Trabalho da 7a Regiao (CE)", "Trabalho",
    "trt8", "Tribunal Regional do Trabalho da 8a Regiao (PA/AP)", "Trabalho",
    "trt9", "Tribunal Regional do Trabalho da 9a Regiao (PR)", "Trabalho",
    "trt10", "Tribunal Regional do Trabalho da 10a Regiao (DF/TO)", "Trabalho",
    "trt11", "Tribunal Regional do Trabalho da 11a Regiao (AM/RR)", "Trabalho",
    "trt12", "Tribunal Regional do Trabalho da 12a Regiao (SC)", "Trabalho",
    "trt13", "Tribunal Regional do Trabalho da 13a Regiao (PB)", "Trabalho",
    "trt14", "Tribunal Regional do Trabalho da 14a Regiao (RO/AC)", "Trabalho",
    "trt15", "Tribunal Regional do Trabalho da 15a Regiao (Campinas-SP)", "Trabalho",
    "trt16", "Tribunal Regional do Trabalho da 16a Regiao (MA)", "Trabalho",
    "trt17", "Tribunal Regional do Trabalho da 17a Regiao (ES)", "Trabalho",
    "trt18", "Tribunal Regional do Trabalho da 18a Regiao (GO)", "Trabalho",
    "trt19", "Tribunal Regional do Trabalho da 19a Regiao (AL)", "Trabalho",
    "trt20", "Tribunal Regional do Trabalho da 20a Regiao (SE)", "Trabalho",
    "trt21", "Tribunal Regional do Trabalho da 21a Regiao (RN)", "Trabalho",
    "trt22", "Tribunal Regional do Trabalho da 22a Regiao (PI)", "Trabalho",
    "trt23", "Tribunal Regional do Trabalho da 23a Regiao (MT)", "Trabalho",
    "trt24", "Tribunal Regional do Trabalho da 24a Regiao (MS)", "Trabalho",
    # Justica Eleitoral
    "tre-ac", "Tribunal Regional Eleitoral do Acre", "Eleitoral",
    "tre-al", "Tribunal Regional Eleitoral de Alagoas", "Eleitoral",
    "tre-am", "Tribunal Regional Eleitoral do Amazonas", "Eleitoral",
    "tre-ap", "Tribunal Regional Eleitoral do Amapa", "Eleitoral",
    "tre-ba", "Tribunal Regional Eleitoral da Bahia", "Eleitoral",
    "tre-ce", "Tribunal Regional Eleitoral do Ceara", "Eleitoral",
    "tre-df", "Tribunal Regional Eleitoral do Distrito Federal", "Eleitoral",
    "tre-es", "Tribunal Regional Eleitoral do Espirito Santo", "Eleitoral",
    "tre-go", "Tribunal Regional Eleitoral de Goias", "Eleitoral",
    "tre-ma", "Tribunal Regional Eleitoral do Maranhao", "Eleitoral",
    "tre-mg", "Tribunal Regional Eleitoral de Minas Gerais", "Eleitoral",
    "tre-ms", "Tribunal Regional Eleitoral do Mato Grosso do Sul", "Eleitoral",
    "tre-mt", "Tribunal Regional Eleitoral do Mato Grosso", "Eleitoral",
    "tre-pa", "Tribunal Regional Eleitoral do Para", "Eleitoral",
    "tre-pb", "Tribunal Regional Eleitoral da Paraiba", "Eleitoral",
    "tre-pe", "Tribunal Regional Eleitoral de Pernambuco", "Eleitoral",
    "tre-pi", "Tribunal Regional Eleitoral do Piaui", "Eleitoral",
    "tre-pr", "Tribunal Regional Eleitoral do Parana", "Eleitoral",
    "tre-rj", "Tribunal Regional Eleitoral do Rio de Janeiro", "Eleitoral",
    "tre-rn", "Tribunal Regional Eleitoral do Rio Grande do Norte", "Eleitoral",
    "tre-ro", "Tribunal Regional Eleitoral de Rondonia", "Eleitoral",
    "tre-rr", "Tribunal Regional Eleitoral de Roraima", "Eleitoral",
    "tre-rs", "Tribunal Regional Eleitoral do Rio Grande do Sul", "Eleitoral",
    "tre-sc", "Tribunal Regional Eleitoral de Santa Catarina", "Eleitoral",
    "tre-se", "Tribunal Regional Eleitoral de Sergipe", "Eleitoral",
    "tre-sp", "Tribunal Regional Eleitoral de Sao Paulo", "Eleitoral",
    "tre-to", "Tribunal Regional Eleitoral do Tocantins", "Eleitoral",
    # Justica Militar Estadual
    "tjmmg", "Tribunal de Justica Militar de Minas Gerais", "Militar",
    "tjmrs", "Tribunal de Justica Militar do Rio Grande do Sul", "Militar",
    "tjmsp", "Tribunal de Justica Militar de Sao Paulo", "Militar"
  )

  return(tribunais)
}


#' Pesquisa multiplos processos no Datajud
#'
#' @param tribunal Sigla do tribunal
#' @param processos Vetor com numeros de processos
#' @param intervalo Intervalo em segundos entre requisicoes (default: 1)
#' @param api_key Chave de API (opcional)
#'
#' @return Um tibble com os dados de todos os processos encontrados
#' @export
#'
#' @examples
#' \dontrun{
#' processos <- c("1000001-00.2020.8.26.0001", "1000002-00.2020.8.26.0001")
#' datajud_pesquisar_lote(tribunal = "tjsp", processos = processos)
#' }
#'
datajud_pesquisar_lote <- function(tribunal,
                                   processos,
                                   intervalo = 1,
                                   api_key = NULL) {

  pesquisar_seguro <- purrr::possibly(
    datajud_pesquisar,
    otherwise = tibble::tibble(),
    quiet = FALSE
  )

  resultados <- purrr::map_dfr(processos, ~{
    Sys.sleep(intervalo)
    pesquisar_seguro(
      tribunal = tribunal,
      numero_processo = .x,
      api_key = api_key
    )
  }, .progress = TRUE)

  return(resultados)
}


#' Pesquisa com paginacao automatica no Datajud
#'
#' @inheritParams datajud_pesquisar
#' @param max_resultados Numero maximo de resultados a retornar (default: 1000)
#' @param intervalo Intervalo em segundos entre requisicoes (default: 1)
#'
#' @return Um tibble com todos os resultados encontrados
#' @export
#'
#' @examples
#' \dontrun
#' # Busca todos os processos de uma classe no TJSP
#' datajud_pesquisar_todos(
#'   tribunal = "tjsp",
#'   classe_codigo = 7,
#'   data_inicio = "2023-01-01",
#'   data_fim = "2023-01-31",
#'   max_resultados = 500
#' )
#' }
#'
datajud_pesquisar_todos <- function(tribunal,
                                    numero_processo = NULL,
                                    classe_codigo = NULL,
                                    orgao_julgador = NULL,
                                    data_inicio = NULL,
                                    data_fim = NULL,
                                    max_resultados = 1000,
                                    intervalo = 1,
                                    api_key = NULL) {

  tamanho_pagina <- min(max_resultados, 1000)

  resultado <- datajud_pesquisar(
    tribunal = tribunal,
    numero_processo = numero_processo,
    classe_codigo = classe_codigo,
    orgao_julgador = orgao_julgador,
    data_inicio = data_inicio,
    data_fim = data_fim,
    tamanho = tamanho_pagina,
    api_key = api_key
  )

  if (nrow(resultado) == 0) {
    return(resultado)
  }

  total <- attr(resultado, "total")
  if (is.null(total)) total <- nrow(resultado)

  todos_resultados <- resultado

  while (nrow(todos_resultados) < min(total, max_resultados)) {

    search_after <- attr(resultado, "search_after")

    if (is.null(search_after)) {
      break
    }

    Sys.sleep(intervalo)

    resultado <- datajud_pesquisar(
      tribunal = tribunal,
      numero_processo = numero_processo,
      classe_codigo = classe_codigo,
      orgao_julgador = orgao_julgador,
      data_inicio = data_inicio,
      data_fim = data_fim,
      tamanho = tamanho_pagina,
      search_after = search_after,
      api_key = api_key
    )

    if (nrow(resultado) == 0) {
      break
    }

    todos_resultados <- dplyr::bind_rows(todos_resultados, resultado)

    message("Baixados: ", nrow(todos_resultados), " de ", min(total, max_resultados))
  }

  if (nrow(todos_resultados) > max_resultados) {
    todos_resultados <- todos_resultados[1:max_resultados, ]
  }

  return(todos_resultados)
}


#' Obtem movimentacoes de um processo no Datajud
#'
#' @inheritParams datajud_pesquisar
#'
#' @return Um tibble com as movimentacoes do processo
#' @export
#'
#' @examples
#' \dontrun{
#' datajud_movimentacoes(tribunal = "tjsp", numero_processo = "1000001-00.2020.8.26.0001")
#' }
#'
datajud_movimentacoes <- function(tribunal,
                                  numero_processo,
                                  api_key = NULL) {

  resultado <- datajud_pesquisar(
    tribunal = tribunal,
    numero_processo = numero_processo,
    api_key = api_key
  )

  if (nrow(resultado) == 0) {
    return(tibble::tibble())
  }

  if ("movimentos" %in% names(resultado)) {
    movimentos <- resultado$movimentos[[1]]

    if (!is.null(movimentos) && length(movimentos) > 0) {
      if (is.data.frame(movimentos)) {
        return(tibble::as_tibble(movimentos))
      } else {
        return(purrr::map_dfr(movimentos, tibble::as_tibble))
      }
    }
  }

  message("Nenhuma movimentacao encontrada.")
  return(tibble::tibble())
}
