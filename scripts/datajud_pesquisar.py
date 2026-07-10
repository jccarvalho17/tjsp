#!/usr/bin/env python3
"""
Script para realizar pesquisas na API Publica do Datajud (CNJ).

A API Publica do Datajud permite acesso aos metadados de processos judiciais
de todo o Brasil. Os dados seguem os criterios da Portaria No 160 de 09/09/2020.

Documentacao: https://datajud-wiki.cnj.jus.br/api-publica/

Exemplo de uso:
    python datajud_pesquisar.py --tribunal tjsp --processo "1000001-00.2020.8.26.0001"
    python datajud_pesquisar.py --tribunal tjsp --classe 7 --data-inicio 2023-01-01
    python datajud_pesquisar.py --listar-tribunais
"""

import argparse
import json
import re
import sys
import time
from dataclasses import dataclass
from typing import Any

import requests


# Chave publica do CNJ (disponivel em https://datajud-wiki.cnj.jus.br/api-publica/acesso)
API_KEY_PUBLICA = "cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw=="

# URL base da API
BASE_URL = "https://api-publica.datajud.cnj.jus.br"


@dataclass
class Tribunal:
    """Representa um tribunal disponivel na API do Datajud."""

    alias: str
    nome: str
    tipo: str


# Lista completa de tribunais disponiveis
TRIBUNAIS = [
    # Tribunais Superiores
    Tribunal("stf", "Supremo Tribunal Federal", "Superior"),
    Tribunal("stj", "Superior Tribunal de Justica", "Superior"),
    Tribunal("tst", "Tribunal Superior do Trabalho", "Superior"),
    Tribunal("tse", "Tribunal Superior Eleitoral", "Superior"),
    Tribunal("stm", "Superior Tribunal Militar", "Superior"),
    # Justica Federal
    Tribunal("trf1", "Tribunal Regional Federal da 1a Regiao", "Federal"),
    Tribunal("trf2", "Tribunal Regional Federal da 2a Regiao", "Federal"),
    Tribunal("trf3", "Tribunal Regional Federal da 3a Regiao", "Federal"),
    Tribunal("trf4", "Tribunal Regional Federal da 4a Regiao", "Federal"),
    Tribunal("trf5", "Tribunal Regional Federal da 5a Regiao", "Federal"),
    Tribunal("trf6", "Tribunal Regional Federal da 6a Regiao", "Federal"),
    # Justica Estadual
    Tribunal("tjac", "Tribunal de Justica do Acre", "Estadual"),
    Tribunal("tjal", "Tribunal de Justica de Alagoas", "Estadual"),
    Tribunal("tjam", "Tribunal de Justica do Amazonas", "Estadual"),
    Tribunal("tjap", "Tribunal de Justica do Amapa", "Estadual"),
    Tribunal("tjba", "Tribunal de Justica da Bahia", "Estadual"),
    Tribunal("tjce", "Tribunal de Justica do Ceara", "Estadual"),
    Tribunal("tjdft", "Tribunal de Justica do Distrito Federal", "Estadual"),
    Tribunal("tjes", "Tribunal de Justica do Espirito Santo", "Estadual"),
    Tribunal("tjgo", "Tribunal de Justica de Goias", "Estadual"),
    Tribunal("tjma", "Tribunal de Justica do Maranhao", "Estadual"),
    Tribunal("tjmg", "Tribunal de Justica de Minas Gerais", "Estadual"),
    Tribunal("tjms", "Tribunal de Justica do Mato Grosso do Sul", "Estadual"),
    Tribunal("tjmt", "Tribunal de Justica do Mato Grosso", "Estadual"),
    Tribunal("tjpa", "Tribunal de Justica do Para", "Estadual"),
    Tribunal("tjpb", "Tribunal de Justica da Paraiba", "Estadual"),
    Tribunal("tjpe", "Tribunal de Justica de Pernambuco", "Estadual"),
    Tribunal("tjpi", "Tribunal de Justica do Piaui", "Estadual"),
    Tribunal("tjpr", "Tribunal de Justica do Parana", "Estadual"),
    Tribunal("tjrj", "Tribunal de Justica do Rio de Janeiro", "Estadual"),
    Tribunal("tjrn", "Tribunal de Justica do Rio Grande do Norte", "Estadual"),
    Tribunal("tjro", "Tribunal de Justica de Rondonia", "Estadual"),
    Tribunal("tjrr", "Tribunal de Justica de Roraima", "Estadual"),
    Tribunal("tjrs", "Tribunal de Justica do Rio Grande do Sul", "Estadual"),
    Tribunal("tjsc", "Tribunal de Justica de Santa Catarina", "Estadual"),
    Tribunal("tjse", "Tribunal de Justica de Sergipe", "Estadual"),
    Tribunal("tjsp", "Tribunal de Justica de Sao Paulo", "Estadual"),
    Tribunal("tjto", "Tribunal de Justica do Tocantins", "Estadual"),
    # Justica do Trabalho
    Tribunal("trt1", "Tribunal Regional do Trabalho da 1a Regiao (RJ)", "Trabalho"),
    Tribunal("trt2", "Tribunal Regional do Trabalho da 2a Regiao (SP)", "Trabalho"),
    Tribunal("trt3", "Tribunal Regional do Trabalho da 3a Regiao (MG)", "Trabalho"),
    Tribunal("trt4", "Tribunal Regional do Trabalho da 4a Regiao (RS)", "Trabalho"),
    Tribunal("trt5", "Tribunal Regional do Trabalho da 5a Regiao (BA)", "Trabalho"),
    Tribunal("trt6", "Tribunal Regional do Trabalho da 6a Regiao (PE)", "Trabalho"),
    Tribunal("trt7", "Tribunal Regional do Trabalho da 7a Regiao (CE)", "Trabalho"),
    Tribunal("trt8", "Tribunal Regional do Trabalho da 8a Regiao (PA/AP)", "Trabalho"),
    Tribunal("trt9", "Tribunal Regional do Trabalho da 9a Regiao (PR)", "Trabalho"),
    Tribunal("trt10", "Tribunal Regional do Trabalho da 10a Regiao (DF/TO)", "Trabalho"),
    Tribunal("trt11", "Tribunal Regional do Trabalho da 11a Regiao (AM/RR)", "Trabalho"),
    Tribunal("trt12", "Tribunal Regional do Trabalho da 12a Regiao (SC)", "Trabalho"),
    Tribunal("trt13", "Tribunal Regional do Trabalho da 13a Regiao (PB)", "Trabalho"),
    Tribunal("trt14", "Tribunal Regional do Trabalho da 14a Regiao (RO/AC)", "Trabalho"),
    Tribunal("trt15", "Tribunal Regional do Trabalho da 15a Regiao (Campinas-SP)", "Trabalho"),
    Tribunal("trt16", "Tribunal Regional do Trabalho da 16a Regiao (MA)", "Trabalho"),
    Tribunal("trt17", "Tribunal Regional do Trabalho da 17a Regiao (ES)", "Trabalho"),
    Tribunal("trt18", "Tribunal Regional do Trabalho da 18a Regiao (GO)", "Trabalho"),
    Tribunal("trt19", "Tribunal Regional do Trabalho da 19a Regiao (AL)", "Trabalho"),
    Tribunal("trt20", "Tribunal Regional do Trabalho da 20a Regiao (SE)", "Trabalho"),
    Tribunal("trt21", "Tribunal Regional do Trabalho da 21a Regiao (RN)", "Trabalho"),
    Tribunal("trt22", "Tribunal Regional do Trabalho da 22a Regiao (PI)", "Trabalho"),
    Tribunal("trt23", "Tribunal Regional do Trabalho da 23a Regiao (MT)", "Trabalho"),
    Tribunal("trt24", "Tribunal Regional do Trabalho da 24a Regiao (MS)", "Trabalho"),
    # Justica Eleitoral
    Tribunal("tre-ac", "Tribunal Regional Eleitoral do Acre", "Eleitoral"),
    Tribunal("tre-al", "Tribunal Regional Eleitoral de Alagoas", "Eleitoral"),
    Tribunal("tre-am", "Tribunal Regional Eleitoral do Amazonas", "Eleitoral"),
    Tribunal("tre-ap", "Tribunal Regional Eleitoral do Amapa", "Eleitoral"),
    Tribunal("tre-ba", "Tribunal Regional Eleitoral da Bahia", "Eleitoral"),
    Tribunal("tre-ce", "Tribunal Regional Eleitoral do Ceara", "Eleitoral"),
    Tribunal("tre-df", "Tribunal Regional Eleitoral do Distrito Federal", "Eleitoral"),
    Tribunal("tre-es", "Tribunal Regional Eleitoral do Espirito Santo", "Eleitoral"),
    Tribunal("tre-go", "Tribunal Regional Eleitoral de Goias", "Eleitoral"),
    Tribunal("tre-ma", "Tribunal Regional Eleitoral do Maranhao", "Eleitoral"),
    Tribunal("tre-mg", "Tribunal Regional Eleitoral de Minas Gerais", "Eleitoral"),
    Tribunal("tre-ms", "Tribunal Regional Eleitoral do Mato Grosso do Sul", "Eleitoral"),
    Tribunal("tre-mt", "Tribunal Regional Eleitoral do Mato Grosso", "Eleitoral"),
    Tribunal("tre-pa", "Tribunal Regional Eleitoral do Para", "Eleitoral"),
    Tribunal("tre-pb", "Tribunal Regional Eleitoral da Paraiba", "Eleitoral"),
    Tribunal("tre-pe", "Tribunal Regional Eleitoral de Pernambuco", "Eleitoral"),
    Tribunal("tre-pi", "Tribunal Regional Eleitoral do Piaui", "Eleitoral"),
    Tribunal("tre-pr", "Tribunal Regional Eleitoral do Parana", "Eleitoral"),
    Tribunal("tre-rj", "Tribunal Regional Eleitoral do Rio de Janeiro", "Eleitoral"),
    Tribunal("tre-rn", "Tribunal Regional Eleitoral do Rio Grande do Norte", "Eleitoral"),
    Tribunal("tre-ro", "Tribunal Regional Eleitoral de Rondonia", "Eleitoral"),
    Tribunal("tre-rr", "Tribunal Regional Eleitoral de Roraima", "Eleitoral"),
    Tribunal("tre-rs", "Tribunal Regional Eleitoral do Rio Grande do Sul", "Eleitoral"),
    Tribunal("tre-sc", "Tribunal Regional Eleitoral de Santa Catarina", "Eleitoral"),
    Tribunal("tre-se", "Tribunal Regional Eleitoral de Sergipe", "Eleitoral"),
    Tribunal("tre-sp", "Tribunal Regional Eleitoral de Sao Paulo", "Eleitoral"),
    Tribunal("tre-to", "Tribunal Regional Eleitoral do Tocantins", "Eleitoral"),
    # Justica Militar Estadual
    Tribunal("tjmmg", "Tribunal de Justica Militar de Minas Gerais", "Militar"),
    Tribunal("tjmrs", "Tribunal de Justica Militar do Rio Grande do Sul", "Militar"),
    Tribunal("tjmsp", "Tribunal de Justica Militar de Sao Paulo", "Militar"),
]


def listar_tribunais(tipo: str | None = None) -> list[Tribunal]:
    """
    Lista os tribunais disponiveis na API do Datajud.

    Args:
        tipo: Filtrar por tipo (Superior, Federal, Estadual, Trabalho, Eleitoral, Militar)

    Returns:
        Lista de tribunais
    """
    if tipo:
        return [t for t in TRIBUNAIS if t.tipo.lower() == tipo.lower()]
    return TRIBUNAIS


def obter_alias_tribunais() -> list[str]:
    """Retorna lista de alias validos para os tribunais."""
    return [t.alias for t in TRIBUNAIS]


def limpar_numero_processo(numero: str) -> str:
    """Remove pontuacao do numero do processo, mantendo apenas digitos."""
    return re.sub(r"\D", "", numero)


def construir_query(
    numero_processo: str | None = None,
    classe_codigo: int | None = None,
    orgao_julgador: str | None = None,
    data_inicio: str | None = None,
    data_fim: str | None = None,
    tamanho: int = 10,
    pagina: int = 1,
    search_after: list | None = None,
) -> dict[str, Any]:
    """
    Constroi a query Elasticsearch para a API do Datajud.

    Args:
        numero_processo: Numero do processo (formato CNJ)
        classe_codigo: Codigo numerico da classe processual
        orgao_julgador: Nome do orgao julgador
        data_inicio: Data inicial (YYYY-MM-DD)
        data_fim: Data final (YYYY-MM-DD)
        tamanho: Quantidade de resultados por pagina (max 10000)
        pagina: Numero da pagina
        search_after: Valor para paginacao via search_after

    Returns:
        Dicionario com a query Elasticsearch
    """
    query: dict[str, Any] = {}
    must_clauses: list[dict] = []

    if numero_processo:
        numero_limpo = limpar_numero_processo(numero_processo)
        must_clauses.append({"match": {"numeroProcesso": numero_limpo}})

    if classe_codigo is not None:
        must_clauses.append({"match": {"classe.codigo": classe_codigo}})

    if orgao_julgador:
        must_clauses.append({"match": {"orgaoJulgador.nome": orgao_julgador}})

    if data_inicio or data_fim:
        range_filter: dict[str, Any] = {"dataAjuizamento": {}}
        if data_inicio:
            range_filter["dataAjuizamento"]["gte"] = data_inicio
        if data_fim:
            range_filter["dataAjuizamento"]["lte"] = data_fim
        must_clauses.append({"range": range_filter})

    # Construir query principal
    if not must_clauses:
        query["query"] = {"match_all": {}}
    elif len(must_clauses) == 1:
        query["query"] = must_clauses[0]
    else:
        query["query"] = {"bool": {"must": must_clauses}}

    # Tamanho da pagina (max 10000)
    query["size"] = min(tamanho, 10000)

    # Paginacao
    if search_after:
        query["search_after"] = search_after
        query["sort"] = [{"@timestamp": "asc"}]
    else:
        from_value = (pagina - 1) * tamanho
        if from_value > 0:
            query["from"] = from_value

    return query


def pesquisar(
    tribunal: str,
    numero_processo: str | None = None,
    classe_codigo: int | None = None,
    orgao_julgador: str | None = None,
    data_inicio: str | None = None,
    data_fim: str | None = None,
    tamanho: int = 10,
    pagina: int = 1,
    search_after: list | None = None,
    api_key: str | None = None,
    timeout: int = 30,
) -> dict[str, Any]:
    """
    Pesquisa processos na API Publica do Datajud.

    Args:
        tribunal: Sigla do tribunal (ex: tjsp, tjrj, trf1, stj)
        numero_processo: Numero do processo (formato CNJ)
        classe_codigo: Codigo numerico da classe processual
        orgao_julgador: Nome do orgao julgador
        data_inicio: Data inicial para filtro (YYYY-MM-DD)
        data_fim: Data final para filtro (YYYY-MM-DD)
        tamanho: Quantidade de resultados por pagina (max 10000)
        pagina: Numero da pagina
        search_after: Valor para paginacao via search_after
        api_key: Chave de API (usa chave publica se nao informada)
        timeout: Timeout da requisicao em segundos

    Returns:
        Dicionario com a resposta da API

    Raises:
        ValueError: Se o tribunal nao for valido
        requests.RequestException: Se houver erro na requisicao
    """
    tribunal = tribunal.lower()
    aliases_validos = obter_alias_tribunais()

    if tribunal not in aliases_validos:
        raise ValueError(
            f"Tribunal '{tribunal}' nao encontrado. "
            f"Use listar_tribunais() para ver a lista completa."
        )

    if api_key is None:
        api_key = API_KEY_PUBLICA

    url = f"{BASE_URL}/api_publica_{tribunal}/_search"

    query = construir_query(
        numero_processo=numero_processo,
        classe_codigo=classe_codigo,
        orgao_julgador=orgao_julgador,
        data_inicio=data_inicio,
        data_fim=data_fim,
        tamanho=tamanho,
        pagina=pagina,
        search_after=search_after,
    )

    headers = {
        "Authorization": f"APIKey {api_key}",
        "Content-Type": "application/json",
    }

    response = requests.post(url, headers=headers, json=query, timeout=timeout)
    response.raise_for_status()

    return response.json()


def extrair_processos(resposta: dict[str, Any]) -> list[dict[str, Any]]:
    """
    Extrai os processos da resposta da API.

    Args:
        resposta: Resposta da API do Datajud

    Returns:
        Lista de dicionarios com os dados dos processos
    """
    hits = resposta.get("hits", {}).get("hits", [])
    return [hit.get("_source", {}) for hit in hits]


def obter_total(resposta: dict[str, Any]) -> int:
    """Retorna o total de processos encontrados."""
    return resposta.get("hits", {}).get("total", {}).get("value", 0)


def obter_search_after(resposta: dict[str, Any]) -> list | None:
    """Retorna o valor de search_after do ultimo resultado."""
    hits = resposta.get("hits", {}).get("hits", [])
    if hits:
        return hits[-1].get("sort")
    return None


def pesquisar_lote(
    tribunal: str,
    processos: list[str],
    intervalo: float = 1.0,
    api_key: str | None = None,
) -> list[dict[str, Any]]:
    """
    Pesquisa multiplos processos em lote.

    Args:
        tribunal: Sigla do tribunal
        processos: Lista de numeros de processos
        intervalo: Intervalo em segundos entre requisicoes
        api_key: Chave de API (opcional)

    Returns:
        Lista com os dados de todos os processos encontrados
    """
    resultados = []
    total = len(processos)

    for i, processo in enumerate(processos, 1):
        print(f"Pesquisando {i}/{total}: {processo}", file=sys.stderr)
        try:
            resposta = pesquisar(
                tribunal=tribunal, numero_processo=processo, api_key=api_key
            )
            resultados.extend(extrair_processos(resposta))
        except requests.RequestException as e:
            print(f"Erro ao pesquisar {processo}: {e}", file=sys.stderr)

        if i < total:
            time.sleep(intervalo)

    return resultados


def pesquisar_todos(
    tribunal: str,
    classe_codigo: int | None = None,
    orgao_julgador: str | None = None,
    data_inicio: str | None = None,
    data_fim: str | None = None,
    max_resultados: int = 1000,
    intervalo: float = 1.0,
    api_key: str | None = None,
) -> list[dict[str, Any]]:
    """
    Pesquisa com paginacao automatica usando search_after.

    Args:
        tribunal: Sigla do tribunal
        classe_codigo: Codigo da classe processual
        orgao_julgador: Nome do orgao julgador
        data_inicio: Data inicial (YYYY-MM-DD)
        data_fim: Data final (YYYY-MM-DD)
        max_resultados: Numero maximo de resultados
        intervalo: Intervalo entre requisicoes
        api_key: Chave de API (opcional)

    Returns:
        Lista com todos os processos encontrados
    """
    tamanho_pagina = min(max_resultados, 1000)
    todos_resultados: list[dict[str, Any]] = []
    search_after = None

    while len(todos_resultados) < max_resultados:
        resposta = pesquisar(
            tribunal=tribunal,
            classe_codigo=classe_codigo,
            orgao_julgador=orgao_julgador,
            data_inicio=data_inicio,
            data_fim=data_fim,
            tamanho=tamanho_pagina,
            search_after=search_after,
            api_key=api_key,
        )

        processos = extrair_processos(resposta)
        if not processos:
            break

        todos_resultados.extend(processos)
        total = obter_total(resposta)

        print(
            f"Baixados: {len(todos_resultados)}/{min(total, max_resultados)}",
            file=sys.stderr,
        )

        search_after = obter_search_after(resposta)
        if not search_after:
            break

        if len(todos_resultados) >= min(total, max_resultados):
            break

        time.sleep(intervalo)

    return todos_resultados[:max_resultados]


def obter_movimentacoes(
    tribunal: str, numero_processo: str, api_key: str | None = None
) -> list[dict[str, Any]]:
    """
    Obtem as movimentacoes de um processo.

    Args:
        tribunal: Sigla do tribunal
        numero_processo: Numero do processo
        api_key: Chave de API (opcional)

    Returns:
        Lista de movimentacoes do processo
    """
    resposta = pesquisar(
        tribunal=tribunal, numero_processo=numero_processo, api_key=api_key
    )

    processos = extrair_processos(resposta)
    if not processos:
        return []

    return processos[0].get("movimentos", [])


def main():
    """Funcao principal para execucao via linha de comando."""
    parser = argparse.ArgumentParser(
        description="Pesquisa processos na API Publica do Datajud (CNJ)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Exemplos:
  %(prog)s --tribunal tjsp --processo "1000001-00.2020.8.26.0001"
  %(prog)s --tribunal tjsp --classe 7 --data-inicio 2023-01-01
  %(prog)s --listar-tribunais
  %(prog)s --listar-tribunais --tipo Estadual
        """,
    )

    parser.add_argument(
        "--tribunal", "-t", help="Sigla do tribunal (ex: tjsp, tjrj, stj)"
    )
    parser.add_argument("--processo", "-p", help="Numero do processo (formato CNJ)")
    parser.add_argument(
        "--classe", "-c", type=int, help="Codigo da classe processual"
    )
    parser.add_argument("--orgao", "-o", help="Nome do orgao julgador")
    parser.add_argument("--data-inicio", help="Data inicial (YYYY-MM-DD)")
    parser.add_argument("--data-fim", help="Data final (YYYY-MM-DD)")
    parser.add_argument(
        "--tamanho", type=int, default=10, help="Resultados por pagina (default: 10)"
    )
    parser.add_argument("--pagina", type=int, default=1, help="Numero da pagina")
    parser.add_argument(
        "--listar-tribunais", action="store_true", help="Lista tribunais disponiveis"
    )
    parser.add_argument(
        "--tipo",
        choices=["Superior", "Federal", "Estadual", "Trabalho", "Eleitoral", "Militar"],
        help="Filtrar tribunais por tipo",
    )
    parser.add_argument(
        "--movimentacoes",
        action="store_true",
        help="Exibir apenas movimentacoes do processo",
    )
    parser.add_argument("--api-key", help="Chave de API customizada")
    parser.add_argument(
        "--formato",
        choices=["json", "tabela"],
        default="json",
        help="Formato de saida (default: json)",
    )

    args = parser.parse_args()

    # Listar tribunais
    if args.listar_tribunais:
        tribunais = listar_tribunais(args.tipo)
        if args.formato == "json":
            print(
                json.dumps(
                    [{"alias": t.alias, "nome": t.nome, "tipo": t.tipo} for t in tribunais],
                    indent=2,
                    ensure_ascii=False,
                )
            )
        else:
            print(f"{'Alias':<10} {'Tipo':<10} Nome")
            print("-" * 70)
            for t in tribunais:
                print(f"{t.alias:<10} {t.tipo:<10} {t.nome}")
        return

    # Validar argumentos
    if not args.tribunal:
        parser.error("--tribunal e obrigatorio para pesquisas")

    # Pesquisar movimentacoes
    if args.movimentacoes:
        if not args.processo:
            parser.error("--processo e obrigatorio para --movimentacoes")
        movimentos = obter_movimentacoes(
            tribunal=args.tribunal,
            numero_processo=args.processo,
            api_key=args.api_key,
        )
        print(json.dumps(movimentos, indent=2, ensure_ascii=False))
        return

    # Pesquisa padrao
    resposta = pesquisar(
        tribunal=args.tribunal,
        numero_processo=args.processo,
        classe_codigo=args.classe,
        orgao_julgador=args.orgao,
        data_inicio=args.data_inicio,
        data_fim=args.data_fim,
        tamanho=args.tamanho,
        pagina=args.pagina,
        api_key=args.api_key,
    )

    processos = extrair_processos(resposta)
    total = obter_total(resposta)

    print(f"Total encontrado: {total}", file=sys.stderr)

    if args.formato == "json":
        print(json.dumps(processos, indent=2, ensure_ascii=False))
    else:
        for p in processos:
            numero = p.get("numeroProcesso", "N/A")
            classe = p.get("classe", {}).get("nome", "N/A")
            orgao = p.get("orgaoJulgador", {}).get("nome", "N/A")
            data = p.get("dataAjuizamento", "N/A")
            print(f"{numero} | {classe} | {orgao} | {data}")


if __name__ == "__main__":
    main()
