# recinf — Modelo Vetorial e BM25 sobre a coleção Cranfield

Trabalho Prático 1 de SCC0282 Recuperação de Informação (ICMC/USP).

## Integrantes

| Nome | NUSP | E-mail |
|---|---|---|
| Gustavo Blois | 13688612 | gustavobloisrr@usp.br |

## Base de dados

Coleção **Cranfield** (1400 documentos, 225 consultas, julgamentos de relevância).
Fontes: [ir_datasets – Cranfield](https://ir-datasets.com/cranfield.html), também
disponível na coleção Cranfield da University of Glasgow e em `irds/cranfield` no Hugging Face.

Os três arquivos originais já estão versionados em `data/`, então não é preciso baixar nada:

| Arquivo | Conteúdo |
|---|---|
| `data/cran.all.1400` | documentos (`.I`, `.T`, `.A`, `.B`, `.W`) |
| `data/cran.qry` | consultas |
| `data/cranqrel` | qrels: `consulta doc grau` (1 = resposta completa … 4 = interesse mínimo; -1 = não relevante) |

Atenção: os ids do `cran.qry` (`001, 002, 004, 008…`) **não** são os usados no `cranqrel`, que numera as
consultas de 1 a 225 pela posição. O código usa a posição como número da consulta (o mesmo 1–225 do ir_datasets); as tabelas e CSVs mostram também o `.I` original (`cran_id`), e `--by-id` permite buscar por ele. Relevante = grau ≥ 1; grau -1 e documentos
não julgados são tratados como não relevantes.

## Versões e bibliotecas

- OCaml **5.5.1** (`>= 5.5.0`), dune **3.24**
- `stem` 0.0.2 — stemmer Snowball (inglês)
- `unix` (biblioteca padrão) — detectar se a saída é um terminal, para as cores
- programa `gnuplot` (testado com 6.0.5) — gráficos; o OCaml escreve scripts `.gp` e chama o programa, sem binding opam

BM25, modelo vetorial (tf-idf + cosseno) e métricas são implementados em `lib/` sem bibliotecas de
recuperação; `stem` só faz o stemming.

## Instalação

### 1. OCaml e opam

- **Linux**: instale o `opam` pelo gerenciador de pacotes da distribuição (`sudo pacman -S opam`,
  `sudo apt install opam`, `sudo dnf install opam`).
- **macOS**: `brew install opam`.

```sh
opam init
opam switch create . ocaml-base-compiler.5.5.1   # ou use um switch existente com OCaml >= 5.5
eval $(opam env)
```

### 2. gnuplot (para os gráficos)

O comando `plot` gera scripts `.gp` e chama o programa `gnuplot`, que precisa estar no `PATH`.
Nenhum pacote opam de gnuplot é necessário.

| Sistema | Comando |
|---|---|
| Arch / CachyOS | `sudo pacman -S gnuplot` |
| Debian / Ubuntu | `sudo apt install gnuplot` |
| Fedora | `sudo dnf install gnuplot` |
| **macOS** (Homebrew) | `brew install gnuplot` |

Verifique com `gnuplot --version` e confira se o terminal `pngcairo` (ou `pdfcairo`) está disponível:

```sh
gnuplot -e "set terminal" 2>&1 | grep -E "pngcairo|pdfcairo"
```

No macOS o Homebrew já compila o gnuplot com cairo. Em Linux, se nenhum dos dois aparecer, instale a
variante completa (com Qt) do gnuplot da sua distribuição.

### 3. Dependências OCaml

```sh
opam install . --deps-only --with-test
dune build
```

## Execução

Os comandos podem ser rodados de qualquer subdiretório do projeto: o programa procura `data/cran.all.1400` subindo a partir do diretório atual e usa a raiz encontrada (`data/` e `results/` são relativos a ela).

```sh
dune test                           # testes com valores calculados à mão (BM25, cosseno, métricas, qrels)
dune exec recinf -- experiments     # 4 pré-processamentos x (vetorial + BM25 k1∈{0,5;1,2;2,0} x b∈{0;0,75;1})
dune exec recinf -- compare         # item 5: vetorial x BM25 (36 configs), todas as métricas, e uma tabela de maiores diferenças por métrica; --metric ap|ap10|p10|r10|f1|rr|ndcg10 restringe a uma
dune exec recinf -- cases           # item 6: candidatas em três categorias (BM25 melhor, vetorial melhor, ambos ruins); --top N por categoria, --detail N imprime o top-5 dos dois modelos
dune exec recinf -- b-effect        # item 7: consultas cujo AP mais muda com b (k1 fixo); com uma consulta (b-effect 67), o top-10 com b=0 e b=1
dune exec recinf -- analyze         # os três acima de uma vez
dune exec recinf -- plot            # gráficos em results/plots/ (PNG, PDF e os scripts .gp)
dune exec recinf -- show 167        # top-N dos dois modelos para a consulta 167
dune exec recinf -- explain 167 553 # contribuição de cada termo ao score de um documento
dune exec recinf -- modify 167 "ablative mass loss hypersonic"   # consulta modificada (item 8)
dune exec recinf -- modify --file data/modified_queries.txt      # várias, grava results/modifications.csv
dune exec recinf -- help            # todas as opções
```

Opções de `show`, `explain` e `modify`: `--preproc raw|stopwords|stemming|stopwords+stemming`
(padrão `stopwords+stemming`), `--k1`, `--b` (padrão 1,2 e 0,75), `--top N`, `--text "..."`
(`show`/`explain`: substitui o texto da consulta mantendo seus qrels), `--by-id` (o número da consulta é o `.I` do `cran.qry` em vez da posição), `--color` / `--no-color`.

`data/modified_queries.txt` usa o formato do `cran.qry`: uma linha `.I <id>` (o `.I` do `cran.qry`, não a posição) seguida de `.T <texto modificado>`,
que pode continuar nas linhas seguintes até o próximo `.I`. Linhas em branco e iniciadas por `#` são ignoradas.

## Resultados (`results/`)

Todos gerados pelos comandos acima; reprodutíveis (não há aleatoriedade).

| Arquivo | Conteúdo |
|---|---|
| `summary.csv` | P@10, R@10, F1@10, MAP, MRR, NDCG@10 por configuração (40 execuções) |
| `per_query.csv` | as mesmas métricas por consulta, por configuração |
| `rankings.csv` | posição, documento, score e grau de relevância; top-100 nas execuções base (vetorial e BM25 k1=1,2 b=0,75), top-10 nas demais |
| `comparison_summary.csv` | item 5: uma linha por (configuração, métrica): média de cada modelo, diferença, vitórias por consulta, empates e p-valor de Wilcoxon, para MAP, P@10, R@10, F1@10, MRR e NDCG@10 |
| `largest_differences.csv` | item 5: as 10 consultas de maior diferença em cada direção **para cada métrica** (coluna `ranked_by`; empates desfeitos pela diferença de AP@all), com todas as métricas dos dois modelos, nº de termos, nº de relevantes e tamanho relativo dos relevantes. Configuração: `--preproc --k1 --b` (padrão stopwords+stemming, 1,2, 0,75) |
| `candidates.csv` | item 6: as 10 melhores candidatas por categoria (BM25 melhor, vetorial melhor, ambos ruins) |
| `case_studies.txt` | item 6: top-5 dos dois modelos, com relevantes marcados, para as candidatas |
| `b_sensitivity.csv` | item 7: consultas cujo AP mais varia com b (k1 fixo), tamanho relativo dos relevantes e sobreposição do top-10 entre b=0 e b=1 |
| `plots/` | gráficos em PNG e PDF, cada um com seu script `.gp` (dados embutidos, editável) |
| `modifications.csv` | item 8: métricas e top-10 das consultas originais e modificadas |
