#' Função que retorna Lista de dados sobre os blocos partidários
#'
#' Nas atividades parlamentares, partidos podem se juntar em blocos partidários. Quando associados, os partidos passam a trabalhar como se fossem um "partidão", com um só líder e um mesmo conjunto de vice-líderes.
#' Os blocos só podem existir até o fim da legislatura em que foram criados: na legislatura seguinte, os mesmos partidos, se associados, formam um novo bloco.
#' Este recurso é uma lista dos blocos em atividade no momento da requisição. Se forem passados números de legislaturas com o parâmetro idLegislatura, são listados também os blocos formados e extintos nessas legislaturas.
#' @param id Número(s) identificador(es) de um ou mais bloco(s), separados por vírgulas.
#' @param idLegislatura Número(s) identificador(es) de uma ou mais legislatura(s), separados por vírgulas. Se presente, faz com que sejam retornados todos os blocos que existiram nessa(s) legislatura(s).
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição.
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Nome do campo pelo qual a lista será ordenada: idLegislatura, id ou nome.
#' @keywords blocos
#' @export
#' @examples
#' GETBlocos()


GETBlocos <- function( id = NULL, idLegislatura = NULL, pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','blocos') , query = list(id = id , idLegislatura = idLegislatura ,
                                                                                        pagina = pagina, itens = itens , ordem = ordem, ordenarPor = ordenarPor)) %>%  content(as = 'text') %>% fromJSON()
  return(lista)
}

#' Função que retorna Informações sobre um bloco partidário específico
#'
#' Retorna informações sobre o bloco identificado por {id} além daquelas obtidas na listagem.
#' @param id Número identificador do bloco de partidos.
#' @keywords blocos
#' @export
#' @examples
#' GETBlocosId(id = '57')

GETBlocosId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','blocos', id) , query = list() )  %>%  content(as = 'text') %>% fromJSON()
  return(lista)
}

#' Função que retorna Listagem e busca de deputados, segundo critérios
#'
#' Retorna uma lista de dados básicos sobre deputados que estiveram em exercício parlamentar em algum intervalo de tempo.
#' Se não for passado um parâmetro de tempo, como idLegislatura ou dataInicio, a lista enumerará somente os deputados em exercício no momento da requisição.
#' @param id Número(s) identificador(es) de um deputado, separados por vírgulas.
#' @param nome Parte nome parlamentar.
#' @param idLegislatura Número(s) identificador(es) de uma ou mais legislatura(s) de que os parlamentares tenham participado, separados por vírgulas.
#' @param siglaUf Uma ou mais sigla(s) de unidades federativas (estados e Distrito Federal). Uma lista de siglas válidas pode ser obtida em /referencias/deputados/siglaUf. Se ausente, serão retornados deputados de todos os estados.
#' @param siglaPartido Uma ou mais sigla(s) de partidos aos quais sejam filiados os deputados. Para obter as siglas válidas, consulte /partidos. Atenção: partidos diferentes podem usar a mesma sigla em diferentes legislaturas!
#' @param siglaSexo Letra que designe o gênero dos parlamentares que se deseja buscar, sendo M para masculino e F para feminino
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição.
#' @param dataInicio Data de início de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param dataFim Data de término de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Nome do campo pelo qual a lista deve ser ordenada: id, idLegislatura, nome, siglaUF ou siglaPartido
#' @keywords deputados
#' @export
#' @examples
#' GETDeputados(idLegislatura = '56')

GETDeputados <- function( id = NULL, nome = NULL, idLegislatura = NULL , siglaUf = NULL,
                          siglaPartido = NULL, siglaSexo = NULL, pagina = NULL, itens = NULL, dataInicio = NULL , dataFim = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados') , query = list(id = id, nome = nome,
                                                                                           idLegislatura = idLegislatura , siglaUf = siglaUf,
                                                                                           siglaPartido = siglaPartido, siglaSexo = siglaSexo,
                                                                                           pagina = pagina, itens = itens,
                                                                                           dataInicio = dataInicio , dataFim = dataFim,
                                                                                           ordem = ordem, ordenarPor = ordenarPor)) %>%  content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Informações detalhadas sobre um deputado específico
#'
#' Retorna os dados cadastrais de um parlamentar identificado por {id} que, em algum momento da história e por qualquer período, entrou em exercício na Câmara.
#' @param id Identificador numérico do parlamentar.
#' @keywords deputados
#' @export
#' @examples
#' GETDeputados(id = '204554')

GETDeputadosId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados', id) , query = list()) %>%  content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna As despesas com exercício parlamentar do deputado
#'
#' Dá acesso aos registros de pagamentos e reembolsos feitos pela Câmara em prol do deputado identificado por {id}, a título da Cota para Exercício da Atividade Parlamentar, a chamada "cota parlamentar".
#' A lista pode ser filtrada por mês, ano, legislatura, CNPJ ou CPF de um fornecedor.
#' Se não forem passados os parâmetros de tempo, o serviço retorna os dados dos seis meses anteriores à requisição.
#' @param id Identificador numérico do parlamentar.
#' @param idLegislatura Número(s) de uma ou mais legislatura(s), separados por vírgulas, em que tenham ocorrido as despesas.
#' @param ano Um ou mais ano(s) de ocorrência das despesas.
#' @param mes Um ou mais número(s) do(s) mês(es) de ocorrência das despesas.
#' @param cnpjCpfFornecedor CNPJ de uma pessoa jurídica, ou CPF de uma pessoa física, fornecedora do produto ou serviço (apenas números)
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição.
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Nome do campo pelo qual a lista deverá ser ordenada: qualquer um dos campos do retorno, e também idLegislatura
#' @keywords deputados
#' @export
#' @examples
#' GETDeputadoDespesa(id = '204554')

GETDeputadoDespesa <- function( id ,idLegislatura = NULL, ano = NULL, mes = NULL , cnpjCpfFornecedor = NULL,
                                pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados', id , 'despesas') , query = list( idLegislatura = idLegislatura, ano = ano,
                                                                                                             mes = mes , cnpjCpfFornecedor = cnpjCpfFornecedor,
                                                                                                             pagina = pagina, itens = itens,
                                                                                                             ordem = ordem, ordenarPor = ordenarPor)) %>%  content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Os discursos feitos por um deputado em eventos diversos
#'
#' Retorna uma lista de informações sobre os pronunciamentos feitos pelo deputado identificado por {id} que tenham sido registrados, em quaisquer eventos, nos sistemas da Câmara.
#' Caso os parâmetros de tempo (dataInicio, dataFim e idLegislatura) não sejam configurados na requisição, são buscados os discursos ocorridos nos sete dias anteriores ao da requisição
#' @param id Identificador numérico do parlamentar.
#' @param idLegislatura Número(s) de uma ou mais legislatura(s), separados por vírgulas, em que tenham ocorrido as despesas.
#' @param dataInicio Data de início de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param dataFim Data de término de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição.
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Qual dos elementos da representação deverá ser usado para aplicar ordenação à lista.
#' @keywords deputados
#' @export
#' @examples
#' GETDeputadoDiscurso(id = '204554')

GETDeputadoDiscurso <- function( id ,idLegislatura = NULL, dataInicio = NULL, dataFim = NULL, pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados', id , 'discursos') , query = list( idLegislatura = idLegislatura, dataInicio = dataInicio,
                                                                                                              dataFim = dataFim, pagina = pagina,
                                                                                                              itens = itens, ordem = ordem, ordenarPor = ordenarPor)) %>%  content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Uma lista de eventos com a participação do parlamentar
#'
#' Retorna uma lista de objetos evento nos quais a participação do parlamentar identificado por {id} era ou é prevista.
#' Um período de tempo pode ser delimitado para a busca.
#' Se não forem passados parâmetros de tempo, são retornados os eventos num período de cinco dias, sendo dois antes e dois depois do dia da requisição.
#' Os itens podem ser ordenados por id, siglaOrgao ou dataHoraInicio.
#' @param id Identificador numérico do parlamentar.
#' @param dataInicio Data de início de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param dataFim Data de término de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição.
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Qual dos elementos da representação deverá ser usado para aplicar ordenação à lista.
#' @keywords deputados
#' @export
#' @examples
#' GETDeputadoDiscurso(id = '204554')

GETDeputadoEventos <- function( id , dataInicio = NULL, dataFim = NULL, pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados', id , 'eventos') , query = list(  dataInicio = dataInicio,
                                                                                                             dataFim = dataFim, pagina = pagina,
                                                                                                             itens = itens, ordem = ordem, ordenarPor = ordenarPor)) %>%  content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna As frentes parlamentares das quais um deputado é integrante
#'
#' Retorna uma lista de informações básicas sobre as frentes parlamentares das quais o parlamentar identificado por {id} seja membro, ou, no caso de frentes existentes em legislaturas anteriores, tenha encerrado a legislatura como integrante.
#' @param id Identificador numérico do parlamentar.
#' @keywords deputados
#' @export
#' @examples
#' GETDeputadoFrentes(id = '204554')

GETDeputadoFrentes <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados', id , 'frentes') , query = list()) %>%  content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Os órgãos dos quais um deputado é integrante
#'
#' Retorna uma lista de órgãos, como as comissões e procuradorias, dos quais o deputado identificado por {id} participa ou participou durante um intervalo de tempo.
#' Cada item identifica um órgão, o cargo ocupado pelo parlamentar neste órgão (como presidente, vice-presidente, titular ou suplente) e as datas de início e fim da ocupação deste cargo.
#' Se não for passado algum parâmetro de tempo, são retornados os órgãos ocupados pelo parlamentar no momento da requisição. Neste caso a lista será vazia se o deputado não estiver em exercício.
#' @param id O identificador numérico do parlamentar.
#' @param dataInicio Data de início de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param dataFim Data de término de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisiçã
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Nome do campo de dados pelo qual a lista deve ser ordenada: idOrgao, siglaOrgao, nomeOrgao, titulo, dataInicio ou dataFim
#' @keywords deputados
#' @export
#' @examples
#' GETDeputadoOrgaos(id = '204554')

GETDeputadoOrgaos <- function( id , dataInicio = NULL, dataFim = NULL, pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados', id , 'orgaos') , query = list(  dataInicio = dataInicio,
                                                                                                            dataFim = dataFim, pagina = pagina,
                                                                                                            itens = itens, ordem = ordem, ordenarPor = ordenarPor)) %>%  content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Lista de eventos ocorridos ou previstos nos diversos órgãos da Câmara
#'
#' Retorna uma lista cujos elementos trazem informações básicas sobre eventos dos órgãos legislativos da Câmara, previstos ou já ocorridos, em um certo intervalo de tempo.
#' Esse intervalo pode ser configurado pelos parâmetros de data e hora listados abaixo. Se nenhum for passado, são listados eventos dos cinco dias anteriores, dos cinco dias seguintes e do próprio dia em que é feita a requisição.
#' @param id Identificador(es) numérico(s) de um ou mais eventos, separados por vírgulas.
#' @param codTipoEvento Um ou mais identificador(es) numérico(s) do(s) tipo(s) de evento que se deseja obter. Os valores válidos podem ser obtidos por uma requisição a /referencias/tiposEvento.
#' @param codSituacao Um ou mais identificador(es) numéricos de tipo(s) de situação de evento, separados por vírgula. Valores válidos podem ser obtidos em /referencias/situacoesEvento.
#' @param codTipoOrgao Um ou mais identificador(es) numérico(s) de tipo(s) de órgão(s) realizadores dos eventos que se deseja obter, separados por vírgula. Os valores válidos podem ser obtidos em /referencias/tiposOrgao.
#' @param idOrgao Um ou mais identificador(es) numérico(s) de órgão(s), separados por vírgula. Os identificadores podem ser obtidos em uma requisição a /orgaos.
#' @param dataInicio Data de início de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param dataFim Data de término de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param horaInicio Hora inicial de um intervalo de tempo, no formato hh:mm, em horário de Brasília.
#' @param horaFim Hora final de um intervalo de tempo, no formato hh:mm, em horário de Brasília.
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição.
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Nome do campo pelo qual a lista deve ser ordenada: id, dataHoraInicio, dataHoraFim, descricaoSituacao, descricaoTipo ou titulo
#' @keywords eventos
#' @export
#' @examples
#' GETEventos()

GETEventos <- function( id = NULL , codTipoEvento = NULL, codSituacao = NULL, codTipoOrgao = NULL, idOrgao = NULL, dataInicio = NULL, dataFim = NULL, horaInicio = NULL , horaFim = NULL, pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','eventos') , query = list( id = id , codTipoEvento = codTipoEvento,
                                                                                          codSituacao = codSituacao, codTipoOrgao = codTipoOrgao,
                                                                                          idOrgao = idOrgao, dataInicio = dataInicio,
                                                                                          dataFim = dataFim, horaInicio = horaInicio ,
                                                                                          horaFim = horaFim, pagina = pagina,
                                                                                          itens = itens, ordem = ordem, ordenarPor = ordenarPor )) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Informações detalhadas sobre um evento específico
#'
#' Retorna um conjunto detalhado de informações sobre o evento da Câmara identificado por id.
#' @param id O identificador numérico do evento do qual se deseja informações
#' @keywords eventos
#' @export
#' @examples
#' GETEventoId()

GETEventoId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','eventos', id) , query = list()) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Os deputados participantes de um evento específico
#'
#' Retorna uma lista de dados resumidos sobre deputados participantes do evento identificado por {id}.
#' Se o evento já ocorreu, a lista identifica os deputados que efetivamente registraram presença no evento. Se o evento ainda não ocorreu, a lista mostra os deputados que devem participar do evento, por serem convidados ou por serem membros do(s) órgão(s) responsável pelo evento.
#' @param id O identificador numérico do evento do qual se deseja informações
#' @keywords eventos
#' @export
#' @examples
#' GETEventoDeputados()

GETEventoDeputados <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','eventos', id, 'deputados') , query = list()) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Lista de órgãos organizadores do evento {id}
#'
#' Retorna uma lista em que cada item é um conjunto mínimo de dados sobre o(s) órgão(s) responsável(veis) pelo evento identificado por {id}. Atualmente, mas provisoriamente, esta informação já vem incorporada ao retorno de /eventos/{id}, mas este endpoint facilita a importação destes dados em planilhas eletrônicas.
#' @param id O identificador numérico da entidade.
#' @keywords eventos
#' @export
#' @examples
#' GETEventoOrgaos()

GETEventoOrgaos <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','eventos', id, 'orgaos') , query = list()) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Lista de proposições que foram ou deverão ser avaliadas em um evento de caráter deliberativo
#'
#' Se o evento {id} for de caráter deliberativo (uma reunião ordinária, por exemplo) este serviço retorna a lista de proposições previstas para avaliação pelos parlamentares. Cada item identifica, se as informações estiverem disponíveis, a proposição avaliada, o regime de preferência para avaliação, o relator e seu parecer, o resultado da apreciação e a votação realizada.
#' @param id O identificador numérico da entidade.
#' @keywords eventos
#' @export
#' @examples
#' GETEventoPauta()

GETEventoPauta <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','eventos', id, 'pauta') , query = list()) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Informações detalhadas de votações sobre um evento específico
#'
#' Retorna uma lista de dados básicos sobre votações que tenham sido realizadas no evento identificado por {id}. Votações só ocorrem em eventos de caráter deliberativo. Dados complementares sobre cada votação listada podem ser obtidos no recurso /votacoes/{id}.
#' Para compreender melhor os dados sobre votações, veja a página de tutorial do Portal de Dados Abertos.
#' @param id O identificador numérico da entidade.
#' @keywords eventos
#' @export
#' @examples
#' GETEventoVotacoes()

GETEventoVotacoes <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','eventos', id, 'votacoes') , query = list()) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Lista de frentes parlamentares de uma ou mais legislaturas
#'
#' Retorna uma lista de informações sobre uma frente parlamentar - um agrupamento oficial de parlamentares em torno de um determinado tema ou proposta.
#' As frentes existem até o fim da legislatura em que foram criadas, e podem ser recriadas a cada legislatura. Algumas delas são compostas por deputados e senadores.
#' Um ou mais número de legislatura(s) pode(m) ser passado(s) como parâmetro, mas se for omitido são retornadas todas as frentes parlamentares criadas desde 2003.
#' @param idLegislatura Número da(s) legislatura(s), separados por vírgulas, às quais os dados buscados devem corresponder.
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @keywords frentes
#' @export
#' @examples
#' GETFrentes()

GETFrentes <- function( idLegislatura = NULL , pagina = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','frentes') , query = list(idLegislatura = idLegislatura , pagina = pagina)) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Informações detalhadas sobre uma frente parlamentar
#'
#' Este recurso traz informações detalhadas sobre a frente parlamentar identificada por {id}.
#' @param id Identificador numérico da frente parlamentar.
#' @keywords frentes
#' @export
#' @examples
#' GETFrenteId()

GETFrenteId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','frentes', id) , query = list()) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Os deputados que participam de uma frente parlamentar
#'
#' Uma lista dos deputados participantes da frente parlamentar identificada por {id} e os papéis que exerceram nessa frente (signatário, coordenador ou presidente).
#' Observe que, mesmo no caso de frentes parlamentares mistas (compostas por deputados e senadores), são retornados apenas dados sobre os deputados.
#' @param id Identificador numérico da frente parlamentar.
#' @keywords frentes
#' @export
#' @examples
#' GETFrenteMembros()

GETFrenteMembros <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','frentes', id, 'membros') , query = list()) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Os períodos de mandatos e atividades parlamentares da Câmara
#'
#' Legislatura é o nome dado ao período de trabalhos parlamentares entre uma eleição e outra.
#' Este serviço retorna uma lista em que cada item contém as informações básicas sobre um desses períodos.
#' Os números que identificam as legislaturas são sequenciais, desde a primeira que ocorreu.
#' @param id Um ou mais número(s) de legislatura(s), separados por vírgulas. Se omitido, serão retornados dados sobre todas as legislaturas.
#' @param data Uma ou mais data(s) no formato AAAA-MM-DD, separadas por vírgulas. Se este parâmetro estiver presente, a requisição retornará as informações básicas sobre as legislaturas que estavam em curso nas datas informadas.
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição.
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Qual dos elementos da representação deverá ser usado para aplicar ordenação à lista.
#' @keywords LEGISLATURAS
#' @export
#' @examples
#' GETLegislatura(id = '56')

GETLegislatura <- function( id = NULL, data = NULL, pagina  = NULL, itens  = NULL, ordem  = NULL, ordenarPor  = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','legislaturas') , query = list(id = NULL, data = NULL,
                                                                                              pagina  = NULL, itens  = NULL,
                                                                                              ordem  = NULL, ordenarPor  = NULL )) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Informações extras sobre uma determinada legislatura da Câmara
#'
#' Retorna informações adicionais sobre o período de atividades da Câmara identificado por {id}.
#' @param id Número da legislatura da qual se quer os dados.
#' @keywords LEGISLATURAS
#' @export
#' @examples
#' GETLegislaturaId(id = '56')

GETLegislaturaId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','legislaturas', id ) , query = list()) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Quais deputados fizeram parte da Mesa Diretora em uma legislatura
#'
#' Retorna uma lista com dados básicos sobre todos os deputados que ocuparam algum posto na Mesa Diretora da Câmara em algum período de tempo dentro da legislatura identificada por {id}.
#' Normalmente, cada legislatura tem duas Mesas Diretoras, com presidente, dois vice-presidentes, quatro secretários parlamentares e os suplentes dos secretários.
#' @param id Número da legislatura da qual se deseja os dados.
#' @param dataInicio Dia de início do intervalo de tempo do qual se deseja saber a composição da Mesa, no formato AAAA-MM-DD.
#' @param dataFim Data de término do intervalo de tempo do qual se deseja saber a composição da Mesa, no formato AAAA-MM-DD.
#' @keywords LEGISLATURAS
#' @export
#' @examples
#' GETLegislaturaMesa(id = '56')

GETLegislaturaMesa <- function( idLegislatura , dataInicio = NULL, dataFim = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','legislaturas', idLegislatura , 'mesa') , query = list( id = id,  dataInicio = dataInicio,
                                                                                                                        dataFim = dataInicio)) %>%  content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Os partidos políticos que têm ou já tiveram parlamentares em exercício na Câmara
#'
#' Retorna uma lista de dados básicos sobre os partidos políticos que têm ou já tiveram deputados na Câmara.
#' Se não forem passados parâmetros, o serviço retorna os partidos que têm deputados em exercício no momento da requisição.
#' É possível obter uma lista de partidos representados na Câmara em um certo intervalo de datas ou de legislaturas. Se um intervalo e uma ou mais legislatura(s) não coincidentes forem passados, todos os intervalos de tempo serão somados.
#' Também se pode fazer busca por uma ou mais sigla(s), mas atenção: em diferentes legislaturas, pode haver mais de um partido usando a mesma sigla.
#' @param sigla Sigla de um ou mais partido(s), separadas por vírgulas.
#' @param dataInicio Data de início de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param dataFim Data de término de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param idLegislatura Número da(s) legislatura(s), separados por vírgulas, às quais os dados buscados devem corresponder.
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição.
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Nome do campo pelo qual a lista deve ser ordenada: id, sigla, nome, dataInicio ou dataFim
#' @keywords partidos
#' @export
#' @examples
#' GETPartidos(sigla = 'PSL')

GETPartidos <- function( sigla = NULL, dataInicio = NULL, dataFim  = NULL, idLegislatura  = NULL, pagina  = NULL, itens  = NULL, ordem = NULL , ordenarPor = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','partidos') , query = list(sigla = sigla, dataInicio = dataInicio,
                                                                                          dataFim  = dataFim, idLegislatura  = idLegislatura,
                                                                                          pagina  = pagina, itens  = itens, ordem = ordem , ordenarPor = ordenarPor)) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Informações detalhadas sobre um partido
#'
#' @param id Id do Partido
#' @keywords partidos
#' @export
#' @examples
#' GETPartidoId()

GETPartidoId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','partidos', id) , query = list()) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Uma lista dos parlamentares de um partido durante um período
#'
#' Retorna uma lista de deputados que estão ou estiveram em exercício pelo partido {id}.
#' Opcionalmente, pode-se usar os parâmetros dataInicio, dataFim ou idLegislatura para se obter uma lista de deputados filiados ao partido num certo intervalo de tempo. Isso é equivalente ao serviço /deputados com filtro por partido, mas é melhor para obter informações sobre membros de partidos já extintos.
#' @param id Id do Partido
#' @param dataInicio Data de início de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param dataFim Data de término de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param idLegislatura Número da(s) legislatura(s), separados por vírgulas, às quais os dados buscados devem corresponder.
#' @param pagina Qual dos elementos da representação deverá ser usado para aplicar ordenação à lista.
#' @param itens O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordem Número máximo de itens na “página” que se deseja obter com esta requisição.
#' @param ordenarPor Número da “página” de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @keywords partidos
#' @export
#' @examples
#' GETPartidoId()

GETPartidoMembros <- function( id , dataInicio = NULL, dataFim  = NULL, idLegislatura  = NULL, pagina  = NULL, itens  = NULL, ordem = NULL , ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','partidos', id, 'membros') , query = list(dataInicio = dataInicio, dataFim  = dataFim,
                                                                                                         idLegislatura  = idLegislatura, pagina  = pagina,
                                                                                                         itens  = itens, ordem = ordem , ordenarPor = ordenarPor)) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}


#' Função que retorna Lista configurável de proposições na Câmara
#'
#' Lista de informações básicas sobre projetos de lei, resoluções, medidas provisórias, emendas, pareceres e todos os outros tipos de proposições na Câmara.
#' Por padrão, são retornadas todas as proposições que foram apresentadas ou tiveram alguma mudança de situação nos últimos 30 dias. Esse intervalo de tramitação pode ser configurado pelos parâmetros dataInicio e dataFim.
#' Se for(em) passado(s) um ou mais dos parâmetros…
#' id
#' ano
#' dataApresentacaoInicio
#' dataApresentacaoFim
#' idAutor
#' autor
#' …o intervalo de tramitação só será levado em consideração se os parâmetros dataInicio e/ou dataFim estiverem explicitamente configurados. Se não estiverem, poderão ser listadas proposições que não tiveram tramitação recente (e a resposta pode demorar bastante).
#' @param id Número(s) identificador(es) de uma ou mais proposições no Dados Abertos, separados por vírgulas.
#' @param siglaTipo Uma ou mais sigla(s) separadas por vírgulas do(s) tipo(s) das proposições que se deseja obter. A lista de tipos e siglas existentes pode ser obtida em /referencias/proposicoes/siglaTipo
#' @param numero Um ou mais número(s), separados por vírgula, oficialmente atribuídos às proposições segundo o art. 137 do Regimento Interno, como “PL 1234/2016”
#' @param ano Um ou mais ano(s) de apresentação das proposições que serão listadas, separados por vírgulas, no formato AAAA
#' @param idDeputadoAutor Um ou mais números identificador(es), separados por vírgula, do(s) deputado(s) autor(es) das proposições que serão listadas. Cada número deve ser o identificador exclusivo de um parlamentar no Dados Abertos
#' @param autor Nome ou parte do nome do(s) autor(es) das proposições que se deseja obter. Deve estar entre aspas
#' @param siglaPartidoAutor Uma ou mais sigla(s) separadas por vírgulas do(s) partido(s) a que pertençam os autores das proposições a serem listadas
#' @param idPartidoAutor Identificador numérico no Dados Abertos do partido a que pertençam os autores das proposições que serão listadas. Esses identificadores podem ser obtidos em /partidos e são mais precisos do que as siglas, que podem ser usadas por partidos diferentes em épocas diferentes
#' @param siglaUfAutor Uma ou mais sigla(s) de unidade(s) da federação (estados e Distrito Federal) pela(s) qual(quais) o(s) autor(es) das proposições selecionadas tenha(m) sido eleito(s)
#' @param keywords Uma ou mais palavras chaves sobre o tema a que a proposição se relaciona
#' @param tramitacaoSenado Indicador booleano, com valor TRUE ou FALSE para trazer apenas proposições que já tenha tramitado no Senado
#' @param dataInicio Data do início do intervalo de tempo em que tenha havido tramitação das proposições a serem listadas, no formato AAAA-MM-DD. Se omitido, é assumido como a data de 30 dias anteriores à proposição
#' @param dataFim Data do fim do intervalo de tempo em que tenha havido tramitação das proposições a serem listadas. Se omitido, é considerado ser o dia em que é feita a requisição
#' @param dataApresentacaoInicio Data do início do intervalo de tempo em que tenham sido apresentadas as proposições a serem listadas, no formato AAAA-MM-DD
#' @param dataApresentacaoFim Data do fim do intervalo de tempo em que tenham sido apresentadas as proposições a serem listadas
#' @param codSituacao Código(s) numérico(s), separados por vírgulas, do tipo de situação em que se encontram as proposições que serão listadas. As situações possíveis podem ser obtidas em /referencias/proposicoes/codSituacao. Atenção: este parâmetro pode apresentar resultados inesperados, por problemas com o registro dos dados.
#' @param codTema Código(s) numérico(s), separados por vírgulas, das áreas temáticas das proposições que serão listadas. Os temas possíveis podem ser obtidos em /referencias/proposicoes/codTema
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição.
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Nome do campo pelo qual a lista deve ser ordenada: id, codTipo, siglaTipo, numero ou ano
#' @keywords proposições
#' @export
#' @examples
#' GETProposicoes( ano = '2020' )

GETProposicoes <- function( id = NULL, siglaTipo = NULL,
                            numero  = NULL, ano  = NULL,
                            idDeputadoAutor  = NULL, autor  = NULL,
                            siglaPartidoAutor = NULL , idPartidoAutor = NULL,
                            siglaUfAutor = NULL, keywords = NULL, tramitacaoSenado = NULL,
                            dataInicio = NULL, dataFim = NULL, dataApresentacaoInicio = NULL,
                            dataApresentacaoFim = NULL, codSituacao = NULL, codTema = NULL,
                            pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes') , query = list(id = id, siglaTipo = siglaTipo,
                                                                                             numero  = numero, ano  = ano,
                                                                                             idDeputadoAutor  = idDeputadoAutor, autor  = autor,
                                                                                             siglaPartidoAutor = siglaPartidoAutor , idPartidoAutor = idPartidoAutor,
                                                                                             siglaUfAutor = siglaUfAutor, keywords = keywords,
                                                                                             tramitacaoSenado = tramitacaoSenado, dataInicio = dataInicio,
                                                                                             dataFim = dataFim, dataApresentacaoInicio = dataApresentacaoInicio,
                                                                                             dataApresentacaoFim = dataApresentacaoFim, codSituacao = codSituacao,
                                                                                             codTema = codTema, pagina = pagina, itens = itens,
                                                                                             ordem = ordem, ordenarPor = ordenarPor)) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Informações detalhadas sobre uma proposição específica
#'
#'
#' @param id Identificador numérico da proposição
#' @keywords proposições
#' @export
#' @examples
#' GETProposicaoId()


GETProposicaoId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes', id) , query = list() ) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Lista pessoas e/ou entidades autoras de uma proposição
#'
#' Retorna uma lista em que cada item identifica uma pessoa ou entidade que é autora da proposição identificada por {id}. Além de deputados, também podem ser autores de proposições os senadores, a sociedade civil, assembleias legislativas e os poderes Executivo e Judiciário.
#' Pelo Regimento da Câmara, todos os que assinam uma proposição são considerados autores (art. 102), tanto os proponentes quanto os apoiadores.
#' Para obter mais informações sobre cada autor, é recomendável acessar, se disponível, a URL que é valor do campo uri.
#' @param id Identificador numérico da proposição
#' @keywords proposições
#' @export
#' @examples
#' GETProposicaoAutores()

GETProposicaoAutores <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes', id, 'autores') , query = list() ) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Uma lista de proposições relacionadas a uma em especial
#'
#' Lista de informações básicas sobre proposições que de alguma forma se relacionam com a proposição identificada por {id}, como pareceres, requerimentos, substitutivos, etc.
#' @param id Identificador numérico da proposição
#' @keywords proposições
#' @export
#' @examples
#' GETProposicaoRelacionadas()

GETProposicaoRelacionadas <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes', id, 'relacionadas') , query = list() ) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Lista de áreas temáticas de uma proposição
#'
#' Lista em que cada item traz informações sobre uma área temática à qual a proposição identificada por {id} se relaciona, segundo classificação oficial do Centro de Documentação e Informação da Câmara.
#' @param id Identificador numérico da proposição
#' @keywords proposições
#' @export
#' @examples
#' GETProposicaoTemas()

GETProposicaoTemas <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes', id, 'temas') , query = list() ) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna O histórico de passos na tramitação de uma proposta
#'
#' Lista que traz, como cada item, um “retrato” de informações que podem ser alteradas a cada etapa de tramitação na vida de uma proposição (como regime de tramitação e situação) e informações sobre o que causou esse novo estado.
#' Esta representação das tramitações ainda é provisória.
#' @param id Identificador numérico da proposição
#' @param dataInicio Data do início da tramitação, no formato AAAA-MM-DD.
#' @param dataFim Data do fim da tramitação, no formato AAAA-MM-DD.
#' @keywords proposições
#' @export
#' @examples
#' GETProposicaoTramitacoes()

GETProposicaoTramitacoes <- function( id , dataInicio = NULL, dataFim = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes', id , 'tramitacoes') , query = list(dataInicio = dataInicio, dataFim = dataFim ) ) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Informações detalhadas de votações sobre uma proposição específica
#'
#' Retorna uma lista de identificadores básicos sobre as votações na Câmara que tiveram a proposição {id} como objeto ou como afetada pelos seus resultados. Dados complementares sobre cada votação listada podem ser obtidos no recurso /votacoes/{id}.
#' Para compreender melhor os dados sobre votações, veja a página de tutorial do Portal de Dados Abertos.
#' @param id Identificador numérico da proposição
#' @param ordem Nome do campo pelo qual a lista será ordenada: id, dataHoraRegistro
#' @param ordenarPor Identificador numérico da proposição
#' @keywords proposições
#' @export
#' @examples
#' GETProposicaoVotacoes()

GETProposicaoVotacoes <- function( id , ordem = NULL, ordenarPor = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes', id , 'votacoes') , query = list(ordem = ordem, ordenarPor = ordenarPor ) ) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Lista das votações da Câmara
#'
#' Retorna uma lista de informações básicas sobre as votações ocorridas em eventos dos diversos órgãos da Câmara.
#' Se não forem passados parâmetros que delimitem o intervalo de tempo da pesquisa, são retornados dados sobre todas as votações ocorridas nos últimos 30 dias, em eventos de todos os órgãos.
#' Os parâmetros de data permitem estender o período, mas por enquanto é necessário que as duas datas sejam de um mesmo ano. Quando apenas uma delas está presente, são retornadas somente as votações ocorridas no mesmo ano, antes de dataFim ou após dataInicio.
#' Também é possível filtrar a listagem por identificadores de órgãos da Câmara, de proposições e de eventos.
#' Quando não há identificação da proposição que foi efetivamente votada, é preciso consultar o endpoint /votacoes/{id} para obter uma lista de proposições das quais uma pode ter sido o objeto da votação.
#' Para mais informações sobre o uso dos endpoints de votações, veja a página de tutorial do Portal de Dados Abertos.
#' @param id Um ou mais identificador(es) alfanuméricos de votação, separados por vírgulas, para que seja(m) listado(s) dados sobre uma ou mais votações específicas.
#' @param idProposicao Um ou mais identificador(es) numéricos de proposições, que podem ser obtidos por meio do recurso /proposicoes. Se presente, listará as votações que tiveram a(s) proposição(ções) como objeto de votação ou que afetaram as proposições listadas.
#' @param idEvento Identificador de um ou mais evento(s) realizado(s) na Câmara, separados por vírgula, nos quais tenham sido realizadas as votações a serem listadas. Os identificadores podem ser obtidos por meio do recurso /eventos. Somente os eventos deliberativos podem ter votações. Os eventos podem ter ocorrido fora do intervalo de tempo padrão ou definido por dataInicio e/ou dataFim.
#' @param idOrgao Um ou mais identificador(es) numéricos de órgãos da Câmara, separados por vírgulas. Se presente, serão retornadas somente votações dos órgãos enumerados. Os identificadores existentes podem ser obtidos por meio do recurso /orgaos.
#' @param dataInicio Data em formato AAAA-MM-DD para início do intervalo de tempo no qual tenham sido realizadas as votações a serem listadas. Se usado sozinho, esse parâmetro faz com que sejam retornadas votações ocorridas dessa data até o fim do mesmo ano. Se usado com dataFim, as duas datas devem ser de um mesmo ano.
#' @param dataFim Data em formato AAAA-MM-DD que define o fim do intervalo de tempo no qual tenham sido realizadas as votações a serem listadas. Se usado sozinho, esse parâmetro faz com que sejam retornadas todas as votações ocorridas desde 1º de janeiro do mesmo ano até esta data. Se usado com dataInicio, é preciso que as duas datas sejam de um mesmo ano.
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição. O valor padrão e máximo para este endpoint é 200, e valores maiores serão ignorados.
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Nome do campo pelo qual a lista será ordenada. Pode ser id, idOrgao, siglaOrgao, idEvento, idProposicao, data, dataHoraRegistro ou idProposicaoObjeto.
#' @keywords votações
#' @export
#' @examples
#' GETVotacoes()

GETVotacoes<- function( id = NULL, idProposicao = NULL,
                        idEvento  = NULL, idOrgao  = NULL,
                        dataInicio  = NULL, dataFim  = NULL,
                        pagina = NULL , itens = NULL,
                        ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", progress() ,url = base_url, path = list('api','v2','votacoes') , query = list(id = id, idProposicao = idProposicao,
                                                                                                      idEvento  = idEvento, idOrgao  = idOrgao,
                                                                                                      dataInicio  = dataInicio, dataFim  = dataFim,
                                                                                                      pagina = pagina , itens = itens,
                                                                                                      ordem = ordem, ordenarPor = ordenarPor)) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Informações detalhadas sobre uma votação da Câmara
#'
#' Retorna um conjunto detalhado de dados sobre a votação identificada por {id}, tais como as proposições que podem ter sido o objeto da votação e os efeitos de tramitação de outras proposições que eventualmente tenham sido cadastrados em consequência desta votação.
#' Para compreender melhor os dados retornados, veja o tutorial sobre votações do Portal de Dados Abertos.
#' @param id Identificador alfanumérico da votação
#' @keywords votações
#' @export
#' @examples
#' GETVotacaoId()

GETVotacaoId<- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','votacoes', id) , query = list()) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna O voto recomendado pelas lideranças aos seus deputados em uma votação
#'
#' Em muitas votações, os líderes de partidos e blocos – as bancadas – fazem recomendações de voto para seus parlamentares. Essas orientações de uma votação também são feitas pelas lideranças de Governo, Minoria e as mais recentes Maioria e Oposição. Uma liderança também pode liberar a bancada para que cada deputado vote como quiser, ou entrar em obstrução, para que seus parlamentares não sejam contados para o quórum da votação.
#' Se a votação identificada por {id} teve orientações, este recurso retorna uma lista em que cada item contém os identificadores de um partido, bloco ou liderança, e o posicionamento ou voto que foi recomendado aos seus parlamentares.
#' Até o momento, só estão disponíveis dados sobre orientações dadas em votações no Plenário.
#' @param id Identificador alfanumérico da votação
#' @keywords votações
#' @export
#' @examples
#' GETVotacaoOrientacao()

GETVotacaoOrientacao<- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','votacoes', id, 'orientacoes') , query = list()) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Como cada parlamentar votou em uma votação nominal e aberta
#'
#' Se {id} é o identificador de uma votação da Câmara nominal que não tenha sido secreta, este endpoint retorna uma lista em que cada item contém os identificadores básicos de um deputado e o voto ou posicionamento que ele registrou.
#' O resultado é uma lista vazia se {id} foi uma votação simbólica, em que os votos individuais não são contabilizados. Mas há algumas votações simbólicas que também têm registros de "votos": nesses casos, normalmente se trata de parlamentares que pediram expressamente que seus posicionamentos fossem registrados.
#' Não são listados parlamentares ausentes à votação.
#' @param id Identificador alfanumérico da votação
#' @keywords votações
#' @export
#' @examples
#' GETVotacaoVotos()

GETVotacaoVotos<- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','votacoes', id, 'votos') , query = list()) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna A lista das comissões e outros órgãos legislativos da Câmara
#'
#' Retorna uma lista de informações básicas sobre os órgãos legislativos e seus identificadores, tipos e descrições.
#' Pela query string é possível filtrar a lista por identificadores, tipos de órgãos, sigla, situação do órgão ou período de tempo em que os órgãos estiveram ativos, se aplicável.
#' @param id Um ou mais identificador(es) numérico(s), separados por vírgulas.
#' @param sigla Uma ou mais sigla(s) oficialmente usadas para designar um ou mais órgão(s) da Câmara, separadas por vírgulas.
#' @param codTipoOrgao Um ou mais identificador(es) numérico(s) do(s) tipo(s) de órgãos que se deseja buscar dados. Pode(m) ser obtido(s) em /referencias/orgaos/codTipoOrgao.
#' @param dataInicio Data de início, no formato AAAA-MM-DD, de um intervalo de tempo no qual os órgãos buscados devem ter estado em atividade.
#' @param dataFim Data de término, no formato AAAA-MM-DD, de um intervalo de tempo no qual os órgãos buscados devem ter estado em atividade.
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição.
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Nome do campo pelo qual a lista será ordenada: id, sigla, nome, apelido, codTipoOrgao, tipoOrgao, dataInicio ou dataFim
#' @keywords orgãos
#' @export
#' @examples
#' GETOrgaos()

GETOrgaos <- function( id = NULL, sigla = NULL,
                       codTipoOrgao  = NULL, dataInicio  = NULL,
                       dataFim  = NULL, pagina  = NULL,
                       itens = NULL , ordem = NULL,
                       ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','orgaos') , query = list(id = id, sigla = sigla,
                                                                                        codTipoOrgao  = codTipoOrgao, dataInicio  = dataInicio,
                                                                                        dataFim  = dataFim, pagina  = pagina,
                                                                                        itens = itens , ordem = ordem,
                                                                                        ordenarPor = ordenarPor)) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Informações detalhadas sobre um órgão da Câmara
#'
#' Retorna todas as informações disponíveis sobre o órgão da Câmara identificado por id.
#' @param id Identificador numérico do órgão
#' @keywords orgãos
#' @export
#' @examples
#' GETOrgaosId()

GETOrgaosId <- function( id){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','orgaos', id) , query = list()) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Os eventos ocorridos ou previstos em um órgão legislativo
#'
#' Retorna uma lista de informações resumidas dos eventos realizados (ou a realizar) pelo órgão legislativo identificado por {id}.
#' Por padrão, são retornados eventos em andamento ou previstos para o mesmo dia, dois dias antes e dois dias depois da requisição. Parâmetros podem ser passados para alterar esse período, bem como os tipos de eventos.
#' @param id Identificador numérico do órgão
#' @param idTipoEvento Um ou mais identificador(es) numérico(s) de tipos de evento que se deseja obter. os valores válidos podem ser obtidos por uma requisição anterior a /referencias/tiposEvento.
#' @param dataInicio Data de início de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param dataFim Data de término de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição.
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Qual dos elementos da representação deverá ser usado para aplicar ordenação à lista.
#' @keywords orgãos
#' @export
#' @examples
#' GETOrgaoEventos()

GETOrgaoEventos <- function( id , idTipoEvento = NULL,
                             dataInicio  = NULL,
                             dataFim  = NULL, pagina  = NULL,
                             itens = NULL , ordem = NULL,
                             ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','orgaos', id, 'eventos') , query = list(idTipoEvento = idTipoEvento, dataInicio  = dataInicio,
                                                                                                       dataFim  = dataFim, pagina  = pagina,
                                                                                                       itens = itens , ordem = ordem,
                                                                                                       ordenarPor = ordenarPor)) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Lista de cargos de um órgão e parlamentares que os ocupam.
#'
#' Retorna uma lista de dados resumidos que identificam cada parlamentar e o cargo ou posição que ocupa ou ocupou no órgão parlamentar identificado por {id} durante um certo período de tempo.
#' Se não forem passados parâmetros que delimitem esse período, o serviço retorna os membros do órgão no momento da requisição. Se o órgão não existir mais ou não estiver instalado, é retornada uma lista vazia.
#' @param id Identificador numérico do órgão
#' @param dataInicio Data de início de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param dataFim Data de término de um intervalo de tempo, no formato AAAA-MM-DD.
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição.
#' @keywords orgãos
#' @export
#' @examples
#' GETOrgaoMembros()

GETOrgaoMembros <- function( id ,
                             dataInicio  = NULL,
                             dataFim  = NULL, pagina  = NULL,
                             itens = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','orgaos', id, 'membros') , query = list(dataInicio  = dataInicio,
                                                                                                       dataFim  = dataFim, pagina  = pagina,
                                                                                                       itens = itens)) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

#' Função que retorna Informações detalhadas sobre votações de um órgão da Câmara
#'
#' Retorna uma lista de dados básicos de votações que tenham sido realizadas em eventos realizados no órgão {id}.
#' Se {id} for um órgão permanente da Câmara, são retornados, por padrão, dados sobre as votações realizadas pelo órgão nos últimos 30 dias. Esse período pode ser alterado com o uso dos parâmetros dataInicio e/ou dataFim, que por enquanto são limitados a selecionar somente votações ocorridas em um mesmo ano.
#' Caso {id} seja um órgão temporário, como uma comissão especial, são listadas por padrão todas as votações ocorridas no órgão, em qualquer período de tempo.
#' Dados complementares sobre cada votação listada podem ser obtidos no recurso /votacoes/{id}.
#' Para compreender melhor os dados sobre votações, veja a página de tutorial do Portal de Dados Abertos.
#' @param id Identificador numérico do órgão
#' @param idProposicao Um ou mais identificador(es) numéricos de proposições, que podem ser obtidos por meio do recurso /proposicoes. Se presente, listará as votações que tiveram a(s) proposição(ções) como objeto de votação ou que afetaram as proposições listadas.
#' @param dataInicio Data em formato AAAA-MM-DD para início do intervalo de tempo no qual tenham sido realizadas as votações a serem listadas. Se usado sozinho, esse parâmetro faz com que sejam retornadas votações ocorridas dessa data até o fim do mesmo ano. Se usado com dataFim, as duas datas devem ser de um mesmo ano.
#' @param dataFim Data em formato AAAA-MM-DD que define o fim do intervalo de tempo no qual tenham sido realizadas as votações a serem listadas. Se usado sozinho, esse parâmetro faz com que sejam retornadas todas as votações ocorridas desde 1º de janeiro do mesmo ano até esta data. Se usado com dataInicio, é preciso que as duas datas sejam de um mesmo ano.
#' @param pagina Número da página de resultados, a partir de 1, que se deseja obter com a requisição, contendo o número de itens definido pelo parâmetro itens. Se omitido, assume o valor 1.
#' @param itens Número máximo de itens na página que se deseja obter com esta requisição. O valor padrão e máximo para este endpoint é 200, e valores maiores serão ignorados.
#' @param ordem O sentido da ordenação: asc para A a Z ou 0 a 9, e desc para Z a A ou 9 a 0.
#' @param ordenarPor Nome do campo pelo qual a lista será ordenada. Pode ser id, idOrgao, siglaOrgao, idEvento, idProposicao, data, dataHoraRegistro ou idProposicaoObjeto.
#' @keywords orgãos
#' @export
#' @examples
#' GETOrgaoVotacoes()

GETOrgaoVotacoes <- function( id ,
                              idProposicao  = NULL,
                              dataInicio  = NULL, dataFim  = NULL,
                              pagina = NULL, itens = NULL, ordem = NULL, ordenarPor= NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','orgaos', id, 'votacoes') , query = list(idProposicao  = idProposicao,
                                                                                                        dataInicio  = dataInicio, dataFim  = dataFim,
                                                                                                        pagina = pagina, itens = itens, ordem = ordem, ordenarPor= ordenarPor)) %>% content(as = 'text') %>% fromJSON()

  return(lista)
}

