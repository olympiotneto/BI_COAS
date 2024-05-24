library(shiny)
library(tidyverse)
library(bs4Dash)

dados <- readRDS("data/dados.RDS")
cid_cat <- readRDS("data/Cid_cat.RDS")
cid_sub <- readRDS("data/Cid_sub.RDS")
opcoes_cat <- setNames(cid_cat$cat,paste(cid_cat$cat,cid_cat$descricao, sep="-"))

#data atualização dos dados  ( ultima data dos dados)
data_final <-
  dados |>
  slice_max(data_inicio_licenca) |>
  pull(data_inicio_licenca) |>
  unique()

#data atualização dos dados  ( primeira data dos dados)
data_inicial <-
  dados |>
  slice_min(data_inicio_licenca) |>
  pull(data_inicio_licenca) |>
  unique()


####Versão bs4Dash

ui <- bs4DashPage(
  dark = NULL,
  help = NULL,

  header = bs4DashNavbar(
    #Aqui coloco o título com quebra de página
    title = HTML("<center>Comportamento de <br> saúde-adoecimento<br>dos servidores</center>")
  ),
  sidebar = bs4DashSidebar(
    collapsed = FALSE,
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        text = "Visão Geral",
        tabName = "visao_geral",
        icon = icon("eye"),
        startExpanded = TRUE,

        bs4SidebarMenuSubItem(
          text = "Servidores",
          tabName = "vg_servidores"
        ),
        bs4SidebarMenuSubItem(
          text = "Licenças",
          tabName = "vg_licencas"
        )
      ),

      bs4SidebarMenuItem(
        text = "Casos",
        tabName = "casos",
        icon = icon("briefcase-medical"),
        startExpanded = TRUE,

        bs4SidebarMenuSubItem(
          text = "Distribuição",
          tabName = "ca_distr"
        ),
        bs4SidebarMenuSubItem(
          text = "Tempo de afastamento",
          tabName = "ca_afast"
        ),
        bs4SidebarMenuSubItem(
          text = "Estatísticas de Servidores",
          tabName = "ca_serv"
        )
      ),
      bs4SidebarMenuItem(
        text = "Prevalência",
        tabName = "prevalencia",
        icon = icon("chart-line")
      )
    )
  ),

  # Rodapé ------------------------------------------------------------------

  footer = bs4DashFooter(right =  glue::glue("Dados atualizados até {format(as.Date(data_final), '%d-%m-%Y')}"),
                         fixed = TRUE),
  # BODY --------------------------------------------------------------------
  body = bs4DashBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "custom.css"
      )
    ),

    bs4TabItems(
      # UI VISAO GERAL SERVIDORES -----------------------------------------------
      ###Caixinhas de infos gerais no submenu servidores
      #Total de servidores
      #Sexo
      #Lotação
      #Situação funcional (Não vai existir mais)
      #Gráfico no final com a evolução mensal
      bs4TabItem(
        tabName = "vg_servidores",
        titlePanel("Visão Geral - Servidores"),
        #glue::glue(" -  Atualizado até {format(as.Date(atualizacao), '%d-%m-%Y')}"
        hr(),
        fluidRow(
          bs4ValueBoxOutput(
            "vg_serv_total",
            width = 4
          ),
          bs4ValueBoxOutput(
            "vg_serv_sexo",
            width = 4
          ),
          bs4ValueBoxOutput(
            "vg_serv_lot",
            width = 4
          ),
          # bs4ValueBoxOutput(
          #   "vg_serv_sit",
          #   width = 3
          # )
        ),
        bs4Card(
          width = 12,
          title = "Servidores com licenças concedidas por ano",
          collapsible = FALSE,
          echarts4r::echarts4rOutput(
            outputId ="vg_serie_nr_serv"
          )
        )
      ),
      # UI VISAO GERAL LICENÇAS -----------------------------------------------
      ###Caixinhas de infos gerais no submenu licenças
      #Total de licenças
      #Sexo
      #Lotação
      #Situação funcional
      #Gráfico no final com a evolução mensal
      bs4TabItem(
        tabName = "vg_licencas",
        titlePanel("Visão Geral - Licenças"),
        hr(),
        fluidRow(
          bs4ValueBoxOutput(
            "vg_lic_total",
            width = 4
          ),
          bs4ValueBoxOutput(
            "vg_lic_sexo",
            width = 4
          ),
          bs4ValueBoxOutput(
            "vg_lic_lot",
            width = 4
          ),
          # bs4ValueBoxOutput(
          #   "vg_slic_sit",
          #   width = 3
          # )
        ),
        bs4Card(
          width = 12,
          title = "Número de licenças concedidas por ano",
          collapsible = FALSE,
          # plotly::plotlyOutput("vg_serie_nr_lic")
          echarts4r::echarts4rOutput(
            outputId ="vg_serie_nr_lic"
          )
        )
      ),
      # UI CASOS DISTR -----------------------------------------------
      #Fazer selects com:
      #Grupo de CIDs
      #Periodo
      #Sexo: masculino/Feminino/Todos
      #Lotação: Sercretaria/Cartório/Todos
      #situação funcional
      bs4TabItem(
        tabName = "ca_distr",
        titlePanel(HTML("Distribuição de casos <br> Licenças")),
        fluidRow(
          bs4Card(
            title = "Filtros",
            width = 12,
            fluidRow(
              column(
                5,
                dateRangeInput(
                  inputId = "casos_data",
                  label ="Selecione as datas",
                  start = "2022-01-01",
                  end = "2022-12-31",
                  min = as.Date(data_inicial),
                  max = as.Date(data_final),
                  separator = " - ",
                  format = "dd/mm/yyyy",
                  language = 'pt'
                )
              ),
              column(
                offset = 2,
                5,
                shinyWidgets::pickerInput(
                  inputId = "cid_cat",
                  label = "Escolha uma ou mais categoria(s) de CID",
                  choices = NULL,
                  # selected = "Z76",
                  options = shinyWidgets::pickerOptions(
                    actionsBox = TRUE,
                    deselectAllText = "desmarcar tudo",
                    selectAllText = "marcar tudo",
                    liveSearch = TRUE,
                    noneSelectedText = "Nenhum CID selecionado",
                    virtualScroll = 100,
                    width = 600,
                    size = 5,
                    header = "Escolha um CID",
                    hideDisabled = FALSE,
                    multipleSeparator = "|"
                  ),
                  multiple = TRUE
                )
              )
            )
          )
        ),
        fluidRow(
          bs4Card(
            title = "Sexo",
            width = 6,
            echarts4r::echarts4rOutput(
              outputId ="graf_casos_sexo"
            )
          ),
          bs4Card(
            title = "Lotação",
            width = 6,
            echarts4r::echarts4rOutput(
              outputId ="graf_casos_lot"
            )
          )
        ),
        fluidRow(
          bs4Card(
            title = "Situação Funcional",
            width = 12,
            echarts4r::echarts4rOutput(
              outputId ="graf_casos_sitf"
            )
          )
        ),
        fluidRow(
          bs4Card(
            title = "Distribuição por categoria de CID",
            width = 12,
            echarts4r::echarts4rOutput(
              outputId ="graf_casos_cid"
            )
          )
        ),
        fluidRow(
          bs4Card(
            width = 4,
            title = "Idade X Sexo",
            echarts4r::echarts4rOutput(
              outputId ="graf_casos_idade_sexo"
            )
          ),
          bs4Card(
            width = 4,
            title = "Idade X Lotação",
            echarts4r::echarts4rOutput(
              outputId ="graf_casos_idade_lot"
            )
          ),
          bs4Card(
            width = 4,
            title = "Idade X Situação Funcional",
            echarts4r::echarts4rOutput(
              outputId ="graf_casos_idade_sitf"
            )
          )
        ),
        downloadButton(
          outputId = "down_casos",
          label = "Download dados filtrados"
        )
      ),

      # UI CASOS TEMPO AFAST -----------------------------------------------

      bs4TabItem(
        tabName = "ca_afast",
        titlePanel(HTML("Tempos de afastamento <br>Licenças")),
        fluidRow(
          bs4Card(
            title = "Filtros",
            width = 12,
            fluidRow(
              column(
                5,
                dateRangeInput(
                  inputId = "casos_data_t",
                  label ="Selecione as datas",
                  start = "2022-01-01",
                  end = "2022-12-31",
                  min = as.Date(data_inicial),
                  max = as.Date(data_final),
                  separator = " - ",
                  format = "dd/mm/yyyy",
                  language = 'pt'
                )
              ),
              column(
                offset = 2,
                5,
                shinyWidgets::pickerInput(
                  inputId = "cid_cat_t",
                  label = "Escolha uma ou mais categoria(s) de CID",
                  choices = NULL,
                  # selected = "Z76",
                  options = shinyWidgets::pickerOptions(
                    actionsBox = TRUE,
                    deselectAllText = "desmarcar tudo",
                    selectAllText = "marcar tudo",
                    liveSearch = TRUE,
                    noneSelectedText = "Nenhum CID selecionado",
                    virtualScroll = 100,
                    width = 600,
                    size = 5,
                    header = "Escolha um CID",
                    hideDisabled = FALSE,
                    multipleSeparator = "|"
                  ),
                  multiple = TRUE
                )
              )
            )
          )
        ),
        fluidRow(
          bs4Card(
            title = "Sexo",
            width = 6,
            echarts4r::echarts4rOutput(
              outputId ="graf_ca_tempo_sexo"
            )
          ),
          bs4Card(
            title = "Lotação",
            width = 6,
            echarts4r::echarts4rOutput(
              outputId ="graf_ca_tempo_lot"
            )
          )
        )
      ),

# UI CASOS SERVIDORES  -----------------------------------------------
bs4TabItem(
  tabName = "ca_serv",
  titlePanel("Estatísticas de Servidores"),
  fluidRow(
    bs4Card(
      title = "Filtros",
      width = 12,
      fluidRow(
        column(
          5,
          dateRangeInput(
            inputId = "casos_data_serv",
            label ="Selecione as datas",
            start = "2022-01-01",
            end = "2022-12-31",
            min = as.Date(data_inicial),
            max = as.Date(data_final),
            separator = " - ",
            format = "dd/mm/yyyy",
            language = 'pt'
          )
        ),
        column(
          offset = 2,
          5,
          shinyWidgets::pickerInput(
            inputId = "cid_cat_serv",
            label = "Escolha uma ou mais categoria(s) de CID",
            choices = NULL,
            # selected = "Z76",
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              deselectAllText = "desmarcar tudo",
              selectAllText = "marcar tudo",
              liveSearch = TRUE,
              noneSelectedText = "Nenhum CID selecionado",
              virtualScroll = 100,
              width = 600,
              size = 5,
              header = "Escolha um CID",
              hideDisabled = FALSE,
              multipleSeparator = "|"
            ),
            multiple = TRUE
          )
        )
      )
    )
  ),
  fluidRow(
    bs4Card(
      title = "Sexo",
      width = 4,
      echarts4r::echarts4rOutput(
        outputId ="graf_ca_serv_sexo"
      )
    ),
    bs4Card(
      title = "Lotação",
      width = 4,
      echarts4r::echarts4rOutput(
        outputId ="graf_ca_serv_lot"
      )
    ),
    bs4Card(
      title = "Servidores por CID",
      width = 4,
      echarts4r::echarts4rOutput(
        outputId ="graf_ca_serv_cid"
      )
    )
  ),
  fluidRow(
    bs4Card(
      title = "Distribuição por idades",
      width = 12,
      echarts4r::echarts4rOutput(
        outputId ="graf_ca_serv_idade"
      )
    )
  )
),



      # UI Prevalência -----------------------------------------------
      #Fazer selects com:
      #Grupo de CIDs e depois selecionar os cids disponíveis
      #Evolução total
      #Evolução por Sexo
      bs4TabItem(
        tabName = "prevalencia",
        fluidRow(
          bs4Card(
            title = "Filtros",
            width = 12,
            fluidRow(
              column(
                6,
                shinyWidgets::pickerInput(
                  inputId = "cid_cat_prev",
                  label = "Selecione as categorias de CID",
                  choices = c("Carregando..." = ""),
                  multiple = TRUE,
                  options = shinyWidgets::pickerOptions(
                    actionsBox = TRUE,
                    liveSearch = TRUE,
                    width = 600,
                    size = 5,
                    deselectAllText = "desmarcar tudo",
                    selectAllText = "marcar tudo",
                    noneSelectedText = "Nenhum CID selecionado",
                    hideDisabled = FALSE
                  )
                ),
                shinyWidgets::pickerInput(
                  inputId = "cid_subcat_prev",
                  label = "Selecione um ou mais CIDs disponíveis",
                  multiple = TRUE,
                  choices = c("Carregando..." = ""),
                  selected = c(""),
                  inline = TRUE,
                  options = shinyWidgets::pickerOptions(
                    actionsBox = TRUE,
                    liveSearch = TRUE,
                    width = "600",
                    size = 5,
                    deselectAllText = "desmarcar tudo",
                    selectAllText = "marcar tudo",
                    multipleSeparator = "|",
                    noneSelectedText = "Nenhum CID selecionado",
                    hideDisabled = FALSE
                  )
                )
              ),
              column(
                6,
                shinyWidgets::sliderTextInput(
                  inputId = "prev_anos",
                  label = "Selecione um intervalo de anos:",
                  choices = dados |>
                    pull(data_inicio_licenca) |>
                    as.Date() |>
                    year() |>
                    unique() |>
                    sort(),
                  selected = c(
                    year(as.Date(data_inicial)),
                    year(as.Date(data_final)
                    )
                  ),
                  grid = TRUE
                )
              )
            )
          )
        ),
        fluidRow(
          bs4Card(
            title = "Evolução de licenças",
            width = 12,
            plotOutput(
              outputId = "graf_prev_evol"
            )
          )
        )
      )
    )
  )
)




# Servidor ----------------------------------------------------------------



server <- function(input, output, session) {

# Aba CASOS - Distribuição ----------------------------------------------------

# Filtro aba casos distribuição  ------------------------------------------------

#Essa parte filtra por data o bd
  dados_casos_filtrados <- reactive({
    dados |>
      # filter(cid_grupo %in% input$cid_cat,
      #        data_inicio_licenca>=input$casos_data[1],
      #        data_inicio_licenca<=input$casos_data[2]
      # )
      filter(
        data_inicio_licenca>=input$casos_data[1],
        data_inicio_licenca<=input$casos_data[2])
  })

  #essa parte relaciona só os cids que aparecem nas datas especificadas
  observeEvent(dados_casos_filtrados(),{
    freezeReactiveValue(input,"cid_cat")
    list_cid_cat <- dados_casos_filtrados() |>
      mutate(cid_grupo = as.character(cid_grupo)) |>
      filter(!is.na(cid_grupo)) |>
      pull(cid_grupo) |>
      unique() |>
      sort()

      descricao <- cid_cat |>
        filter(cat %in% list_cid_cat) |>
        mutate(nome = paste(cat, descricao, sep="-")) |>
        pull(nome)

      opcoes_cat <- setNames(list_cid_cat,descricao)


    shinyWidgets::updatePickerInput(
      inputId = "cid_cat",
      # choices = list_cid_cat,
      choices = opcoes_cat,
      # selected = list_cid_cat[1]
      selected = opcoes_cat[1]
    )

  })

  #Esse valor reativo já leva em conta o filtro de data e CID
  dados_c_filtrados_2 <- reactive({
    dados_casos_filtrados() |>
      filter(cid_grupo %in% input$cid_cat)
  })


  #### Download dos dados
  output$down_casos <- downloadHandler(
    filename = function() {
      paste0("dados_filtrados", ".csv")
    },
    content = function(file) {
      dados_down_filt <- dados_c_filtrados_2() |>
        left_join(cid_cat, by = c("cid_grupo"="cat")) |>
        group_by(cid_grupo,descricao,sexo,lotacao,situacao) |>
        summarise(freq = n()) |>
        rename("categoria_Cid" = cid_grupo)
      readr::write_excel_csv2(dados_down_filt, file)
    }

  )



  output$graf_casos_sexo <- echarts4r::renderEcharts4r({
    # browser()
    dados_c_filtrados_2() |>
      summarise(n = n(), .by = sexo) |>
      mutate(prop = round(n/sum(n),2),
             nome = paste(sexo,prop,sep=";")) |>
      echarts4r::e_chart(
        x = nome
      ) |>
      echarts4r::e_pie(
        serie = n,
        radius = c("50%", "70%"),
        label = list(
          show=FALSE
        )
      ) |>
      echarts4r::e_legend(
        formatter = htmlwidgets::JS(
          "function(nome){
          var vals = nome.split(';')
          return(vals[0])}"
        )
      ) |>
      echarts4r::e_tooltip(
        formatter = htmlwidgets::JS("function(params){
                                    var vals = params.name.split(';')
                                    return('<strong>' + vals[0] +
                                    '</strong><br />n: ' + params.value +
                                    '<br />porcentagem: ' +  (vals[1]*100).toFixed(2) + '%')}")
      ) |>
      echarts4r::e_color(
        c("pink","royalblue")
      )
  })

  output$graf_casos_lot <- echarts4r::renderEcharts4r({
    dados_c_filtrados_2() |>
      summarise(n = n(), .by = lotacao) |>
      mutate(prop = round(n/sum(n),2),
             nome = paste(lotacao,prop,sep=";")) |>
      echarts4r::e_chart(
        x = nome
      ) |>
      echarts4r::e_pie(serie = n,
                       radius = c("50%", "70%"),
                       legend = TRUE,
                       label = list(show=FALSE)) |>
      echarts4r::e_legend(
        formatter = htmlwidgets::JS(
          "function(nome){
          var vals = nome.split(';')
          return(vals[0])}"
        )
      ) |>
      echarts4r::e_tooltip(
        formatter = htmlwidgets::JS("function(params){
                                    var vals = params.name.split(';')
                                    return('<strong>' + vals[0] +
                                    '</strong><br />n: ' + params.value +
                                    '<br />porcentagem: ' +  (vals[1]*100).toFixed(2) + '%')}")) |>
      echarts4r::e_color(cores_Assec[c(2,3)])
  })

  output$graf_casos_sitf <- echarts4r::renderEcharts4r({
    dados_c_filtrados_2() |>
      summarise(n = n(), .by = situacao) |>
      mutate(prop = n/sum(n),
             # n_situacao = n_distinct(situacao),
             # color = RColorBrewer::brewer.pal(n_situacao, "GnBu")
      ) |>

      arrange(desc(n)) |>
      # ungroup() |>
      echarts4r::e_chart(
        x = situacao
      ) |>
      echarts4r::e_bar(
        serie = n,
        legend = FALSE) |>
      # echarts4r::e_add_nested("itemStyle", color) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_color("#253C59")
  })

  output$graf_casos_cid <- echarts4r::renderEcharts4r({
    dados_c_filtrados_2() |>
      summarise(n = n(), .by = cid_grupo) |>
      mutate(prop = n/sum(n)) |>
      arrange(desc(n)) |>
      echarts4r::e_chart(
        x = cid_grupo
      ) |>
      echarts4r::e_bar(
        serie = n,
        legend = FALSE) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_color("#253C59")

  })

  output$graf_casos_idade_sexo<- echarts4r::renderEcharts4r({

    dados_c_filtrados_2() |>
      group_by(sexo) |>
      summarise(media = round(mean(idade_inicio_licenca,na.rm = TRUE),2))|>
      # mutate(color = c("pink", "royalblue")) |>
      # ungroup() |
      echarts4r::e_chart(x = sexo
      ) |>
      echarts4r::e_bar(
        serie = media
      ) |>
      echarts4r::e_legend(show = FALSE) |>

      echarts4r::e_tooltip() |>
      echarts4r::e_color("#253C59")
    # echarts4r::e_add_nested("itemStyle", color)

  })

  output$graf_casos_idade_lot<- echarts4r::renderEcharts4r({

    dados_c_filtrados_2() |>
      group_by(lotacao) |>
      summarise(Media = mean(idade_inicio_licenca,na.rm = TRUE))|>
      # mutate(color = c("darkgreen", "royalblue")) |>
      # ungroup() |>
      echarts4r::e_chart(x = lotacao
      ) |>
      echarts4r::e_bar(
        serie = Media
      ) |>
      echarts4r::e_legend(show = FALSE) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_color("#253C59")
    # echarts4r::e_add_nested("itemStyle", color)


  })

  output$graf_casos_idade_lot<- echarts4r::renderEcharts4r({
    dados_c_filtrados_2() |>
      group_by(lotacao) |>
      summarise(media = round(mean(idade_inicio_licenca,na.rm = TRUE), 2))|>
      # mutate(color = cores_Assec[c(2,4)]) |>
      # ungroup() |>
      echarts4r::e_chart(x = lotacao
      ) |>
      echarts4r::e_bar(
        serie = media
      ) |>
      echarts4r::e_legend(show = FALSE) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_color("#253C59")
    # echarts4r::e_add_nested("itemStyle", color)

  })

  output$graf_casos_idade_sitf <- echarts4r::renderEcharts4r({
    dados_c_filtrados_2() |>
      group_by(situacao) |>
      summarise(media = round(mean(idade_inicio_licenca,na.rm = TRUE), 2)) |>
      ungroup() |>
      echarts4r::e_chart(
        x = situacao
      ) |>
      echarts4r::e_bar(
        serie = media,
        legend = FALSE) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_x_axis(axisLabel = list(rotate = 45)) |>
      echarts4r::e_color("#253C59")
  })


  # Aba CASOS - TEMPOS ----------------------------------------------------


  # Filtro aba casos tempos ------------------------------------------------

  #Essa parte filtra por data o bd
  dados_casos_t_filtrados <- reactive({
    dados |>
      # filter(cid_grupo %in% input$cid_cat,
      #        data_inicio_licenca>=input$casos_data[1],
      #        data_inicio_licenca<=input$casos_data[2]
      # )
      filter(
        data_inicio_licenca>=input$casos_data_t[1],
        data_inicio_licenca<=input$casos_data_t[2])
  })

  #essa parte relaciona só os cids que aparecem nas datas especificadas
  observeEvent(dados_casos_t_filtrados(),{
    freezeReactiveValue(input,"cid_cat_t")
    list_cid_cat_t <- dados_casos_t_filtrados() |>
      mutate(cid_grupo = as.character(cid_grupo)) |>
      filter(!is.na(cid_grupo)) |>
      pull(cid_grupo) |>
      unique() |>
      sort()

    descricao <- cid_cat |>
      filter(cat %in% list_cid_cat_t) |>
      mutate(nome = paste(cat, descricao, sep="-")) |>
      pull(nome)

    opcoes_cat_t <- setNames(list_cid_cat_t,descricao)


    shinyWidgets::updatePickerInput(
      inputId = "cid_cat_t",
      # choices = list_cid_cat,
      choices = opcoes_cat_t,
      # selected = list_cid_cat[1]
      selected = opcoes_cat_t[1]
    )

  })

  #Esse valor reativo já leva em conta o filtro de data e CID
  dados_c_filtrados_2_t <- reactive({
    dados_casos_t_filtrados() |>
      filter(cid_grupo %in% input$cid_cat_t)
  })



  output$graf_ca_tempo_sexo <- echarts4r::renderEcharts4r({
    dados_c_filtrados_2_t() |>
      group_by(sexo) |>
      summarise(media = round(mean(numero_de_dias,na.rm = TRUE), 2)) |>
      ungroup() |>
      echarts4r::e_chart(
        x = sexo
      ) |>
      echarts4r::e_bar(
        serie = media,
        legend = FALSE) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_x_axis(axisLabel = list(rotate = 0)) |>
      echarts4r::e_color(cores_Assec[1])
  })


  output$graf_ca_tempo_lot <- echarts4r::renderEcharts4r({
    dados_c_filtrados_2_t() |>
      group_by(lotacao) |>
      summarise(media = round(mean(numero_de_dias,na.rm = TRUE), 2)) |>
      ungroup() |>
      echarts4r::e_chart(
        x = lotacao
      ) |>
      echarts4r::e_bar(
        serie = media,
        legend = FALSE) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_x_axis(axisLabel = list(rotate = 0)) |>
      echarts4r::e_color(cores_Assec[4])
  })


  output$graf_ca_tempo_sitf <- echarts4r::renderEcharts4r({
    dados_c_filtrados_2_t() |>
      group_by(situacao) |>
      summarise(media = round(mean(numero_de_dias,na.rm = TRUE), 2)) |>
      ungroup() |>
      echarts4r::e_chart(
        x = situacao
      ) |>
      echarts4r::e_bar(
        serie = media,
        legend = FALSE) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_x_axis(axisLabel = list(rotate = 0)) |>
      echarts4r::e_color(cores_Assec[5])
  })



  # Aba CASOS - SERVIDORES ----------------------------------------------------

  # Filtro aba casos serv ------------------------------------------------

  #Essa parte filtra por data o bd
  dados_casos_serv_filtrados <- reactive({
    dados |>
      # filter(cid_grupo %in% input$cid_cat,
      #        data_inicio_licenca>=input$casos_data[1],
      #        data_inicio_licenca<=input$casos_data[2]
      # )
      filter(
        data_inicio_licenca>=input$casos_data_serv[1],
        data_inicio_licenca<=input$casos_data_serv[2])
  })

  #essa parte relaciona só os cids que aparecem nas datas especificadas
  observeEvent(dados_casos_serv_filtrados(),{
    freezeReactiveValue(input,"cid_cat_serv")
    list_cid_cat_serv <- dados_casos_serv_filtrados() |>
      mutate(cid_grupo = as.character(cid_grupo)) |>
      filter(!is.na(cid_grupo)) |>
      pull(cid_grupo) |>
      unique() |>
      sort()

    descricao <- cid_cat |>
      filter(cat %in% list_cid_cat_serv) |>
      mutate(nome = paste(cat, descricao, sep="-")) |>
      pull(nome)

    opcoes_cat_serv <- setNames(list_cid_cat_serv,descricao)


    shinyWidgets::updatePickerInput(
      inputId = "cid_cat_serv",
      # choices = list_cid_cat,
      choices = opcoes_cat_serv,
      # selected = list_cid_cat[1]
      selected = opcoes_cat_serv[1]
    )

  })

  #Esse valor reativo já leva em conta o filtro de data e CID
  dados_c_filtrados_2_serv <- reactive({
    dados_casos_serv_filtrados() |>
      filter(cid_grupo %in% input$cid_cat_serv)
  })

  #Gráficos e tabelas

  output$graf_ca_serv_sexo <- echarts4r::renderEcharts4r({
    # browser()
    dados_c_filtrados_2_serv() |>
      summarise(n = n_distinct(codigo), .by = sexo) |>
      mutate(prop = round(n/sum(n),2),
             nome = paste(sexo,prop,sep=";")) |>
      echarts4r::e_chart(
        x = nome
      ) |>
      echarts4r::e_pie(
        serie = n,
        radius = c("50%", "70%"),
        label = list(
          show=FALSE
        )
      ) |>
      echarts4r::e_legend(
        formatter = htmlwidgets::JS(
          "function(nome){
          var vals = nome.split(';')
          return(vals[0])}"
        )
      ) |>
      echarts4r::e_tooltip(
        formatter = htmlwidgets::JS("function(params){
                                    var vals = params.name.split(';')
                                    return('<strong>' + vals[0] +
                                    '</strong><br />n: ' + params.value +
                                    '<br />porcentagem: ' +  (vals[1]*100).toFixed(2) + '%')}")
      ) |>
      echarts4r::e_color(
        c("pink","royalblue")
      )
  })

  output$graf_ca_serv_lot <- echarts4r::renderEcharts4r({
    dados_c_filtrados_2_serv() |>
      summarise(n = n_distinct(codigo), .by = lotacao) |>
      mutate(prop = round(n/sum(n),2),
             nome = paste(lotacao,prop,sep=";")) |>
      echarts4r::e_chart(
        x = nome
      ) |>
      echarts4r::e_pie(serie = n,
                       radius = c("50%", "70%"),
                       legend = TRUE,
                       label = list(show=FALSE)) |>
      echarts4r::e_legend(
        formatter = htmlwidgets::JS(
          "function(nome){
          var vals = nome.split(';')
          return(vals[0])}"
        )
      ) |>
      echarts4r::e_tooltip(
        formatter = htmlwidgets::JS("function(params){
                                    var vals = params.name.split(';')
                                    return('<strong>' + vals[0] +
                                    '</strong><br />n: ' + params.value +
                                    '<br />porcentagem: ' +  (vals[1]*100).toFixed(2) + '%')}")) |>
      echarts4r::e_color(cores_Assec[c(2,3)])
  })

  output$graf_ca_serv_cid<- echarts4r::renderEcharts4r({
    dados_c_filtrados_2_serv() |>
      summarise(n = n_distinct(codigo), .by = cid_grupo) |>
      mutate(prop = n/sum(n)) |>
      arrange(desc(n)) |>
      echarts4r::e_chart(
        x = cid_grupo
      ) |>
      echarts4r::e_bar(
        serie = n,
        legend = FALSE) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_color(cores_Assec[2])

  })

  output$graf_ca_serv_idade<- echarts4r::renderEcharts4r({
    dados_c_filtrados_2_serv() |>
      group_by(codigo) |>
      summarise(
        idade = max(idade_inicio_licenca),
      ) |>
      echarts4r::e_charts() |>
      echarts4r::e_histogram(
        serie = idade,
        legend = FALSE) |>
      echarts4r::e_axis_labels(
        x = "idade",
        y = "Freq"
      ) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_color(cores_Assec[2])

  })

  # Aba Visão Geral - Servidor ----------------------------------------------------

  output$vg_serv_total <- renderbs4ValueBox({
    valor <- dados|>
      summarise(
        n_distinct(codigo)
      )|>
      pull() |>
      formatar_numero()

    bs4ValueBox(
      value = valor ,
      subtitle = "Total de servidores na base de dados" ,
      icon = icon("users"),
      color = "lightblue"
    )
  })

  output$vg_serv_sexo <- renderbs4ValueBox({
    valor <- dados|>
      summarise(
        n_distinct(codigo),
        .by = sexo
      )|>
      pull() |> formatar_numero()
    valor <- glue::glue(valor[1], " homens ", "<br> ", valor[2], " mulheres")


    bs4ValueBox(
      value = HTML(valor) ,
      subtitle = "Total servidores por sexo" ,
      icon = icon("venus-mars"),
      color = "lightblue"
    )
  })

  output$vg_serv_lot <- renderbs4ValueBox({
    valor <- dados|>
      summarise(
        n_distinct(codigo),
        .by = lotacao
      )|>
      pull() |>
      formatar_numero()
    valor <- glue::glue(valor[1], " em cartório ", "<br> ", valor[2], " na secretaria")


    bs4ValueBox(
      value = HTML(valor) ,
      subtitle = "Total servidores por lotação" ,
      icon = icon("building"),
      color = "lightblue"
    )
  })

  output$vg_lic_total <- renderbs4ValueBox({
    valor <- dados|>
      contar_linhas() |>
      formatar_numero()

    bs4ValueBox(
      value = valor ,
      subtitle = "Total de licenças na base de dados" ,
      icon = icon("users"),
      color = "lightblue"
    )
  })

  output$vg_lic_sexo <- renderbs4ValueBox({
    valor <- dados|>
      summarise(
        n = n(),
        .by = sexo
      )|>
      pull() |> formatar_numero()
    valor <- glue::glue(valor[1], " homens ", "<br> ", valor[2], " mulheres")


    bs4ValueBox(
      value = HTML(valor) ,
      subtitle = "Total de licenças por sexo" ,
      icon = icon("venus-mars"),
      color = "lightblue"
    )
  })

  output$vg_lic_lot <- renderbs4ValueBox({
    valor <- dados|>
      summarise(
        n = n(),
        .by = lotacao
      )|>
      pull() |> formatar_numero()
    valor <- glue::glue(valor[1], " em cartório ", "<br> ", valor[2], " na secretaria")

    bs4ValueBox(
      value = HTML(valor) ,
      subtitle = "Total de licenças por lotação" ,
      icon = icon("building") ,
      color = "lightblue"
    )
  })

  #Gráfico servidores/ano
  output$vg_serie_nr_serv <- echarts4r::renderEcharts4r({
    dados |>
      mutate(ano = lubridate::year(data_inicio_licenca)) |>
      summarise(Freq = n_distinct(codigo),.by = ano) |>
      filter(!is.na(ano)) |>
      mutate(ano = as.character(ano)) |>
      arrange(ano) |>
    #   ggplot(aes(x = ano, y = Freq)) +
    #   geom_bar(stat = "identity", fill = cores_Assec[2], position = "dodge")+
    #   ylab("Nº servidores") +
    #   scale_x_continuous(name="Ano",breaks = seq(2004,2023,1))+
    #   theme_classic() +
    #   theme(axis.text.x = element_text(angle = 60, vjust = .5))
    #
    # plotly::ggplotly(p)
      echarts4r::e_charts(
        x = ano
      ) |>
      echarts4r::e_bar(
        serie = Freq,
        legend = FALSE) |>
      echarts4r::e_axis_labels(
        x = "ano",
        y = "Freq"
      ) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_color(cores_Assec[2])
  })

  output$vg_serie_nr_lic <- echarts4r::renderEcharts4r({
    dados |>
      mutate(ano = lubridate::year(data_inicio_licenca)) |>
      summarise(Freq = n(),.by = ano) |>
      filter(!is.na(ano)) |>
      mutate(ano = as.character(ano)) |>
      arrange(ano) |>
    #   ggplot(aes(x = ano, y = Freq)) +
    #   geom_bar(stat = "identity", fill = cores_Assec[2], position = "dodge")+
    #   ylab("Nº servidores") +
    #   scale_x_continuous(name="Ano",breaks = seq(2004,2023,1))+
    #   theme_classic() +
    #   theme(axis.text.x = element_text(angle = 60, vjust = .5))
    # plotly::ggplotly(p)
      echarts4r::e_charts(
        x = ano
      ) |>
      echarts4r::e_bar(
        serie = Freq,
        legend = FALSE) |>
      echarts4r::e_axis_labels(
        x = "ano",
        y = "Freq"
      ) |>
      echarts4r::e_tooltip() |>
      echarts4r::e_color(cores_Assec[1])
  })


  # Aba Prevalência ---------------------------------------------------------
  #Faz o update dos inputbox aninhados
  shinyWidgets::updatePickerInput(
    session =  session,
    inputId = "cid_cat_prev",
    choices = opcoes_cat
  )

  observe({
    sub_cat_prev <- cid_sub |>
      filter(cat %in% input$cid_cat_prev)

    choices_subcat <- set_names(sub_cat_prev$subcat,
                                paste(sub_cat_prev$subcat,
                                      sub_cat_prev$descricao,
                                      sep = "-")
    )
    #
    #     sub_cat_prev <- sub_cat_prev |>
    #       pull(subcat) |>
    #       unique()


    shinyWidgets::updatePickerInput(
      session =  session,
      inputId = "cid_subcat_prev",
      choices = choices_subcat,
      selected = choices_subcat
    )

  })

  dados_prev_filtrados <- reactive({
    dados |>
      filter(cid %in% input$cid_subcat_prev,
             year(as.Date(data_inicio_licenca))>=input$prev_anos[1],
             year(as.Date(data_inicio_licenca))<=input$prev_anos[2]
      )
  })

  output$graf_prev_evol<- renderPlot({

    dados_prev_filtrados() |>
      group_by(mes = floor_date(data_inicio_licenca, "month")) |>
      #group_by(mes = floor_date(`Data início licença`, "month")) |>
      summarise(Freq = n(), .groups = "drop" )|>
      ggplot(aes(x= mes, y = Freq)) +
      geom_point(size=1)+
      geom_line()+
      scale_x_date(breaks ="1 year", date_labels = "%m-%Y") +
      # scale_color_manual("Lotação", values = cores_Assec[c(4)])+
      ylab("Frequência")+
      xlab("Mês-Ano")+
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 60, vjust = .5),
        legend.text = element_text(size=8),
        legend.position="top")

  })

}


shinyApp(ui, server, options = list(launch.browser=FALSE, port = 4242))
