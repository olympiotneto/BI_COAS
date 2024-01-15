library(shiny)
library(tidyverse)
library(bs4Dash)

dados <- readRDS("data/dados.RDS")
cid_cat <- readRDS("data/Cid_cat.RDS")
cid_sub <- readRDS("data/Cid_sub.RDS")

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
        icon = icon("briefcase-medical")

      ),
      bs4SidebarMenuItem(
        text = "Prevalência",
        tabName = "prevalencia",
        icon = icon("chart-line")
      )
    )
  ),
  # BODY --------------------------------------------------------------------
  body = bs4DashBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "custom.css"
      )
    ),
    # fluidRow(
    #   #Pesquisa geral por data
    #   column(
    #     width = 6,
    #     dateRangeInput(
    #       inputId = "data_pesquisa",
    #       label = "Selecione uma data inicial e final",
    #       min =min(dados$data_inicio_licenca) ,
    #       max = max(dados$data_inicio_licenca) ,
    #       start = min(dados$data_inicio_licenca),
    #       end = max(dados$data_inicio_licenca) ,
    #       format = "dd/mm/yyyy",
    #       separator = "a",
    #       language = "pt-BR"
    #     )
    #   ),
    #   column(
    #     width = 6,
    #     #Escolhi o selectsizeinput porque são muitos elementos
    #     selectizeInput(
    #       inputId = "cid_cat",
    #       label = "Escolha uma ou mais categoria(s) de CID",
    #       choices =  NULL,
    #       multiple = TRUE,
    #       #selectize = TRUE,
    #       options = list(maxOptions = 5)
    #     )
    #   )
    # ),
    # fluidRow(
    #   column(
    #     width = 6,
    #     offset = 6,
    #     tableOutput("cid_cat_choosed")
    #   ),
    bs4TabItems(
      #  bs4TabItem(
      #   tabName = "visao_geral"
      # ),
      # UI VISAO GERAL SERVIDORES -----------------------------------------------
      ###Caixinhas de infos gerais no submenu servidores
      #Total de servidores
      #Sexo
      #Lotação
      #Situação funcional
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
          plotly::plotlyOutput("vg_serie_nr_serv")
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
          plotly::plotlyOutput("vg_serie_nr_lic")
        )
      ),
      # UI Casos -----------------------------------------------
      #Fazer selects com:
      #Grupo de CIDs
      #Periodo
      #Sexo: masculino/Feminino/Todos
      #Lotação: Sercretaria/Cartório/Todos
      bs4TabItem(
        tabName = "casos",
        titlePanel("Casos"),
        fluidRow(
          column(12,
                 selectizeInput(
                   inputId = "cid_cat",
                   label = "Escolha uma ou mais categoria(s) de CID",
                   choices =  NULL,
                   multiple = TRUE,
                   #selectize = TRUE,
                   #options = list(maxOptions = 10)
                 )
                 )
        )
      )
    ),

    fluidRow(
      column(2,
             offset = 10,
             glue::glue("Dados atualizados até {format(as.Date(atualizacao), '%d-%m-%Y')}")
      )
    )
  )
)






server <- function(input, output, session) {

  updateSelectizeInput(session, 'cid_cat', choices = cid_cat$cat, server = TRUE)

  # output$cid_cat_choosed<- renderTable({
  #   cid_cat |>
  #     filter(cat %in% input$cid_cat) |>
  #     rename(
  #       "Categoria" = cat,
  #            "Descrição" = descricao
  #       )
  # },
  # align = "cl"
  # )

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

  output$vg_serie_nr_serv <- plotly::renderPlotly({
    p <-  dados |>
      mutate(ano = lubridate::year(data_inicio_licenca)) |>
      summarise(Freq = n_distinct(codigo),.by = ano) |>
      filter(!is.na(ano)) |>
      ggplot(aes(x = ano, y = Freq)) +
      geom_bar(stat = "identity", fill = cores_Assec[2], position = "dodge")+
      ylab("Nº servidores") +
      scale_x_continuous(name="Ano",breaks = seq(2004,2023,1))+
      theme_classic() +
      theme(axis.text.x = element_text(angle = 60, vjust = .5))

    plotly::ggplotly(p)
  })

  output$vg_serie_nr_lic <- plotly::renderPlotly({
    p <- dados |>
      mutate(ano = lubridate::year(data_inicio_licenca)) |>
      summarise(Freq = n(),.by = ano) |>
      filter(!is.na(ano)) |>
      ggplot(aes(x = ano, y = Freq)) +
      geom_bar(stat = "identity", fill = cores_Assec[2], position = "dodge")+
      ylab("Nº servidores") +
      scale_x_continuous(name="Ano",breaks = seq(2004,2023,1))+
      theme_classic() +
      theme(axis.text.x = element_text(angle = 60, vjust = .5))

    plotly::ggplotly(p)
  })

}


shinyApp(ui, server)
