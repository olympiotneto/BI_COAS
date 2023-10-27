library(shiny)
library(tidyverse)
library(bs4Dash)

 dados <- readRDS("data/dados.RDS")
 cid_cat <- readRDS("data/Cid_cat.RDS")
 cid_sub <- readRDS("data/Cid_sub.RDS")

####Versão bs4Dash

 ui <- bs4DashPage(
   header = bs4DashNavbar(
     #Aqui coloco o título com quebra de página
     title = HTML("<center>Comportamento de <br> saúde-adoecimento<br>dos servidores</center>")
   ),
   sidebar = bs4DashSidebar(collapsed = FALSE,
                            bs4SidebarMenu(
                              bs4SidebarMenuItem(
                                text = "Visão Geral",
                                tabName = "visao_geral",
                                icon = icon("eye")

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
   body = bs4DashBody(
     fluidRow(
       #Pesquisa geral por data
       column(
         width = 6,
         dateRangeInput(
           inputId = "data_pesquisa",
           label = "Selecione uma data inicial e final",
           min =min(dados$data_inicio_licenca) ,
           max = max(dados$data_inicio_licenca) ,
           start = min(dados$data_inicio_licenca),
           end = max(dados$data_inicio_licenca) ,
           format = "dd/mm/yyyy",
           separator = "a",
           language = "pt-BR"
         )
       ),
       column(
         width = 6,
         selectInput(
           inputId = "cid_cat",
           label = "Escolha uma ou mais categoria(s) de CID",
           choices = cid_cat$cat,
           multiple = TRUE,
           selectize = TRUE
         )
       )
     ),
     fluidRow(
       column(
         width = 6,
         offset = 6,
         textOutput("cid_cat_choose")
       )
     )

   )
 )


server <- function(input, output, session) {

  output$cid_cat_choose <- renderText({
    glue::glue("Os cids foram:{input$cid_cat}"," ",)

  })

}


shinyApp(ui, server)
