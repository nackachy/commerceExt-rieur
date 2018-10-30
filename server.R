library(shiny)
library(shinyjs)
library(plotly)
library(sqldf)
library(rvest)
library(curl)
library(stringr)
library(treemap)
library(ggplot2)
library(treemapify)
library(D3partitionR)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(rgdal)
library(ShinyRatingInput)
library(mailR)
library(sqldf)
library(rvest)
library(curl)
library(stringr)
library(png)
library(grid)
library(treemap)
library(ggplot2)
library(magick)

codePays = readRDS("www/codePays.RDS",.GlobalEnv)
paysNames = readRDS("www/paysNames.RDS",.GlobalEnv)
allProduits = readRDS("www/allProduits.rds",.GlobalEnv)
sectionNames = readRDS("www/sectionNames.RDS",.GlobalEnv)
chapterNames = readRDS("www/chapterNames.RDS",.GlobalEnv)
paysSelect = readRDS("www/paysSelect.rds",.GlobalEnv)
world_spdf=readOGR( dsn= "www" , layer="TM_WORLD_BORDERS_SIMPL-0.3")
commentdata = read.csv("www/commentdata.txt",sep = ";",encoding = "UTF-8")

isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), 
        ignore.case=TRUE)
}

shinyServer(function(input,output,session){
  
  USER <- reactiveValues(Logged = FALSE , session = session$user) 
  
  ####################################
  ################sign-in################
  ####################################

  observeEvent(input$signin,{
  shinyjs::disable("signin")
  shinyjs::show("jssignin")  
  comptes = read.csv("www/comptes.csv",fileEncoding = "UTF-8",sep=";")
  comptes = data.frame(comptes)
  
  if(input$nom == "" | input$prenom == "" | input$mail == "" | input$identifiant == "" | input$mdp1 == "" | input$mdp2 == ""){  
    if(input$nom == ""){
      updateTextInput(session,"nom",placeholder = "Veuillez remplir ce champs")
      }
    if(input$prenom == ""){
      updateTextInput(session,"prenom",placeholder = "Veuillez remplir ce champs")
    }
    if(input$mail == ""){
      updateTextInput(session,"mail",placeholder = "Veuillez remplir ce champs")
    }
    if(input$identifiant == ""){
      updateTextInput(session,"identifiant",placeholder = "Veuillez remplir ce champs")
    }
    if(input$mdp1 == ""){
      updateTextInput(session,"mdp1",placeholder = "Veuillez remplir ce champs")
    }
    if(input$mdp2 == ""){
      updateTextInput(session,"mdp2",placeholder = "Veuillez remplir ce champs")
    }
  }else if(input$mdp2 != input$mdp1){
    updateTextInput(session,"mdp2",value = "",placeholder = "Mot de passe non confirmé")
  }else if(isValidEmail(input$mail)==F){
    updateTextInput(session,"mail",value = "",placeholder = "Mail non confirmé")
  }else if(length(which(comptes$identifiant == input$identifiant)>0)){
    updateTextInput(session,"identifiant",value = "",placeholder = "Veuillez changer cet identifiant")
  }else if(length(str_split(input$mdp1,"")[[1]])<5){
    updateTextInput(session,"mdp1",value = "",placeholder = "Mot de passe trop courte")
    updateTextInput(session,"mdp2",value = "",placeholder = "")
    
  }else if(str_split(input$mdp1,"")[[1]][1] == "0"){
    updateTextInput(session,"mdp1",value = "",placeholder = "Ne commencez pas par 0")
    updateTextInput(session,"mdp2",value = "",placeholder = "")
    
  }else if(length(which(comptes$identifiant == input$identifiant)) > 0){
    updateTextInput(session,"identifiant",placeholder = "Changer identifiant")
    sendSweetAlert(
      session = session, title =NULL, text = paste("<h5><b><font color='black'> Identifiant déjà utilisé ! </font></b></h5>")%>%
        lapply(htmltools::HTML), type = "error",html = T
    )  
  }else if(length(which(comptes$identifiant == input$identifiant)) > 0){
    updateTextInput(session,"mail",placeholder = "Adresse mail déjà utilisée")
    sendSweetAlert(
      session = session, title =NULL, text = paste("<h5><b><font color='black'> Adresse mail déjà utilisée ! </font></b></h5>")%>%
        lapply(htmltools::HTML), type = "error",html = T
    )  
  }else{
       
    dataCompte = data.frame(
      nom = input$nom,
      prenom = input$prenom,
      jour = input$date1,
      mois = input$date2,
      annee = input$date3,
      sexe = input$sexe,
      mail = input$mail,
      identifiant = input$identifiant,
      passwd = input$mdp1
      )
    
    comptes = rbind(comptes,dataCompte)
   try( write.table(comptes, file = "www/comptes.csv", append = FALSE, quote = TRUE, sep = ";",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = TRUE, qmethod = c("escape", "double"),
                fileEncoding = "UTF-8")
    )
    
   try( send.mail(from = "nakkachi.chiheb1@gmail.com",
              to = c(input$mail),
              subject = "Bienvenue",
              body = paste("<html><body>Bonjour,<br></br><br></br><b>Nous vous remercions pour votre inscription et votre utilisation de l'application.</b><br></br><br></br>
                           Pour plus d'informations ou si vous avez des suggestions, n'hésitez pas à nous contacter ou visitez notre site web <a href='http://www.openway.com.tn/'>OpenWay</a>.
                           <br>Nous restons à votre disposition pour tous renseignements. </br><br></br></br><br></br>
                           Bien cordialement,<br></br>
                           <i>Equipe OpenWay</i><br></br>
                           </body></html>"),
            html = TRUE,
            inline = TRUE,
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "nakkachi.chiheb1@gmail.com", passwd = "maxtor007", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE))
  
 try( send.mail(from = "nakkachi.chiheb1@gmail.com",
            to = "nakkachi.chiheb1@gmail.com",
            subject = "Comptes",
            body = paste("compte",Sys.time()),
            html = TRUE,
            inline = TRUE,
            attach.files = "www/comptes.csv",
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "nakkachi.chiheb1@gmail.com", passwd = "maxtor007", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
    )
  
    output$pass <- renderUI({  
      if (USER$Logged == FALSE) {
        return()
      }else{
        tags$span(style = "color:white",tags$h5(input$identifiant))
      }  
    })
    
  sendSweetAlert(
    session = session, title =NULL, text = paste("<br><h5><b><font color='black'>Votre compte a été créé avec succès !</font></b></h5></br>")%>%
      lapply(htmltools::HTML), type = "success",html = T
  )  
  
    USER$Logged = TRUE
    
     } 
  
  
  shinyjs::enable("signin")
  shinyjs::hide("jssignin")
  })
  
  
  ####################################
  ################login################
  ####################################
  
  
  
  
  output$uiLogin <- renderUI({
    if (USER$Logged == FALSE) {
      fluidPage(
        
           textInput("userName", "Identifiant",value = "",width = "250", placeholder = "Identifiant"),
           passwordInput("passwd","Mot de passe",value = "",width = "250", placeholder = "Mot de passe"),
           column(12,actionButton("Login", label =list('Log-in',icon('sign-in')),styleclass = "primary"),
                  shinyjs::hidden(span(id="jslogin",tags$img(src = "mainloding3.gif", width = "50px", height = "42px"))),align="right")
           )
         
    }
  })
  
  
  
  # Login info during session ----
  output$userPanel <- renderUI({
    if (USER$Logged == TRUE) {
      fluidPage(
     column(12,actionButton("logout",label =list('Log-Out',icon('sign-out')),styleclass = "primary"),
            shinyjs::hidden(span(id="jslogout",tags$img(src = "mainloding3.gif", width = "50px", height = "42px"))),align = "right")    
     )
    }  
  })
  
  # control login
  observeEvent(input$Login , {
    PASSWORD <- read.csv("www/comptes.csv",fileEncoding = "UTF-8",sep=";")
    PASSWORD = data.frame(PASSWORD)
    shinyjs::disable("Login")
    shinyjs::show("jslogin")
    shinyjs::show("comExt")
    Username <- isolate(input$userName)
    Password <- isolate(input$passwd)
    Id.username <- which(PASSWORD$identifiant == Username)
    Id.password <- which(PASSWORD$passwd    == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        USER$Logged <- TRUE
        USER$name <- Username      
      } 
    } else {
      Sys.sleep(2)
      shinyjs::enable("Login")
      shinyjs::hide("jslogin")
      sendSweetAlert(
        session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez votre<font color='#006e87'> Identifiant</font> ou votre<font color='#006e87'> mot de passe</font></b></h5>")%>%
          lapply(htmltools::HTML), type = "error",html = T
      ) 
      return()      
      
      
    }
    Sys.sleep(2)
    output$pass <- renderUI({  
      if (USER$Logged == FALSE) {
        return()
      }else{
        tags$span(style = "color:white",tags$h5(USER$name))
      }  
    })
    shinyjs::enable("Login")
    shinyjs::hide("jslogin")
    shinyjs::hide("comExt")
  })
 
  # control logout
  observeEvent(input$logout , {
    shinyjs::disable("logout")
    shinyjs::show("jslogout")
    shinyjs::show("comExt")
    USER$Logged <- FALSE
    USER$pass <- ""
 
    output$BilanPlot = renderPlotly({return()})
      output$txtAnneeBalance = renderText({return()})
        output$boxImport = renderUI({return()})
          output$boxExport = renderUI({return()})
            output$boxBalance = renderUI({return()})
              output$BalancePlot = renderPlotly({return()})
                output$txtAnneePdt = renderText({return()})
                  output$ProduitPlotBar = renderUI({return()})
                    output$ProduitPlotcam = renderUI({return()})
                      output$txtAnneeTMap = renderText({return()})
                        output$uniteTmap = renderText({return()})
                          output$tmapPlot = renderD3partitionR({return()})
                            output$txtAnneeMap = renderText({return()})
                              output$mapPlot = renderLeaflet({return()})
                                output$uniteMap = renderText({return()})

    updateTextInput(session,"nom",value = "", placeholder = "")

    updateTextInput(session,"prenom",value = "", placeholder = "")
  
    updateTextInput(session,"mail",value = "", placeholder = "")
  
    updateTextInput(session,"identifiant",value = "", placeholder = "")
  
    updateTextInput(session,"mdp1",value = "", placeholder = "")
  
    updateTextInput(session,"mdp2",value = "", placeholder = "")
                                    
updateAwesomeCheckboxGroup(session,'parNSHmap',label = NULL,choices = c('Sections ( NSH2 )','Chapitres ( NSH4 )','Produits ( NSH6 )'))
updateSelectInput(session,'produitSpecNSH2map', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSpecNSH4map', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1])
updateSelectInput(session,'produitSpecNSH6map', 'Produits ( NSH6 )', choices = sectionNames,selected = sectionNames[1])

updateSelectInput(session,'produitSpecNSH22map', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSpecNSH44map', 'Chapitres ( NSH4 )', choices = sectionNames,selected =sectionNames[1])

updateSelectInput(session,'produitSpecNSH26map', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSpecNSH626map', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateSelectInput(session,'produitSpecNSH46map', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1])
updateSelectInput(session,'produitSpecNSH646map', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateSelectInput(session,'produitSpecNSH266map', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSpecNSH466map', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1])
updateSelectInput(session,'produitSpecNSH666map', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])

updateAwesomeCheckboxGroup(session,'parNSH',label = NULL,choices = c('Sections ( NSH2 )','Chapitres ( NSH4 )','Produits ( NSH6 )'))
updateSelectInput(session,'produitSpecNSH2Tm', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSpecNSH4Tm', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1])
updateSelectInput(session,'produitSpecNSH6Tm', 'Produits ( NSH6 )', choices = allProduits,selected = allProduits[1])
  
updateSelectInput(session,'produitSpecNSH22Tm', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSpecNSH44Tm', 'Chapitres ( NSH4 )', choices = sectionNames,selected =sectionNames[1])
  
updateSelectInput(session,'produitSpecNSH26Tm', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSpecNSH626Tm', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateSelectInput(session,'produitSpecNSH46Tm', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1])
updateSelectInput(session,'produitSpecNSH646Tm', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateSelectInput(session,'produitSpecNSH266Tm', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSpecNSH466Tm', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1])
updateSelectInput(session,'produitSpecNSH666Tm', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateAwesomeRadio(session,'chart_type_cal', 'Type Treemap', c('sunburst','circle_treemap','treemap','icicle','partition_chart'),selected = 'treemap')

updateAwesomeRadio(session,"pdtRadio","Produits",choices = c("Tous","Choisir Produits"))
updateAwesomeCheckboxGroup(session,'produitGlobal',label = NULL,choices = c('Sections ( NSH2 )','Chapitres ( NSH4 )','Produits ( NSH6 )'))
updateSelectInput(session,'produitSpecNSH2', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSpecNSH4', 'Chapitres ( NSH4 )', choices = sectionNames,selected =sectionNames[1])
updateSelectInput(session,'produitSpecNSH6', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateSelectInput(session,'produitSpecNSH22', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSpecNSH44', 'Chapitres ( NSH4 )', choices = sectionNames,selected =sectionNames[1])
updateSelectInput(session,'produitSpecNSH26', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSpecNSH626', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateSelectInput(session,'produitSpecNSH46', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1])
updateSelectInput(session,'produitSpecNSH646', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateSelectInput(session,'produitSpecNSH266', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSpecNSH466', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1])
updateSelectInput(session,'produitSpecNSH666', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])

updateAwesomeRadio(session,"choixPays","Pays",choices = c("Tous","Choisir Pays"))
updateSelectInput(session,"paysSelect",NULL,choices = paysSelect, selected = paysSelect[1])
updateAwesomeRadio(session,"choixProduit","Produits",choices = c("Tous","Choisir Produits"))
updateAwesomeCheckboxGroup(session,"produitSelect",NULL,choices = c('Sections ( NSH2 )','Chapitres ( NSH4 )','Produits ( NSH6 )'))
updateSelectInput(session,'produitSelectNSH2', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSelectNSH4', 'Chapitres ( NSH4 )', choices = sectionNames,selected =sectionNames[1])
updateSelectInput(session,'produitSelectNSH6', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateSelectInput(session,'produitSelectNSH22', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSelectNSH44', 'Chapitres ( NSH4 )', choices = sectionNames,selected =sectionNames[1])
updateSelectInput(session,'produitSelectNSH626', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateSelectInput(session,'produitSelectNSH46', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1])
updateSelectInput(session,'produitSelectNSH646', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateSelectInput(session,'produitSelectNSH266', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSelectNSH466', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1])
updateSelectInput(session,'produitSelectNSH666', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])

updateAwesomeRadio(session,"choixPaysBalance","Pays",choices = c("Tous","Choisir Pays"))
updateSelectInput(session,"paysSelectBalance",NULL,choices = paysSelect, selected = paysSelect[1])
updateAwesomeRadio(session,"choixProduitBalance","Produits",choices = c("Tous","Choisir Produits"))
updateAwesomeCheckboxGroup(session,"produitSelectBalance",NULL,choices = c('Sections ( NSH2 )','Chapitres ( NSH4 )','Produits ( NSH6 )'))
updateSelectInput(session,'produitSelectBalanceNSH2', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSelectBalanceNSH4', 'Chapitres ( NSH4 )', choices = sectionNames,selected =sectionNames[1])
updateSelectInput(session,'produitSelectBalanceNSH6', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateSelectInput(session,'produitSelectBalanceNSH22', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSelectBalanceNSH44', 'Chapitres ( NSH4 )', choices = sectionNames,selected =sectionNames[1])
updateSelectInput(session,'produitSelectBalanceNSH26', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSelectBalanceNSH626', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateSelectInput(session,'produitSelectBalanceNSH46', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1])
updateSelectInput(session,'produitSelectBalanceNSH646', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateSelectInput(session,'produitSelectBalanceNSH266', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])
updateSelectInput(session,'produitSelectBalanceNSH466', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1])
updateSelectInput(session,'produitSelectBalanceNSH666', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
updateAwesomeRadio(session,"mapsRadio","Produits",choices = c("Tous","Choisir Produits"))
updateAwesomeRadio(session,"tmapsRadio","Produits",choices = c("Tous","Choisir Produits"))

shinyjs::hide("jsbtnAnneeBalance")
shinyjs::hide("jsAnneePdt")
shinyjs::hide("jsAnneeTMap")
shinyjs::hide("jsAnneeMap")

    shinyjs::enable("logout")
    shinyjs::hide("comExt")
    shinyjs::hide("jslogout")
  })
  
  
  output$comExt=renderMenu({
    if(USER$Logged == F){
      shinyjs::show("comExt")
      sidebarMenu(menuItem(tags$b("Accueil"), tabName = "home", icon = icon("home")))
    }else if (USER$Logged == TRUE & (input$userName == "openway")) { 
      shinyjs::show("comExt")
      sidebarMenu( 
       
        menuItem(tags$b("Balance Commerciale"), tabName = "Balance", icon = icon("balance-scale")),
        menuItem(tags$b("Variation Import Export"), tabName = "bilan", icon = icon("line-chart")),
        menuItem(tags$b("Produits"), tabName = "pdt", icon = icon("pie-chart")),
        menuItem(tags$b("Bilan"), tabName = "gif", icon = icon("table")),
        menuItem(tags$b("TreeMaps"), tabName = "tmap", icon = icon("tree")),
        menuItem(tags$b("Maps"), tabName = "map", icon = icon("globe")),
        fluidPage( shinyjs::hidden( 
          span(id = "jsradio",hr()
               ,awesomeRadio(status = "danger","importExportGeneral","Type d'échange",choices = c("Import","Export"),selected = "Import",inline = T))),
          hr(),
          shinyjs::hidden( 
            span(id = "jsradio1",awesomeRadio(status = "danger","quantValGeneral","Nature du Produit",choices = c("Valeur","Quantite"),inline = T)))
          
          ,hr()),
        menuItem(tags$b("FeedBack"), tabName = "feedback", icon = icon("question-circle"),
                 menuSubItem(tags$b("Commentaires et opinions"),tabName = "comments"),
                 menuSubItem(tags$b("Nous contacter"),tabName = "contact"),
                 menuSubItem(tags$b("Comptes"),tabName = "comptes123")
                 )
      )
      
    }else if(USER$Logged == TRUE & (input$userName != "openway")){
      shinyjs::show("comExt")
      sidebarMenu( 
        
        menuItem(tags$b("Balance Commerciale"), tabName = "Balance", icon = icon("balance-scale")),
        menuItem(tags$b("Variation Import Export"), tabName = "bilan", icon = icon("line-chart")),
        menuItem(tags$b("Produits"), tabName = "pdt", icon = icon("pie-chart")),
        menuItem(tags$b("Bilan"), tabName = "gif", icon = icon("table")),
        menuItem(tags$b("TreeMaps"), tabName = "tmap", icon = icon("tree")),
        menuItem(tags$b("Maps"), tabName = "map", icon = icon("globe")),
        fluidPage( shinyjs::hidden( 
          span(id = "jsradio",hr()
               ,awesomeRadio(status = "danger","importExportGeneral","Type d'échange",choices = c("Import","Export"),selected = "Import",inline = T))),
          hr(),
          shinyjs::hidden( 
            span(id = "jsradio1",awesomeRadio(status = "danger","quantValGeneral","Nature du Produit",choices = c("Valeur","Quantite"),inline = T)),hr())
          
          ,hr()),
        menuItem(tags$b("FeedBack"), tabName = "feedback", icon = icon("question-circle"),
                 menuSubItem(tags$b("Commentaires et opinions"),tabName = "comments"),
                 menuSubItem(tags$b("Nous contacter"),tabName = "contact")
        )
      )
      
      
    }
  })
  
  
  
  
  
  #####################################
  
  
  
  
  
  observeEvent(list(input$dpOkBalance), {
    toggleDropdownButton(inputId = "dpBalance")
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$dpOkBilan), {
    toggleDropdownButton(inputId = "dpBalance")
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$dpOkPdt), {
    toggleDropdownButton(inputId = "dpBalance")
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$dpOkTmap), {
    toggleDropdownButton(inputId = "dpBalance")
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$dpOkMap), {
    toggleDropdownButton(inputId = "dpBalance")
  }, ignoreInit = TRUE)
  
  observeEvent(input$comExt2,{
    if(input$comExt2 == "comments"){
      shinyjs::hide('jsradio1')
      shinyjs::hide('jsradio')
    }else if(input$comExt2 == "contact"){
      shinyjs::hide('jsradio1')
      shinyjs::hide('jsradio')
    }else if(input$comExt2 == "comptes123"){
      shinyjs::hide('jsradio1')
      shinyjs::hide('jsradio')
    }else if(input$comExt2 == "Balance"){
      shinyjs::show('jsradio1')
      shinyjs::hide('jsradio')
      
    }else if(input$comExt2 == "gif"){
      shinyjs::show('jsradio1')
      shinyjs::hide('jsradio')
      
    }else{
      shinyjs::show('jsradio')
      shinyjs::show('jsradio1')
      
    }
  })
  
  observe({
    shinyjs::show("loading-content")
    shinyjs::show("loading-content1")
  })
  
  BaseGlobaleImport = reactive({
    
    j = paste0("www/data_globale_NSH6_",input$quantValGeneral,"_Import.csv")
    BaseGlobale = read.csv(j,sep=";",fileEncoding = "UTF-8")
    
    
    #View(BaseGlobale)
    
    for(i in 1:length(BaseGlobale)){
      BaseGlobale[,i] = as.character(BaseGlobale[,i])
    }
    
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"rythrée"))] = "Érythrée"
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"<U\\+062E>les mariannes du nord"))] = "les mariannes du nord"
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"<U\\+062E>les féroé"))] = "les féroé"
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"<U\\+062E>les turks et caïques"))] = "les turks et caïques"
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"<U\\+062E>les"))] = "les (malvinas) falkland"
    
    BaseGlobale
  })
  
  b_variableImport = reactive({
    b_variable = data.frame(BaseGlobaleImport())
    b_variable = data.frame(b_variable)
    #View(b_variable)
    for(i in 1999:2017){
      b_variable[,paste0("X",i)] = str_replace_all(b_variable[,paste0("X",i)],"-","0")
      b_variable[,paste0("X",i)] = as.numeric(b_variable[,paste0("X",i)])
    }
    
    b_variable$longitude = as.numeric(b_variable$longitude)
    b_variable$latitude = as.numeric(b_variable$latitude)
    b_variable$code2[which(is.na(b_variable$code2))]  = "NA"
    
    for(i in 1:9){
      k = which(b_variable$NSH2 == i)
      b_variable$NSH2[k]  = paste0("0",i)
      b_variable$NSH4[k] = paste0("0",b_variable$NSH4[k])
      b_variable$NSH6[k] = paste0("0",b_variable$NSH6[k])
    }
    
    b_variable
  })
  
  BaseGlobaleExport = reactive({
    
    
    j = paste0("www/data_globale_NSH6_",input$quantValGeneral,"_Export.csv")
    BaseGlobale = read.csv(j,sep=";",fileEncoding = "UTF-8")
    
    
    #View(BaseGlobale)
    
    for(i in 1:length(BaseGlobale)){
      BaseGlobale[,i] = as.character(BaseGlobale[,i])
    }
    
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"rythrée"))] = "Érythrée"
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"<U\\+062E>les mariannes du nord"))] = "les mariannes du nord"
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"<U\\+062E>les féroé"))] = "les féroé"
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"<U\\+062E>les turks et caïques"))] = "les turks et caïques"
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"<U\\+062E>les"))] = "les (malvinas) falkland"
    
    BaseGlobale
  })
  
  b_variableExport = reactive({
    b_variable = data.frame(BaseGlobaleExport())
    b_variable = data.frame(b_variable)
    #View(b_variable)
    for(i in 1999:2017){
      b_variable[,paste0("X",i)] = str_replace_all(b_variable[,paste0("X",i)],"-","0")
      b_variable[,paste0("X",i)] = as.numeric(b_variable[,paste0("X",i)])
    }
    
    b_variable$longitude = as.numeric(b_variable$longitude)
    b_variable$latitude = as.numeric(b_variable$latitude)
    b_variable$code2[which(is.na(b_variable$code2))]  = "NA"
    
    for(i in 1:9){
      k = which(b_variable$NSH2 == i)
      b_variable$NSH2[k]  = paste0("0",i)
      b_variable$NSH4[k] = paste0("0",b_variable$NSH4[k])
      b_variable$NSH6[k] = paste0("0",b_variable$NSH6[k])
    }
    
    b_variable
  })
  
  
  
  BaseGlobale = reactive({
    
    
    j = paste0("www/data_globale_NSH6_",input$quantValGeneral,"_",input$importExportGeneral,".csv")
    BaseGlobale = read.csv(j,sep=";",fileEncoding = "UTF-8")
    
    
    #View(BaseGlobale)
    
    for(i in 1:length(BaseGlobale)){
      BaseGlobale[,i] = as.character(BaseGlobale[,i])
    }
    
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"rythrée"))] = "Érythrée"
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"<U\\+062E>les mariannes du nord"))] = "les mariannes du nord"
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"<U\\+062E>les féroé"))] = "les féroé"
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"<U\\+062E>les turks et caïques"))] = "les turks et caïques"
    BaseGlobale$Nom_Pays[which(str_detect(BaseGlobale$Nom_Pays,"<U\\+062E>les"))] = "les (malvinas) falkland"
    
    BaseGlobale
  })
  
  b_variable = reactive({
    b_variable = data.frame(BaseGlobale())
    b_variable = data.frame(b_variable)
    #View(b_variable)
    for(i in 1999:2017){
      b_variable[,paste0("X",i)] = str_replace_all(b_variable[,paste0("X",i)],"-","0")
      b_variable[,paste0("X",i)] = as.numeric(b_variable[,paste0("X",i)])
    }
    
    b_variable$longitude = as.numeric(b_variable$longitude)
    b_variable$latitude = as.numeric(b_variable$latitude)
    b_variable$code2[which(is.na(b_variable$code2))]  = "NA"
    
    for(i in 1:9){
      k = which(b_variable$NSH2 == i)
      b_variable$NSH2[k]  = paste0("0",i)
      b_variable$NSH4[k] = paste0("0",b_variable$NSH4[k])
      b_variable$NSH6[k] = paste0("0",b_variable$NSH6[k])
    }

    b_variable
  })
  
  observe({
    shinyjs::hide("loading-content")
    shinyjs::hide("loading-content1")
  })
  
  ############################################################
  ########################Bilan update selectinput########################
  ############################################################ 
  
  observe({
    
    if(length(input$produitSelect)==2){
      
      if(input$produitSelect[1] == "Sections ( NSH2 )" & input$produitSelect[2] == "Chapitres ( NSH4 )" & is.na(input$produitSelect[3])){
        
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSelectNSH22)){
          cc = str_split(input$produitSelectNSH22[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:98){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(sectionNames,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,sectionNames[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSelectNSH44",choices = section_choisie,selected = section_choisie[1])
        }
      }else if(input$produitSelect[1] == "Sections ( NSH2 )" & input$produitSelect[2] == "Produits ( NSH6 )" & is.na(input$produitSelect[3])){
        
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSelectNSH26)){
          cc = str_split(input$produitSelectNSH26[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 100:999){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 1000:9999){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSelectNSH626",choices = section_choisie,selected = section_choisie[1])
        }
      }else if(input$produitSelect[1] == "Chapitres ( NSH4 )" & input$produitSelect[2] == "Produits ( NSH6 )" & is.na(input$produitSelect[3])){
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSelectNSH46)){
          cc = str_split(input$produitSelectNSH46[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2],cc[[1]][3],cc[[1]][4]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:99){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSelectNSH646",choices = section_choisie,selected = section_choisie[1])
        }
      }
      
      
    }
    
    
  })
  
  ############################################################
  ########################Balance update selectinput##########
  ############################################################ 
  
  
  
  observe({
    
    if(length(input$produitSelectBalance)==2){
      
      if(input$produitSelectBalance[1] == "Sections ( NSH2 )" & input$produitSelectBalance[2] == "Chapitres ( NSH4 )" & is.na(input$produitSelectBalance[3])){
        
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSelectBalanceNSH22)){
          cc = str_split(input$produitSelectBalanceNSH22[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:98){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(sectionNames,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,sectionNames[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSelectBalanceNSH44",choices = section_choisie,selected = section_choisie[1])
        }
      }else if(input$produitSelectBalance[1] == "Sections ( NSH2 )" & input$produitSelectBalance[2] == "Produits ( NSH6 )" & is.na(input$produitSelectBalance[3])){
        
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSelectBalanceNSH26)){
          cc = str_split(input$produitSelectBalanceNSH26[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 100:999){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 1000:9999){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSelectBalanceNSH626",choices = section_choisie,selected = section_choisie[1])
        }
      }else if(input$produitSelectBalance[1] == "Chapitres ( NSH4 )" & input$produitSelectBalance[2] == "Produits ( NSH6 )" & is.na(input$produitSelectBalance[3])){
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSelectBalanceNSH46)){
          cc = str_split(input$produitSelectBalanceNSH46[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2],cc[[1]][3],cc[[1]][4]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:99){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSelectBalanceNSH646",choices = section_choisie,selected = section_choisie[1])
        }
      }
      
      
    }
    
    
  })

  
  ############################################################
  ########################Produit update selectinput########################
  ############################################################ 
  
  observe({
    
    if(length(input$produitGlobal)==2){
      
      if(input$produitGlobal[1] == "Sections ( NSH2 )" & input$produitGlobal[2] == "Chapitres ( NSH4 )" & is.na(input$produitGlobal[3])){
        
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSpecNSH22)){
          cc = str_split(input$produitSpecNSH22[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:98){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(sectionNames,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,sectionNames[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSpecNSH44",choices = section_choisie,selected = section_choisie[1])
        }
      }else if(input$produitGlobal[1] == "Sections ( NSH2 )" & input$produitGlobal[2] == "Produits ( NSH6 )" & is.na(input$produitGlobal[3])){
        
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSpecNSH26)){
          cc = str_split(input$produitSpecNSH26[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 100:999){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 1000:9999){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSpecNSH626",choices = section_choisie,selected = section_choisie[1])
        }
      }else if(input$produitGlobal[1] == "Chapitres ( NSH4 )" & input$produitGlobal[2] == "Produits ( NSH6 )" & is.na(input$produitGlobal[3])){
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSpecNSH46)){
          cc = str_split(input$produitSpecNSH46[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2],cc[[1]][3],cc[[1]][4]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:99){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSpecNSH646",choices = section_choisie,selected = section_choisie[1])
        }
      }
      
      
    }
    
    
  })
  
  observe({
    if(length(input$produitGlobal)==3){
      
      indice_NSH2 = "" 
      for(i in 1:length(input$produitSpecNSH266)){
        cc = str_split(input$produitSpecNSH266[i],"")
        cc = try(paste0(cc[[1]][1],cc[[1]][2]))
        indice_NSH2 = c(indice_NSH2,cc)
      }
      indice_NSH2 = indice_NSH2[-1]
      
      indice_NSH2_final = ""
      for(i in 1:9){
        for(j in 1:length(indice_NSH2)){
          cc = paste0(indice_NSH2[j],"0",i)
          indice_NSH2_final = c(indice_NSH2_final,cc)
        }
        
      }
      
      for(i in 10:99){
        
        for(j in 1:length(indice_NSH2)){
          cc = paste0(indice_NSH2[j],i)
          indice_NSH2_final = c(indice_NSH2_final,cc)
        }
        
      }
      
      section_choisie = ""
      for(i in 2:length(indice_NSH2_final)){
        k = which(str_detect(sectionNames,paste0(indice_NSH2_final[i],"-")))
        if(length(k)!=0){
          section_choisie = c(section_choisie,sectionNames[k])
        }
      }
      section_choisie = section_choisie[-1]
      if(!is.na(length(section_choisie))){
        updateSelectInput(session,"produitSpecNSH466",choices = section_choisie,selected = section_choisie[1])
      }
      
    }  
    
  })
  
  observe({
    if(length(input$produitGlobal) == 3){
      if(input$produitGlobal[1] == "Sections ( NSH2 )" & input$produitGlobal[2] == "Chapitres ( NSH4 )" & input$produitGlobal[3] == "Produits ( NSH6 )"){
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSpecNSH466)){
          cc = str_split(input$produitSpecNSH466[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2],cc[[1]][3],cc[[1]][4]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:99){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSpecNSH666",choices = section_choisie,selected = section_choisie[1])
        }
      }}
  })
  
  
  ############################################################
  ########################Treemap update selectinput########################
  ############################################################ 
  observe({
    
    if(length(input$parNSH)==2){
      
      if(input$parNSH[1] == "Sections ( NSH2 )" & input$parNSH[2] == "Chapitres ( NSH4 )" & is.na(input$parNSH[3])){
        
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSpecNSH22Tm)){
          cc = str_split(input$produitSpecNSH22Tm[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:98){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(sectionNames,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,sectionNames[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSpecNSH44Tm",choices = section_choisie,selected = section_choisie[1])
        }
      }else if(input$parNSH[1] == "Sections ( NSH2 )" & input$parNSH[2] == "Produits ( NSH6 )" & is.na(input$parNSH[3])){
        
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSpecNSH26Tm)){
          cc = str_split(input$produitSpecNSH26Tm[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 100:999){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 1000:9999){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSpecNSH626Tm",choices = section_choisie,selected = section_choisie[1])
        }
      }else if(input$parNSH[1] == "Chapitres ( NSH4 )" & input$parNSH[2] == "Produits ( NSH6 )" & is.na(input$parNSH[3])){
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSpecNSH46Tm)){
          cc = str_split(input$produitSpecNSH46Tm[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2],cc[[1]][3],cc[[1]][4]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:99){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSpecNSH646Tm",choices = section_choisie,selected = section_choisie[1])
        }
      }
      
      
    }
    
    
  })
  
  observe({
    if(length(input$parNSH)==3){
      
      indice_NSH2 = "" 
      for(i in 1:length(input$produitSpecNSH266Tm)){
        cc = str_split(input$produitSpecNSH266Tm[i],"")
        cc = try(paste0(cc[[1]][1],cc[[1]][2]))
        indice_NSH2 = c(indice_NSH2,cc)
      }
      indice_NSH2 = indice_NSH2[-1]
      
      indice_NSH2_final = ""
      for(i in 1:9){
        for(j in 1:length(indice_NSH2)){
          cc = paste0(indice_NSH2[j],"0",i)
          indice_NSH2_final = c(indice_NSH2_final,cc)
        }
        
      }
      
      for(i in 10:99){
        
        for(j in 1:length(indice_NSH2)){
          cc = paste0(indice_NSH2[j],i)
          indice_NSH2_final = c(indice_NSH2_final,cc)
        }
        
      }
      
      section_choisie = ""
      for(i in 2:length(indice_NSH2_final)){
        k = which(str_detect(sectionNames,paste0(indice_NSH2_final[i],"-")))
        if(length(k)!=0){
          section_choisie = c(section_choisie,sectionNames[k])
        }
      }
      section_choisie = section_choisie[-1]
      if(!is.na(length(section_choisie))){
        updateSelectInput(session,"produitSpecNSH466Tm",choices = section_choisie,selected = section_choisie[1])
      }
      
    }  
    
  })
  
  observe({
    if(length(input$parNSH) == 3){
      if(input$parNSH[1] == "Sections ( NSH2 )" & input$parNSH[2] == "Chapitres ( NSH4 )" & input$parNSH[3] == "Produits ( NSH6 )"){
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSpecNSH466Tm)){
          cc = str_split(input$produitSpecNSH466Tm[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2],cc[[1]][3],cc[[1]][4]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:99){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSpecNSH666Tm",choices = section_choisie,selected = section_choisie[1])
        }
      }}
  })
  
  
  ############################################################
  ########################Maps update selectinput########################
  ############################################################ 
  observe({
    
    if(length(input$parNSHmap)==2){
      
      if(input$parNSHmap[1] == "Sections ( NSH2 )" & input$parNSHmap[2] == "Chapitres ( NSH4 )" & is.na(input$parNSHmap[3])){
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSpecNSH22map)){
          cc = str_split(input$produitSpecNSH22map[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:98){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(sectionNames,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,sectionNames[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSpecNSH44map",choices = section_choisie,selected = section_choisie[1])
        }
      }else if(input$parNSHmap[1] == "Sections ( NSH2 )" & input$parNSHmap[2] == "Produits ( NSH6 )" & is.na(input$parNSHmap[3])){
        
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSpecNSH26map)){
          cc = str_split(input$produitSpecNSH26map[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 100:999){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 1000:9999){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSpecNSH626map",choices = section_choisie,selected = section_choisie[1])
        }
      }else if(input$parNSHmap[1] == "Chapitres ( NSH4 )" & input$parNSHmap[2] == "Produits ( NSH6 )" & is.na(input$parNSHmap[3])){
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSpecNSH46map)){
          cc = str_split(input$produitSpecNSH46map[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2],cc[[1]][3],cc[[1]][4]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:99){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSpecNSH646map",choices = section_choisie,selected = section_choisie[1])
        }
      }
      
      
    }
    
    
  })
  
  observe({
    if(length(input$parNSHmap)==3){
      
      indice_NSH2 = "" 
      for(i in 1:length(input$produitSpecNSH266map)){
        cc = str_split(input$produitSpecNSH266map[i],"")
        cc = try(paste0(cc[[1]][1],cc[[1]][2]))
        indice_NSH2 = c(indice_NSH2,cc)
      }
      indice_NSH2 = indice_NSH2[-1]
      
      indice_NSH2_final = ""
      for(i in 1:9){
        for(j in 1:length(indice_NSH2)){
          cc = paste0(indice_NSH2[j],"0",i)
          indice_NSH2_final = c(indice_NSH2_final,cc)
        }
        
      }
      
      for(i in 10:99){
        
        for(j in 1:length(indice_NSH2)){
          cc = paste0(indice_NSH2[j],i)
          indice_NSH2_final = c(indice_NSH2_final,cc)
        }
        
      }
      
      section_choisie = ""
      for(i in 2:length(indice_NSH2_final)){
        k = which(str_detect(sectionNames,paste0(indice_NSH2_final[i],"-")))
        if(length(k)!=0){
          section_choisie = c(section_choisie,sectionNames[k])
        }
      }
      section_choisie = section_choisie[-1]
      if(!is.na(length(section_choisie))){
        updateSelectInput(session,"produitSpecNSH466map",choices = section_choisie,selected = section_choisie[1])
      }
      
    }  
    
  })
  
  observe({
    if(length(input$parNSHmap) == 3){
      if(input$parNSHmap[1] == "Sections ( NSH2 )" & input$parNSHmap[2] == "Chapitres ( NSH4 )" & input$parNSHmap[3] == "Produits ( NSH6 )"){
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSpecNSH466map)){
          cc = str_split(input$produitSpecNSH466map[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2],cc[[1]][3],cc[[1]][4]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:99){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSpecNSH666map",choices = section_choisie,selected = section_choisie[1])
        }
      }}
  })
  
  
  ############################################################
  ########################End update selectInput########################
  ############################################################
  
  ############################################################
  ########################Bilan graphes########################
  ############################################################
  
  observeEvent(input$btnBilan,{ 
    
    shinyjs::show("jsbtnbilan")
    shinyjs::disable("btnBilan")
    b_variable = data.frame(b_variable())
    if(input$choixPays == "Tous" & input$choixProduit == "Tous"){
      
      statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                         sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                         sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                         sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                         XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                         from b_variable")
      d = sqldf(statement)
      dataBilan = data.frame(d)
      for(i in 1:length(dataBilan)){
        colnames(dataBilan)[i] = paste0(1998+i)
      }
      dataBilan = data.frame(t(dataBilan))
      colnames(dataBilan) = paste(input$quantValGeneral,input$importExportGeneral)
      ax <- list(
        title = "",
        tickangle = 45
      )
      ay = list(title = colnames(dataBilan))
      
      dataBilan[,1] = round(dataBilan[,1]/1000000,digits = 3)
      if(input$quantValGeneral == "Quantite"){
        txtTitle = paste("en milles Tonne")
      }else{
        txtTitle = paste("en Milliards de Dinars")
      }
      
      
      p <- plot_ly(dataBilan, x = ~rownames(dataBilan), y = ~dataBilan[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                   text = ~paste("Année :",rownames(dataBilan),
                                 "<br>",input$quantValGeneral,input$importExportGeneral," : ", dataBilan[,1],"<br>"))%>%
        layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie ",txtTitle," avec tous les pays"),
               showlegend = FALSE)
      
      
    }else if(input$choixPays == "Choisir Pays" & input$choixProduit == "Tous"){
      
      dataBilan = matrix(0,19,1)
      dataBilan = data.frame(dataBilan)
      for(i in 1:length(input$paysSelect)){
        statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                           sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                           sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                           sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                           XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                           from b_variable where Nom_Pays ='",input$paysSelect[i],"'")
        d = sqldf(statement)
        
        for(i in 1:length(d)){
          colnames(d)[i] = paste0(1998+i)
        }
        d = data.frame(t(d))
        dataBilan = data.frame(dataBilan,d)
        
      }
      dataBilan = dataBilan[-1]

      if(length(input$paysSelect[1])!=0){
      for(i in 1: length(dataBilan)){
        colnames(dataBilan)[i] = try(paste0(input$paysSelect[i]))
      }
      dataBilan[,1] = round(dataBilan[,1]/1000000,digits = 3)
      if(input$quantValGeneral == "Quantite"){
        txtTitle = paste("en milles Tonne")
      }else{
        txtTitle = paste("en Milliards de Dinars")
      }
      pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
      ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
      for(i in 1:length(dataBilan)){
        dd = dataBilan[,i]
        pString <- paste0(pString," %>%add_trace(ploti,name = ",  eval(paste0("input$paysSelect[",i,"]")),", x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                          text = ~paste('Année :',rownames(dataBilan),
                          '<br>Pays : '",  eval(paste0(",colnames(dataBilan)[",i,"]")),",
                          '<br>',input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'<br>'))")
      }
      text = ""
      for(i in 1:length(input$paysSelect)){
        text = paste(text,input$paysSelect[i],sep = ",")
      }
      }
      try(eval(parse(text=pString)))
      p = try(print(ploti))
      
      ax <- list(
        title = "",
        tickangle = 45
      )
      
      
      
      ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
      p = try(p%>%
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie ",txtTitle,"<br> avec ",text,"</br>"),showlegend = T)
      )
      
      
      }else if(input$choixPays == "Tous" & input$choixProduit == "Choisir Produits"){
        
        if(!is.null(input$produitSelect)){
          
          if(input$produitSelect[1] == "Sections ( NSH2 )" & is.na(input$produitSelect[2])){
            
            dataBilan = matrix(0,0,19)
            dataBilan = data.frame(dataBilan)
            coloneNSH = ""
            for(i in 1:length(input$produitSelectNSH2)){
              NSH_graph = str_split(input$produitSelectNSH2[i],"")
              
              NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2]))
              
              statement = paste0("select * from b_variable where NSH2 ='",NSH_graph,"'")
              
              d = try(sqldf(statement))
              if(class(d)!="try-error"){
                datawork = d
                statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                   sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                   sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                   sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                   XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                   from datawork")
                d = sqldf(statement)
                
                colnames(dataBilan) = colnames(d)
                
                dataBilan = rbind(dataBilan,d)
                coloneNSH = c(coloneNSH,NSH_graph)
                
                
              }
            }
            
            dataBilan = data.frame(t(dataBilan))
            
            for(i in 1:(length(coloneNSH)-1)){
              colnames(dataBilan)[i] =  coloneNSH[i+1]
            }
            
            for(i in 1:nrow(dataBilan)){
              rownames(dataBilan)[i] =  paste(1998+i)
            }
            
            for(i in 1:length(dataBilan)){
            dataBilan[,i] = round(dataBilan[,i]/1000000,digits = 3)
            }
            if(input$quantValGeneral == "Quantite"){
              txtTitle = paste("en milles Tonne")
            }else{
              txtTitle = paste("en Milliards de Dinars")
            }
            
            pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
            ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
            for(i in 1:length(dataBilan)){
              pString <- paste0(pString," %>%add_trace(ploti,name = paste('NSH2 : ',",  eval(paste0("colnames(dataBilan)[",i,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                text = ~paste('Année :',rownames(dataBilan),
                                '<br>',input$produitSelect,' : '",  eval(paste0(",colnames(dataBilan)[",i,"]")),",'<br>'
                                ,input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
            }
            try(eval(parse(text=pString)))
            p = try(print(ploti))
            
            ax <- list(
              title = "",
              tickangle = 45
            )
            
            text = ""
            for(i in 1:(length(coloneNSH)-1)){
              text = paste(text,coloneNSH[i+1])
            }
            
            ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
            p = try(p%>%
                      layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec tous les pays ",txtTitle,"<br>et pour les produits",text,"</br>"),showlegend = T)
            )
            
            
            }else if(input$produitSelect[1] == "Chapitres ( NSH4 )" & is.na(input$produitSelect[2])){
              
              dataBilan = matrix(0,0,19)
              dataBilan = data.frame(dataBilan)
              coloneNSH = ""
              for(i in 1:length(input$produitSelectNSH4)){
                NSH_graph = str_split(input$produitSelectNSH4[i],"")
                
                NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
                
                statement = paste0("select * from b_variable where NSH4 ='",NSH_graph,"'")
                
                d = try(sqldf(statement))
                if(class(d)!="try-error"){
                  datawork = d
                  statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                     sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                     sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                     sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                     XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                     from datawork")
                  d = sqldf(statement)
                  
                  colnames(dataBilan) = colnames(d)
                  
                  dataBilan = rbind(dataBilan,d)
                  coloneNSH = c(coloneNSH,NSH_graph)
                  
                  
                }
              }
              
              dataBilan = data.frame(t(dataBilan))
              
              for(i in 1:(length(coloneNSH)-1)){
                colnames(dataBilan)[i] =  coloneNSH[i+1]
              }
              
              for(i in 1:nrow(dataBilan)){
                rownames(dataBilan)[i] =  paste(1998+i)
              }
              for(i in 1:length(dataBilan)){
                dataBilan[,i] = round(dataBilan[,i]/1000,digits = 3)
              }
              if(input$quantValGeneral == "Quantite"){
                txtTitle = paste("en Tonne")
              }else{
                txtTitle = paste("en Millions de Dinars")
              }
              pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
              ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
              for(i in 1:length(dataBilan)){
                pString <- paste0(pString," %>%add_trace(ploti,name = paste('NSH4 : ',",  eval(paste0("colnames(dataBilan)[",i,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                  text = ~paste('Année :',rownames(dataBilan),
                                  '<br>',input$produitSelect,' : '",  eval(paste0(",colnames(dataBilan)[",i,"]")),",'<br>'
                                  ,input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
              }
              try(eval(parse(text=pString)))
              p = try(print(ploti))
              
              ax <- list(
                title = "",
                tickangle = 45
              )
              
              text = ""
              for(i in 1:(length(coloneNSH)-1)){
                text = paste(text,coloneNSH[i+1])
              }
              
              ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
              p = try(p%>%
                        layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec tous les pays ",txtTitle,"<br>et pour les produits",text,"</br>"),showlegend = T)
              )
              
              
              }else if(input$produitSelect[1] == "Produits ( NSH6 )" & is.na(input$produitSelect[2])){
                
                dataBilan = matrix(0,0,19)
                dataBilan = data.frame(dataBilan)
                coloneNSH = ""
                for(i in 1:length(input$produitSelectNSH6)){
                  NSH_graph = str_split(input$produitSelectNSH6[i],"")
                  
                  NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
                  
                  statement = paste0("select * from b_variable where NSH6 ='",NSH_graph,"'")
                  
                  d = try(sqldf(statement))
                  if(class(d)!="try-error"){
                    datawork = d
                    statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                       sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                       sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                       sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                       XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                       from datawork")
                    d = sqldf(statement)
                    
                    colnames(dataBilan) = colnames(d)
                    
                    dataBilan = rbind(dataBilan,d)
                    coloneNSH = c(coloneNSH,NSH_graph)
                    
                    
                  }
                }
                
                dataBilan = data.frame(t(dataBilan))
                
                for(i in 1:(length(coloneNSH)-1)){
                  colnames(dataBilan)[i] =  coloneNSH[i+1]
                }
                
                for(i in 1:nrow(dataBilan)){
                  rownames(dataBilan)[i] =  paste(1998+i)
                }
                
                
                if(input$quantValGeneral == "Quantite"){
                  txtTitle = paste("en Kg")
                }else{
                  txtTitle = paste("en Dinars")
                }
                
                pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
                ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
                for(i in 1:length(dataBilan)){
                  pString <- paste0(pString," %>%add_trace(ploti,name = paste('NSH6 : ',",  eval(paste0("colnames(dataBilan)[",i,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                    text = ~paste('Année :',rownames(dataBilan),
                                    '<br>',input$produitSelect,' : '",  eval(paste0(",colnames(dataBilan)[",i,"]")),",'<br>'
                                    ,input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
                }
                try(eval(parse(text=pString)))
                p = try(print(ploti))
                
                ax <- list(
                  title = "",
                  tickangle = 45
                )
                
                text = ""
                for(i in 1:(length(coloneNSH)-1)){
                  text = paste(text,coloneNSH[i+1],sep = ",")
                }
                
                ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
                p = try(p%>%
                          layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec tous les pays ",txtTitle,"<br> et pour les produits",text,"</br>"),showlegend = T)
                )
                
                
                }else if(input$produitSelect[1] == "Sections ( NSH2 )" & input$produitSelect[2] == "Chapitres ( NSH4 )" & is.na(input$produitSelect[3])){
                  
                  dataBilan = matrix(0,0,19)
                  dataBilan = data.frame(dataBilan)
                  coloneNSH = ""
                  coloneNSH1 = ""
                  for(i in 1:length(input$produitSelectNSH44)){
                    NSH_graph = str_split(input$produitSelectNSH44[i],"")
                    NSH_graph1 = str_split(input$produitSelectNSH44[i],"")
                    
                    NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
                    NSH_graph1 = try(paste0(NSH_graph1[[1]][1],NSH_graph1[[1]][2]))
                    statement = paste0("select * from b_variable where NSH4 ='",NSH_graph,"'")
                    
                    d = try(sqldf(statement))
                    if(class(d)!="try-error"){
                      datawork = d
                      statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                         sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                         sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                         sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                         XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                         from datawork")
                      d = sqldf(statement)
                      
                      colnames(dataBilan) = colnames(d)
                      
                      dataBilan = rbind(dataBilan,d)
                      coloneNSH = c(coloneNSH,NSH_graph)
                      coloneNSH1 = c(coloneNSH1,NSH_graph1)
                      
                    }
                  }
                  
                  dataBilan = data.frame(t(dataBilan))
                  
                  for(i in 1:(length(coloneNSH)-1)){
                    colnames(dataBilan)[i] =  coloneNSH[i+1]
                  }
                  
                  for(i in 1:nrow(dataBilan)){
                    rownames(dataBilan)[i] =  paste(1998+i)
                  }
                  for(i in 1:length(dataBilan)){
                    dataBilan[,i] = round(dataBilan[,i]/1000,digits = 3)
                  }
                  if(input$quantValGeneral == "Quantite"){
                    txtTitle = paste("en Tonne")
                  }else{
                    txtTitle = paste("en Millions de Dinars")
                  }
                  pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
                  ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
                  for(i in 1:length(dataBilan)){
                    pString <- paste0(pString," %>%add_trace(ploti,name = paste0('NSH2 : ',",  eval(paste0("coloneNSH1[",i+1,"]")),",'<br>NSH4 : ',",  eval(paste0("coloneNSH[",i+1,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                      text = ~paste('Année :',rownames(dataBilan),
                                      '<br>NSH2 : '",  eval(paste0(",coloneNSH1[",i+1,"]")),
                                      ",'<br>NSH4 : '",  eval(paste0(",coloneNSH[",i+1,"]")),
                                      ",'<br>',input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
                  }
                  try(eval(parse(text=pString)))
                  p = try(print(ploti))
                  
                  ax <- list(
                    title = "",
                    tickangle = 45
                  )
                  
                  text = ""
                  for(i in 1:(length(coloneNSH)-1)){
                    text = paste(text,coloneNSH[i+1],sep = ",")
                  }
                  
                  ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
                  p =try( p%>%
                            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec tous les pays",txtTitle,"<br> et pour les produits",text,"</br>"),showlegend = T)
                  )
                  
                }else if(input$produitSelect[1] == "Sections ( NSH2 )" & input$produitSelect[2] == "Produits ( NSH6 )" & is.na(input$produitSelect[3])){
                  
                  dataBilan = matrix(0,0,19)
                  dataBilan = data.frame(dataBilan)
                  coloneNSH = ""
                  coloneNSH1 = ""
                  for(i in 1:length(input$produitSelectNSH626)){
                    NSH_graph = str_split(input$produitSelectNSH626[i],"")
                    NSH_graph1 = str_split(input$produitSelectNSH626[i],"")
                    
                    NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
                    NSH_graph1 = try(paste0(NSH_graph1[[1]][1],NSH_graph1[[1]][2]))
                    statement = paste0("select * from b_variable where NSH6 ='",NSH_graph,"'")
                    
                    d = try(sqldf(statement))
                    if(class(d)!="try-error"){
                      datawork = d
                      statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                         sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                         sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                         sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                         XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                         from datawork")
                      d = sqldf(statement)
                      
                      colnames(dataBilan) = colnames(d)
                      
                      dataBilan = rbind(dataBilan,d)
                      coloneNSH = c(coloneNSH,NSH_graph)
                      coloneNSH1 = c(coloneNSH1,NSH_graph1)
                      
                    }
                  }
                  
                  dataBilan = data.frame(t(dataBilan))
                  
                  for(i in 1:(length(coloneNSH)-1)){
                    colnames(dataBilan)[i] =  coloneNSH[i+1]
                  }
                  
                  for(i in 1:nrow(dataBilan)){
                    rownames(dataBilan)[i] =  paste(1998+i)
                  }
              
                  if(input$quantValGeneral == "Quantite"){
                    txtTitle = paste("en Kg")
                  }else{
                    txtTitle = paste("en Dinars")
                  }
                  pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
                  ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
                  for(i in 1:length(dataBilan)){
                    pString <- paste0(pString," %>%add_trace(ploti,name = paste0('NSH2 : ',",  eval(paste0("coloneNSH1[",i+1,"]")),",'<br>NSH6 : ',",  eval(paste0("coloneNSH[",i+1,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                      text = ~paste('Année :',rownames(dataBilan),
                                      '<br>NSH2 : '",  eval(paste0(",coloneNSH1[",i+1,"]")),
                                      ",'<br>NSH6 : '",  eval(paste0(",coloneNSH[",i+1,"]")),
                                      ",'<br>',input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
                  }
                  try(eval(parse(text=pString)))
                  p = try(print(ploti))
                  
                  ax <- list(
                    title = "",
                    tickangle = 45
                  )
                  
                  text = ""
                  for(i in 1:(length(coloneNSH)-1)){
                    text = paste(text,coloneNSH[i+1],sep = ",")
                  }
                  
                  ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
                  p = try(p%>%
                            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec tous les pays ",txtTitle,"<br>et pour les produits",text,"</br>"),showlegend = T)
                  )
                  
                }else if(input$produitSelect[1] == "Chapitres ( NSH4 )" & input$produitSelect[2] == "Produits ( NSH6 )" & is.na(input$produitSelect[3])){
                  dataBilan = matrix(0,0,19)
                  dataBilan = data.frame(dataBilan)
                  coloneNSH = ""
                  coloneNSH1 = ""
                  for(i in 1:length(input$produitSelectNSH646)){
                    NSH_graph = str_split(input$produitSelectNSH646[i],"")
                    NSH_graph1 = str_split(input$produitSelectNSH646[i],"")
                    
                    NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
                    NSH_graph1 = try(paste0(NSH_graph1[[1]][1],NSH_graph1[[1]][2],NSH_graph1[[1]][3],NSH_graph1[[1]][4]))
                    statement = paste0("select * from b_variable where NSH6 ='",NSH_graph,"'")
                    
                    d = try(sqldf(statement))
                    if(class(d)!="try-error"){
                      datawork = d
                      statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                         sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                         sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                         sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                         XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                         from datawork")
                      d = sqldf(statement)
                      
                      colnames(dataBilan) = colnames(d)
                      
                      dataBilan = rbind(dataBilan,d)
                      coloneNSH = c(coloneNSH,NSH_graph)
                      coloneNSH1 = c(coloneNSH1,NSH_graph1)
                      
                    }
                  }
                  
                  dataBilan = data.frame(t(dataBilan))
                  
                  for(i in 1:(length(coloneNSH)-1)){
                    colnames(dataBilan)[i] =  coloneNSH[i+1]
                  }
                  
                  for(i in 1:nrow(dataBilan)){
                    rownames(dataBilan)[i] =  paste(1998+i)
                  }
                  if(input$quantValGeneral == "Quantite"){
                    txtTitle = paste("en Kg")
                  }else{
                    txtTitle = paste("en Dinars")
                  }
                  pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
                  ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
                  for(i in 1:length(dataBilan)){
                    pString <- paste0(pString," %>%add_trace(ploti,name = paste0('NSH4 : ',",  eval(paste0("coloneNSH1[",i+1,"]")),",'<br>NSH6 : ',",  eval(paste0("coloneNSH[",i+1,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                      text = ~paste('Année :',rownames(dataBilan),
                                      '<br>NSH4 : '",  eval(paste0(",coloneNSH1[",i+1,"]")),
                                      ",'<br>NSH6 : '",  eval(paste0(",coloneNSH[",i+1,"]")),
                                      ",'<br>',input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
                  }
                  try(eval(parse(text=pString)))
                  p = try(print(ploti))
                  
                  ax <- list(
                    title = "",
                    tickangle = 45
                  )
                  
                  text = ""
                  for(i in 1:(length(coloneNSH)-1)){
                    text = paste(text,coloneNSH[i+1],sep = ",")
                  }
                  
                  ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
                  p = try(p%>%
                            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec tous les pays ",txtTitle,"<br>et pour les produits",text,"</br>"),showlegend = T)
                  )
                  
                }else if(input$produitSelect[1] == "Sections ( NSH2 )" & input$produitSelect[2] == "Chapitres ( NSH4 )" & input$produitSelect[3] == "Produits ( NSH6 )"){
                  
                  dataBilan = matrix(0,0,19)
                  dataBilan = data.frame(dataBilan)
                  coloneNSH = ""
                  coloneNSH1 = ""
                  coloneNSH2 = ""
                  for(i in 1:length(input$produitSelectNSH666)){
                    NSH_graph = str_split(input$produitSelectNSH666[i],"")
                    NSH_graph1 = str_split(input$produitSelectNSH666[i],"")
                    NSH_graph2 = str_split(input$produitSelectNSH666[i],"")
                    
                    NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
                    NSH_graph1 = try(paste0(NSH_graph1[[1]][1],NSH_graph1[[1]][2],NSH_graph1[[1]][3],NSH_graph1[[1]][4]))
                    NSH_graph2 = try(paste0(NSH_graph2[[1]][1],NSH_graph2[[1]][2]))
                    
                    statement = paste0("select * from b_variable where NSH6 ='",NSH_graph,"'")
                    
                    d = try(sqldf(statement))
                    if(class(d)!="try-error"){
                      datawork = d
                      statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                         sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                         sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                         sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                         XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                         from datawork")
                      d = sqldf(statement)
                      
                      colnames(dataBilan) = colnames(d)
                      
                      dataBilan = rbind(dataBilan,d)
                      coloneNSH = c(coloneNSH,NSH_graph)
                      coloneNSH1 = c(coloneNSH1,NSH_graph1)
                      coloneNSH2 = c(coloneNSH2,NSH_graph2)
                      
                    }
                  }
                  
                  dataBilan = data.frame(t(dataBilan))
                  
                  for(i in 1:(length(coloneNSH)-1)){
                    colnames(dataBilan)[i] =  coloneNSH[i+1]
                  }
                  
                  for(i in 1:nrow(dataBilan)){
                    rownames(dataBilan)[i] =  paste(1998+i)
                  }
                  if(input$quantValGeneral == "Quantite"){
                    txtTitle = paste("en Kg")
                  }else{
                    txtTitle = paste("en Dinars")
                  }
                  pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
                  ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
                  for(i in 1:length(dataBilan)){
                    pString <- paste0(pString," %>%add_trace(ploti,name = paste0('NSH2 : ',",  eval(paste0("coloneNSH2[",i+1,"]")),",'<br>NSH4 : ',",  eval(paste0("coloneNSH1[",i+1,"]")),",'<br>NSH6 : ',",  eval(paste0("coloneNSH[",i+1,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                      text = ~paste('Année :',rownames(dataBilan),
                                      '<br>NSH2 : '",  eval(paste0(",coloneNSH2[",i+1,"]")),
                                      ",'<br>NSH4 : '",  eval(paste0(",coloneNSH1[",i+1,"]")),
                                      ",'<br>NSH6 : '",  eval(paste0(",coloneNSH[",i+1,"]")),
                                      ",'<br>',input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
                  }
                  try(eval(parse(text=pString)))
                  p = try(print(ploti))
                  
                  ax <- list(
                    title = "",
                    tickangle = 45
                  )
                  
                  text = ""
                  for(i in 1:(length(coloneNSH)-1)){
                    text = paste(text,coloneNSH[i+1],sep = ",")
                  }
                  
                  ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
                  p = try(p%>%
                            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec tous les pays ",txtTitle,"<br>et pour les produits",text,"</br>"),showlegend = T)
                  )
                  
                }
          
              }
        
                }else if(input$choixPays == "Choisir Pays" & input$choixProduit == "Choisir Produits"){
                  if(!is.null(input$produitSelect)){
                    if(input$produitSelect[1] == "Sections ( NSH2 )" & is.na(input$produitSelect[2])){
                      
                      dataBilan = matrix(0,19,0)
                      dataBilan = data.frame(dataBilan)
                      coloneNSH = ""
                      pp = ""
                      for(j in 1:length(input$paysSelect)){
                        dataBilanNSH = matrix(0,0,19)
                        dataBilanNSH = data.frame(dataBilanNSH)
                        for(i in 1:length(input$produitSelectNSH2)){
                          
                          NSH_graph = str_split(input$produitSelectNSH2[i],"")
                          
                          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2]))
                          
                          statement = paste0("select * from b_variable where NSH2 ='",NSH_graph,"'")
                          
                          d = try(sqldf(statement))
                          colnames(d)
                          if(class(d)!="try-error"){
                            datawork = d
                            statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                               sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                               sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                               sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                               XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                               from datawork where Nom_Pays ='",input$paysSelect[j],"'")
                            d = sqldf(statement)
                            
                            colnames(dataBilanNSH) = colnames(d)
                            
                            dataBilanNSH = rbind(dataBilanNSH,d)
                            coloneNSH = c(coloneNSH,NSH_graph)
                            pp = c(pp,input$paysSelect[j])
                            
                          }
                        }
                        dataBilanNSH = data.frame(t(dataBilanNSH))
                        
                        dataBilan = cbind(dataBilan,dataBilanNSH)
                      }
                      
                      for(i in 1:length(dataBilan)){
                        colnames(dataBilan)[i] = paste0("col",i)
                      }
                      
                      for(i in 1:length(dataBilan)){
                        k = which(is.na(dataBilan[,i]))
                        dataBilan[k,i] = 0
                      }
                      
                      for(i in 1:nrow(dataBilan)){
                        rownames(dataBilan)[i] = paste(1998+i)
                      }
                      for(i in 1:length(dataBilan)){
                        dataBilan[,i] = round(dataBilan[,i]/1000000,digits = 3)
                      }
                      if(input$quantValGeneral == "Quantite"){
                        txtTitle = paste("en milles Tonne")
                      }else{
                        txtTitle = paste("en Milliards de Dinars")
                      }
                      pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
                      ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
                      for(i in 1:length(dataBilan)){
                        pString <- paste0(pString," %>%add_trace(ploti,name = paste0('Pays : ',",  eval(paste0("pp[",i+1,"]")),",'<br>NSH2 : ',",  eval(paste0("coloneNSH[",i+1,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                          text = ~paste('Année :',rownames(dataBilan),
                                          '<br>Pays : '",  eval(paste0(",pp[",i+1,"]")),",'<br>NSH2 : '",  
                                          eval(paste0(",coloneNSH[",i+1,"]")),",'<br>'
                                          ,input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
                      }
                      try(eval(parse(text=pString)))
                      p = try(print(ploti))
                      
                      ax <- list(
                        title = "",
                        tickangle = 45
                      )
                      
                      text = ""
                      for(i in 1:length(input$produitSelectNSH2)){
                        NSH_graph = str_split(input$produitSelectNSH2[i],"")
                        NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2]))
                        text = paste(text,NSH_graph,sep = ",")
                      }
                      
                      text1 = ""
                      for(i in 1:length(input$paysSelect)){
                        text1 = paste(text1,input$paysSelect[i],sep = ",")
                      }
                      
                      ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
                      p = try(p%>%
                                layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec ",text1," ",txtTitle,"<br> pour les produits ",text,"</br>"),showlegend = T)
                      )
                      
                      
                      
                      }else if(input$produitSelect[1] == "Chapitres ( NSH4 )" & is.na(input$produitSelect[2])){
                        
                        dataBilan = matrix(0,19,0)
                        dataBilan = data.frame(dataBilan)
                        coloneNSH = ""
                        pp = ""
                        for(j in 1:length(input$paysSelect)){
                          dataBilanNSH = matrix(0,0,19)
                          dataBilanNSH = data.frame(dataBilanNSH)
                          for(i in 1:length(input$produitSelectNSH4)){
                            
                            NSH_graph = str_split(input$produitSelectNSH4[i],"")
                            
                            NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
                            
                            statement = paste0("select * from b_variable where NSH4 ='",NSH_graph,"'")
                            
                            d = try(sqldf(statement))
                            colnames(d)
                            if(class(d)!="try-error"){
                              datawork = d
                              statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                                 sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                                 sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                                 sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                                 XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                                 from datawork where Nom_Pays ='",input$paysSelect[j],"'")
                              d = sqldf(statement)
                              
                              colnames(dataBilanNSH) = colnames(d)
                              
                              dataBilanNSH = rbind(dataBilanNSH,d)
                              coloneNSH = c(coloneNSH,NSH_graph)
                              pp = c(pp,input$paysSelect[j])
                              
                            }
                          }
                          dataBilanNSH = data.frame(t(dataBilanNSH))
                          
                          dataBilan = cbind(dataBilan,dataBilanNSH)
                        }
                        
                        for(i in 1:length(dataBilan)){
                          colnames(dataBilan)[i] = paste0("col",i)
                        }
                        
                        for(i in 1:length(dataBilan)){
                          k = which(is.na(dataBilan[,i]))
                          dataBilan[k,i] = 0
                        }
                        
                        for(i in 1:nrow(dataBilan)){
                          rownames(dataBilan)[i] = paste(1998+i)
                        }
                        for(i in 1:length(dataBilan)){
                          dataBilan[,i] = round(dataBilan[,i]/1000,digits = 3)
                        }
                        if(input$quantValGeneral == "Quantite"){
                          txtTitle = paste("en Tonne")
                        }else{
                          txtTitle = paste("en Millions de Dinars")
                        }
                        pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
                        ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
                        for(i in 1:length(dataBilan)){
                          pString <- paste0(pString," %>%add_trace(ploti,name = paste0('Pays : ',",  eval(paste0("pp[",i+1,"]")),",'<br>NSH4 : ',",  eval(paste0("coloneNSH[",i+1,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                            text = ~paste('Année :',rownames(dataBilan),
                                            '<br>Pays : '",  eval(paste0(",pp[",i+1,"]")),",'<br>NSH4 : '",  
                                            eval(paste0(",coloneNSH[",i+1,"]")),",'<br>'
                                            ,input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
                        }
                        try(eval(parse(text=pString)))
                        p = try(print(ploti))
                        
                        ax <- list(
                          title = "",
                          tickangle = 45
                        )
                        
                        text = ""
                        for(i in 1:length(input$produitSelectNSH4)){
                          NSH_graph = str_split(input$produitSelectNSH4[i],"")
                          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
                          text = paste(text,NSH_graph,sep=",")
                        }
                        
                        text1 = ""
                        for(i in 1:length(input$paysSelect)){
                          text1 = paste(text1,input$paysSelect[i],sep = ",")
                        }
                        
                        ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
                        p = try(p%>%
                                  layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec ",text1," ",txtTitle,"<br> pour les produits ",text,"</br>"),showlegend = T)
                        )
                        
                        
                        }else if(input$produitSelect[1] == "Produits ( NSH6 )" & is.na(input$produitSelect[2])){
                          
                          dataBilan = matrix(0,19,0)
                          dataBilan = data.frame(dataBilan)
                          coloneNSH = ""
                          pp = ""
                          for(j in 1:length(input$paysSelect)){
                            dataBilanNSH = matrix(0,0,19)
                            dataBilanNSH = data.frame(dataBilanNSH)
                            for(i in 1:length(input$produitSelectNSH6)){
                              
                              NSH_graph = str_split(input$produitSelectNSH6[i],"")
                              
                              NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
                              
                              statement = paste0("select * from b_variable where NSH6 ='",NSH_graph,"'")
                              
                              d = try(sqldf(statement))
                              colnames(d)
                              if(class(d)!="try-error"){
                                datawork = d
                                statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                                   sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                                   sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                                   sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                                   XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                                   from datawork where Nom_Pays ='",input$paysSelect[j],"'")
                                d = sqldf(statement)
                                
                                colnames(dataBilanNSH) = colnames(d)
                                
                                dataBilanNSH = rbind(dataBilanNSH,d)
                                coloneNSH = c(coloneNSH,NSH_graph)
                                pp = c(pp,input$paysSelect[j])
                                
                              }
                            }
                            dataBilanNSH = data.frame(t(dataBilanNSH))
                            
                            dataBilan = cbind(dataBilan,dataBilanNSH)
                          }
                          
                          for(i in 1:length(dataBilan)){
                            colnames(dataBilan)[i] = paste0("col",i)
                          }
                          
                          for(i in 1:length(dataBilan)){
                            k = which(is.na(dataBilan[,i]))
                            dataBilan[k,i] = 0
                          }
                          
                          for(i in 1:nrow(dataBilan)){
                            rownames(dataBilan)[i] = paste(1998+i)
                          }
                     
                          if(input$quantValGeneral == "Quantite"){
                            txtTitle = paste("en Kg")
                          }else{
                            txtTitle = paste("en Dinars")
                          }
                          pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
                          ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
                          for(i in 1:length(dataBilan)){
                            pString <- paste0(pString," %>%add_trace(ploti,name = paste0('Pays : ',",  eval(paste0("pp[",i+1,"]")),",'<br>NSH6 : ',",  eval(paste0("coloneNSH[",i+1,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                              text = ~paste('Année :',rownames(dataBilan),
                                              '<br>Pays : '",  eval(paste0(",pp[",i+1,"]")),",'<br>NSH6 : '",  
                                              eval(paste0(",coloneNSH[",i+1,"]")),",'<br>'
                                              ,input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
                          }
                          try(eval(parse(text=pString)))
                          p = try(print(ploti))
                          
                          ax <- list(
                            title = "",
                            tickangle = 45
                          )
                          
                          text = ""
                          for(i in 1:length(input$produitSelectNSH6)){
                            NSH_graph = str_split(input$produitSelectNSH6[i],"")
                            NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
                            text = paste(text,NSH_graph,sep=",")
                          }
                          
                          text1 = ""
                          for(i in 1:length(input$paysSelect)){
                            text1 = paste(text1,input$paysSelect[i],sep = ",")
                          }
                          
                          ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
                          p = try(p%>%
                                    layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec ",text1," ",txtTitle,"<br> pour les produits ",text,"</br>"),showlegend = T)
                          )
                          
                          
                          }else if(input$produitSelect[1] == "Sections ( NSH2 )" & input$produitSelect[2] == "Chapitres ( NSH4 )" & is.na(input$produitSelect[3])){
                            
                            dataBilan = matrix(0,19,0)
                            dataBilan = data.frame(dataBilan)
                            coloneNSH = ""
                            coloneNSH1 = ""
                            
                            pp = ""
                            for(j in 1:length(input$paysSelect)){
                              dataBilanNSH = matrix(0,0,19)
                              dataBilanNSH = data.frame(dataBilanNSH)
                              for(i in 1:length(input$produitSelectNSH44)){
                                
                                NSH_graph = str_split(input$produitSelectNSH44[i],"")
                                NSH_graph1 = str_split(input$produitSelectNSH44[i],"")
                                
                                NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
                                NSH_graph1 = try(paste0(NSH_graph1[[1]][1],NSH_graph1[[1]][2]))
                                
                                statement = paste0("select * from b_variable where NSH4 ='",NSH_graph,"'")
                                
                                d = try(sqldf(statement))
                                colnames(d)
                                if(class(d)!="try-error"){
                                  datawork = d
                                  statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                                     sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                                     sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                                     sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                                     XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                                     from datawork where Nom_Pays ='",input$paysSelect[j],"'")
                                  d = sqldf(statement)
                                  
                                  colnames(dataBilanNSH) = colnames(d)
                                  
                                  dataBilanNSH = rbind(dataBilanNSH,d)
                                  coloneNSH = c(coloneNSH,NSH_graph)
                                  coloneNSH1 = c(coloneNSH1,NSH_graph1)
                                  pp = c(pp,input$paysSelect[j])
                                  
                                }
                              }
                              dataBilanNSH = data.frame(t(dataBilanNSH))
                              
                              dataBilan = cbind(dataBilan,dataBilanNSH)
                            }
                            
                            for(i in 1:length(dataBilan)){
                              colnames(dataBilan)[i] = paste0("col",i)
                            }
                            
                            for(i in 1:length(dataBilan)){
                              k = which(is.na(dataBilan[,i]))
                              dataBilan[k,i] = 0
                            }
                            
                            for(i in 1:nrow(dataBilan)){
                              rownames(dataBilan)[i] = paste(1998+i)
                            }
                            for(i in 1:length(dataBilan)){
                              dataBilan[,i] = round(dataBilan[,i]/1000,digits = 3)
                            }
                            if(input$quantValGeneral == "Quantite"){
                              txtTitle = paste("en Tonne")
                            }else{
                              txtTitle = paste("en Millions de Dinars")
                            }
                            pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
                            ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
                            for(i in 1:length(dataBilan)){
                              pString <- paste0(pString," %>%add_trace(ploti,name = paste0('Pays : ',",  eval(paste0("pp[",i+1,"]")),",'<br>NSH2 : ',",  eval(paste0("coloneNSH1[",i+1,"]")),",'<br>NSH4 : ',",  eval(paste0("coloneNSH[",i+1,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                                text = ~paste('Année :',rownames(dataBilan),
                                                '<br>Pays : '",  eval(paste0(",pp[",i+1,"]")),",
                                                '<br>NSH2 : '",eval(paste0(",coloneNSH1[",i+1,"]")),",
                                                '<br>NSH4 : '",eval(paste0(",coloneNSH[",i+1,"]")),",'<br>'
                                                ,input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
                            }
                            try(eval(parse(text=pString)))
                            p = try(print(ploti))
                            
                            ax <- list(
                              title = "",
                              tickangle = 45
                            )
                            
                            text = ""
                            for(i in 1:length(input$produitSelectNSH44)){
                              NSH_graph = str_split(input$produitSelectNSH44[i],"")
                              NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
                              text = paste(text,NSH_graph,sep=",")
                            }
                            
                            text1 = ""
                            for(i in 1:length(input$paysSelect)){
                              text1 = paste(text1,input$paysSelect[i],sep = ",")
                            }
                            
                            ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
                            p = try(p%>%
                                      layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec ",text1," ",txtTitle,"<br> pour les produits ",text,"</br>"),showlegend = T)
                            )
                            
                            
                            
                            }else if(input$produitSelect[1] == "Sections ( NSH2 )" & input$produitSelect[2] == "Produits ( NSH6 )" & is.na(input$produitSelect[3])){
                              
                              dataBilan = matrix(0,19,0)
                              dataBilan = data.frame(dataBilan)
                              coloneNSH = ""
                              coloneNSH1 = ""
                              
                              pp = ""
                              for(j in 1:length(input$paysSelect)){
                                dataBilanNSH = matrix(0,0,19)
                                dataBilanNSH = data.frame(dataBilanNSH)
                                for(i in 1:length(input$produitSelectNSH626)){
                                  
                                  NSH_graph = str_split(input$produitSelectNSH626[i],"")
                                  NSH_graph1 = str_split(input$produitSelectNSH626[i],"")
                                  
                                  NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
                                  NSH_graph1 = try(paste0(NSH_graph1[[1]][1],NSH_graph1[[1]][2]))
                                  
                                  statement = paste0("select * from b_variable where NSH6 ='",NSH_graph,"'")
                                  
                                  d = try(sqldf(statement))
                                  colnames(d)
                                  if(class(d)!="try-error"){
                                    datawork = d
                                    statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                                       sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                                       sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                                       sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                                       XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                                       from datawork where Nom_Pays ='",input$paysSelect[j],"'")
                                    d = sqldf(statement)
                                    
                                    colnames(dataBilanNSH) = colnames(d)
                                    
                                    dataBilanNSH = rbind(dataBilanNSH,d)
                                    coloneNSH = c(coloneNSH,NSH_graph)
                                    coloneNSH1 = c(coloneNSH1,NSH_graph1)
                                    pp = c(pp,input$paysSelect[j])
                                    
                                  }
                                }
                                dataBilanNSH = data.frame(t(dataBilanNSH))
                                
                                dataBilan = cbind(dataBilan,dataBilanNSH)
                              }
                              
                              for(i in 1:length(dataBilan)){
                                colnames(dataBilan)[i] = paste0("col",i)
                              }
                              
                              for(i in 1:length(dataBilan)){
                                k = which(is.na(dataBilan[,i]))
                                dataBilan[k,i] = 0
                              }
                              
                              for(i in 1:nrow(dataBilan)){
                                rownames(dataBilan)[i] = paste(1998+i)
                              }
                           
                              if(input$quantValGeneral == "Quantite"){
                                txtTitle = paste("en Kg")
                              }else{
                                txtTitle = paste("en Dinars")
                              }
                              pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
                              ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
                              for(i in 1:length(dataBilan)){
                                pString <- paste0(pString," %>%add_trace(ploti,name = paste0('Pays : ',",  eval(paste0("pp[",i+1,"]")),",'<br>NSH2 : ',",  eval(paste0("coloneNSH1[",i+1,"]")),",'<br>NSH6 : ',",  eval(paste0("coloneNSH[",i+1,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                                  text = ~paste('Année :',rownames(dataBilan),
                                                  '<br>Pays : '",  eval(paste0(",pp[",i+1,"]")),",
                                                  '<br>NSH2 : '",eval(paste0(",coloneNSH1[",i+1,"]")),",
                                                  '<br>NSH6 : '",eval(paste0(",coloneNSH[",i+1,"]")),",'<br>'
                                                  ,input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
                              }
                              try(eval(parse(text=pString)))
                              p = try(print(ploti))
                              
                              ax <- list(
                                title = "",
                                tickangle = 45
                              )
                              
                              text = ""
                              for(i in 1:length(input$produitSelectNSH626)){
                                NSH_graph = str_split(input$produitSelectNSH626[i],"")
                                NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
                                text = paste(text,NSH_graph,sep=",")
                              }
                              
                              text1 = ""
                              for(i in 1:length(input$paysSelect)){
                                text1 = paste(text1,input$paysSelect[i],sep = ",")
                              }
                              
                              ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
                              p = try(p%>%
                                        layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec ",text1," ",txtTitle,"<br> pour les produits ",text,"</br>"),showlegend = T)
                              )
                              
                              
                              }else if(input$produitSelect[1] == "Chapitres ( NSH4 )" & input$produitSelect[2] == "Produits ( NSH6 )" & is.na(input$produitSelect[3])){
                                
                                
                                dataBilan = matrix(0,19,0)
                                dataBilan = data.frame(dataBilan)
                                coloneNSH = ""
                                coloneNSH1 = ""
                                
                                pp = ""
                                for(j in 1:length(input$paysSelect)){
                                  dataBilanNSH = matrix(0,0,19)
                                  dataBilanNSH = data.frame(dataBilanNSH)
                                  for(i in 1:length(input$produitSelectNSH646)){
                                    
                                    NSH_graph = str_split(input$produitSelectNSH646[i],"")
                                    NSH_graph1 = str_split(input$produitSelectNSH646[i],"")
                                    
                                    NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
                                    NSH_graph1 = try(paste0(NSH_graph1[[1]][1],NSH_graph1[[1]][2],NSH_graph1[[1]][3],NSH_graph1[[1]][4]))
                                    
                                    statement = paste0("select * from b_variable where NSH6 ='",NSH_graph,"'")
                                    
                                    d = try(sqldf(statement))
                                    colnames(d)
                                    if(class(d)!="try-error"){
                                      datawork = d
                                      statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                                         sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                                         sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                                         sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                                         XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                                         from datawork where Nom_Pays ='",input$paysSelect[j],"'")
                                      d = sqldf(statement)
                                      
                                      colnames(dataBilanNSH) = colnames(d)
                                      
                                      dataBilanNSH = rbind(dataBilanNSH,d)
                                      coloneNSH = c(coloneNSH,NSH_graph)
                                      coloneNSH1 = c(coloneNSH1,NSH_graph1)
                                      pp = c(pp,input$paysSelect[j])
                                      
                                    }
                                  }
                                  dataBilanNSH = data.frame(t(dataBilanNSH))
                                  
                                  dataBilan = cbind(dataBilan,dataBilanNSH)
                                }
                                
                                for(i in 1:length(dataBilan)){
                                  colnames(dataBilan)[i] = paste0("col",i)
                                }
                                
                                for(i in 1:length(dataBilan)){
                                  k = which(is.na(dataBilan[,i]))
                                  dataBilan[k,i] = 0
                                }
                                
                                for(i in 1:nrow(dataBilan)){
                                  rownames(dataBilan)[i] = paste(1998+i)
                                }
                                if(input$quantValGeneral == "Quantite"){
                                  txtTitle = paste("en Kg")
                                }else{
                                  txtTitle = paste("en Dinars")
                                }
                                pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
                                ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
                                for(i in 1:length(dataBilan)){
                                  pString <- paste0(pString," %>%add_trace(ploti,name = paste0('Pays : ',",  eval(paste0("pp[",i+1,"]")),",'<br>NSH4 : ',",  eval(paste0("coloneNSH1[",i+1,"]")),",'<br>NSH6 : ',",  eval(paste0("coloneNSH[",i+1,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                                    text = ~paste('Année :',rownames(dataBilan),
                                                    '<br>Pays : '",  eval(paste0(",pp[",i+1,"]")),",
                                                    '<br>NSH4 : '",eval(paste0(",coloneNSH1[",i+1,"]")),",
                                                    '<br>NSH6 : '",eval(paste0(",coloneNSH[",i+1,"]")),",'<br>'
                                                    ,input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
                                }
                                try(eval(parse(text=pString)))
                                p = try(print(ploti))
                                
                                ax <- list(
                                  title = "",
                                  tickangle = 45
                                )
                                
                                text = ""
                                for(i in 1:length(input$produitSelectNSH646)){
                                  NSH_graph = str_split(input$produitSelectNSH646[i],"")
                                  NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
                                  text = paste(text,NSH_graph,sep=",")
                                }
                                
                                text1 = ""
                                for(i in 1:length(input$paysSelect)){
                                  text1 = paste(text1,input$paysSelect[i],sep = ",")
                                }
                                
                                ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
                                p = try(p%>%
                                          layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec ",text1," ",txtTitle,"<br> pour les produits ",text,"</br>"),showlegend = T)
                                )
                                
                                
                                
                                }else if(input$produitSelect[1] == "Sections ( NSH2 )" & input$produitSelect[2] == "Chapitres ( NSH4 )" & input$produitSelect[3] == "Produits ( NSH6 )"){
                                  
                                  
                                  
                                  dataBilan = matrix(0,19,0)
                                  dataBilan = data.frame(dataBilan)
                                  coloneNSH = ""
                                  coloneNSH1 = ""
                                  coloneNSH2 = ""
                                  pp = ""
                                  for(j in 1:length(input$paysSelect)){
                                    dataBilanNSH = matrix(0,0,19)
                                    dataBilanNSH = data.frame(dataBilanNSH)
                                    for(i in 1:length(input$produitSelectNSH666)){
                                      
                                      NSH_graph = str_split(input$produitSelectNSH666[i],"")
                                      NSH_graph1 = str_split(input$produitSelectNSH666[i],"")
                                      NSH_graph2 = str_split(input$produitSelectNSH666[i],"")
                                      
                                      NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
                                      NSH_graph1 = try(paste0(NSH_graph1[[1]][1],NSH_graph1[[1]][2],NSH_graph1[[1]][3],NSH_graph1[[1]][4]))
                                      NSH_graph2 = try(paste0(NSH_graph2[[1]][1],NSH_graph2[[1]][2]))
                                      
                                      statement = paste0("select * from b_variable where NSH6 ='",NSH_graph,"'")
                                      
                                      d = try(sqldf(statement))
                                      colnames(d)
                                      if(class(d)!="try-error"){
                                        datawork = d
                                        statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                                                           sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                                                           sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                                                           sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                                                           XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                                                           from datawork where Nom_Pays ='",input$paysSelect[j],"'")
                                        d = sqldf(statement)
                                        
                                        colnames(dataBilanNSH) = colnames(d)
                                        
                                        dataBilanNSH = rbind(dataBilanNSH,d)
                                        coloneNSH = c(coloneNSH,NSH_graph)
                                        coloneNSH1 = c(coloneNSH1,NSH_graph1)
                                        coloneNSH2 = c(coloneNSH2,NSH_graph2)
                                        pp = c(pp,input$paysSelect[j])
                                        
                                      }
                                    }
                                    dataBilanNSH = data.frame(t(dataBilanNSH))
                                    
                                    dataBilan = cbind(dataBilan,dataBilanNSH)
                                  }
                                  
                                  for(i in 1:length(dataBilan)){
                                    colnames(dataBilan)[i] = paste0("col",i)
                                  }
                                  
                                  for(i in 1:length(dataBilan)){
                                    k = which(is.na(dataBilan[,i]))
                                    dataBilan[k,i] = 0
                                  }
                                  
                                  for(i in 1:nrow(dataBilan)){
                                    rownames(dataBilan)[i] = paste(1998+i)
                                  }
                                  if(input$quantValGeneral == "Quantite"){
                                    txtTitle = paste("en Kg")
                                  }else{
                                    txtTitle = paste("en Dinars")
                                  }
                                  pString = "ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))"
                                  ploti <- try(plot_ly(dataBilan, x = ~rownames(dataBilan)))
                                  for(i in 1:length(dataBilan)){
                                    pString <- paste0(pString," %>%add_trace(ploti,name = paste0('Pays : ',",  eval(paste0("pp[",i+1,"]")),",'<br>NSH2 : ',",  eval(paste0("coloneNSH2[",i+1,"]")),",'<br>NSH4 : ',",  eval(paste0("coloneNSH1[",i+1,"]")),",'<br>NSH6 : ',",  eval(paste0("coloneNSH[",i+1,"]")),",'</br>'), x = ~rownames(dataBilan),y = ~",  eval(paste0("dataBilan[,",i,"]")),", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                                                      text = ~paste('Année :',rownames(dataBilan),
                                                      '<br>Pays : '",  eval(paste0(",pp[",i+1,"]")),",
                                                      '<br>NSH2 : '",eval(paste0(",coloneNSH2[",i+1,"]")),",
                                                      '<br>NSH4 : '",eval(paste0(",coloneNSH1[",i+1,"]")),",
                                                      '<br>NSH6 : '",eval(paste0(",coloneNSH[",i+1,"]")),",'<br>'
                                                      ,input$quantValGeneral,input$importExportGeneral,' : '",  eval(paste0(",dataBilan[,",i,"]")),",'</br>'))")
                                  }
                                  try(eval(parse(text=pString)))
                                  p = try(print(ploti))
                                  
                                  ax <- list(
                                    title = "",
                                    tickangle = 45
                                  )
                                  
                                  text = ""
                                  for(i in 1:length(input$produitSelectNSH666)){
                                    NSH_graph = str_split(input$produitSelectNSH666[i],"")
                                    NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
                                    text = paste(text,NSH_graph,sep=",")
                                  }
                                  
                                  text1 = ""
                                  for(i in 1:length(input$paysSelect)){
                                    text1 = paste(text1,input$paysSelect[i],sep = ",")
                                  }
                                  
                                  ay = list(title = paste(input$quantValGeneral,input$importExportGeneral))
                                  p = try(p%>%
                                            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," d'",input$importExportGeneral,"ation de la Tunisie avec ",text1," ",txtTitle,"<br> pour les produits ",text,"</br>"),showlegend = T)
                                  )
                                  
                                  
                                  }
                                }
                              }
    
    output$BilanPlot = renderPlotly({
      p = try(p)

isolate({  
  if(input$choixPays == "Tous" & input$choixProduit == "Tous"){
    p
  }else if(input$choixPays == "Choisir Pays" & input$choixProduit == "Tous"){    
  
    if(is.null(input$paysSelect[1])){
      sendSweetAlert(
        session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
          lapply(htmltools::HTML), type = "error",html = T
      )   
    }
  else{          
   if(class(p)[1]=="plotly" & (str_detect(text,"Error")==F) ){
        p
      }else{
        sendSweetAlert(
          session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
            lapply(htmltools::HTML), type = "error",html = T
        )   
        return()
        
      }
    }
  }else if(input$choixPays == "Tous" & input$choixProduit == "Choisir Produits"){    
    
    if(is.null(input$produitSelect[1])){
      sendSweetAlert(
        session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
          lapply(htmltools::HTML), type = "error",html = T
      )   
    }
    else{          
      if(class(p)[1]=="plotly" & (str_detect(text,"Error")==F) ){
        p
      }else{
        sendSweetAlert(
          session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
            lapply(htmltools::HTML), type = "error",html = T
        )   
        return()
        
      }
    }
  }else if(input$choixPays == "Choisir Pays" & input$choixProduit == "Choisir Produits"){    
    
    if(is.null(input$produitSelect[1]) | is.null(input$paysSelect[1])){
      sendSweetAlert(
        session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
          lapply(htmltools::HTML), type = "error",html = T
      )   
    }
    else{          
      if(class(p)[1]=="plotly" & (str_detect(text,"Error")==F) ){
        p
      }else{
        sendSweetAlert(
          session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
            lapply(htmltools::HTML), type = "error",html = T
        )   
        return()
        
      }
    }
  }
  })
    })
    
    shinyjs::hide("jsbtnbilan")
    shinyjs::enable("btnBilan")
    
                            })
  
  
  
  ############################################################
  ########################Balance graphes########################
  ############################################################
  observeEvent(input$btnBalance,{ 
    
    shinyjs::show("jsbtnBalance")
    shinyjs::disable("btnBalance")
    
    output$txtAnneeBalance = renderText({
      paste("<b>Année :<font color='#B33'><b>",input$selectAnneeBalance,"</font>")
    })
    
    b_variableImport = data.frame(b_variableImport())
    b_variableExport = data.frame(b_variableExport())
    
    if(input$choixPaysBalance == "Tous" & input$choixProduitBalance == "Tous"){
      
      
      statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                         sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                         sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                         sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                         XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                         from b_variableImport")
      d = sqldf(statement)
      dataBalanceImport = data.frame(d)
      for(i in 1:length(dataBalanceImport)){
        colnames(dataBalanceImport)[i] = paste0(1998+i)
      }
      dataBalanceImport = data.frame(t(dataBalanceImport))
      
      statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                         sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                         sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                         sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                         XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                         from b_variableExport")
      d = sqldf(statement)
      dataBalanceExport = data.frame(d)
      for(i in 1:length(dataBalanceExport)){
        colnames(dataBalanceExport)[i] = paste0(1998+i)
      }
      
      dataBalanceExport = data.frame(t(dataBalanceExport))
      
      colnames(dataBalanceExport) = paste(input$quantValGeneral)
      
      dataBalanceEco = matrix(0,19,1)
      dataBalanceEco = data.frame(dataBalanceEco)
      colnames(dataBalanceEco) = paste(input$quantValGeneral)
      dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
      dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
      for(i in 1:19){
        dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
      }
      
      for(i in 1:19){
        rownames(dataBalanceEco)[i] = paste(1998+i)
      }
      
      dataBalanceCouv = matrix(0,19,1)
      dataBalanceCouv = data.frame(dataBalanceCouv)
      colnames(dataBalanceCouv) = paste(input$quantValGeneral)
      
      for(i in 1:19){
        rownames(dataBalanceCouv)[i] = paste(1998+i)
      }
      
      for(i in 1:19){
        dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
        
      }
    
      dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
      colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
      
      for(i in 1:19){
        rownames(dataBalanceGlob)[i] = paste(1998+i)
      }
      ax <- list(
        title = "",
        tickangle = 45
      )
      ay = list(title = paste(input$quantValGeneral))
      
      for(i in 1:3){
        
        dataBalanceGlob[,i] = round(dataBalanceGlob[,i]/10^(6),digits = 3)
        
      }
      
      if(input$quantValGeneral == "Quantite"){
        txtTitle = paste("en milles Tonne")
      }else{
        txtTitle = paste("en Milliards de Dinars")
      }
      
      p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                   text = ~paste("Année :",rownames(dataBalanceGlob),
                                 "<br>",input$quantValGeneral," : ", dataBalanceGlob[,1],"<br>"))%>%
        
        add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                  text = ~paste("Année :",rownames(dataBalanceGlob),
                                "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
        
        add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                  text = ~paste("Année :",rownames(dataBalanceGlob),
                                "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
        
        layout(xaxis = ax, yaxis = ay,title = paste0("La variation e la ",input$quantValGeneral," de la Balance commerciale de la Tunisie ",txtTitle," avec tous les pays"),
               showlegend = T)
      
      
    }
    else if(input$choixPaysBalance == "Choisir Pays" & input$choixProduitBalance == "Tous"){
      
      
      statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                         sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                         sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                         sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                         XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                         from b_variableImport where Nom_Pays ='",input$paysSelectBalance,"'")
      d = sqldf(statement)
      dataBalanceImport = data.frame(d)
      for(i in 1:length(dataBalanceImport)){
        colnames(dataBalanceImport)[i] = paste0(1998+i)
      }
      dataBalanceImport = data.frame(t(dataBalanceImport))
      
      statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                         sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                         sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                         sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                         XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                         from b_variableExport where Nom_Pays ='",input$paysSelectBalance,"'")
      d = sqldf(statement)
      dataBalanceExport = data.frame(d)
      for(i in 1:length(dataBalanceExport)){
        colnames(dataBalanceExport)[i] = paste0(1998+i)
      }
      
      dataBalanceExport = data.frame(t(dataBalanceExport))
      
      colnames(dataBalanceExport) = paste(input$quantValGeneral)
      
      dataBalanceEco = matrix(0,19,1)
      dataBalanceEco = data.frame(dataBalanceEco)
      colnames(dataBalanceEco) = paste(input$quantValGeneral)
      dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
      dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
      for(i in 1:19){
        dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
      }
      
      dataBalanceCouv = matrix(0,19,1)
      dataBalanceCouv = data.frame(dataBalanceCouv)
      colnames(dataBalanceCouv) = paste(input$quantValGeneral)
      
      for(i in 1:19){
        rownames(dataBalanceCouv)[i] = paste(1998+i)
      }
      
      for(i in 1:19){
        dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
        if(is.na(dataBalanceCouv[i,])){
          dataBalanceCouv[i,] = 0
        }
      }
      
      dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
      colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
      
      for(i in 1:19){
        rownames(dataBalanceGlob)[i] = paste(1998+i)
      }
      
      ax <- list(
        title = "",
        tickangle = 45
      )
      ay = list(title = paste(input$quantValGeneral))
      
      for(i in 1:3){
        
        dataBalanceGlob[,i] = round(dataBalanceGlob[,i]/10^(6),digits = 3)
        
      }
      
      if(input$quantValGeneral == "Quantite"){
        txtTitle = paste("en milles Tonne")
      }else{
        txtTitle = paste("en Milliards de Dinars")
      }
      
      p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                   text = ~paste("Pays :",input$paysSelectBalance,
                                 "<br>Année :",rownames(dataBalanceGlob),
                                 "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
        
        add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                  text = ~paste("Pays :",input$paysSelectBalance,
                                "<br>Année :",rownames(dataBalanceGlob),
                                "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
        
        add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                  text = ~paste("Pays :",input$paysSelectBalance,
                                "<br>Année :",rownames(dataBalanceGlob),
                                "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
        
        layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec ",input$paysSelectBalance," ",txtTitle),
               showlegend = T)
      
    }
    else if(input$choixPaysBalance == "Tous" & input$choixProduitBalance == "Choisir Produits"){
      
      if(!is.null(input$produitSelectBalance)){
        
        if(input$produitSelectBalance[1] == "Sections ( NSH2 )" & is.na(input$produitSelectBalance[2])){
          
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH2,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2]))
          
          statement = paste0("select * from b_variableImport where NSH2 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where NSH2 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          
          dataBalanceExport = data.frame(t(dataBalanceExport))
          
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          dataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
          
          for(i in 1:3){
            
            dataBalanceGlob[,i] = round(dataBalanceGlob[,i]/10^(6),digits = 3)
            
          }
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en milles Tonne")
          }else{
            txtTitle = paste("en Milliards de Dinars")
          }
          
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste(
                         "<br>Année :",rownames(dataBalanceGlob),
                         "<br>Section ( NSH2 ) :",NSH_graph,
                         "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Section ( NSH2 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Section ( NSH2 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec Tous les Pays ",txtTitle," <br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
          
        }else if(input$produitSelectBalance[1] == "Chapitres ( NSH4 )" & is.na(input$produitSelectBalance[2])){
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH4,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
          
          statement = paste0("select * from b_variableImport where NSH4 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where NSH4 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          
          dataBalanceExport = data.frame(t(dataBalanceExport))
          
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          dataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
          
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
          
          for(i in 1:3){
            
            dataBalanceGlob[,i] = round(dataBalanceGlob[,i]/10^(3),digits = 3)
            
          }
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Tonne")
          }else{
            txtTitle = paste("en Millions de Dinars")
          }
          
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste(
                         "<br>Année :",rownames(dataBalanceGlob),
                         "<br>Chapitre ( NSH4 ) :",NSH_graph,
                         "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Chapitre ( NSH4 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Chapitre ( NSH4 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec Tous les Pays ",txtTitle," <br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
        }else if(input$produitSelectBalance[1] == "Produits ( NSH6 )" & is.na(input$produitSelectBalance[2])){
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH6,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          
          statement = paste0("select * from b_variableImport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          
          dataBalanceExport = data.frame(t(dataBalanceExport))
          
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          dataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
          
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
          
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste(
                         "<br>Année :",rownames(dataBalanceGlob),
                         "<br>Produit ( NSH6 ) :",NSH_graph,
                         "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Produit ( NSH6 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Produit ( NSH6 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec Tous les Pays ",txtTitle," <br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
        }else if(input$produitSelectBalance[1] == "Sections ( NSH2 )" & input$produitSelectBalance[2] == "Chapitres ( NSH4 )" & is.na(input$produitSelectBalance[3])){
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH44,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
          
          statement = paste0("select * from b_variableImport where NSH4 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where NSH4 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          
          dataBalanceExport = data.frame(t(dataBalanceExport))
          
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          dataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
          
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
          for(i in 1:3){
            
            dataBalanceGlob[,i] = round(dataBalanceGlob[,i]/10^(3),digits = 3)
            
          }
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Tonne")
          }else{
            txtTitle = paste("en Millions de Dinars")
          }
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste(
                         "<br>Année :",rownames(dataBalanceGlob),
                         "<br>Chapitre ( NSH4 ) :",NSH_graph,
                         "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Chapitre ( NSH4 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Chapitre ( NSH4 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec Tous les Pays ",txtTitle," <br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
        }else if(input$produitSelectBalance[1] == "Sections ( NSH2 )" & input$produitSelectBalance[2] == "Produits ( NSH6 )" & is.na(input$produitSelectBalance[3])){
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH626,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          
          statement = paste0("select * from b_variableImport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          
          dataBalanceExport = data.frame(t(dataBalanceExport))
          
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          dataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
          
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
       
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste(
                         "<br>Année :",rownames(dataBalanceGlob),
                         "<br>Produit ( NSH6 ) :",NSH_graph,
                         "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Produit ( NSH6 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Produit ( NSH6 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec Tous les Pays ",txtTitle,"<br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
        }else if(input$produitSelectBalance[1] == "Chapitres ( NSH4 )" & input$produitSelectBalance[2] == "Produits ( NSH6 )" & is.na(input$produitSelectBalance[3])){
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH646,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          
          statement = paste0("select * from b_variableImport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          
          dataBalanceExport = data.frame(t(dataBalanceExport))
          
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          dataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste(
                         "<br>Année :",rownames(dataBalanceGlob),
                         "<br>Produit ( NSH6 ) :",NSH_graph,
                         "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Produit ( NSH6 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Produit ( NSH6 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec Tous les Pays ",txtTitle,"<br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
        }else if(input$produitSelectBalance[1] == "Sections ( NSH2 )" & input$produitSelectBalance[2] == "Chapitres ( NSH4 )" & input$produitSelectBalance[3] == "Produits ( NSH6 )"){
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH666,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          
          statement = paste0("select * from b_variableImport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          
          dataBalanceExport = data.frame(t(dataBalanceExport))
          
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          dataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
          
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste(
                         "<br>Année :",rownames(dataBalanceGlob),
                         "<br>Produit ( NSH6 ) :",NSH_graph,
                         "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Produit ( NSH6 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste(
                        "<br>Année :",rownames(dataBalanceGlob),
                        "<br>Produit ( NSH6 ) :",NSH_graph,
                        "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec tous les Pays ",txtTitle,"<br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
        }
        
      }
      
    }
    else if(input$choixPaysBalance == "Choisir Pays" & input$choixProduitBalance == "Choisir Produits"){
      
      
      
      if(!is.null(input$produitSelectBalance)){
        
        if(input$produitSelectBalance[1] == "Sections ( NSH2 )" & is.na(input$produitSelectBalance[2])){
          
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH2,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2]))
          
          statement = paste0("select * from b_variableImport where Nom_Pays = '",input$paysSelectBalance,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where Nom_Pays = '",input$paysSelectBalance,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport where NSH2 ='",NSH_graph,"'")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport where NSH2 ='",NSH_graph,"'")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          dataBalanceExport = data.frame(t(dataBalanceExport))
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),] = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          dataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
          for(i in 1:3){
            
            dataBalanceGlob[,i] = round(dataBalanceGlob[,i]/10^(6),digits = 3)
            
          }
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en milles Tonne")
          }else{
            txtTitle = paste("en Milliards de Dinars")
          }
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste("Pays :",input$paysSelectBalance,
                                     "<br>Année :",rownames(dataBalanceGlob),
                                     "<br>Section ( NSH2 ) :",NSH_graph,
                                     "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Section ( NSH2 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Section ( NSH2 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec ",input$paysSelectBalance,txtTitle," <br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
          
        }else if(input$produitSelectBalance[1] == "Chapitres ( NSH4 )" & is.na(input$produitSelectBalance[2])){
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH4,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
          
          statement = paste0("select * from b_variableImport where NSH4 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where NSH4 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport where Nom_Pays = '",input$paysSelectBalance,"'")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport where Nom_Pays = '",input$paysSelectBalance,"'")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          
          dataBalanceExport = data.frame(t(dataBalanceExport))
          
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          dataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
          for(i in 1:3){
            
            dataBalanceGlob[,i] = round(dataBalanceGlob[,i]/10^(3),digits = 3)
            
          }
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Tonne")
          }else{
            txtTitle = paste("en Millions de Dinars")
          }
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste("Pays :",input$paysSelectBalance,
                                     "<br>Année :",rownames(dataBalanceGlob),
                                     "<br>Chapitre ( NSH4 ) :",NSH_graph,
                                     "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Chapitre ( NSH4 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Chapitre ( NSH4 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral,txtTitle," de la Balance commerciale de la Tunisie avec ",input$paysSelectBalance," <br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
        }else if(input$produitSelectBalance[1] == "Produits ( NSH6 )" & is.na(input$produitSelectBalance[2])){
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH6,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          
          statement = paste0("select * from b_variableImport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport where Nom_Pays = '",input$paysSelectBalance,"'")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport where Nom_Pays = '",input$paysSelectBalance,"'")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          
          dataBalanceExport = data.frame(t(dataBalanceExport))
          
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          ddataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
          
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
        
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste("Pays :",input$paysSelectBalance,
                                     "<br>Année :",rownames(dataBalanceGlob),
                                     "<br>Produit ( NSH6 ) :",NSH_graph,
                                     "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Produit ( NSH6 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Produit ( NSH6 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec ",input$paysSelectBalance,txtTitle," <br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
        }else if(input$produitSelectBalance[1] == "Sections ( NSH2 )" & input$produitSelectBalance[2] == "Chapitres ( NSH4 )" & is.na(input$produitSelectBalance[3])){
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH44,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
          
          statement = paste0("select * from b_variableImport where NSH4 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where NSH4 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport where Nom_Pays = '",input$paysSelectBalance,"'")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport where Nom_Pays = '",input$paysSelectBalance,"'")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          
          dataBalanceExport = data.frame(t(dataBalanceExport))
          
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          dataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
          for(i in 1:3){
            
            dataBalanceGlob[,i] = round(dataBalanceGlob[,i]/10^(3),digits = 3)
            
          }
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Tonne")
          }else{
            txtTitle = paste("en Millions de Dinars")
          }
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste("Pays :",input$paysSelectBalance,
                                     "<br>Année :",rownames(dataBalanceGlob),
                                     "<br>Chapitre ( NSH4 ) :",NSH_graph,
                                     "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Chapitre ( NSH4 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Chapitre ( NSH4 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec ",input$paysSelectBalance,txtTitle," <br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
        }else if(input$produitSelectBalance[1] == "Sections ( NSH2 )" & input$produitSelectBalance[2] == "Produits ( NSH6 )" & is.na(input$produitSelectBalance[3])){
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH626,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          
          statement = paste0("select * from b_variableImport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport where Nom_Pays = '",input$paysSelectBalance,"'")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport where Nom_Pays = '",input$paysSelectBalance,"'")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          
          dataBalanceExport = data.frame(t(dataBalanceExport))
          
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          dataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
        
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste("Pays :",input$paysSelectBalance,
                                     "<br>Année :",rownames(dataBalanceGlob),
                                     "<br>Produit ( NSH6 ) :",NSH_graph,
                                     "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Produit ( NSH6 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Produit ( NSH6 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec ",input$paysSelectBalance,txtTitle," <br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
        }else if(input$produitSelectBalance[1] == "Chapitres ( NSH4 )" & input$produitSelectBalance[2] == "Produits ( NSH6 )" & is.na(input$produitSelectBalance[3])){
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH646,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          
          statement = paste0("select * from b_variableImport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport where Nom_Pays = '",input$paysSelectBalance,"'")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport where Nom_Pays = '",input$paysSelectBalance,"'")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          
          dataBalanceExport = data.frame(t(dataBalanceExport))
          
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          dataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv)
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture") 
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste("Pays :",input$paysSelectBalance,
                                     "<br>Année :",rownames(dataBalanceGlob),
                                     "<br>Produit ( NSH6 ) :",NSH_graph,
                                     "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Produit ( NSH6 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Produit ( NSH6 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec ",input$paysSelectBalance,txtTitle," <br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
        }else if(input$produitSelectBalance[1] == "Sections ( NSH2 )" & input$produitSelectBalance[2] == "Chapitres ( NSH4 )" & input$produitSelectBalance[3] == "Produits ( NSH6 )"){
          dataBalance = matrix(0,0,19)
          dataBalance = data.frame(dataBalance)
          coloneNSH = ""
          
          NSH_graph = str_split(input$produitSelectBalanceNSH666,"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          
          statement = paste0("select * from b_variableImport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkImport = data.frame(d)
          
          statement = paste0("select * from b_variableExport where NSH6 ='",NSH_graph,"'")
          
          d = try(sqldf(statement))
          dataworkExport = data.frame(d)
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkImport where Nom_Pays = '",input$paysSelectBalance,"'")
          d = sqldf(statement)
          dataBalanceImport = data.frame(d)
          for(i in 1:length(dataBalanceImport)){
            colnames(dataBalanceImport)[i] = paste0(1998+i)
          }
          dataBalanceImport = data.frame(t(dataBalanceImport))
          
          statement = paste0("select sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from dataworkExport where Nom_Pays = '",input$paysSelectBalance,"'")
          d = sqldf(statement)
          dataBalanceExport = data.frame(d)
          for(i in 1:length(dataBalanceExport)){
            colnames(dataBalanceExport)[i] = paste0(1998+i)
          }
          
          dataBalanceExport = data.frame(t(dataBalanceExport))
          
          colnames(dataBalanceExport) = paste(input$quantValGeneral)
          
          dataBalanceEco = matrix(0,19,1)
          dataBalanceEco = data.frame(dataBalanceEco)
          colnames(dataBalanceEco) = paste(input$quantValGeneral)
          dataBalanceImport[which(is.na(dataBalanceImport[,1])),]  = 0
          dataBalanceExport[which(is.na(dataBalanceExport[,1])),] = 0
          for(i in 1:19){
            dataBalanceEco[i,] = dataBalanceExport[i,]-dataBalanceImport[i,]
          }
          
          for(i in 1:19){
            rownames(dataBalanceEco)[i] = paste(1998+i)
          }
          
          dataBalanceCouv = matrix(0,19,1)
          dataBalanceCouv = data.frame(dataBalanceCouv)
          colnames(dataBalanceCouv) = paste(input$quantValGeneral)
          
          for(i in 1:19){
            rownames(dataBalanceCouv)[i] = paste(1998+i)
          }
          
          for(i in 1:19){
            dataBalanceCouv[i,] = try(dataBalanceExport[i,]/dataBalanceImport[i,])
            if(is.na(dataBalanceCouv[i,])){
              dataBalanceCouv[i,] = 0
            }
          }
          
          dataBalanceGlob = try(data.frame(dataBalanceImport,dataBalanceExport,dataBalanceEco,dataBalanceCouv))
          colnames(dataBalanceGlob) = c("Import","Export","Balance","Couverture")
          for(i in 1:19){
            rownames(dataBalanceGlob)[i] = paste(1998+i)
          }
          
          ax <- list(
            title = "",
            tickangle = 45
          )
          ay = list(title = paste(input$quantValGeneral))
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
          p <- plot_ly(dataBalanceGlob,  x = ~rownames(dataBalanceGlob),name = "Import", y = ~dataBalanceGlob[,1], type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                       text = ~paste("Pays :",input$paysSelectBalance,
                                     "<br>Année :",rownames(dataBalanceGlob),
                                     "<br>Produit ( NSH6 ) :",NSH_graph,
                                     "<br>",input$quantValGeneral," :", dataBalanceGlob[,1],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,2], name = "Export", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Produit ( NSH6 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,2],"<br>"))%>%
            
            add_trace(y = ~dataBalanceGlob[,3],name = "Balance Commerciale", type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
                      text = ~paste("Pays :",input$paysSelectBalance,
                                    "<br>Année :",rownames(dataBalanceGlob),
                                    "<br>Produit ( NSH6 ) :",NSH_graph,
                                    "<br>",input$quantValGeneral," : ", dataBalanceGlob[,3],"<br>"))%>%
            
            layout(xaxis = ax, yaxis = ay,title = paste0("La variation de la ",input$quantValGeneral," de la Balance commerciale de la Tunisie avec ",input$paysSelectBalance,txtTitle," <br> pour le produit ",NSH_graph,"</br>"),
                   showlegend = T)
        }
        
      }
      
      
      
    }
    
    
    if(input$tmapsRadio == "Tous"){
      if(input$quantValGeneral == "Quantite"){
        txtTitle = paste("en milles Tonne")
      }else{
        txtTitle = paste("en Milliards de Dinars")
      }
    }
    else{
      
      if(!is.null(input$parNSH)){   
        if(input$parNSH[1] == 'Sections ( NSH2 )' & is.na(input$parNSH[2])){
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en milles Tonne")
          }else{
            txtTitle = paste("en Milliards de Dinars")
          }
        }else if(input$parNSH[1] == 'Chapitres ( NSH4 )' & is.na(input$parNSH[2])){
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Tonne")
          }else{
            txtTitle = paste("en Millions de Dinars")
          }
        }else if(input$parNSH[1] == 'Produits ( NSH6 )' & is.na(input$parNSH[2])){
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
        }else if(input$parNSH[1] == 'Sections ( NSH2 )' & input$parNSH[2] == 'Chapitres ( NSH4 )' & is.na(input$parNSH[3])){
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Tonne")
          }else{
            txtTitle = paste("en Millions de Dinars")
          }
        }else if(input$parNSH[1] == 'Sections ( NSH2 )' & input$parNSH[2] == 'Produits ( NSH6 )' & is.na(input$parNSH[3])){
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
        }else if(input$parNSH[1] == 'Chapitres ( NSH4 )' & input$parNSH[2] == 'Produits ( NSH6 )' & is.na(input$parNSH[3])){
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
        }else if(input$parNSH[1] == 'Sections ( NSH2 )' & input$parNSH[2] == 'Chapitres ( NSH4 )' & input$parNSH[3] == 'Produits ( NSH6 )'){
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          } 
        }
      }
    }
    
    output$boxImport = renderUI({
      
      dd = try(dataBalanceGlob)
      if(class(dd) != "try-error"){
      
      if(input$selectAnneeBalance == 1999){
        try(infoBox(value = tags$h6(tags$b("Import",txtTitle)), subtitle = tags$h4(tags$b(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,1])), title = tags$b(paste(input$quantValGeneral)),
                icon = shiny::icon("circle"), color = "teal",width = 4, fill = T))
        
      }else if(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,1] > dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1999,1]){
        
       try( infoBox(value = tags$h6(tags$b("Import",txtTitle)), subtitle = tags$h4(tags$b(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,1])), title = tags$b(paste(input$quantValGeneral)),
                icon = shiny::icon("caret-up"), color = "teal",width = 4, fill = T))
      }else if(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,1] <= dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1999,1]){
        
       try( infoBox(value = tags$h6(tags$b("Import",txtTitle)), subtitle = tags$h4(tags$b(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,1])), title = tags$b(paste(input$quantValGeneral)),
                icon = shiny::icon("caret-down"), color = "teal",width = 4, fill = T))
      }
      }else{
        return()
      }
      
      
    })
    
    output$boxExport = renderUI({
      dd = try(dataBalanceGlob)
      if(class(dd) != "try-error"){
      if(input$selectAnneeBalance == 1999){
        try(infoBox(value = tags$h6(tags$b("Export",txtTitle)), subtitle = tags$h4(tags$b(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,2])), title = tags$b(paste(input$quantValGeneral)),
                icon = shiny::icon("circle"), color = "orange",width = 4, fill = T)
        )
      }else if(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,2] > dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1999,2]){
        
       try( infoBox(value = tags$h6(tags$b("Export",txtTitle)), subtitle = tags$h4(tags$b(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,2])), title = tags$b(paste(input$quantValGeneral)),
                icon = shiny::icon("caret-up"), color = "orange",width = 4, fill = T))
      }else if(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,2] <= dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1999,2]){
        
        try(infoBox(value = tags$h6(tags$b("Export",txtTitle)), subtitle = tags$h4(tags$b(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,2])), title = tags$b(paste(input$quantValGeneral)),
                icon = shiny::icon("caret-down"), color = "orange",width = 4, fill = T))
      }
      }else{
        return()
      }
    })
    
    output$boxBalance = renderUI({
      dd = try(dataBalanceGlob)
      if(class(dd) != "try-error"){
      if(input$selectAnneeBalance == 1999){
        if(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,3]<=0){
          try(infoBox(value = tags$h6(tags$b("de couverture")), subtitle = tags$h4(tags$b(round(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,4]*100,digits = 2),"%")), title = tags$b("Taux"),
                  icon = shiny::icon("circle"), color = "red",width = 4, fill = T))
        }else if(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,3]>0){
          try(infoBox(value = tags$h6(tags$b("de couverture")), subtitle = tags$h4(tags$b(round(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,4]*100,digits = 2),"%")), title = tags$b("Taux"),
                  icon = shiny::icon("circle"), color = "green",width = 4, fill = T))
          
        }
        
      }else if(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,3] > dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1999,3]){
        if(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,3]<=0){
          infoBox(value = tags$h6(tags$b("de couverture")), subtitle = tags$h4(tags$b(round(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,4]*100,digits = 2),"%")), title = tags$b("Taux"),
                  icon = shiny::icon("caret-up"), color = "red",width = 4, fill = T)
        }else if(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,3]>0){
          try(infoBox(value = tags$h6(tags$b("de couverture")), subtitle = tags$h4(tags$b(round(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,4]*100,digits = 2),"%")), title = tags$b("Taux"),
                  icon = shiny::icon("caret-up"), color = "green",width = 4, fill = T))
        }
      }else if(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,3] <= dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1999,3]){
        if(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,3]<=0){
          try(infoBox(value = tags$h6(tags$b("de couverture")), subtitle = tags$h4(tags$b(round(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,4]*100,digits = 2),"%")), title = tags$b("Taux"),
                  icon = shiny::icon("caret-down"), color = "red",width = 4, fill = T))
        }else if(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,3]>0){
          try(infoBox(value = tags$h6(tags$b("de couverture")), subtitle = tags$h4(tags$b(round(dataBalanceGlob[as.numeric(input$selectAnneeBalance)-1998,4]*100,digits = 2),"%")), title = tags$b("Taux"),
                  icon = shiny::icon("caret-down"), color = "green",width = 4, fill = T))
        }
      }
      }else{
        return()
      }
    })
    
    output$BalancePlot = renderPlotly({
      p = try(p)
      if(class(p)[1]=="plotly"){
        shinyjs::show("jsbtnAnneeBalance")
        
        p
      }else{
        shinyjs::hide("jsbtnAnneeBalance")
        
        sendSweetAlert(
          session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
            lapply(htmltools::HTML), type = "error",html = T
        )   
        return()
        
      }
    })
    shinyjs::hide("jsbtnBalance")
    shinyjs::enable("btnBalance")
    
    })
  ############################################################
  ########################Produits graphes########################
  ############################################################
  observeEvent(input$selectAnnee,{
    if(!is.null(input$produitGlobal) & input$pdtRadio != "Tous"){
      if(input$btnprod != 0){
        click("btnprod")
      }
    }else  if(input$pdtRadio == "Tous"){
      if(input$btnprod != 0){
        click("btnprod")
      }
    }
  })
  
  observeEvent(input$btnprod,{   
    output$txtAnneePdt = renderText({
      paste("<b>Année :<font color='#B33'><b>",input$selectAnnee,"</font>")
    })
    
    shinyjs::show("jsbtnprod")
    shinyjs::disable("btnprod")
    
    b_variable = data.frame(b_variable())
    if(input$pdtRadio == "Tous"){
      
      datawork = matrix(0,0,ncol(b_variable)+19)
      datawork = data.frame(datawork)

        
        statement = paste0("select * , sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                           sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                           sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                           sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                           XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                           from b_variable group by Nom_Pays")
        
        d = try(sqldf(statement))
        if(class(d)!="try-error"){
          colnames(datawork) = colnames(d)
          datawork = data.frame(d)
        }
        
        plotcam = list()
        plotbar = list()
        ax <- list(
          title = "",
          zeroline = F,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        )
        dataworkmodified = datawork
        dataworkmodified = dataworkmodified[c(2,which(colnames(dataworkmodified)== paste0("XX",input$selectAnnee)))]
        
        colnames(dataworkmodified) = c("Nom_Pays","annee")
        dataworkmodified = dataworkmodified[which(dataworkmodified$annee>0),]
        dataworkmodified = dataworkmodified[order(-dataworkmodified$annee),]
        
        dataworkmodified$Nom_Pays <- factor(dataworkmodified$Nom_Pays, levels = unique(dataworkmodified$Nom_Pays)[order(dataworkmodified$annee, decreasing = TRUE)])
        
        dataworkmodified$annee = round(dataworkmodified$annee/1000000,digits = 3)
        if(input$quantValGeneral == "Quantite"){
          txtTitle = paste("en milles Tonne")
        }else{
          txtTitle = paste("en Milliards de Dinars")
        }
        
        if(nrow(dataworkmodified)>10){  
          p1 = try(plot_ly(dataworkmodified[1:10,], x = ~Nom_Pays,
                       y = ~annee,
                       split = ~Nom_Pays,
                       type = 'bar',hoverinfo = 'text',
                       text = ~paste("Pays :",Nom_Pays,
                                     "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
            layout(xaxis = ax, yaxis = ax,title = paste("Tous les produit",txtTitle)))
        }else{
          p1 = try(plot_ly(dataworkmodified, x = ~annee,
                       y = ~Nom_Pays,
                       split = ~Nom_Pays,
                       type = 'bar',hoverinfo = 'text',
                       text = ~paste("Pays :",Nom_Pays,
                                     "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
            layout(xaxis = ax, yaxis = ax,title = paste("Tous les produits",txtTitle)))
        }
        plotbar = c(plotbar,list(p1)) 
        
        p2 = try(dataworkmodified%>%
          group_by(Nom_Pays) %>%
          plot_ly(labels = ~Nom_Pays, values = ~annee, textposition = 'inside',
                  textinfo = 'label+percent',hoverinfo = 'text',
                  text = ~paste(input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee)) %>%
          add_pie(hole = 0.4))
        plotcam = c(plotcam,list(p2)) 
        
        
        
 }else{
    
    if(!is.null(input$produitGlobal)){    
      if(input$produitGlobal[1] == "Sections ( NSH2 )" & is.na(input$produitGlobal[2])){
        
        datawork = matrix(0,0,ncol(b_variable)+19)
        datawork = data.frame(datawork)
        for(i in 1:length(input$produitSpecNSH2)){
          NSH_graph = str_split(input$produitSpecNSH2[i],"")
          
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2]))
          
          statement = paste0("select * , sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from b_variable where NSH2 ='",NSH_graph,"' group by Nom_Pays")
          
          d = try(sqldf(statement))
          if(class(d)!="try-error"){
            colnames(datawork) = colnames(d)
            datawork = rbind(datawork,d)
            }
        }
      }
      else if(input$produitGlobal[1] == "Chapitres ( NSH4 )" & is.na(input$produitGlobal[2])){
        datawork = matrix(0,0,ncol(b_variable)+19)
        datawork = data.frame(datawork)
        for(i in 1:length(input$produitSpecNSH4)){
          
          NSH_graph = str_split(input$produitSpecNSH4[i],"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
          statement = paste0("select * , sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from b_variable where NSH4 ='",NSH_graph,"' group by Nom_Pays")
          d = sqldf(statement)
          colnames(datawork) = colnames(d)
          datawork = rbind(datawork,d)
        }
      }
      else if(input$produitGlobal[1] == "Produits ( NSH6 )" & is.na(input$produitGlobal[2])){
        datawork = matrix(0,0,ncol(b_variable)+19)
        datawork = data.frame(datawork)
        for(i in 1:length(input$produitSpecNSH6)){
          
          NSH_graph = str_split(input$produitSpecNSH6[i],"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          statement = paste0("select * , sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from b_variable where NSH6 ='",NSH_graph,"' group by Nom_Pays")
          d = sqldf(statement)
          colnames(datawork) = colnames(d)
          datawork = rbind(datawork,d)
        }
      }
      else if(input$produitGlobal[1] == "Sections ( NSH2 )" & input$produitGlobal[2] == "Chapitres ( NSH4 )" & is.na(input$produitGlobal[3]) ){
        
        datawork = matrix(0,0,ncol(b_variable)+19)
        datawork = data.frame(datawork)
        for(i in 1:length(input$produitSpecNSH44)){
          
          NSH_graph = str_split(input$produitSpecNSH44[i],"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
          statement = paste0("select * , sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from b_variable where NSH4 ='",NSH_graph,"' group by Nom_Pays")
          d = sqldf(statement)
          colnames(datawork) = colnames(d)
          datawork = rbind(datawork,d)
        }
        
      }
      else if(input$produitGlobal[1] == "Sections ( NSH2 )" & input$produitGlobal[2] == "Produits ( NSH6 )" & is.na(input$produitGlobal[3]) ){
        
        datawork = matrix(0,0,ncol(b_variable)+19)
        datawork = data.frame(datawork)
        for(i in 1:length(input$produitSpecNSH626)){
          
          NSH_graph = str_split(input$produitSpecNSH626[i],"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          statement = paste0("select * , sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from b_variable where NSH6 ='",NSH_graph,"' group by Nom_Pays")
          d = sqldf(statement)
          colnames(datawork) = colnames(d)
          datawork = rbind(datawork,d)
        }
        
      }
      else if(input$produitGlobal[1] == "Chapitres ( NSH4 )" & input$produitGlobal[2] == "Produits ( NSH6 )" & is.na(input$produitGlobal[3]) ){
        
        datawork = matrix(0,0,ncol(b_variable)+19)
        datawork = data.frame(datawork)
        for(i in 1:length(input$produitSpecNSH646)){
          
          NSH_graph = str_split(input$produitSpecNSH646[i],"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          statement = paste0("select * , sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from b_variable where NSH6 ='",NSH_graph,"' group by Nom_Pays")
          d = sqldf(statement)
          colnames(datawork) = colnames(d)
          datawork = rbind(datawork,d)
        }
        
      }
      else if(input$produitGlobal[1] == "Sections ( NSH2 )" & input$produitGlobal[2] == "Chapitres ( NSH4 )" & input$produitGlobal[3] == "Produits ( NSH6 )" ){
        
        datawork = matrix(0,0,ncol(b_variable)+19)
        datawork = data.frame(datawork)
        for(i in 1:length(input$produitSpecNSH666)){
          
          NSH_graph = str_split(input$produitSpecNSH666[i],"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          statement = paste0("select * , sum(X1999) XX1999,sum(X2000) XX2000,sum(X2001) XX2001,
                             sum(X2002) XX2002,sum(X2003) XX2003,sum(X2004) XX2004,sum(X2005) XX2005,
                             sum(X2006) XX2006,sum(X2007) XX2007,sum(X2008) XX2008,sum(X2009) XX2009,
                             sum(X2010) XX2010,sum(X2011) XX2011,sum(X2012) XX2012,sum(X2013) XX2013,sum(X2014)
                             XX2014,sum(X2015) XX2015,sum(X2016) XX2016,sum(X2017) XX2017 
                             from b_variable where NSH6 ='",NSH_graph,"' group by Nom_Pays")
          d = sqldf(statement)
          colnames(datawork) = colnames(d)
          datawork = rbind(datawork,d)
        }
        
      }
      
      plotcam = list()
      plotbar = list()
      ax <- list(
        title = "",
        zeroline = F,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
      if(input$produitGlobal[1] == "Sections ( NSH2 )" & is.na(input$produitGlobal[2])){
        
        for(i in 1:length(input$produitSpecNSH2)){
          NSH_graph = str_split(input$produitSpecNSH2[i],"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2]))
          
          k = which(datawork$NSH2 == NSH_graph)
          dataworkmodified = datawork[k,]
          
          dataworkmodified = dataworkmodified[c(2,which(colnames(dataworkmodified)== paste0("XX",input$selectAnnee)))]
          
          colnames(dataworkmodified) = c("Nom_Pays","annee")
          
          dataworkmodified = dataworkmodified[which(dataworkmodified$annee>0),]
          
          dataworkmodified = dataworkmodified[order(-dataworkmodified$annee),]
          dataworkmodified$Nom_Pays <- factor(dataworkmodified$Nom_Pays, levels = unique(dataworkmodified$Nom_Pays)[order(dataworkmodified$annee, decreasing = TRUE)])
          
          dataworkmodified$annee = round(dataworkmodified$annee/1000000,digits = 3)
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en milles Tonne")
          }else{
            txtTitle = paste("en Milliards de Dinars")
          }
          
          if(nrow(dataworkmodified)>10){  
            p1 = try(plot_ly(dataworkmodified[1:10,], x = ~Nom_Pays,
                         y = ~annee,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle)))
          }else{
            p1 = try(plot_ly(dataworkmodified, x = ~annee,
                         y = ~Nom_Pays,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle)))
          }
          plotbar = c(plotbar,list(p1)) 
          
          p2 = try(dataworkmodified%>%
            group_by(Nom_Pays) %>%
            plot_ly(labels = ~Nom_Pays, values = ~annee, textposition = 'inside',textinfo = 'label+percent',hoverinfo = 'text',
                    text = ~paste(input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee)) %>%
            add_pie(hole = 0.4))
          plotcam = c(plotcam,list(p2)) 
        }
      }else if(input$produitGlobal[1] == "Chapitres ( NSH4 )" & is.na(input$produitGlobal[2])){
        
        for(i in 1:length(input$produitSpecNSH4)){
          NSH_graph = str_split(input$produitSpecNSH4[i],"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
          
          k = which(datawork$NSH4 == NSH_graph)
          dataworkmodified = datawork[k,]
          
          dataworkmodified = dataworkmodified[c(2,which(colnames(dataworkmodified)== paste0("XX",input$selectAnnee)))]
          
          colnames(dataworkmodified) = c("Nom_Pays","annee")
          dataworkmodified = dataworkmodified[which(dataworkmodified$annee>0),]
          dataworkmodified = dataworkmodified[order(-dataworkmodified$annee),]
          
          dataworkmodified$Nom_Pays <- factor(dataworkmodified$Nom_Pays, levels = unique(dataworkmodified$Nom_Pays)[order(dataworkmodified$annee, decreasing = TRUE)])
          
          dataworkmodified$annee = round(dataworkmodified$annee/1000,digits = 3)
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Tonne")
          }else{
            txtTitle = paste("en Millions de Dinars")
          }
          
          if(nrow(dataworkmodified)>10){  
            p1 = try(plot_ly(dataworkmodified[1:10,], x = ~Nom_Pays,
                         y = ~annee,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle)))
          }else{
            p1 = try(plot_ly(dataworkmodified, x = ~annee,
                         y = ~Nom_Pays,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle)))
          }
          plotbar = c(plotbar,list(p1)) 
          
          p2 = try(dataworkmodified%>%
            group_by(Nom_Pays) %>%
            plot_ly(labels = ~Nom_Pays, values = ~annee, textposition = 'inside',
                    textinfo = 'label+percent',hoverinfo = 'text',
                    text = ~paste(input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee)) %>%
            add_pie(hole = 0.4))
          plotcam = c(plotcam,list(p2)) 
        }
        
      }else if(input$produitGlobal[1] == "Produits ( NSH6 )" & is.na(input$produitGlobal[2])){
        
        for(i in 1:length(input$produitSpecNSH6)){
          NSH_graph = str_split(input$produitSpecNSH6[i],"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          
          k = which(datawork$NSH6 == NSH_graph)
          dataworkmodified = datawork[k,]
          
          dataworkmodified = dataworkmodified[c(2,which(colnames(dataworkmodified)== paste0("XX",input$selectAnnee)))]
          
          colnames(dataworkmodified) = c("Nom_Pays","annee")
          dataworkmodified = dataworkmodified[which(dataworkmodified$annee>0),]
          dataworkmodified = dataworkmodified[order(-dataworkmodified$annee),]
          
          dataworkmodified$Nom_Pays <- factor(dataworkmodified$Nom_Pays, levels = unique(dataworkmodified$Nom_Pays)[order(dataworkmodified$annee, decreasing = TRUE)])
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
          
          if(nrow(dataworkmodified)>10){  
            p1 = try(plot_ly(dataworkmodified[1:10,], x = ~Nom_Pays,
                         y = ~annee,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle)))
          }else{
            p1 = try(plot_ly(dataworkmodified, x = ~annee,
                         y = ~Nom_Pays,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle)))
          }
          plotbar = c(plotbar,list(p1)) 
          
          p2 = try(dataworkmodified%>%
            group_by(Nom_Pays) %>%
            plot_ly(labels = ~Nom_Pays, values = ~annee, textposition = 'inside',
                    textinfo = 'label+percent',hoverinfo = 'text',
                    text = ~paste(input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee)) %>%
            add_pie(hole = 0.4))
          plotcam = c(plotcam,list(p2)) 
        }
        
      }else if(input$produitGlobal[1] == "Sections ( NSH2 )" & input$produitGlobal[2] == "Chapitres ( NSH4 )" & is.na(input$produitGlobal[3])){
        
        
        for(i in 1:length(input$produitSpecNSH44)){
          NSH_graph = str_split(input$produitSpecNSH44[i],"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
          
          k = which(datawork$NSH4 == NSH_graph)
          dataworkmodified = datawork[k,]
          
          dataworkmodified = dataworkmodified[c(2,which(colnames(dataworkmodified)== paste0("XX",input$selectAnnee)))]
          
          colnames(dataworkmodified) = c("Nom_Pays","annee")
          dataworkmodified = dataworkmodified[which(dataworkmodified$annee>0),]
          dataworkmodified = dataworkmodified[order(-dataworkmodified$annee),]
          
          dataworkmodified$Nom_Pays <- factor(dataworkmodified$Nom_Pays, levels = unique(dataworkmodified$Nom_Pays)[order(dataworkmodified$annee, decreasing = TRUE)])
          
          dataworkmodified$annee = round(dataworkmodified$annee/1000,digits = 3)
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Tonne")
          }else{
            txtTitle = paste("en Millions de Dinars")
          }
          
          if(nrow(dataworkmodified)>10){  
            p1 = try(plot_ly(dataworkmodified[1:10,], x = ~Nom_Pays,
                         y = ~annee,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle)))
          }else{
            p1 = try(plot_ly(dataworkmodified, x = ~annee,
                         y = ~Nom_Pays,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle)))
          }
          plotbar = c(plotbar,list(p1)) 
          
          p2 = try(dataworkmodified%>%
            group_by(Nom_Pays) %>%
            plot_ly(labels = ~Nom_Pays, values = ~annee, textposition = 'inside',
                    textinfo = 'label+percent',hoverinfo = 'text',
                    text = ~paste(input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee)) %>%
            add_pie(hole = 0.4))
          plotcam = c(plotcam,list(p2)) 
        }
        
      }else if(input$produitGlobal[1] == "Sections ( NSH2 )" & input$produitGlobal[2] == "Produits ( NSH6 )" & is.na(input$produitGlobal[3])){
        
        
        for(i in 1:length(input$produitSpecNSH626)){
          NSH_graph = str_split(input$produitSpecNSH626[i],"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          
          k = which(datawork$NSH6 == NSH_graph)
          dataworkmodified = datawork[k,]
          
          dataworkmodified = dataworkmodified[c(2,which(colnames(dataworkmodified)== paste0("XX",input$selectAnnee)))]
          
          colnames(dataworkmodified) = c("Nom_Pays","annee")
          dataworkmodified = dataworkmodified[which(dataworkmodified$annee>0),]
          dataworkmodified = dataworkmodified[order(-dataworkmodified$annee),]
          
          dataworkmodified$Nom_Pays <- factor(dataworkmodified$Nom_Pays, levels = unique(dataworkmodified$Nom_Pays)[order(dataworkmodified$annee, decreasing = TRUE)])
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
          
          if(nrow(dataworkmodified)>10){  
            p1 = try(plot_ly(dataworkmodified[1:10,], x = ~Nom_Pays,
                         y = ~annee,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle)))
          }else{
            p1 = try(plot_ly(dataworkmodified, x = ~annee,
                         y = ~Nom_Pays,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle)))
          }
          plotbar = c(plotbar,list(p1)) 
          
          p2 = try(dataworkmodified%>%
            group_by(Nom_Pays) %>%
            plot_ly(labels = ~Nom_Pays, values = ~annee, textposition = 'inside',
                    textinfo = 'label+percent',hoverinfo = 'text',
                    text = ~paste(input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee)) %>%
            add_pie(hole = 0.4))
          plotcam = c(plotcam,list(p2)) 
        }
        
      }else if(input$produitGlobal[1] == "Chapitres ( NSH4 )" & input$produitGlobal[2] == "Produits ( NSH6 )" & is.na(input$produitGlobal[3])){
        
        
        for(i in 1:length(input$produitSpecNSH646)){
          NSH_graph = str_split(input$produitSpecNSH646[i],"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          
          k = which(datawork$NSH6 == NSH_graph)
          dataworkmodified = datawork[k,]
          
          dataworkmodified = dataworkmodified[c(2,which(colnames(dataworkmodified)== paste0("XX",input$selectAnnee)))]
          
          colnames(dataworkmodified) = c("Nom_Pays","annee")
          dataworkmodified = dataworkmodified[which(dataworkmodified$annee>0),]
          dataworkmodified = dataworkmodified[order(-dataworkmodified$annee),]
          
          dataworkmodified$Nom_Pays <- factor(dataworkmodified$Nom_Pays, levels = unique(dataworkmodified$Nom_Pays)[order(dataworkmodified$annee, decreasing = TRUE)])
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
          
          if(nrow(dataworkmodified)>10){  
            p1 = plot_ly(dataworkmodified[1:10,], x = ~Nom_Pays,
                         y = ~annee,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle))
          }else{
            p1 = try(plot_ly(dataworkmodified, x = ~annee,
                         y = ~Nom_Pays,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle)))
          }
          plotbar = c(plotbar,list(p1)) 
          
          p2 = try(dataworkmodified%>%
            group_by(Nom_Pays) %>%
            plot_ly(labels = ~Nom_Pays, values = ~annee, textposition = 'inside',
                    textinfo = 'label+percent',hoverinfo = 'text',
                    text = ~paste(input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee)) %>%
            add_pie(hole = 0.4))
          plotcam = c(plotcam,list(p2)) 
        }
        
      }else if(input$produitGlobal[1] == "Sections ( NSH2 )" & input$produitGlobal[2] == "Chapitres ( NSH4 )" & input$produitGlobal[3] == "Produits ( NSH6 )"){
        
        
        for(i in 1:length(input$produitSpecNSH666)){
          NSH_graph = str_split(input$produitSpecNSH666[i],"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          
          k = which(datawork$NSH6 == NSH_graph)
          dataworkmodified = datawork[k,]
          
          dataworkmodified = dataworkmodified[c(2,which(colnames(dataworkmodified)== paste0("XX",input$selectAnnee)))]
          
          colnames(dataworkmodified) = c("Nom_Pays","annee")
          dataworkmodified = dataworkmodified[which(dataworkmodified$annee>0),]
          dataworkmodified = dataworkmodified[order(-dataworkmodified$annee),]
          
          dataworkmodified$Nom_Pays <- factor(dataworkmodified$Nom_Pays, levels = unique(dataworkmodified$Nom_Pays)[order(dataworkmodified$annee, decreasing = TRUE)])
          
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("en Kg")
          }else{
            txtTitle = paste("en Dinars")
          }
          
          if(nrow(dataworkmodified)>10){  
            p1 = try(plot_ly(dataworkmodified[1:10,], x = ~Nom_Pays,
                         y = ~annee,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle)))
          }else{
            p1 = try(plot_ly(dataworkmodified, x = ~annee,
                         y = ~Nom_Pays,
                         split = ~Nom_Pays,
                         type = 'bar',hoverinfo = 'text',
                         text = ~paste("Pays :",Nom_Pays,
                                       "<br>",input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee,"<br>"))%>%
              layout(xaxis = ax, yaxis = ax,title = paste(NSH_graph,"-",txtTitle)))
          }
          plotbar = c(plotbar,list(p1)) 
          
          p2 = try(dataworkmodified%>%
            group_by(Nom_Pays) %>%
            plot_ly(labels = ~Nom_Pays, values = ~annee, textposition = 'inside',
                    textinfo = 'label+percent',hoverinfo = 'text',
                    text = ~paste(input$quantValGeneral,input$importExportGeneral,input$selectAnnee,":", annee)) %>%
            add_pie(hole = 0.4))
          plotcam = c(plotcam,list(p2)) 
        }
        
      }
    }
    
  }
    output$ProduitPlotBar = renderUI({
      dd = try(dataworkmodified)
      if(class(dd)=="try-error" ){
        sendSweetAlert(
          session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
            lapply(htmltools::HTML), type = "error",html = T
        ) 
      }else if(nrow(dataworkmodified)<1){
        return()
      }else{
      p = try(
        
        isolate(
          fluidPage(lapply(plotbar,function(x){
            tags$div(renderPlotly(try(x)
            ))
            
          }),hr()
          )
        ))
      
isolate({    
  if(input$pdtRadio == "Tous"){
        p
      }else{
      if(class(p)!="try-error" & class(NSH_graph) !="try-error"){
        p
      }else{
        sendSweetAlert(
          session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
            lapply(htmltools::HTML), type = "error",html = T
        )  
        return()
      }
      }
})
}
    })
    
    output$ProduitPlotcam = renderUI({
      
      dd = try(dataworkmodified)
      if(class(dd)=="try-error" ){
        shinyjs::hide("jsAnneePdt")
        
      }else if(nrow(dataworkmodified)<1){
        
        shinyjs::show("jsAnneePdt")
        
        return()
        }else{
      p = try(
        isolate(
          fluidPage(lapply(plotcam,function(x){
            tags$div(renderPlotly(try(x)
            ))
            
          }),hr()
          )
        ))
      isolate({   
      if(input$pdtRadio == "Tous"){
        shinyjs::show("jsAnneePdt")
        p
      }else{
      if(class(p)!="try-error"& class(NSH_graph) !="try-error"){
        shinyjs::show("jsAnneePdt")
        
        p
      }else{
        shinyjs::hide("jsAnneePdt")
        
        sendSweetAlert(
          session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
            lapply(htmltools::HTML), type = "error",html = T
        )  
        return()
      }
      }
      })
      }
    })
    
    
    shinyjs::hide("jsbtnprod")
    shinyjs::enable("btnprod")
    
    })
  ############################################################
  ########################Treemap########################
  ############################################################
  
  
  observeEvent(input$selectAnneeTMap,{
    if(!is.null(input$parNSH) & input$tmapsRadio != "Tous"){
      if(input$btntmap != 0){
        click("btntmap")
      }
    }else if( input$tmapsRadio == "Tous"){
      if(input$btntmap != 0){
        click("btntmap")
      }
    }
  })
  
  observeEvent(input$btntmap,{
    
    shinyjs::show("jsbtntmap")
    shinyjs::disable("btntmap")
    
    output$txtAnneeTMap = renderText({
      paste("<b>Année :<font color='#B33'><b>",input$selectAnneeTMap,"</font>")
    })
    b_variable = data.frame(b_variable())
    if(input$tmapsRadio == "Tous"){
      
      for(i in 9:27){
        colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
      }
      dataworkNSH4 = matrix(0,0,ncol(b_variable)+19)
      dataworkNSH4 = data.frame(dataworkNSH4)
      
      annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_en_",input$selectAnneeTMap) 
      
            statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                               sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                               sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                               sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                               sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                               from b_variable group by Nom_Pays")
            
            d = sqldf(statement)
            d = try(d[-which(d[annee] < 10000000),])
            for(i in 32:length(d)){
              d[,i] = round(d[,i]/1000000,digits = 3)
            }
            
            colnames(dataworkNSH4) = colnames(d)
            if(class(d)!="try-error"){
              dataworkNSH4 = data.frame(d)
            }
     
      dataworkNSH4final = try(dataworkNSH4)
    }
    else{
    
    if(!is.null(input$parNSH)){   
      if(input$parNSH[1] == 'Sections ( NSH2 )' & is.na(input$parNSH[2])){
        for(i in 9:27){
          colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
        }
        dataworkNSH2 = matrix(0,0,ncol(b_variable)+19)
        dataworkNSH2 = data.frame(dataworkNSH2)
        annee = paste0("X",input$selectAnneeTMap) 
        annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeTMap) 
        
        for(i in 1:length(input$produitSpecNSH2Tm)){
          
          NSH4 = str_split(input$produitSpecNSH2Tm[i],"")
          NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2]))
          if(class(NSH4)!="try-error"){
            statement = paste0("select * from b_variable where NSH2 ='",NSH4,"'")
            d = try(sqldf(statement))
            d = try(d[-which(d[annee] < 10000),])
          
            
            if(class(d)!="try-error"){
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
              d = try(sqldf(statement))
              colnames(dataworkNSH2) = colnames(d)
              for(i in 32:length(d)){
                d[,i] = round(d[,i]/1000000,digits = 3)
              }
              dataworkNSH2 = rbind(dataworkNSH2,d)
            }
          }
        }
        dataworkNSH2final = try(dataworkNSH2)
        
      }
      else if(input$parNSH[1] == 'Chapitres ( NSH4 )' & is.na(input$parNSH[2])){
        
        for(i in 9:27){
          colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
        }
        dataworkNSH4 = matrix(0,0,ncol(b_variable)+19)
        dataworkNSH4 = data.frame(dataworkNSH4)
        
        annee = paste0("X",input$selectAnneeTMap) 
        annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeTMap) 
        
        for(i in 1:length(input$produitSpecNSH4Tm)){
          
          NSH4 = str_split(input$produitSpecNSH4Tm[i],"")
          NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2],NSH4[[1]][3],NSH4[[1]][4]))
          if(class(NSH4)!="try-error"){
            statement = paste0("select * from b_variable where NSH4 ='",NSH4,"'")
            d = sqldf(statement)
            d = try(d[-which(d[annee] < 100),])
           
            if(class(d)!="try-error"){
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
              
              d = sqldf(statement)
              for(i in 32:length(d)){
                d[,i] = round(d[,i]/1000,digits = 3)
              }
              colnames(dataworkNSH4) = colnames(d)
              if(class(d)!="try-error"){
                dataworkNSH4 = rbind(dataworkNSH4,d)
              }
            }
          }
        }
        
        dataworkNSH4final = try(dataworkNSH4)
        
      }
      else if(input$parNSH[1] == 'Produits ( NSH6 )' & is.na(input$parNSH[2])){
        
        for(i in 9:27){
          colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
        }
        dataworkNSH4 = matrix(0,0,ncol(b_variable)+19)
        dataworkNSH4 = data.frame(dataworkNSH4)
        
        annee = paste0("X",input$selectAnneeTMap) 
        annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeTMap) 
        
        for(i in 1:length(input$produitSpecNSH6Tm)){
          
          NSH4 = str_split(input$produitSpecNSH6Tm[i],"")
          NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2],NSH4[[1]][3],NSH4[[1]][4],NSH4[[1]][5],NSH4[[1]][6]))
          if(class(NSH4)!="try-error"){
            statement = paste0("select * from b_variable where NSH6 ='",NSH4,"'")
            d = sqldf(statement)
            d = try(d[-which(d[annee] < 100),])
            if(class(d)!="try-error"){
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
              
              d = sqldf(statement)
              colnames(dataworkNSH4) = colnames(d)
              if(class(d)!="try-error"){
                dataworkNSH4 = rbind(dataworkNSH4,d)
              }
            }
          }
        }
        
        dataworkNSH4final = try(dataworkNSH4)
        
      }
      else if(input$parNSH[1] == 'Sections ( NSH2 )' & input$parNSH[2] == 'Chapitres ( NSH4 )' & is.na(input$parNSH[3])){
        
        for(i in 9:27){
          colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
        }
        dataworkNSH4 = matrix(0,0,ncol(b_variable)+19)
        dataworkNSH4 = data.frame(dataworkNSH4)
        annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeTMap) 
        for(i in 1:length(input$produitSpecNSH44Tm)){
          
          NSH4 = str_split(input$produitSpecNSH44Tm[i],"")
          NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2],NSH4[[1]][3],NSH4[[1]][4]))
          if(class(NSH4)!="try"){
            statement = paste0("select * from b_variable where NSH4 ='",NSH4,"'")
            d = try(sqldf(statement))
            d = try(d[-which(d[annee] < 100),])
            if(class(d)!="try-error"){    
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
              d = try(sqldf(statement))
              for(i in 32:length(d)){
                d[,i] = round(d[,i]/1000,digits = 3)
              }
              
              colnames(dataworkNSH4) = colnames(d)
              
              if(class(d)!="try-error"){
                dataworkNSH4 = rbind(dataworkNSH4,d)
              }
            }
          }
        }
        
        dataworkNSH4final = try(dataworkNSH4)
      }
      else if(input$parNSH[1] == 'Sections ( NSH2 )' & input$parNSH[2] == 'Produits ( NSH6 )' & is.na(input$parNSH[3])){
        
        for(i in 9:27){
          colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
        }
        dataworkNSH4 = matrix(0,0,ncol(b_variable)+19)
        dataworkNSH4 = data.frame(dataworkNSH4)
        annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeTMap) 
        for(i in 1:length(input$produitSpecNSH626Tm)){
          
          NSH4 = str_split(input$produitSpecNSH626Tm[i],"")
          NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2],NSH4[[1]][3],NSH4[[1]][4],NSH4[[1]][5],NSH4[[1]][6]))
          if(class(NSH4)!="try"){
            statement = paste0("select * from b_variable where NSH6 ='",NSH4,"'")
            d = try(sqldf(statement))
            d = try(d[-which(d[annee] < 100),])
            
            if(class(d)!="try-error"){    
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
              d = try(sqldf(statement))
              colnames(dataworkNSH4) = colnames(d)
              
              if(class(d)!="try-error"){
                dataworkNSH4 = rbind(dataworkNSH4,d)
              }
            }
          }
        }
        
        dataworkNSH4final = try(dataworkNSH4)
      }
      else if(input$parNSH[1] == 'Chapitres ( NSH4 )' & input$parNSH[2] == 'Produits ( NSH6 )' & is.na(input$parNSH[3])){
        
        for(i in 9:27){
          colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
        }
        dataworkNSH4 = matrix(0,0,ncol(b_variable)+19)
        dataworkNSH4 = data.frame(dataworkNSH4)
        annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeTMap) 
        for(i in 1:length(input$produitSpecNSH646Tm)){
          
          NSH4 = str_split(input$produitSpecNSH646Tm[i],"")
          NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2],NSH4[[1]][3],NSH4[[1]][4],NSH4[[1]][5],NSH4[[1]][6]))
          if(class(NSH4)!="try"){
            statement = paste0("select * from b_variable where NSH6 ='",NSH4,"'")
            d = try(sqldf(statement))
            d = try(d[-which(d[annee] < 100),])
            
            if(class(d)!="try-error"){    
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
              d = try(sqldf(statement))
              colnames(dataworkNSH4) = colnames(d)
              
              if(class(d)!="try-error"){
                dataworkNSH4 = rbind(dataworkNSH4,d)
              }
            }
          }
        }
        
        dataworkNSH4final = try(dataworkNSH4)
      }
      else if(input$parNSH[1] == 'Sections ( NSH2 )' & input$parNSH[2] == 'Chapitres ( NSH4 )' & input$parNSH[3] == 'Produits ( NSH6 )'){
        
        for(i in 9:27){
          colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
        }
        dataworkNSH4 = matrix(0,0,ncol(b_variable)+19)
        dataworkNSH4 = data.frame(dataworkNSH4)
        annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeTMap) 
        for(i in 1:length(input$produitSpecNSH666Tm)){
          
          NSH4 = str_split(input$produitSpecNSH666Tm[i],"")
          NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2],NSH4[[1]][3],NSH4[[1]][4],NSH4[[1]][5],NSH4[[1]][6]))
          if(class(NSH4)!="try"){
            statement = paste0("select * from b_variable where NSH6 ='",NSH4,"'")
            d = try(sqldf(statement))
            d = try(d[-which(d[annee] < 100),])
            
            if(class(d)!="try-error"){    
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
              d = try(sqldf(statement))
              colnames(dataworkNSH4) = colnames(d)
              
              if(class(d)!="try-error"){
                dataworkNSH4 = rbind(dataworkNSH4,d)
              }
            }
          }
        }
        
        dataworkNSH4final = try(dataworkNSH4)
      }
  }
    }
    
    
    output$uniteTmap = renderText({
      dd = try(NSH4)
      if(input$tmapsRadio == "Tous"){
        if(input$quantValGeneral == "Quantite"){
          txtTitle = paste("<b><h3>",input$quantValGeneral,"en milles Tonne")
        }else{
          txtTitle = paste("<b><h3>",input$quantValGeneral,"en Milliards de Dinars")
        }
      }else if(class(dd) == "try-error"){
        txtTitle = ""
      }
      else{
      
      if((str_detect(NSH4,"Error")==F)){
      
 
        
        if(!is.null(input$parNSH)){   
          if(input$parNSH[1] == 'Sections ( NSH2 )' & is.na(input$parNSH[2])){
            if(input$quantValGeneral == "Quantite"){
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en milles Tonne")
            }else{
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en Milliards de Dinars")
            }
          }else if(input$parNSH[1] == 'Chapitres ( NSH4 )' & is.na(input$parNSH[2])){
            if(input$quantValGeneral == "Quantite"){
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en Tonne")
            }else{
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en Millions de Dinars")
            }
          }else if(input$parNSH[1] == 'Produits ( NSH6 )' & is.na(input$parNSH[2])){
            if(input$quantValGeneral == "Quantite"){
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en Kg")
            }else{
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en Dinars")
            }
          }else if(input$parNSH[1] == 'Sections ( NSH2 )' & input$parNSH[2] == 'Chapitres ( NSH4 )' & is.na(input$parNSH[3])){
            if(input$quantValGeneral == "Quantite"){
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en Tonne")
            }else{
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en Millions de Dinars")
            }
          }else if(input$parNSH[1] == 'Sections ( NSH2 )' & input$parNSH[2] == 'Produits ( NSH6 )' & is.na(input$parNSH[3])){
            if(input$quantValGeneral == "Quantite"){
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en Kg")
            }else{
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en Dinars")
            }
          }else if(input$parNSH[1] == 'Chapitres ( NSH4 )' & input$parNSH[2] == 'Produits ( NSH6 )' & is.na(input$parNSH[3])){
            if(input$quantValGeneral == "Quantite"){
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en Kg")
            }else{
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en Dinars")
            }
          }else if(input$parNSH[1] == 'Sections ( NSH2 )' & input$parNSH[2] == 'Chapitres ( NSH4 )' & input$parNSH[3] == 'Produits ( NSH6 )'){
            if(input$quantValGeneral == "Quantite"){
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en Kg")
            }else{
              txtTitle = paste("<b><h3>",input$quantValGeneral,"en Dinars")
            } 
          }
        }
      }else{
        txtTitle = ""
      } 
      txt = try(txtTitle)
      if(class(txt)=="try-error"){
        return()
      }else{
        txt
      }  
      } 
      })
    
    output$tmapPlot = renderD3partitionR({
      try( isolate({ 
        annee = paste0("",input$quantValGeneral,"_",input$importExportGeneral,"_","en_",input$selectAnneeTMap) 
        
        if(input$tmapsRadio == "Tous"){
          treemap = try(D3partitionR()%>%
                          add_data(dataworkNSH4final, count = annee,steps = c("Nom_Pays"),tooltip=c("name",annee)) %>%
                          set_labels_parameters(cut_off=1)%>%
                          set_legend_parameters(zoom_subset=T)%>%
                          set_chart_type(input$chart_type_cal) )
            
        }else{
          steps = paste(input$parNSH)
          steps = str_replace_all(steps,"Sections \\( ","")
          steps = str_replace_all(steps,"Chapitres \\( ","")
          steps = str_replace_all(steps,"Produits \\( ","")
          steps = str_replace_all(steps," \\)","")
        if(input$parNSH[1] == 'Sections ( NSH2 )' & is.na(input$parNSH[2])){
          
          treemap = try(D3partitionR()%>%
                          add_data(dataworkNSH2final, count = annee,steps = c(steps,'Nom_Pays'),tooltip=c("name",annee)) %>%
                          set_labels_parameters(cut_off=1)%>%
                          set_legend_parameters(zoom_subset=T)%>%
                          set_chart_type(input$chart_type_cal))
          
        }else {
          
         
          
          treemap = try(D3partitionR()%>%
                          add_data(dataworkNSH4final, count = annee,steps = c(steps,'Nom_Pays'),tooltip=c("name",annee)) %>%
                          set_labels_parameters(cut_off=1)%>%
                          set_legend_parameters(zoom_subset=T)%>%
                          set_chart_type(input$chart_type_cal) )
        }
      }
        }))
      
      
      if(input$tmapsRadio == "Tous"){
        shinyjs::show("jsAnneeTMap")
        
        isolate(plot(treemap))
      }else{
      
      dd = try(NSH4)   
      if(class(dd)=="try-error"){
        shinyjs::hide("jsAnneeTMap")
        
        sendSweetAlert(
          session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
            lapply(htmltools::HTML), type = "error",html = T
        ) 
      }else if(class(treemap)=="D3partitionR"& (str_detect(NSH4,"Error")==F)){
        shinyjs::show("jsAnneeTMap")
        
        isolate(plot(treemap))
      }else{
        shinyjs::hide("jsAnneeTMap")
        
        sendSweetAlert(
          session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
            lapply(htmltools::HTML), type = "error",html = T
        )  
      }
      }
    })
    
    shinyjs::hide("jsbtntmap")
    shinyjs::enable("btntmap")
    
    })
  
  ############################################################
  ########################Maps########################
  ############################################################
  
  observeEvent(input$selectAnneeMap,{
    if(!is.null(input$parNSHmap) & input$mapsRadio !="Tous"){
      if(input$btnmap != 0){
        shinyjs::click("btnmap")
        
      }
    }else if(input$mapsRadio == "Tous"){
      if(input$btnmap != 0){
        click("btnmap")
        
      }
    }
  })
  
  observeEvent(input$btnmap,{
    
    output$txtAnneeMap = renderText({
      paste("<b>Année :<font color='#B33'><b>",input$selectAnneeMap,"</font>")
    })
    
    shinyjs::show("jsbtnmap")
    shinyjs::disable("btnmap")
    
    b_variable = data.frame(b_variable())
    #if(input$selectAnneeMap == 1999){
    #click("btnmap")
    #}
    if(input$mapsRadio == "Tous"){
      
      for(i in 9:27){
        colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
      }
      dataworkNSH2map = matrix(0,0,ncol(b_variable)+19)
      dataworkNSH2map = data.frame(dataworkNSH2map)
      
      annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeMap) 
      
      statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                         sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                         sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                         sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                         sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                         from b_variable group by Nom_Pays")
      d = try(sqldf(statement))
      colnames(dataworkNSH2map) = colnames(d)
      dataworkNSH2map = data.frame(d)
      
      for(i in 32:length(dataworkNSH2map)){
        dataworkNSH2map[,i] = round(dataworkNSH2map[,i]/1000000,digits = 3)
      }
      dataworkNSH2finalmap = try(dataworkNSH2map)
      
    }
    else{
      
      if(!is.null(input$parNSHmap)){  
        if(input$parNSHmap[1] == 'Sections ( NSH2 )' & is.na(input$parNSHmap[2])){
          for(i in 9:27){
            colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
          }
          dataworkNSH2map = matrix(0,0,ncol(b_variable)+19)
          dataworkNSH2map = data.frame(dataworkNSH2map)
          
          annee = paste0("X",input$selectAnneeMap) 
          annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeMap) 
          
          for(i in 1:length(input$produitSpecNSH2map)){
            
            NSH4 = str_split(input$produitSpecNSH2map[i],"")
            NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2]))
            if(class(NSH4)!="try-error"){
              statement = paste0("select * from b_variable where NSH2 ='",NSH4,"'")
              d = try(sqldf(statement))
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
              d = try(sqldf(statement))
              colnames(dataworkNSH2map) = colnames(d)
              dataworkNSH2map = rbind(dataworkNSH2map,d)
            }
          }
          for(i in 32:length(dataworkNSH2map)){
            dataworkNSH2map[,i] = round(dataworkNSH2map[,i]/1000000,digits = 3)
          }
          dataworkNSH2finalmap = try(dataworkNSH2map)
        }else if(input$parNSHmap[1] == 'Chapitres ( NSH4 )' & is.na(input$parNSHmap[2])){
          
          for(i in 9:27){
            colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
          }
          dataworkNSH4map = matrix(0,0,ncol(b_variable)+19)
          dataworkNSH4map = data.frame(dataworkNSH4map)
          annee = paste0("X",input$selectAnneeMap) 
          annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeMap) 
          
          for(i in 1:length(input$produitSpecNSH4map)){
            
            NSH4 = str_split(input$produitSpecNSH4map[i],"")
            NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2],NSH4[[1]][3],NSH4[[1]][4]))
            if(class(NSH4)!="try-error"){
              statement = paste0("select * from b_variable where NSH4 ='",NSH4,"'")
              d = sqldf(statement)
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
              
              d = try(sqldf(statement))
              colnames(dataworkNSH4map) = colnames(d)
              
              if(class(d)!="try-error"){
                dataworkNSH4map = rbind(dataworkNSH4map,d)
              }
            }
          }
          
          for(i in 32:length(dataworkNSH4map)){
            dataworkNSH4map[,i] = round(dataworkNSH4map[,i]/1000,digits = 3)
          }
          dataworkNSH4finalmap = try(dataworkNSH4map)
          
        }else if(input$parNSHmap[1] == 'Produits ( NSH6 )' & is.na(input$parNSHmap[2])){
          
          for(i in 9:27){
            colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
          }
          dataworkNSH4map = matrix(0,0,ncol(b_variable)+19)
          dataworkNSH4map = data.frame(dataworkNSH4map)
          annee = paste0("X",input$selectAnneeMap) 
          annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeMap) 
          
          for(i in 1:length(input$produitSpecNSH6map)){
            
            NSH4 = str_split(input$produitSpecNSH4map[i],"")
            NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2],NSH4[[1]][3],NSH4[[1]][4]),NSH4[[1]][5],NSH4[[1]][6])
            if(class(NSH4)!="try-error"){
              statement = paste0("select * from b_variable where NSH6 ='",NSH4,"'")
              d = sqldf(statement)
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
              
              d = try(sqldf(statement))
              colnames(dataworkNSH4map) = colnames(d)
              
              if(class(d)!="try-error"){
                dataworkNSH4map = rbind(dataworkNSH4map,d)
              }
            }
          }
          
          
          dataworkNSH4finalmap = try(dataworkNSH4map)
          
        }else if(input$parNSHmap[1] == 'Sections ( NSH2 )' & input$parNSHmap[2] == 'Chapitres ( NSH4 )' & is.na(input$parNSHmap[3])){
          
          for(i in 9:27){
            colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
          }
          dataworkNSH4map = matrix(0,0,ncol(b_variable)+19)
          dataworkNSH4map = data.frame(dataworkNSH4map)
          annee = paste0("X",input$selectAnneeMap) 
          annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeMap) 
          
          for(i in 1:length(input$produitSpecNSH44map)){
            
            NSH4 = str_split(input$produitSpecNSH44map[i],"")
            NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2],NSH4[[1]][3],NSH4[[1]][4]))
            if(class(NSH4)!="try-error"){
              statement = paste0("select * from b_variable where NSH4 ='",NSH4,"'")
              d = try(sqldf(statement))
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
              d = try(sqldf(statement))
              colnames(dataworkNSH4map) = colnames(d)
              if(class(d)!="try-error"){
                dataworkNSH4map = rbind(dataworkNSH4map,d)
              }
            }
          }
          
          for(i in 32:length(dataworkNSH4map)){
            dataworkNSH4map[,i] = round(dataworkNSH4map[,i]/1000,digits = 3)
          }
          dataworkNSH4finalmap = try(dataworkNSH4map)
        }else if(input$parNSHmap[1] == 'Sections ( NSH2 )' & input$parNSHmap[2] == 'Produits ( NSH6 )' & is.na(input$parNSHmap[3])){
          
          for(i in 9:27){
            colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
          }
          dataworkNSH4map = matrix(0,0,ncol(b_variable)+19)
          dataworkNSH4map = data.frame(dataworkNSH4map)
          annee = paste0("X",input$selectAnneeMap) 
          annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeMap) 
          
          for(i in 1:length(input$produitSpecNSH626map)){
            
            NSH4 = str_split(input$produitSpecNSH626map[i],"")
            NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2],NSH4[[1]][3],NSH4[[1]][4],NSH4[[1]][5],NSH4[[1]][6]))
            if(class(NSH4)!="try-error"){
              statement = paste0("select * from b_variable where NSH6 ='",NSH4,"'")
              d = try(sqldf(statement))
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
              d = try(sqldf(statement))
              colnames(dataworkNSH4map) = colnames(d)
              if(class(d)!="try-error"){
                dataworkNSH4map = rbind(dataworkNSH4map,d)
              }
            }
          }
          
          
          dataworkNSH4finalmap = try(dataworkNSH4map)
          
        }else if(input$parNSHmap[1] == 'Chapitres ( NSH4 )' & input$parNSHmap[2] == 'Produits ( NSH6 )' & is.na(input$parNSHmap[3])){
          
          for(i in 9:27){
            colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
          }
          dataworkNSH4map = matrix(0,0,ncol(b_variable)+19)
          dataworkNSH4map = data.frame(dataworkNSH4map)
          annee = paste0("X",input$selectAnneeMap) 
          annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeMap) 
          
          for(i in 1:length(input$produitSpecNSH646map)){
            
            NSH4 = str_split(input$produitSpecNSH646map[i],"")
            NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2],NSH4[[1]][3],NSH4[[1]][4],NSH4[[1]][5],NSH4[[1]][6]))
            if(class(NSH4)!="try-error"){
              statement = paste0("select * from b_variable where NSH6 ='",NSH4,"'")
              d = try(sqldf(statement))
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
              d = try(sqldf(statement))
              colnames(dataworkNSH4map) = colnames(d)
              if(class(d)!="try-error"){
                dataworkNSH4map = rbind(dataworkNSH4map,d)
              }
            }
          }
          
          
          dataworkNSH4finalmap = try(dataworkNSH4map)
          
        }else if(input$parNSHmap[1] == 'Sections ( NSH2 )' & input$parNSHmap[2] == 'Chapitres ( NSH4 )' & input$parNSHmap[3] == 'Produits ( NSH6 )'){
          
          for(i in 9:27){
            colnames(b_variable)[i]=paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",1990+i)
          }
          dataworkNSH4map = matrix(0,0,ncol(b_variable)+19)
          dataworkNSH4map = data.frame(dataworkNSH4map)
          annee = paste0("X",input$selectAnneeMap) 
          annee = paste0(input$quantValGeneral,"_",input$importExportGeneral,"_",input$selectAnneeMap) 
          
          for(i in 1:length(input$produitSpecNSH666map)){
            
            NSH4 = str_split(input$produitSpecNSH666map[i],"")
            NSH4 = try(paste0(NSH4[[1]][1],NSH4[[1]][2],NSH4[[1]][3],NSH4[[1]][4],NSH4[[1]][5],NSH4[[1]][6]))
            if(class(NSH4)!="try-error"){
              statement = paste0("select * from b_variable where NSH6 ='",NSH4,"'")
              d = try(sqldf(statement))
              bb = d
              statement = paste0("select * , sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","1999) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_1999,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2000) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2000,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2001) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2001,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2002) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2002,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2003) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2003,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2004) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2004,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2005) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2005,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2006) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2006,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2007) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2007,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2008) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2008,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2009) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2009,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2010) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2010,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2011) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2011,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2012) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2012,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2013) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2013,
                                 sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2014) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2014,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2015) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2015,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2016) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2016,sum(",input$quantValGeneral,"_",input$importExportGeneral,"_","2017) ",input$quantValGeneral,"_",input$importExportGeneral,"_","en_2017 
                                 from bb group by Nom_Pays")
          d = try(sqldf(statement))
          colnames(dataworkNSH4map) = colnames(d)
          if(class(d)!="try-error"){
            dataworkNSH4map = rbind(dataworkNSH4map,d)
          }
        }
      }
      
      
      dataworkNSH4finalmap = try(dataworkNSH4map)
      
    }
    }
  
  }
    
    

    
    output$mapPlot = renderLeaflet({
      isolate({try({
        annee = paste0("",input$quantValGeneral,"_",input$importExportGeneral,"_","en_",input$selectAnneeMap) 
        
         if(input$mapsRadio == "Tous"){
           if(input$mapRadio == "Icones"){
          dataworkNSH2finalmap$textmap = 0
          for(i in 1:nrow(dataworkNSH2finalmap)){
            k = which(dataworkNSH2finalmap$Code_Pays == dataworkNSH2finalmap$Code_Pays[i])
            cc = ""
            for(i in 1:length(k)){
              
              cc  = paste0(cc,paste("<font color='#006e87'>",input$quantValGeneral,input$importExportGeneral,input$selectAnneeMap,"</font>=",dataworkNSH2finalmap[annee][[1]][k[i]],"<br/>"))
            }
            dataworkNSH2finalmap$textmap[k] = cc
            
          }
          
          map <- try(leaflet(data = dataworkNSH2finalmap,options = leafletOptions(zoomControl = TRUE)) %>%
                       addProviderTiles("CartoDB.Positron", group = "Map") %>%
                       addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
                       addProviderTiles("Esri.WorldShadedRelief", group = "Relief") %>%
                       # Marker data are from the sites data frame. We need the ~ symbols
                       # to indicate the columns of the data frame.
                       addMarkers(~longitude, ~latitude, label = ~Nom_Pays,dataworkNSH2finalmap[annee],popup = dataworkNSH2finalmap$textmap)%>%
                       addScaleBar(position = "bottomleft")%>%
                       addLayersControl(
                         baseGroups = c("Map", "Satellite", "Relief"),
                         options = layersControlOptions(collapsed = FALSE)
                       ))
           }else{
             
         
               
               
               world_spdf@data$year = 0 
               world_spdf@data$ISO3 = as.character(world_spdf@data$ISO3)
               for(i in 1:nrow(dataworkNSH2finalmap)){
                 k =which(world_spdf@data$ISO3 == dataworkNSH2finalmap$code3[i])
                 world_spdf@data$year[k] = world_spdf@data$year[k]+dataworkNSH2finalmap[,annee][i]
               }
               
               
               mybins=c(0,5,50,100,1000,3000)
               mypalette = colorBin( palette="YlOrBr", domain=world_spdf@data$year, na.color="transparent", bins=mybins)
               
               
               mytext=paste("<font color='#006e87'>Pays </font>:",domain=world_spdf@data$NAME,"<br><font color='#006e87'>",input$quantValGeneral,input$importExportGeneral,input$selectAnneeMap,"</font> :",domain=world_spdf@data$year,"<br/>") %>%
                 lapply(htmltools::HTML)
               
               map = leaflet(world_spdf) %>% 
                 addTiles()  %>% 
                 setView( lat=10, lng=0 , zoom=2) %>%
                 addPolygons( 
                   fillColor = ~mypalette(world_spdf@data$year), stroke=TRUE, fillOpacity = 0.9, color="white", weight=0.3,
                   highlight = highlightOptions( weight = 5, color = ~colorNumeric("Blues", year)(year), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE),
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
                 ) %>%
                 addLegend( pal=mypalette, values=~year, opacity=0.9, title = input$quantValGeneral, position = "bottomleft" )
               
               
             
             
             
           }
          
        }else{
        
        if(!is.null(input$parNSHmap)){   
          if(input$parNSHmap[1] == 'Sections ( NSH2 )' & is.na(input$parNSHmap[2])){
            
            dataworkNSH2finalmap$textmap = 0
            for(i in 1:nrow(dataworkNSH2finalmap)){
              k = which(dataworkNSH2finalmap$Code_Pays == dataworkNSH2finalmap$Code_Pays[i])
              cc = ""
              for(i in 1:length(k)){
                
                cc  = paste0(cc,paste("<b><font color='#006e87'>NSH2</font> =", dataworkNSH2finalmap$NSH2[k[i]]),paste("<br/><font color='#006e87'>",input$quantValGeneral,input$importExportGeneral,input$selectAnneeMap,"</font>=",dataworkNSH2finalmap[annee][[1]][k[i]],"<br/>"))
              }
              dataworkNSH2finalmap$textmap[k] = cc
              
            }
            
            map <- try(leaflet(data = dataworkNSH2finalmap,options = leafletOptions(zoomControl = TRUE)) %>%
                         addProviderTiles("CartoDB.Positron", group = "Map") %>%
                         addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
                         addProviderTiles("Esri.WorldShadedRelief", group = "Relief") %>%
                         # Marker data are from the sites data frame. We need the ~ symbols
                         # to indicate the columns of the data frame.
                         addMarkers(~longitude, ~latitude, label = ~Nom_Pays,dataworkNSH2finalmap[annee],popup = dataworkNSH2finalmap$textmap)%>%
                         addScaleBar(position = "bottomleft")%>%
                         addLayersControl(
                           baseGroups = c("Map", "Satellite", "Relief"),
                           options = layersControlOptions(collapsed = FALSE)
                         ))
            
          }else if(input$parNSHmap[1] == 'Chapitres ( NSH4 )' & is.na(input$parNSHmap[2])){
            
            dataworkNSH4finalmap$textmap = 0
            for(i in 1:nrow(dataworkNSH4finalmap)){
              k = which(dataworkNSH4finalmap$Code_Pays == dataworkNSH4finalmap$Code_Pays[i])
              cc = ""
              for(i in 1:length(k)){
                cc  = paste0(cc,paste("<b><font color='#006e87'>NSH4</font> =", dataworkNSH4finalmap$NSH4[k[i]]),paste("<br/><font color='#006e87'>",input$quantValGeneral,input$importExportGeneral,input$selectAnneeMap,"</font>=",dataworkNSH4finalmap[annee][[1]][k[i]],"<br/>"))
              }
              dataworkNSH4finalmap$textmap[k] = cc
              
            }
            
            map <- try(leaflet(data = dataworkNSH4finalmap,options = leafletOptions(zoomControl = TRUE)) %>%
                         addProviderTiles("CartoDB.Positron", group = "Map") %>%
                         addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
                         addProviderTiles("Esri.WorldShadedRelief", group = "Relief") %>%
                         
                         # Marker data are from the sites data frame. We need the ~ symbols
                         # to indicate the columns of the data frame.
                         addMarkers(~longitude, ~latitude, label = ~Nom_Pays,dataworkNSH4finalmap[annee],popup = dataworkNSH4finalmap$textmap )%>%
                         addScaleBar(position = "bottomleft") %>%
                         addLayersControl(
                           baseGroups = c("Map", "Satellite", "Relief"),
                           options = layersControlOptions(collapsed = FALSE)
                         )
            )
          }else if(input$parNSHmap[1] == 'Produits ( NSH6 )' & is.na(input$parNSHmap[2])){
            
            dataworkNSH4finalmap$textmap = 0
            for(i in 1:nrow(dataworkNSH4finalmap)){
              k = which(dataworkNSH4finalmap$Code_Pays == dataworkNSH4finalmap$Code_Pays[i])
              cc = ""
              for(i in 1:length(k)){
                cc  = paste0(cc,paste("<b><font color='#006e87'>NSH6</font> =", dataworkNSH4finalmap$NSH4[k[i]]),paste("<br/><font color='#006e87'>",input$quantValGeneral,input$importExportGeneral,input$selectAnneeMap,"</font>=",dataworkNSH4finalmap[annee][[1]][k[i]],"<br/>"))
              }
              dataworkNSH4finalmap$textmap[k] = cc
              
            }
            
            map <- try(leaflet(data = dataworkNSH4finalmap,options = leafletOptions(zoomControl = TRUE)) %>%
                         addProviderTiles("CartoDB.Positron", group = "Map") %>%
                         addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
                         addProviderTiles("Esri.WorldShadedRelief", group = "Relief") %>%
                         
                         # Marker data are from the sites data frame. We need the ~ symbols
                         # to indicate the columns of the data frame.
                         addMarkers(~longitude, ~latitude, label = ~Nom_Pays,dataworkNSH4finalmap[annee],popup = dataworkNSH4finalmap$textmap )%>%
                         addScaleBar(position = "bottomleft") %>%
                         addLayersControl(
                           baseGroups = c("Map", "Satellite", "Relief"),
                           options = layersControlOptions(collapsed = FALSE)
                         ))
            
          }else if(input$parNSHmap[1] == 'Sections ( NSH2 )' & input$parNSHmap[2] == 'Chapitres ( NSH4 )' & is.na(input$parNSHmap[3])){
            
            dataworkNSH4finalmap$textmap = 0
            for(i in 1:nrow(dataworkNSH4finalmap)){
              k = which(dataworkNSH4finalmap$Code_Pays == dataworkNSH4finalmap$Code_Pays[i])
              cc = ""
              for(i in 1:length(k)){
                cc  = paste0(cc,paste("<b><font color='#006e87'>NSH2</font> =", dataworkNSH4finalmap$NSH2[k[i]]),
                             paste("<br/><font color='#006e87'>NSH4</font> =", dataworkNSH4finalmap$NSH4[k[i]])
                             ,paste("<br/><font color='#006e87'>",input$quantValGeneral,input$importExportGeneral,input$selectAnneeMap,"</font>=",dataworkNSH4finalmap[annee][[1]][k[i]],"<br/>"))
              }
              dataworkNSH4finalmap$textmap[k] = cc
              
            }
            
            map <- try(leaflet(data = dataworkNSH4finalmap,options = leafletOptions(zoomControl = TRUE)) %>%
                         addProviderTiles("CartoDB.Positron", group = "Map") %>%
                         addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
                         addProviderTiles("Esri.WorldShadedRelief", group = "Relief") %>%
                         
                         # Marker data are from the sites data frame. We need the ~ symbols
                         # to indicate the columns of the data frame.
                         addMarkers(~longitude, ~latitude, label = ~Nom_Pays,dataworkNSH4finalmap[annee],popup = dataworkNSH4finalmap$textmap)%>%
                         addScaleBar(position = "bottomleft") %>%
                         addLayersControl(
                           baseGroups = c("Map", "Satellite", "Relief"),
                           options = layersControlOptions(collapsed = FALSE)
                         ))
            
          }else if(input$parNSHmap[1] == 'Sections ( NSH2 )' & input$parNSHmap[2] == 'Produits ( NSH6 )' & is.na(input$parNSHmap[3])){
            
            dataworkNSH4finalmap$textmap = 0
            for(i in 1:nrow(dataworkNSH4finalmap)){
              k = which(dataworkNSH4finalmap$Code_Pays == dataworkNSH4finalmap$Code_Pays[i])
              cc = ""
              for(i in 1:length(k)){
                
                cc  = paste0(cc,paste("<b><font color='#006e87'>NSH2</font> =", dataworkNSH4finalmap$NSH2[k[i]]),
                             paste("<br/><font color='#006e87'>NSH6</font> =", dataworkNSH4finalmap$NSH6[k[i]])
                             ,paste("<br/><font color='#006e87'>",input$quantValGeneral,input$importExportGeneral,input$selectAnneeMap,"</font>=",dataworkNSH4finalmap[annee][[1]][k[i]],"<br/>"))
              }
              dataworkNSH4finalmap$textmap[k] = cc
              
            }
            
            map <- try(leaflet(data = dataworkNSH4finalmap,options = leafletOptions(zoomControl = TRUE)) %>%
                         addProviderTiles("CartoDB.Positron", group = "Map") %>%
                         addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
                         addProviderTiles("Esri.WorldShadedRelief", group = "Relief") %>%
                         
                         # Marker data are from the sites data frame. We need the ~ symbols
                         # to indicate the columns of the data frame.
                         addMarkers(~longitude, ~latitude, label = ~Nom_Pays,dataworkNSH4finalmap[annee],popup = dataworkNSH4finalmap$textmap)%>%
                         addScaleBar(position = "bottomleft") %>%
                         addLayersControl(
                           baseGroups = c("Map", "Satellite", "Relief"),
                           options = layersControlOptions(collapsed = FALSE)
                         )
            )
          }else if( input$parNSHmap[1] == 'Chapitres ( NSH4 )' & input$parNSHmap[2] == 'Produits ( NSH6 )' & is.na(input$parNSHmap[3])){
            
            dataworkNSH4finalmap$textmap = 0
            for(i in 1:nrow(dataworkNSH4finalmap)){
              k = which(dataworkNSH4finalmap$Code_Pays == dataworkNSH4finalmap$Code_Pays[i])
              cc = ""
              for(i in 1:length(k)){
                cc  = paste0(cc,paste("<b><font color='#006e87'>NSH4</font> =", dataworkNSH4finalmap$NSH4[k[i]]),
                             paste("<br/><font color='#006e87'>NSH6</font> =", dataworkNSH4finalmap$NSH6[k[i]])
                             ,paste("<br/><font color='#006e87'>",input$quantValGeneral,input$importExportGeneral,input$selectAnneeMap,"</font>=",dataworkNSH4finalmap[annee][[1]][k[i]],"<br/>"))
              }
              dataworkNSH4finalmap$textmap[k] = cc
              
            }
            
            map <- try(leaflet(data = dataworkNSH4finalmap,options = leafletOptions(zoomControl = TRUE)) %>%
                         addProviderTiles("CartoDB.Positron", group = "Map") %>%
                         addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
                         addProviderTiles("Esri.WorldShadedRelief", group = "Relief") %>%
                         
                         # Marker data are from the sites data frame. We need the ~ symbols
                         # to indicate the columns of the data frame.
                         addMarkers(~longitude, ~latitude, label = ~Nom_Pays,dataworkNSH4finalmap[annee],popup = dataworkNSH4finalmap$textmap)%>%
                         addScaleBar(position = "bottomleft") %>%
                         addLayersControl(
                           baseGroups = c("Map", "Satellite", "Relief"),
                           options = layersControlOptions(collapsed = FALSE)
                         ))
            
          }else if(input$parNSHmap[1] == 'Sections ( NSH2 )' & input$parNSHmap[2] == 'Chapitres ( NSH4 )' & input$parNSHmap[3] == 'Produits ( NSH6 )'){
            
            dataworkNSH4finalmap$textmap = 0
            for(i in 1:nrow(dataworkNSH4finalmap)){
              k = which(dataworkNSH4finalmap$Code_Pays == dataworkNSH4finalmap$Code_Pays[i])
              cc = ""
              for(i in 1:length(k)){
                cc  = paste0(cc,paste("<b><font color='#006e87'>NSH2</font> =", dataworkNSH4finalmap$NSH2[k[i]]),
                             paste("<br/><font color='#006e87'>NSH4</font> =", dataworkNSH4finalmap$NSH4[k[i]]),
                             paste("<br/><font color='#006e87'>NSH6</font> =", dataworkNSH4finalmap$NSH6[k[i]])
                             ,paste("<br/><font color='#006e87'>",input$quantValGeneral,input$importExportGeneral,input$selectAnneeMap,"</font>=",dataworkNSH4finalmap[annee][[1]][k[i]],"<br/>"))
              }
              dataworkNSH4finalmap$textmap[k] = cc
              
            }
            
            map <- try(leaflet(data = dataworkNSH4finalmap,options = leafletOptions(zoomControl = TRUE)) %>%
                         addProviderTiles("CartoDB.Positron", group = "Map") %>%
                         addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
                         addProviderTiles("Esri.WorldShadedRelief", group = "Relief") %>%
                         
                         # Marker data are from the sites data frame. We need the ~ symbols
                         # to indicate the columns of the data frame.
                         addMarkers(~longitude, ~latitude, label = ~Nom_Pays,dataworkNSH4finalmap[annee],popup = dataworkNSH4finalmap$textmap)%>%
                         addScaleBar(position = "bottomleft") %>%
                         addLayersControl(
                           baseGroups = c("Map", "Satellite", "Relief"),
                           options = layersControlOptions(collapsed = FALSE)
                         )
            )
          }
        
          }else{
          map = try(NULL)
        }
        }
          
   
        
        
       })})
      
      
      output$uniteMap = renderText({
        dd = try(NSH4)
        if(input$tmapsRadio == "Tous"){
          if(input$quantValGeneral == "Quantite"){
            txtTitle = paste("<b><h3>",input$quantValGeneral,"en milles Tonne")
          }else{
            txtTitle = paste("<b><h3>",input$quantValGeneral,"en Milliards de Dinars")
          }
        }else if(class(dd) == "try-error"){
          txtTitle = ""
        }
        else{
        if(class(map)[1]=="leaflet" & (str_detect(NSH4,"Error")==F)){
      
        
          
          if(!is.null(input$parNSH)){   
            if(input$parNSH[1] == 'Sections ( NSH2 )' & is.na(input$parNSH[2])){
              if(input$quantValGeneral == "Quantite"){
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en milles Tonne")
              }else{
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en Milliards de Dinars")
              }
            }else if(input$parNSH[1] == 'Chapitres ( NSH4 )' & is.na(input$parNSH[2])){
              if(input$quantValGeneral == "Quantite"){
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en Tonne")
              }else{
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en Millions de Dinars")
              }
            }else if(input$parNSH[1] == 'Produits ( NSH6 )' & is.na(input$parNSH[2])){
              if(input$quantValGeneral == "Quantite"){
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en Kg")
              }else{
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en Dinars")
              }
            }else if(input$parNSH[1] == 'Sections ( NSH2 )' & input$parNSH[2] == 'Chapitres ( NSH4 )' & is.na(input$parNSH[3])){
              if(input$quantValGeneral == "Quantite"){
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en Tonne")
              }else{
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en Millions de Dinars")
              }
            }else if(input$parNSH[1] == 'Sections ( NSH2 )' & input$parNSH[2] == 'Produits ( NSH6 )' & is.na(input$parNSH[3])){
              if(input$quantValGeneral == "Quantite"){
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en Kg")
              }else{
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en Dinars")
              }
            }else if(input$parNSH[1] == 'Chapitres ( NSH4 )' & input$parNSH[2] == 'Produits ( NSH6 )' & is.na(input$parNSH[3])){
              if(input$quantValGeneral == "Quantite"){
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en Kg")
              }else{
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en Dinars")
              }
            }else if(input$parNSH[1] == 'Sections ( NSH2 )' & input$parNSH[2] == 'Chapitres ( NSH4 )' & input$parNSH[3] == 'Produits ( NSH6 )'){
              if(input$quantValGeneral == "Quantite"){
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en Kg")
              }else{
                txtTitle = paste("<b><h3>",input$quantValGeneral,"en Dinars")
              } 
            }
          }
        }else{
          txtTitle = ""
        }
        txt = try(txtTitle)
        if(class(txt)=="try-error"){
          return()
        }else{
          txt
        }
        }
      })
      if(input$mapsRadio == "Tous"){
        shinyjs::show("jsAnneeMap")
        
        invisible(print(map))
      }else{
        dd = try(NSH4)
        if(class(dd) == "try-error"){
          shinyjs::hide("jsAnneeMap")
          
          sendSweetAlert(
            session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
              lapply(htmltools::HTML), type = "error",html = T
          )   
        }else if(class(map)[1]=="leaflet" & (str_detect(NSH4,"Error")==F)){
        shinyjs::show("jsAnneeMap")
        
        invisible(print(map))
      }else{
        shinyjs::hide("jsAnneeMap")
        
        sendSweetAlert(
          session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
            lapply(htmltools::HTML), type = "error",html = T
        )   
      }
      }
    })
    
    shinyjs::hide("jsbtnmap")
    shinyjs::enable("btnmap")
    
      })
  
  output$comnom = renderText({
    if(input$userName != "" & input$identifiant==""){
      paste("<h5><b><font color='#B33'>",input$userName,"</font></b></h5>")
    }else if(input$userName == "" & input$identifiant!=""){
      paste("<h5><b><font color='#B33'>",input$identifiant,"</font></b></h5>")
    }else if(input$userName != "" & input$identifiant!=""){
      paste("<h5><b><font color='#B33'>",input$userName,"</font></b></h5>")
    }
  }) 
  
  
  output$comentdata = DT::renderDataTable({
    commentdata = read.csv("www/commentdata.txt",sep = ";",encoding = "UTF-8")
    
    colnames(commentdata) = "Anciens Commentaires"
    DT::datatable(
      commentdata,
      rownames = FALSE,escape = F,colnames = NULL,
      options = list(pageLength = 7,searching = FALSE, lengthChange = FALSE,ordering=F)
    )
    
  })
  
  observeEvent(input$combtn,{
    commentdata = read.csv("www/commentdata.txt",sep = ";",encoding = "UTF-8")
    
    shinyjs::disable("combtn")
    shinyjs::show("submit_msg_comment")
    shinyjs::hide("error_msg_comment")
    if(input$comtxt ==""){
      
      shinyjs::hide("submit_msg_comment")
      shinyjs::show("error_msg_comment")
      Sys.sleep(1)
      shinyjs::enable("combtn")
    }else{
      
      data = matrix(nrow = 1,ncol = 1)
      data = data.frame(data)
      colnames(data)=c("Anciens Commentaires")
      if(input$userName != "" & input$identifiant==""){
      data[1,1] = paste0('<b><em><font color=#B33>',input$userName,'</font></em></b>',
                         ' ( ',format(Sys.Date(),"%d.%b%Y"),' ',
                         str_c(str_split(str_split(Sys.time()," ")[[1]][2],":")[[1]][1],":",str_split(str_split(Sys.time()," ")[[1]][2],":")[[1]][2]),' ) ',
                         '<br>',input$comtxt,'</br>')
      }else if(input$userName == "" & input$identifiant!=""){
        data[1,1] = paste0('<b><em><font color="#B33">',input$identifiant,'</font></em></b>',
                           ' ( ',format(Sys.Date(),"%d.%b%Y"),' ',
                           str_c(str_split(str_split(Sys.time()," ")[[1]][2],":")[[1]][1],":",str_split(str_split(Sys.time()," ")[[1]][2],":")[[1]][2]),' ) ',
                           '<br>',input$comtxt,'</br>')
        
      }else if(input$userName != "" & input$identifiant!=""){
        data[1,1] = paste0('<b><em><font color="#B33">',input$userName,'</font></em></b>',
                           ' ( ',format(Sys.Date(),"%d.%b%Y"),' ',
                           str_c(str_split(str_split(Sys.time()," ")[[1]][2],":")[[1]][1],":",str_split(str_split(Sys.time()," ")[[1]][2],":")[[1]][2]),' ) ',
                           '<br>',input$comtxt,'</br>')
      }
      colnames(commentdata) = "Anciens Commentaires"
      commentdata = rbind(data,commentdata)
      
      write.table(commentdata, file = "www/commentdata.txt", append = FALSE, quote = TRUE, sep = ";",
                  eol = "\n", na = "NA", dec = ".", row.names = F,
                  col.names = TRUE, qmethod = c("escape", "double"),
                  fileEncoding = "UTF-8")
      
      commentdata = read.csv("www/commentdata.txt",sep = ";",encoding = "UTF-8")
      
      
      output$comentdata = DT::renderDataTable({
        DT::datatable(
          commentdata,
          rownames = FALSE,escape = F,colnames = NULL,
          options = list(pageLength = 7,searching = FALSE, lengthChange = FALSE,ordering=F)
        )}
      )
      
      updateTextAreaInput(session,"comnom",value = "")
      updateTextAreaInput(session,"comtxt",value = "")
      
      Sys.sleep(1)
      shinyjs::enable("combtn")
      shinyjs::hide("submit_msg_comment")
      shinyjs::hide("error_msg_comment")
      
    }
    
  })
  
  observeEvent(input$Rating,{
    rate = read.csv("www/rate.txt",sep = ";")
    rate = rbind(rate,input$Rating)
    colnames(rate)="rates"
    write.table(rate, file = "www/rate.txt", append = FALSE, quote = TRUE, sep = ";",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = TRUE, qmethod = c("escape", "double"),
                fileEncoding = "UTF-8")
    
    output$ratetxt = renderText({
      if(input$Rating != ""){
        paste(input$Rating,"/5")
      }else{
        paste("")
      }
      
    })
  })  
  
  
  #############################
  ############gifs###########
  ##########################
  
  
  observe({
    
    if(length(input$produitSelectGif)==2){
      
      if(input$produitSelectGif[1] == "Sections ( NSH2 )" & input$produitSelectGif[2] == "Chapitres ( NSH4 )" & is.na(input$produitSelectGif[3])){
        
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSelectGifNSH22)){
          cc = str_split(input$produitSelectGifNSH22[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:98){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(sectionNames,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,sectionNames[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSelectGifNSH44",choices = section_choisie,selected = section_choisie[1])
        }
      }else if(input$produitSelectGif[1] == "Sections ( NSH2 )" & input$produitSelectGif[2] == "Produits ( NSH6 )" & is.na(input$produitSelectGif[3])){
        
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSelectGifNSH26)){
          cc = str_split(input$produitSelectGifNSH26[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 100:999){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 1000:9999){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSelectGifNSH626",choices = section_choisie,selected = section_choisie[1])
        }
      }else if(input$produitSelectGif[1] == "Chapitres ( NSH4 )" & input$produitSelectGif[2] == "Produits ( NSH6 )" & is.na(input$produitSelectGif[3])){
        indice_NSH2 = "" 
        for(i in 1:length(input$produitSelectGifNSH46)){
          cc = str_split(input$produitSelectGifNSH46[i],"")
          cc = try(paste0(cc[[1]][1],cc[[1]][2],cc[[1]][3],cc[[1]][4]))
          indice_NSH2 = c(indice_NSH2,cc)
        }
        indice_NSH2 = indice_NSH2[-1]
        
        indice_NSH2_final = ""
        for(i in 1:9){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],"0",i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        for(i in 10:99){
          for(j in 1:length(indice_NSH2)){
            cc = paste0(indice_NSH2[j],i)
            indice_NSH2_final = c(indice_NSH2_final,cc)
          }
          
        }
        
        section_choisie = ""
        for(i in 2:length(indice_NSH2_final)){
          k = which(str_detect(allProduits,paste0(indice_NSH2_final[i],"-")))
          if(length(k)!=0){
            section_choisie = c(section_choisie,allProduits[k])
          }
        }
        section_choisie = section_choisie[-1]
        if(!is.na(length(section_choisie))){
          updateSelectInput(session,"produitSelectGifNSH646",choices = section_choisie,selected = section_choisie[1])
        }
      }
      
      
    }
    
    
  })
  
  observeEvent(input$btn,{
    if(!is.null(input$produitSelectGif[1])){ 
      shinyjs::show("jsbtn")
      shinyjs::disable("btn")
      shinyjs::hide("jsImport")
      shinyjs::hide("jsExport")
      shinyjs::hide("jsBilan")
      
      isolate({
        
        if(input$produitSelectGif[1] == 'Sections ( NSH2 )' & is.na(input$produitSelectGif[2])){
          
         if(input$quantValGeneral == "Valeur"){ 
          NSH_graph = str_split(input$produitSelectGifNSH2,"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2]))
          NSH = "nsh2"
          code = NSH_graph
          nom_produit = input$produitSelectGifNSH2
          valeur = 1000000
          unite = "Milliards de Dinars"
          }else{ 
            NSH_graph = str_split(input$produitSelectGifNSH2,"")
            NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2]))
            NSH = "nsh2"
            code = NSH_graph
            nom_produit = input$produitSelectGifNSH2
            valeur = 1000000
            unite = "Milles Tonnes"
          }
          
        }
        else if(input$produitSelectGif[1] == 'Chapitres ( NSH4 )' & is.na(input$produitSelectGif[2])){
          
          if(input$quantValGeneral == "Valeur"){ 
          NSH_graph = str_split(input$produitSelectGifNSH4,"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
          NSH = "nsh4"
          code = NSH_graph
          nom_produit = input$produitSelectGifNSH4
          valeur = 1000
          unite = "Millions de Dinars"
          }else {
            NSH_graph = str_split(input$produitSelectGifNSH4,"")
            NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
            NSH = "nsh4"
            code = NSH_graph
            nom_produit = input$produitSelectGifNSH4
            valeur = 1000
            unite = "Tonnes"
          }
        }
        else if(input$produitSelectGif[1] == 'Produits ( NSH6 )' & is.na(input$produitSelectGif[2])){
          
          if(input$quantValGeneral == "Valeur"){ 
          NSH_graph = str_split(input$produitSelectGifNSH6,"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          NSH = "nsh6"
          code = NSH_graph
          nom_produit = input$produitSelectGifNSH6
          valeur = 1
          unite = "Dinars"
          }else {
            NSH_graph = str_split(input$produitSelectGifNSH6,"")
            NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
            NSH = "nsh6"
            code = NSH_graph
            nom_produit = input$produitSelectGifNSH6
            valeur = 1
            unite = "Kilogrammes"
          }
        }
        else if(input$produitSelectGif[1] == 'Sections ( NSH2 )' & input$produitSelectGif[2] == 'Chapitres ( NSH4 )' & is.na(input$produitSelectGif[3])){
          
          if(input$quantValGeneral == "Valeur"){ 
          NSH_graph = str_split(input$produitSelectGifNSH44,"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
          NSH = "nsh4"
          code = NSH_graph
          nom_produit = input$produitSelectGifNSH44
          valeur = 1000
          unite = "Millions de Dinars"
          }else {
            NSH_graph = str_split(input$produitSelectGifNSH44,"")
            NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4]))
            NSH = "nsh4"
            code = NSH_graph
            nom_produit = input$produitSelectGifNSH44
            valeur = 1000
            unite = "Tonnes"
          }
        }
        else if(input$produitSelectGif[1] == 'Sections ( NSH2 )' & input$produitSelectGif[2] == 'Produits ( NSH6 )' & is.na(input$produitSelectGif[3])){
          
          if(input$quantValGeneral == "Valeur"){ 
          NSH_graph = str_split(input$produitSelectGifNSH626,"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          NSH = "nsh6"
          code = NSH_graph
          nom_produit = input$produitSelectGifNSH626
          valeur = 1
          unite = "Dinars"
          }else{
            NSH_graph = str_split(input$produitSelectGifNSH626,"")
            NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
            NSH = "nsh6"
            code = NSH_graph
            nom_produit = input$produitSelectGifNSH626
            valeur = 1
            unite = "Kilogrammes"
          }
        }
        else if(input$produitSelectGif[1] == 'Chapitres ( NSH4 )' & input$produitSelectGif[2] == 'Produits ( NSH6 )' & is.na(input$produitSelectGif[3])){
          
          if(input$quantValGeneral == "Valeur"){ 
          NSH_graph = str_split(input$produitSelectGifNSH646,"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          NSH = "nsh6"
          code = NSH_graph
          nom_produit = input$produitSelectGifNSH646
          valeur = 1
          unite = "Dinars"
          }else{
            NSH_graph = str_split(input$produitSelectGifNSH646,"")
            NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
            NSH = "nsh6"
            code = NSH_graph
            nom_produit = input$produitSelectGifNSH646
            valeur = 1
            unite = "Kilogrammes" 
          }
        }
        else if(input$produitSelectGif[1] == 'Sections ( NSH2 )' & input$produitSelectGif[2] == 'Chapitres ( NSH4 )' & input$produitSelectGif[3] == 'Produits ( NSH6 )'){
          
          if(input$quantValGeneral == "Valeur"){ 
          NSH_graph = str_split(input$produitSelectGifNSH666,"")
          NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
          NSH = "nsh6"
          code = NSH_graph
          nom_produit = input$produitSelectGifNSH666
          valeur = 1
          unite = "Dinars"
          }else{
            NSH_graph = str_split(input$produitSelectGifNSH666,"")
            NSH_graph = try(paste0(NSH_graph[[1]][1],NSH_graph[[1]][2],NSH_graph[[1]][3],NSH_graph[[1]][4],NSH_graph[[1]][5],NSH_graph[[1]][6]))
            NSH = "nsh6"
            code = NSH_graph
            nom_produit = input$produitSelectGifNSH666
            valeur = 1
            unite = "Kilogrammes"
          }
          
        }
        
        if(input$quantValGeneral == "Valeur"){ 
          valquan = "v"
        }else{
          valquan = "q"
        }
        
        png(paste0("main/source.png"),width =900,height = 70)
        par(mar=rep(0.1,4)) # no margins
        plot(NA,xlim=c(0,2), ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab = "",ylab = "")
        text(2,1.5, "Source: Institut National de la Statistique",
             adj = 1,cex = 1.5,col = "Black")
        text(2,0.5,"les valeurs relatives à l'année 2017 sont provisoires",
             adj = 1,cex = 1.5,col = "#e60000")
        dev.off()
        
        ##############################################################################
        ################################TreeMap Import#######################################
        ############################################################################
        
  if(input$radioContinent == "Pays"){
    url <- paste0("http://apps.ins.tn/comex/fr/comex_",NSH,"_serie.php?chaptr=",code,"&b=",valquan,"&b1=i&c=d")
        webpage <- read_html(curl(url))
        link.tables <- webpage %>% html_nodes('table')
        table_extracted = data.frame(html_table(link.tables[2],fill = T))
        table_extracted = table_extracted[-1,]
        colnames(table_extracted)[1] = "Pays"
        for(i in 2:length(table_extracted)){
          colnames(table_extracted)[i] = paste0("Annee",i+1997)
        }
        #data.frame(html_table(link.tables[3],fill = T))
        #View(table_extracted)
        
        for(i in 2:length(table_extracted)){
          table_extracted[,i] = str_replace_all(table_extracted[,i],"-","0")
          table_extracted[,i] = as.numeric(table_extracted[,i])
        }
        
        for(i in 2:length(table_extracted)){
          table_extracted[,i] = round(table_extracted[,i]/valeur,digits = 2)
        }
        #View(table_extracted)
        for( i in 1999:2017){
          
          if(all(table_extracted[,i-1997] == 0)==F){
            
            png(paste0("main/",i,".png"),width = 900,height = 600)
            treemap(table_extracted, 
                    fontsize.labels=c(24,17),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
                    fontcolor.labels=c("white","black"),
                    index=c("Pays",paste0("Annee",i)),
                    fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
                    bg.labels=c("transparent"),              # Background color of labels
                    align.labels=list(
                      c("center", "center"), 
                      c("right", "bottom")
                    ),
                    vSize=paste0("Annee",i), 
                    type="index",                            
                    palette = "Set1",                        
                    title=""
                    
            )
            dev.off()
          }else{
            png(paste0("main/",i,".png"),width = 900,height = 600)
            plot(NA,xlim=c(0,2), ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab = "",ylab = "")
            par(mar=rep(0.1,4))
            text(1,1, substitute(paste(bold("Pas D'importation"))),
                 adj = 0.5,cex = 4,col = "#e60000")
            dev.off()
          }
          import = paste("Les valeurs des importations en",unite)
          png(paste0("main/titre",i,".png"),width =900,height = 70)
          par(mar=rep(0.1,4)) # no margins
          plot(NA,xlim=c(0,2), ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab = "",ylab = "")
          text(1,1.75,import,
               adj = 0.5,cex = 1.75,col = "#008ae6")
          text(1,1,nom_produit,
               adj = 0.5,cex = 1,col = "Black")
          text(1.75,0.25,"Année : ",
               adj = 1,cex = 1.75,col = "Black")
          text(1.9,0.25,i,
               adj = 1,cex = 1.75,col = "#e60000")
          dev.off()
          
          
          img1 <- image_read(paste0("main/",i,".png"))
          img2 <- image_read(paste0("main/titre",i,".png"))
          img3 <- image_read(paste0("main/source.png"))
          
          img = c(img2,img1,img3)
          image_append(image_scale(img, "800"), stack = TRUE) %>%
            image_write(paste0("main/import/pdt_import_",i,".png"),'png')
          
          
        }
        
        list.files(path = "main/import/", pattern = "*.png", full.names = T) %>% 
          image_read %>% # reads each path file
          image_join() %>% # joins image
          image_animate(fps=1) %>% # animates, can opt for number of loops
          image_write(paste0("main/gifs/",code,"_import.gif"))
        }else if(input$radioContinent == "Continents"){
          url <- paste0("http://apps.ins.tn/comex/fr/comex_",NSH,"_serie.php?chaptr=",code,"&b=",valquan,"&b1=i&c=d")
          webpage <- read_html(curl(url))
          link.tables <- webpage %>% html_nodes('table')
          table_extracted = data.frame(html_table(link.tables[2],fill = T))
          table_extracted = table_extracted[-1,]
          colnames(table_extracted)[1] = "Pays"
          for(i in 2:length(table_extracted)){
            colnames(table_extracted)[i] = paste0("Annee",i+1997)
          }
          #data.frame(html_table(link.tables[3],fill = T))
          #View(table_extracted)
          
          for(i in 2:length(table_extracted)){
            table_extracted[,i] = str_replace_all(table_extracted[,i],"-","0")
            table_extracted[,i] = as.numeric(table_extracted[,i])
          }
          
          for(i in 2:length(table_extracted)){
            table_extracted[,i] = round(table_extracted[,i]/valeur,digits = 2)
          }
          table_extracted$Pays[which(str_detect(table_extracted$Pays,"rythrée"))] = "Érythrée"
          table_extracted$Pays[which(str_detect(table_extracted$Pays,"خles mariannes du nord"))] = "les mariannes du nord"
          table_extracted$Pays[which(str_detect(table_extracted$Pays,"خles féroé"))] = "les féroé"
          table_extracted$Pays[which(str_detect(table_extracted$Pays,"خles turks et caïques"))] = "les turks et caïques"
          table_extracted$Pays[which(str_detect(table_extracted$Pays,"خes cocos"))] = "les cocos"
          table_extracted$Pays[which(str_detect(table_extracted$Pays,"خles"))] = "les malvinas falkland"
          
          paysMaps = read.csv("www/paysMaps.csv",sep=";",fileEncoding = "UTF-8")
          paysMaps$Pays = as.character(paysMaps$Pays)
          paysMaps$code3 = as.character(paysMaps$code3)
          paysMaps$code2 = as.character(paysMaps$code2)
          paysMaps$code2[164] = "NA"
          
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"rythrée"))] = "Érythrée"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les mariannes du nord"))] = "les mariannes du nord"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les féroé"))] = "les féroé"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les turks et caïques"))] = "les turks et caïques"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les cocos"))] = "les cocos"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les"))] = "les malvinas falkland"
          
          
          
          table_extracted$code2 = 0
          table_extracted$code3 = 0
          
          for(i in 1:nrow(table_extracted)){
            k = which(table_extracted$Pays[i] == paysMaps$Pays)
            table_extracted$code2[i] = paysMaps$code2[k[1]]
            table_extracted$code3[i] = paysMaps$code3[k[1]]
          }
          #View(table_extracted)
          country_continent = read.csv("www/country_continent.csv",sep = ",")
          country_continent$iso.3166.country = as.character(country_continent$iso.3166.country)
          country_continent$iso.3166.country[which(is.na(country_continent$iso.3166.country))] = "NA"
          
          country_continent$continent.code = as.character(country_continent$continent.code)
          country_continent$continent.code[which(is.na(country_continent$continent.code))] = "NA"
          country_continent$continent.code[which(country_continent$iso.3166.country == "SZ")] = "EU"
          
          
          country_continent = country_continent[-which(country_continent$continent.code == "--"),]
          
          country_continent$Nom_continent = 0
          #View(country_continent)
          for(i in 1:nrow(country_continent)){
            if(country_continent$continent.code[i] == "AF"){
              country_continent$Nom_continent[i] = "Afrique"
            }else if(country_continent$continent.code[i] == "AN"){
              country_continent$Nom_continent[i] = "Antarctique"
            }else if(country_continent$continent.code[i] == "AS"){
              country_continent$Nom_continent[i] = "Asie"
            }else if(country_continent$continent.code[i] == "EU"){
              country_continent$Nom_continent[i] = "Europe"
            }else if(country_continent$continent.code[i] == "OC"){
              country_continent$Nom_continent[i] = "Océanie"
            }else if(country_continent$continent.code[i] == "SA"){
              country_continent$Nom_continent[i] = "Amérique du Sud"
            }else if(country_continent$continent.code[i] == "NA"){
              country_continent$Nom_continent[i] = "Amérique du Nord"
            }
          }
          table_extracted$code2[which(is.na(table_extracted$code2))] = "NA"
          
          for(i in 1:nrow(table_extracted)){
            k = which(country_continent$iso.3166.country == table_extracted$code2[i])
            table_extracted$code_continent[i] = country_continent$continent.code[k]
            table_extracted$Nom_continent[i] = country_continent$Nom_continent[k]
            
          }
          #View(table_extracted)
          b_variable = table_extracted
          
          statement = paste0("select Nom_continent Nom_continent,sum(Annee1999) AnneeX1999,sum(Annee2000) AnneeX2000,sum(Annee2001) AnneeX2001,
                             sum(Annee2002) AnneeX2002,sum(Annee2003) AnneeX2003,sum(Annee2004) AnneeX2004,sum(Annee2005) AnneeX2005,
                             sum(Annee2006) AnneeX2006,sum(Annee2007) AnneeX2007,sum(Annee2008) AnneeX2008,sum(Annee2009) AnneeX2009,
                             sum(Annee2010) AnneeX2010,sum(Annee2011) AnneeX2011,sum(Annee2012) AnneeX2012,sum(Annee2013) AnneeX2013,sum(Annee2014)
                             AnneeX2014,sum(Annee2015) AnneeX2015,sum(Annee2016) AnneeX2016,sum(Annee2017) AnneeX2017 
                             from b_variable group by Nom_continent")
          
          d = sqldf(statement)
          
          d = sqldf(statement)
          table_extracted = d
          colnames(table_extracted)[1] = "Pays"
          for(i in 2:length(table_extracted)){
            colnames(table_extracted)[i] = paste0("Annee",1997+i)
          }
          
          for( i in 1999:2017){
            
            if(all(table_extracted[,i-1997] == 0)==F){
              
              png(paste0("main/",i,".png"),width = 900,height = 600)
              treemap(table_extracted, 
                      fontsize.labels=c(24,17),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
                      fontcolor.labels=c("white","black"),
                      index=c("Pays",paste0("Annee",i)),
                      fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
                      bg.labels=c("transparent"),              # Background color of labels
                      align.labels=list(
                        c("center", "center"), 
                        c("right", "bottom")
                      ),
                      vSize=paste0("Annee",i), 
                      type="index",                            
                      palette = "Set1",                        
                      title=""
                      
              )
              dev.off()
            }else{
              png(paste0("main/",i,".png"),width = 900,height = 600)
              plot(NA,xlim=c(0,2), ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab = "",ylab = "")
              par(mar=rep(0.1,4))
              text(1,1, substitute(paste(bold("Pas D'importation"))),
                   adj = 0.5,cex = 4,col = "#e60000")
              dev.off()
            }
            import = paste("Les valeurs des importations en",unite)
            png(paste0("main/titre",i,".png"),width =900,height = 70)
            par(mar=rep(0.1,4)) # no margins
            plot(NA,xlim=c(0,2), ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab = "",ylab = "")
            text(1,1.75,import,
                 adj = 0.5,cex = 1.75,col = "#008ae6")
            text(1,1,nom_produit,
                 adj = 0.5,cex = 1,col = "Black")
            text(1.75,0.25,"Année : ",
                 adj = 1,cex = 1.75,col = "Black")
            text(1.9,0.25,i,
                 adj = 1,cex = 1.75,col = "#e60000")
            dev.off()
            
            
            img1 <- image_read(paste0("main/",i,".png"))
            img2 <- image_read(paste0("main/titre",i,".png"))
            img3 <- image_read(paste0("main/source.png"))
            
            img = c(img2,img1,img3)
            image_append(image_scale(img, "800"), stack = TRUE) %>%
              image_write(paste0("main/import/pdt_import_",i,".png"),'png')
            
            
          }
          
          list.files(path = "main/import/", pattern = "*.png", full.names = T) %>% 
            image_read %>% # reads each path file
            image_join() %>% # joins image
            image_animate(fps=1) %>% # animates, can opt for number of loops
            image_write(paste0("main/gifs/",code,"_import.gif"))
        }
        ##############################################################################
        ################################TreeMap Export#######################################
        ############################################################################
        
        if(input$radioContinent == "Pays"){
          url <- paste0("http://apps.ins.tn/comex/fr/comex_",NSH,"_serie.php?chaptr=",code,"&b=",valquan,"&b1=e&c=d")
        webpage <- read_html(curl(url))
        link.tables <- webpage %>% html_nodes('table')
        table_extracted = data.frame(html_table(link.tables[2],fill = T))
        table_extracted = table_extracted[-1,]
        colnames(table_extracted)[1] = "Pays"
        for(i in 2:length(table_extracted)){
          colnames(table_extracted)[i] = paste0("Annee",i+1997)
        }
        #View(table_extracted)
        
        for(i in 2:length(table_extracted)){
          table_extracted[,i] = str_replace_all(table_extracted[,i],"-","0")
          table_extracted[,i] = as.numeric(table_extracted[,i])
        }
        
        for(i in 2:length(table_extracted)){
          table_extracted[,i] = round(table_extracted[,i]/valeur,digits = 2)
        }
        
        for( i in 1999:2017){
          
          if(all(table_extracted[,i-1997] == 0)==F){
            png(paste0("main/",i,".png"),width = 900,height = 600)
            treemap(table_extracted, 
                    fontsize.labels=c(24,17),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
                    fontcolor.labels=c("white","black"),
                    index=c("Pays",paste0("Annee",i)),
                    fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
                    bg.labels=c("transparent"),              # Background color of labels
                    align.labels=list(
                      c("center", "center"), 
                      c("right", "bottom")
                    ),
                    vSize=paste0("Annee",i), 
                    type="index",                            
                    palette = "Set1",                        
                    title=""
                    
            )     
            dev.off()
          }else{
            png(paste0("main/",i,".png"),width = 900,height = 600)
            plot(NA,xlim=c(0,2), ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab = "",ylab = "")
            par(mar=rep(0.1,4))
            text(1,1, substitute(paste(bold("Pas D'Exportation"))),
                 adj = 0.5,cex = 4,col = "#e60000")
            dev.off()
          }
          export = paste("les valeurs des exportations en",unite)
          png(paste0("main/titre",i,".png"),width =900,height = 70)
          par(mar=rep(0.1,4)) # no margins
          plot(NA,xlim=c(0,2), ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab = "",ylab = "")
          text(1,1.75,export,
               adj = 0.5,cex = 1.75,col = "#008ae6")
          text(1,1,nom_produit,
               adj = 0.5,cex = 1,col = "Black")
          text(1.75,0.25,"Année : ",
               adj = 1,cex = 1.75,col = "Black")
          text(1.9,0.25,i,
               adj = 1,cex = 1.75,col = "#e60000")
          dev.off()
          
          
          
          img1 <- image_read(paste0("main/",i,".png"))
          img2 <- image_read(paste0("main/titre",i,".png"))
          img3 <- image_read(paste0("main/source.png"))
          
          img = c(img2,img1,img3)
          image_append(image_scale(img, "800"), stack = TRUE) %>%
            image_write(paste0("main/export/pdt_export_",i,".png"),'png')
          
        }
        list.files(path = "main/export/", pattern = "*.png", full.names = T) %>% 
          image_read %>% # reads each path file
          image_join() %>% # joins image
          image_animate(fps=1) %>% # animates, can opt for number of loops
          image_write(paste0("main/gifs/",code,"_export.gif"))
        
        }else if(input$radioContinent == "Continents"){
          url <- paste0("http://apps.ins.tn/comex/fr/comex_",NSH,"_serie.php?chaptr=",code,"&b=",valquan,"&b1=e&c=d")
          webpage <- read_html(curl(url))
          link.tables <- webpage %>% html_nodes('table')
          table_extracted = data.frame(html_table(link.tables[2],fill = T))
          table_extracted = table_extracted[-1,]
          colnames(table_extracted)[1] = "Pays"
          for(i in 2:length(table_extracted)){
            colnames(table_extracted)[i] = paste0("Annee",i+1997)
          }
          #View(table_extracted)
          
          for(i in 2:length(table_extracted)){
            table_extracted[,i] = str_replace_all(table_extracted[,i],"-","0")
            table_extracted[,i] = as.numeric(table_extracted[,i])
          }
          
          for(i in 2:length(table_extracted)){
            table_extracted[,i] = round(table_extracted[,i]/valeur,digits = 2)
          }
          table_extracted$Pays[which(str_detect(table_extracted$Pays,"rythrée"))] = "Érythrée"
          table_extracted$Pays[which(str_detect(table_extracted$Pays,"خles mariannes du nord"))] = "les mariannes du nord"
          table_extracted$Pays[which(str_detect(table_extracted$Pays,"خles féroé"))] = "les féroé"
          table_extracted$Pays[which(str_detect(table_extracted$Pays,"خles turks et caïques"))] = "les turks et caïques"
          table_extracted$Pays[which(str_detect(table_extracted$Pays,"خes cocos"))] = "les cocos"
          table_extracted$Pays[which(str_detect(table_extracted$Pays,"خles"))] = "les malvinas falkland"
          
          paysMaps = read.csv("www/paysMaps.csv",sep=";",fileEncoding = "UTF-8")
          paysMaps$Pays = as.character(paysMaps$Pays)
          paysMaps$code3 = as.character(paysMaps$code3)
          paysMaps$code2 = as.character(paysMaps$code2)
          paysMaps$code2[164] = "NA"
          
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"rythrée"))] = "Érythrée"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les mariannes du nord"))] = "les mariannes du nord"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les féroé"))] = "les féroé"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les turks et caïques"))] = "les turks et caïques"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les cocos"))] = "les cocos"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les"))] = "les malvinas falkland"
          
          
          
          table_extracted$code2 = 0
          table_extracted$code3 = 0
          
          for(i in 1:nrow(table_extracted)){
            k = which(table_extracted$Pays[i] == paysMaps$Pays)
            table_extracted$code2[i] = paysMaps$code2[k[1]]
            table_extracted$code3[i] = paysMaps$code3[k[1]]
          }
          #View(table_extracted)
          country_continent = read.csv("www/country_continent.csv",sep = ",")
          country_continent$iso.3166.country = as.character(country_continent$iso.3166.country)
          country_continent$iso.3166.country[which(is.na(country_continent$iso.3166.country))] = "NA"
          
          country_continent$continent.code = as.character(country_continent$continent.code)
          country_continent$continent.code[which(is.na(country_continent$continent.code))] = "NA"
          country_continent$continent.code[which(country_continent$iso.3166.country == "SZ")] = "EU"
          
          
          country_continent = country_continent[-which(country_continent$continent.code == "--"),]
          
          country_continent$Nom_continent = 0
          #View(country_continent)
          for(i in 1:nrow(country_continent)){
            if(country_continent$continent.code[i] == "AF"){
              country_continent$Nom_continent[i] = "Afrique"
            }else if(country_continent$continent.code[i] == "AN"){
              country_continent$Nom_continent[i] = "Antarctique"
            }else if(country_continent$continent.code[i] == "AS"){
              country_continent$Nom_continent[i] = "Asie"
            }else if(country_continent$continent.code[i] == "EU"){
              country_continent$Nom_continent[i] = "Europe"
            }else if(country_continent$continent.code[i] == "OC"){
              country_continent$Nom_continent[i] = "Océanie"
            }else if(country_continent$continent.code[i] == "SA"){
              country_continent$Nom_continent[i] = "Amérique du Sud"
            }else if(country_continent$continent.code[i] == "NA"){
              country_continent$Nom_continent[i] = "Amérique du Nord"
            }
          }
          table_extracted$code2[which(is.na(table_extracted$code2))] = "NA"
          
          for(i in 1:nrow(table_extracted)){
            k = which(country_continent$iso.3166.country == table_extracted$code2[i])
            table_extracted$code_continent[i] = country_continent$continent.code[k]
            table_extracted$Nom_continent[i] = country_continent$Nom_continent[k]
            
          }
          b_variable = table_extracted
          
          statement = paste0("select Nom_continent Nom_continent,sum(Annee1999) AnneeX1999,sum(Annee2000) AnneeX2000,sum(Annee2001) AnneeX2001,
                             sum(Annee2002) AnneeX2002,sum(Annee2003) AnneeX2003,sum(Annee2004) AnneeX2004,sum(Annee2005) AnneeX2005,
                             sum(Annee2006) AnneeX2006,sum(Annee2007) AnneeX2007,sum(Annee2008) AnneeX2008,sum(Annee2009) AnneeX2009,
                             sum(Annee2010) AnneeX2010,sum(Annee2011) AnneeX2011,sum(Annee2012) AnneeX2012,sum(Annee2013) AnneeX2013,sum(Annee2014)
                             AnneeX2014,sum(Annee2015) AnneeX2015,sum(Annee2016) AnneeX2016,sum(Annee2017) AnneeX2017 
                             from b_variable group by Nom_continent")
          
          d = sqldf(statement)
          
          table_extracted = d
          colnames(table_extracted)[1] = "Pays"
          for(i in 2:length(table_extracted)){
            colnames(table_extracted)[i] = paste0("Annee",1997+i)
          }
          for( i in 1999:2017){
            
            if(all(table_extracted[,i-1997] == 0)==F){
              png(paste0("main/",i,".png"),width = 900,height = 600)
              treemap(table_extracted, 
                      fontsize.labels=c(24,17),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
                      fontcolor.labels=c("white","black"),
                      index=c("Pays",paste0("Annee",i)),
                      fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
                      bg.labels=c("transparent"),              # Background color of labels
                      align.labels=list(
                        c("center", "center"), 
                        c("right", "bottom")
                      ),
                      vSize=paste0("Annee",i), 
                      type="index",                            
                      palette = "Set1",                        
                      title=""
                      
              )     
              dev.off()
            }else{
              png(paste0("main/",i,".png"),width = 900,height = 600)
              plot(NA,xlim=c(0,2), ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab = "",ylab = "")
              par(mar=rep(0.1,4))
              text(1,1, substitute(paste(bold("Pas D'Exportation"))),
                   adj = 0.5,cex = 4,col = "#e60000")
              dev.off()
            }
            export = paste("les valeurs des exportations en",unite)
            png(paste0("main/titre",i,".png"),width =900,height = 70)
            par(mar=rep(0.1,4)) # no margins
            plot(NA,xlim=c(0,2), ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab = "",ylab = "")
            text(1,1.75,export,
                 adj = 0.5,cex = 1.75,col = "#008ae6")
            text(1,1,nom_produit,
                 adj = 0.5,cex = 1,col = "Black")
            text(1.75,0.25,"Année : ",
                 adj = 1,cex = 1.75,col = "Black")
            text(1.9,0.25,i,
                 adj = 1,cex = 1.75,col = "#e60000")
            dev.off()
            
            
            
            img1 <- image_read(paste0("main/",i,".png"))
            img2 <- image_read(paste0("main/titre",i,".png"))
            img3 <- image_read(paste0("main/source.png"))
            
            img = c(img2,img1,img3)
            image_append(image_scale(img, "800"), stack = TRUE) %>%
              image_write(paste0("main/export/pdt_export_",i,".png"),'png')
            
          }
          list.files(path = "main/export/", pattern = "*.png", full.names = T) %>% 
            image_read %>% # reads each path file
            image_join() %>% # joins image
            image_animate(fps=1) %>% # animates, can opt for number of loops
            image_write(paste0("main/gifs/",code,"_export.gif"))
        }
        ###################################################"
        #################barPlot##########################
        ####################################################
        
        #####base import
        if(input$radioContinent=="Pays"){
        url <- paste0("http://apps.ins.tn/comex/fr/comex_",NSH,"_serie.php?chaptr=",code,"&b=",valquan,"&b1=i&c=d")
        webpage <- read_html(curl(url))
        link.tables <- webpage %>% html_nodes('table')
        table_extractedImport = data.frame(html_table(link.tables[2],fill = T))
        table_extractedImport = table_extractedImport[-1,]
        colnames(table_extractedImport)[1] = "Pays"
        for(i in 2:length(table_extractedImport)){
          colnames(table_extractedImport)[i] = paste0("Annee",i+1997)
        }
        for(i in 2:length(table_extractedImport)){
          table_extractedImport[,i] = str_replace_all(table_extractedImport[,i],"-","0")
          table_extractedImport[,i] = as.numeric(table_extractedImport[,i])
        }
        
        for(i in 2:length(table_extractedImport)){
          table_extractedImport[,i] = round(table_extractedImport[,i]/valeur,digits = 2)
        } 
        
        table_extractedImport$Echange = "Import"
        
        table_extractedImport$Pays[which(str_detect(table_extractedImport$Pays,"rythrée"))] = "Érythrée"
        table_extractedImport$Pays[which(str_detect(table_extractedImport$Pays,"خles mariannes du nord"))] = "les mariannes du nord"
        table_extractedImport$Pays[which(str_detect(table_extractedImport$Pays,"خles féroé"))] = "les féroé"
        table_extractedImport$Pays[which(str_detect(table_extractedImport$Pays,"خles turks et caïques"))] = "les turks et caïques"
        table_extractedImport$Pays[which(str_detect(table_extractedImport$Pays,"خes cocos"))] = "les cocos"
        table_extractedImport$Pays[which(str_detect(table_extractedImport$Pays,"خles"))] = "les malvinas falkland"
        }else if(input$radioContinent=="Continents"){
          url <- paste0("http://apps.ins.tn/comex/fr/comex_",NSH,"_serie.php?chaptr=",code,"&b=",valquan,"&b1=i&c=d")
          webpage <- read_html(curl(url))
          link.tables <- webpage %>% html_nodes('table')
          table_extractedImport = data.frame(html_table(link.tables[2],fill = T))
          table_extractedImport = table_extractedImport[-1,]
          colnames(table_extractedImport)[1] = "Pays"
          for(i in 2:length(table_extractedImport)){
            colnames(table_extractedImport)[i] = paste0("Annee",i+1997)
          }
          for(i in 2:length(table_extractedImport)){
            table_extractedImport[,i] = str_replace_all(table_extractedImport[,i],"-","0")
            table_extractedImport[,i] = as.numeric(table_extractedImport[,i])
          }
          
          for(i in 2:length(table_extractedImport)){
            table_extractedImport[,i] = round(table_extractedImport[,i]/valeur,digits = 2)
          } 
          
          table_extractedImport$Echange = "Import"
          
          table_extractedImport$Pays[which(str_detect(table_extractedImport$Pays,"rythrée"))] = "Érythrée"
          table_extractedImport$Pays[which(str_detect(table_extractedImport$Pays,"خles mariannes du nord"))] = "les mariannes du nord"
          table_extractedImport$Pays[which(str_detect(table_extractedImport$Pays,"خles féroé"))] = "les féroé"
          table_extractedImport$Pays[which(str_detect(table_extractedImport$Pays,"خles turks et caïques"))] = "les turks et caïques"
          table_extractedImport$Pays[which(str_detect(table_extractedImport$Pays,"خes cocos"))] = "les cocos"
          table_extractedImport$Pays[which(str_detect(table_extractedImport$Pays,"خles"))] = "les malvinas falkland"
          paysMaps = read.csv("www/paysMaps.csv",sep=";",fileEncoding = "UTF-8")
          paysMaps$Pays = as.character(paysMaps$Pays)
          paysMaps$code3 = as.character(paysMaps$code3)
          paysMaps$code2 = as.character(paysMaps$code2)
          paysMaps$code2[164] = "NA"
          
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"rythrée"))] = "Érythrée"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les mariannes du nord"))] = "les mariannes du nord"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les féroé"))] = "les féroé"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les turks et caïques"))] = "les turks et caïques"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les cocos"))] = "les cocos"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les"))] = "les malvinas falkland"
          
          
          
          table_extractedImport$code2 = 0
          table_extractedImport$code3 = 0
          
          for(i in 1:nrow(table_extractedImport)){
            k = which(table_extractedImport$Pays[i] == paysMaps$Pays)
            table_extractedImport$code2[i] = paysMaps$code2[k[1]]
            table_extractedImport$code3[i] = paysMaps$code3[k[1]]
          }
          #View(table_extractedImport)
          country_continent = read.csv("www/country_continent.csv",sep = ",")
          country_continent$iso.3166.country = as.character(country_continent$iso.3166.country)
          country_continent$iso.3166.country[which(is.na(country_continent$iso.3166.country))] = "NA"
          
          country_continent$continent.code = as.character(country_continent$continent.code)
          country_continent$continent.code[which(is.na(country_continent$continent.code))] = "NA"
          country_continent$continent.code[which(country_continent$iso.3166.country == "SZ")] = "EU"
          
          
          country_continent = country_continent[-which(country_continent$continent.code == "--"),]
          
          country_continent$Nom_continent = 0
          #View(country_continent)
          for(i in 1:nrow(country_continent)){
            if(country_continent$continent.code[i] == "AF"){
              country_continent$Nom_continent[i] = "Afrique"
            }else if(country_continent$continent.code[i] == "AN"){
              country_continent$Nom_continent[i] = "Antarctique"
            }else if(country_continent$continent.code[i] == "AS"){
              country_continent$Nom_continent[i] = "Asie"
            }else if(country_continent$continent.code[i] == "EU"){
              country_continent$Nom_continent[i] = "Europe"
            }else if(country_continent$continent.code[i] == "OC"){
              country_continent$Nom_continent[i] = "Océanie"
            }else if(country_continent$continent.code[i] == "SA"){
              country_continent$Nom_continent[i] = "Amérique du Sud"
            }else if(country_continent$continent.code[i] == "NA"){
              country_continent$Nom_continent[i] = "Amérique du Nord"
            }
          }
          table_extractedImport$code2[which(is.na(table_extractedImport$code2))] = "NA"
          
          for(i in 1:nrow(table_extractedImport)){
            k = which(country_continent$iso.3166.country == table_extractedImport$code2[i])
            table_extractedImport$code_continent[i] = country_continent$continent.code[k]
            table_extractedImport$Nom_continent[i] = country_continent$Nom_continent[k]
            
          }
    
          b_variable = table_extractedImport
          
          statement = paste0("select Nom_continent Nom_continent,sum(Annee1999) AnneeX1999,sum(Annee2000) AnneeX2000,sum(Annee2001) AnneeX2001,
                             sum(Annee2002) AnneeX2002,sum(Annee2003) AnneeX2003,sum(Annee2004) AnneeX2004,sum(Annee2005) AnneeX2005,
                             sum(Annee2006) AnneeX2006,sum(Annee2007) AnneeX2007,sum(Annee2008) AnneeX2008,sum(Annee2009) AnneeX2009,
                             sum(Annee2010) AnneeX2010,sum(Annee2011) AnneeX2011,sum(Annee2012) AnneeX2012,sum(Annee2013) AnneeX2013,sum(Annee2014)
                             AnneeX2014,sum(Annee2015) AnneeX2015,sum(Annee2016) AnneeX2016,sum(Annee2017) AnneeX2017 
                             from b_variable group by Nom_continent")
          
          d = sqldf(statement)
          
          table_extractedImport = d
          colnames(table_extractedImport)[1] = "Pays"
          for(i in 2:length(table_extractedImport)){
            colnames(table_extractedImport)[i] = paste0("Annee",1997+i)
          }
          table_extractedImport$Echange = "Import"
        }
        
        ############ base export
        
        if(input$radioContinent == "Pays"){
        url <- paste0("http://apps.ins.tn/comex/fr/comex_",NSH,"_serie.php?chaptr=",code,"&b=",valquan,"&b1=e&c=d")
        webpage <- read_html(curl(url))
        link.tables <- webpage %>% html_nodes('table')
        table_extractedExport = data.frame(html_table(link.tables[2],fill = T))
        table_extractedExport = table_extractedExport[-1,]
        colnames(table_extractedExport)[1] = "Pays"
        for(i in 2:length(table_extractedExport)){
          colnames(table_extractedExport)[i] = paste0("Annee",i+1997)
        }
        for(i in 2:length(table_extractedExport)){
          table_extractedExport[,i] = str_replace_all(table_extractedExport[,i],"-","0")
          table_extractedExport[,i] = as.numeric(table_extractedExport[,i])
        }
        for(i in 2:length(table_extractedExport)){
          table_extractedExport[,i] = round(table_extractedExport[,i]/valeur,digits = 2)
        }
        table_extractedExport$Echange = "Export"
        
        table_extractedExport$Pays[which(str_detect(table_extractedExport$Pays,"rythrée"))] = "Érythrée"
        table_extractedExport$Pays[which(str_detect(table_extractedExport$Pays,"خles mariannes du nord"))] = "les mariannes du nord"
        table_extractedExport$Pays[which(str_detect(table_extractedExport$Pays,"خles féroé"))] = "les féroé"
        table_extractedExport$Pays[which(str_detect(table_extractedExport$Pays,"خles turks et caïques"))] = "les turks et caïques"
        table_extractedExport$Pays[which(str_detect(table_extractedExport$Pays,"خes cocos"))] = "les cocos"
        table_extractedExport$Pays[which(str_detect(table_extractedExport$Pays,"خles"))] = "les malvinas falkland"
        
        }
        else if(input$radioContinent == "Continents"){
          url <- paste0("http://apps.ins.tn/comex/fr/comex_",NSH,"_serie.php?chaptr=",code,"&b=",valquan,"&b1=e&c=d")
          webpage <- read_html(curl(url))
          link.tables <- webpage %>% html_nodes('table')
          table_extractedExport = data.frame(html_table(link.tables[2],fill = T))
          table_extractedExport = table_extractedExport[-1,]
          colnames(table_extractedExport)[1] = "Pays"
          for(i in 2:length(table_extractedExport)){
            colnames(table_extractedExport)[i] = paste0("Annee",i+1997)
          }
          for(i in 2:length(table_extractedExport)){
            table_extractedExport[,i] = str_replace_all(table_extractedExport[,i],"-","0")
            table_extractedExport[,i] = as.numeric(table_extractedExport[,i])
          }
          for(i in 2:length(table_extractedExport)){
            table_extractedExport[,i] = round(table_extractedExport[,i]/valeur,digits = 2)
          }
          table_extractedExport$Echange = "Export"
          
          table_extractedExport$Pays[which(str_detect(table_extractedExport$Pays,"rythrée"))] = "Érythrée"
          table_extractedExport$Pays[which(str_detect(table_extractedExport$Pays,"خles mariannes du nord"))] = "les mariannes du nord"
          table_extractedExport$Pays[which(str_detect(table_extractedExport$Pays,"خles féroé"))] = "les féroé"
          table_extractedExport$Pays[which(str_detect(table_extractedExport$Pays,"خles turks et caïques"))] = "les turks et caïques"
          table_extractedExport$Pays[which(str_detect(table_extractedExport$Pays,"خes cocos"))] = "les cocos"
          table_extractedExport$Pays[which(str_detect(table_extractedExport$Pays,"خles"))] = "les malvinas falkland"
          
          paysMaps = read.csv("www/paysMaps.csv",sep=";",fileEncoding = "UTF-8")
          paysMaps$Pays = as.character(paysMaps$Pays)
          paysMaps$code3 = as.character(paysMaps$code3)
          paysMaps$code2 = as.character(paysMaps$code2)
          paysMaps$code2[164] = "NA"
          
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"rythrée"))] = "Érythrée"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les mariannes du nord"))] = "les mariannes du nord"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les féroé"))] = "les féroé"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les turks et caïques"))] = "les turks et caïques"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les cocos"))] = "les cocos"
          paysMaps$Pays[which(str_detect(paysMaps$Pays,"<U\\+062E>les"))] = "les malvinas falkland"
          
          
          
          table_extractedExport$code2 = 0
          table_extractedExport$code3 = 0
          
          for(i in 1:nrow(table_extractedExport)){
            k = which(table_extractedExport$Pays[i] == paysMaps$Pays)
            table_extractedExport$code2[i] = paysMaps$code2[k[1]]
            table_extractedExport$code3[i] = paysMaps$code3[k[1]]
          }
          #View(table_extractedExport)
          country_continent = read.csv("www/country_continent.csv",sep = ",")
          country_continent$iso.3166.country = as.character(country_continent$iso.3166.country)
          country_continent$iso.3166.country[which(is.na(country_continent$iso.3166.country))] = "NA"
          
          country_continent$continent.code = as.character(country_continent$continent.code)
          country_continent$continent.code[which(is.na(country_continent$continent.code))] = "NA"
          country_continent$continent.code[which(country_continent$iso.3166.country == "SZ")] = "EU"
          
          
          country_continent = country_continent[-which(country_continent$continent.code == "--"),]
          
          country_continent$Nom_continent = 0
          #View(country_continent)
          for(i in 1:nrow(country_continent)){
            if(country_continent$continent.code[i] == "AF"){
              country_continent$Nom_continent[i] = "Afrique"
            }else if(country_continent$continent.code[i] == "AN"){
              country_continent$Nom_continent[i] = "Antarctique"
            }else if(country_continent$continent.code[i] == "AS"){
              country_continent$Nom_continent[i] = "Asie"
            }else if(country_continent$continent.code[i] == "EU"){
              country_continent$Nom_continent[i] = "Europe"
            }else if(country_continent$continent.code[i] == "OC"){
              country_continent$Nom_continent[i] = "Océanie"
            }else if(country_continent$continent.code[i] == "SA"){
              country_continent$Nom_continent[i] = "Amérique du Sud"
            }else if(country_continent$continent.code[i] == "NA"){
              country_continent$Nom_continent[i] = "Amérique du Nord"
            }
          }
          table_extractedExport$code2[which(is.na(table_extractedExport$code2))] = "NA"
          
          for(i in 1:nrow(table_extractedExport)){
            k = which(country_continent$iso.3166.country == table_extractedExport$code2[i])
            table_extractedExport$code_continent[i] = country_continent$continent.code[k]
            table_extractedExport$Nom_continent[i] = country_continent$Nom_continent[k]
            
          }
          b_variable = table_extractedExport
          
          statement = paste0("select Nom_continent Nom_continent,sum(Annee1999) AnneeX1999,sum(Annee2000) AnneeX2000,sum(Annee2001) AnneeX2001,
                             sum(Annee2002) AnneeX2002,sum(Annee2003) AnneeX2003,sum(Annee2004) AnneeX2004,sum(Annee2005) AnneeX2005,
                             sum(Annee2006) AnneeX2006,sum(Annee2007) AnneeX2007,sum(Annee2008) AnneeX2008,sum(Annee2009) AnneeX2009,
                             sum(Annee2010) AnneeX2010,sum(Annee2011) AnneeX2011,sum(Annee2012) AnneeX2012,sum(Annee2013) AnneeX2013,sum(Annee2014)
                             AnneeX2014,sum(Annee2015) AnneeX2015,sum(Annee2016) AnneeX2016,sum(Annee2017) AnneeX2017 
                             from b_variable group by Nom_continent")
          
          d = sqldf(statement)
          
          table_extractedExport = d
          colnames(table_extractedExport)[1] = "Pays"
          for(i in 2:length(table_extractedExport)){
            colnames(table_extractedExport)[i] = paste0("Annee",1997+i)
          } 
          table_extractedExport$Echange = "Export"
          }
        
        
        ########rbind 
        
        if(nrow(table_extractedImport)>10){
          
          for( i in 1999:2017){
            table_extractedImport1 = table_extractedImport[order(-table_extractedImport[,i-1997]),]
            
            table_extractedImport1 = table_extractedImport1[1:10,]
            d = matrix(0,1,21)
            d = data.frame(d)
            d1 = matrix(0,1,21)
            d1 = data.frame(d)
            d2 = matrix(0,1,21)
            d2 = data.frame(d)
            colnames(d)=colnames(table_extractedExport)
            colnames(d1)=colnames(table_extractedExport)
            colnames(d2)=colnames(table_extractedExport)
            
            for(j in 1:10){
              k = which(table_extractedExport$Pays == table_extractedImport1$Pays[j])
              if(length(k)==0){
                d =  d = rbind(d,d1)
                d[nrow(d),1] = table_extractedImport1$Pays[j]
                d[nrow(d),length(d)] = "Export"
              }else if(length(k)==1){
                d = rbind(d,table_extractedExport[k,])
              }else{
                
                for(h in 3:length(table_extractedImport1)-1){
                  d2[1,h] = sum(table_extractedExport[k,h])
                  d2[1,1]=table_extractedExport$Pays[k[1]]
                  d2[1,21]= table_extractedExport$Echange[k[1]]
                }
                d = rbind(d,d2)
              }
              
            }
            table_extractedExport1 = d[-1,]
            colnames(table_extractedExport1)[length(table_extractedExport1)] = "Echange"
            #View(table_extractedExport1)
            data_globale = rbind(table_extractedImport1,table_extractedExport1)
            
            rownames(data_globale) = c(1:20)
            
            
            data_globale <- data_globale[order(data_globale$Pays), ]
            
            p = ggplot(data=data_globale, aes(x=Pays, y=data_globale[,paste0("Annee",i)], fill=Echange)) +
              geom_bar(stat="identity", position=position_dodge())+
              geom_text(aes(label=data_globale[,paste0("Annee",i)]), vjust=-0.3, color="black",
                        position = position_dodge(0.9), size=3.5)+
              scale_fill_brewer(palette="Paired")+
              theme_minimal()+ labs(x = "",y = "")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
            ggsave(paste0("main/",i,".png"),p,width = 23.8125,height = 15.875,units = "cm")
            
            
            png(paste0("main/titre",i,".png"),width =900,height = 70)
            par(mar=rep(0.1,4)) # no margins
            plot(NA,xlim=c(0,2), ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab = "",ylab = "")
            bilan = paste("Bilan en",unite)
            text(1,1.75,bilan,
                 adj = 0.5,cex = 1.75,col = "#008ae6")
            text(1,1,nom_produit,
                 adj = 0.5,cex = 1,col = "Black")
            text(1.75,0.25,"Année : ",
                 adj = 1,cex = 1.75,col = "Black")
            text(1.9,0.25,i,
                 adj = 1,cex = 1.75,col = "#e60000")
            dev.off()
            
            
            img1 <- image_read(paste0("main/",i,".png"))
            img2 <- image_read(paste0("main/titre",i,".png"))
            img3 <- image_read(paste0("main/source.png"))
            
            img = c(img2,img1,img3)
            image_append(image_scale(img, "800"), stack = TRUE) %>%
              image_write(paste0("main/bilan/pdt_bilan",i,".png"),'png')
            
          }
        }else{

          for( i in 1999:2017){
            
            data_globale = rbind(table_extractedImport,table_extractedExport)
            
            rownames(data_globale) = c(1:nrow(data_globale))
            
            
            data_globale <- data_globale[order(data_globale$Pays), ]
            
            p = ggplot(data=data_globale, aes(x=Pays, y=data_globale[,paste0("Annee",i)], fill=Echange)) +
              geom_bar(stat="identity", position=position_dodge())+
              geom_text(aes(label=data_globale[,paste0("Annee",i)]), vjust=-0.3, color="black",
                        position = position_dodge(0.9), size=3.5)+
              scale_fill_brewer(palette="Paired")+
              theme_minimal()+ labs(x = "",y = "")
            ggsave(paste0("main/",i,".png"),p,width = 23.8125,height = 15.875,units = "cm")
            
            
            png(paste0("main/titre",i,".png"),width =900,height = 70)
            par(mar=rep(0.1,4)) # no margins
            plot(NA,xlim=c(0,2), ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab = "",ylab = "")
            bilan = paste("Bilan en",unite)
            text(1,1.75,bilan,
                 adj = 0.5,cex = 1.75,col = "#008ae6")
            text(1,1,nom_produit,
                 adj = 0.5,cex = 1,col = "Black")
            text(1.75,0.25,"Année : ",
                 adj = 1,cex = 1.75,col = "Black")
            text(1.9,0.25,i,
                 adj = 1,cex = 1.75,col = "#e60000")
            dev.off()
            
            img1 <- readPNG(paste0("main/",i,".png"))
            img2 <- readPNG(paste0("main/titre",i,".png"))
            img3 <- readPNG(paste0("main/source.png"))
            
            png(paste0("main/bilan/pdt_bilan",i,".png"),width = 1000,height = 850)
            
            layout(matrix(c(1),4, 2, byrow = TRUE))
            grid.raster(img1,x=unit(0.5,"npc"),y=unit(0.5, "npc"))
            grid.raster(img2,x=unit(0.5, "npc"),y=unit(0.95, "npc"))
            grid.raster(img3,x=unit(0.5, "npc"),y=unit(0.05, "npc"))
            
            dev.off()
            
          } 
        }
        list.files(path = "main/bilan/", pattern = "*.png", full.names = T) %>% 
          image_read %>% # reads each path file
          image_join() %>% # joins image
          image_animate(fps=1) %>% # animates, can opt for number of loops
          image_write(paste0("main/gifs/",code,"_bilan.gif"))
        
      })
      
      
      output$imageImport = renderImage({
        
        list(src = paste0("main/gifs/",code,"_import.gif"),
             contentType = 'image/gif',
             width = 487,
             height = 400
             # alt = "This is alternate text"
        )
      }, deleteFile = F)
      
      output$imageExport = renderImage({
        
        list(src = paste0("main/gifs/",code,"_export.gif"),
             contentType = 'image/gif',
             width = 487,
             height = 400
             # alt = "This is alternate text"
        )
      }, deleteFile = F)
      
      
      output$imageImportLy = renderImage({
        
        list(src = paste0("main/import/pdt_import_",input$selectYear,".png"),
             contentType = 'image/png',
             width = 487,
             height = 400
             # alt = "This is alternate text"
        )
      }, deleteFile = F)
      
      output$imageExportLy = renderImage({
        
        list(src = paste0("main/export/pdt_export_",input$selectYear,".png"),
             contentType = 'image/png',
             width = 487,
             height = 400
             # alt = "This is alternate text"
        )
      }, deleteFile = F)
      
      output$imageBilan = renderImage({
        
        list(src = paste0("main/gifs/",code,"_bilan.gif"),
             contentType = 'image/gif',
             width = 487,
             height = 400
             # alt = "This is alternate text"
        )
      }, deleteFile = F)
      
      output$imageBilanLy = renderImage({
        
        list(src = paste0("main/bilan/pdt_bilan",input$selectYear,".png"),
             contentType = 'image/png',
             width = 487,
             height = 400
             # alt = "This is alternate text"
        )
      }, deleteFile = F)
      
      output$DnldImport <- downloadHandler(
        filename = function() {
          paste0(code,"_import.gif")
        },
        content = function(file) {
          image_read(paste0("main/gifs/",code,"_import.gif"))%>%
            image_write(file)
        }
      ) 
      output$DnldExport <- downloadHandler(
        filename = function() {
          paste0(code,"_export.gif")
        },
        content = function(file) {
          image_read(paste0("main/gifs/",code,"_export.gif"))%>%
            image_write(file)
        }
      ) 
      output$DnldBilan <- downloadHandler(
        filename = function() {
          paste0(code,"_bilan.gif")
        },
        content = function(file) {
          image_read(paste0("main/gifs/",code,"_bilan.gif"))%>%
            image_write(file)
        }
      ) 
      
      
      
      output$DnldImportLy <- downloadHandler(
        filename = function() {
          paste0(code,"_",input$selectYear,"_import.png")
        },
        content = function(file) {
          image_read(paste0("main/import/pdt_import_",input$selectYear,".png"))%>%
            image_write(file)
        }
      ) 
      output$DnldExportLy <- downloadHandler(
        filename = function() {
          paste0(code,"_",input$selectYear,"_export.png")
        },
        content = function(file) {
          image_read(paste0("main/export/pdt_export_",input$selectYear,".png"))%>%
            image_write(file)
        }
      ) 
      output$DnldBilanLy <- downloadHandler(
        filename = function() {
          paste0(code,"_",input$selectYear,"_bilan.png")
        },
        content = function(file) {
          image_read(paste0("main/bilan/pdt_bilan",input$selectYear,".png"))%>%
            image_write(file)
        }
      ) 
      
      shinyjs::hide("jsbtn")
      shinyjs::enable("btn")
      shinyjs::show("jsImport")
      shinyjs::show("jsExport")
      shinyjs::show("jsBilan")
    }else{
      
      sendSweetAlert(
        session = session, title =NULL, text = paste("<h5><b><font color='black'> Vérifiez vos paramètres </font></b></h5>")%>%
          lapply(htmltools::HTML), type = "error",html = T
      ) 
    }
    
    
  })    
  
  
        })

