library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(treemap)
library(ggplot2)
library(treemapify)
library(D3partitionR)
library(plotly)
library(sqldf)
library(rvest)
library(curl)
library(stringr)
library(leaflet)
library(shinythemes)
library(shinyjs)
library(shinysky)
library(shinycssloaders)
library(shinyBS)
library(ShinyRatingInput)
library(sqldf)
library(rvest)
library(openssl)
library(curl)
library(stringr)
library(png)
library(grid)
library(treemap)
library(ggplot2)
library(magick)
appCSS <- "
#loading-content {
position: absolute;
background: #FFFFFF;
opacity: 1;
z-index: 100;
left: 0;
right: 0;
top : 0;
bottom : 0;
height: 100%;
color: #FFFFFF;
}
#loading-content1 {
position: absolute;
background: #FFFFFF;
opacity: 1;
z-index: 100;
left: 0;
right: 0;
top : 0;
bottom : 0;
height: 100%;
color: #FFFFFF;
}

"

codePays = readRDS("www/codePays.RDS",.GlobalEnv)
paysNames = readRDS("www/paysNames.RDS",.GlobalEnv)
allProduits = readRDS("www/allProduits.rds",.GlobalEnv)
sectionNames = readRDS("www/sectionNames.RDS",.GlobalEnv)
chapterNames = readRDS("www/chapterNames.RDS",.GlobalEnv)
paysSelect = readRDS("www/paysSelect.rds",.GlobalEnv)

###Building UI
shinyUI(dashboardPage(skin = "red",title = tagList("Commerce Extérieur"),

dashboardHeader(title = fluidRow(tags$span(style="color:#a8120c","l)"),tagList(tags$img(src="Analytics-Icon-White-small.png",width = "12.5%",height="12.5%"),tags$b("Commerce Extérieur")),align="left"),titleWidth =255,
                tags$li(class="dropdown",fluidPage(
                                                     div(style="display: inline-block;vertical-align:top; width: 40px;",
                                                         circleButton(size = "sm", style = "unite",status = "danger","fb",icon = icon('facebook',class = "bolt1"),onclick="window.open('https://www.facebook.com/profile.php?id=100011217999617')")),         
                                                     div(style="display: inline-block;vertical-align:top; width: 40px;",
                                                         circleButton(size = "sm", style = "unite",status = "danger","in1",icon = icon('linkedin',class = "bolt1"),onclick="window.open('https://www.linkedin.com/company/openway1/')")),         
                                                     div(style="display: inline-block;vertical-align:top; width: 40px;",
                                                         circleButton(size = "sm", style = "unite",status = "danger","twit",icon = icon('twitter',class = "bolt1"),onclick="window.open('https://twitter.com/OpenWayDigitale')")),         
                                                     div(style="display: inline-block;vertical-align:top; width: 40px;",
                                                         circleButton(size = "sm", style = "unite",status = "danger","ggleplus",icon = icon('google-plus',class = "bolt1"),onclick="window.open('https://plus.google.com/u/0/106689471282675327497')")),
                                                     div(style="display: inline-block;vertical-align:top; width: 40px;"),                       
                        div(style="display: inline-block;vertical-align:top; width: 40px;",
                                                         dropdown(inputId = "dpLog",
                                                                        uiOutput("uiLogin"),
                                                                        
                                                                        div(class = "logout",uiOutput("userPanel")
                                                                        ),style = "unite", icon = icon("user"),
                                                                  status = "danger", width = "300px",size = "sm",
                                                                  animate = animateOptions(
                                                                    enter = animations$fading_entrances$fadeInLeftBig,
                                                                    exit = animations$fading_exits$fadeOutRightBig
                                                                  ))), 
                        div(style="display: inline-block;vertical-align:top; width: 5px;"), 
                        div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("pass"))
                                                     )
)
                ),
dashboardSidebar(
  sidebarMenu(id = "comExt2",
              fluidPage( useShinyjs(),
                         inlineCSS(appCSS),
                         
                         # Loading message
                         div(
                           id = "loading-content1",
                           br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                           h2("")
                         ),align = "center")
              ,
              sidebarMenuOutput("comExt")              )
),
##############################
#########body#################
##############################
dashboardBody(fluidPage(
  
  fluidPage( useShinyjs(),
             inlineCSS(appCSS),
             
             # Loading message
             div(
               id = "loading-content",
               br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
               h2(""),
               tags$img(src = "mainloding3.gif")
             ),
             
             # The main app code goes here
             hidden(
               div(
                 id = "app-content"
               )
             ),align = "center"),


  tags$head(tags$style(HTML( "text/css", "
                                    
                              
                                     .irs-bar {width: 100%; height: 0px; background: white; border-top: 1px white; border-bottom: 1px white;}
                                     .irs-bar-edge {background: white; border: 1px solid white; height: 1px; border-radius: 15px; width: 0px;}
                                     .irs-line {border: 15px solid white; height: 15px; border-radius: 15px;background:white;}
                                     .irs-slider {width: 40px; height: 10px; background:#a8120c;border-radius: 40px;}

                                     .irs-grid-text {font-family: 'arial'; color: black; bottom: 10px; z-index: 100;font-size:13px;}
                                     .irs-grid-pol {background: white}
                                     .irs-max {display: none}
                                     .irs-min {display: none}
                                     .irs-single {display: none}
                                     .irs-bar {background: white}
.content-wrapper,

                                     .right-side {background-color: white;
                                     height : 10000px;
                                     }
                                                .dropdown{
                                                 padding-top : 7.5px;
                                                 padding-bottom : 7.5px;
                                                 padding-right : 50px;
                                                 }
                                     @-webkit-keyframes jello {
                                     from, 11.1%, to {
                                     -webkit-transform: none;
                                     transform: none;
                                     }
                                     
                                     22.2% {
                                     -webkit-transform: skewX(-12.5deg) skewY(-12.5deg);
                                     transform: skewX(-12.5deg) skewY(-12.5deg);
                                     }
                                     
                                     33.3% {
                                     -webkit-transform: skewX(6.25deg) skewY(6.25deg);
                                     transform: skewX(6.25deg) skewY(6.25deg);
                                     }
                                     
                                     44.4% {
                                     -webkit-transform: skewX(-3.125deg) skewY(-3.125deg);
                                     transform: skewX(-3.125deg) skewY(-3.125deg);
                                     }
                                     
                                     55.5% {
                                     -webkit-transform: skewX(1.5625deg) skewY(1.5625deg);
                                     transform: skewX(1.5625deg) skewY(1.5625deg);
                                     }
                                     
                                     66.6% {
                                     -webkit-transform: skewX(-0.78125deg) skewY(-0.78125deg);
                                     transform: skewX(-0.78125deg) skewY(-0.78125deg);
                                     }
                                     
                                     77.7% {
                                     -webkit-transform: skewX(0.390625deg) skewY(0.390625deg);
                                     transform: skewX(0.390625deg) skewY(0.390625deg);
                                     }
                                     
                                     88.8% {
                                     -webkit-transform: skewX(-0.1953125deg) skewY(-0.1953125deg);
                                     transform: skewX(-0.1953125deg) skewY(-0.1953125deg);
                                     }
                                     }
                                     
                                     @keyframes jello {
                                     from, 11.1%, to {
                                     -webkit-transform: none;
                                     transform: none;
                                     }
                                     
                                     22.2% {
                                     -webkit-transform: skewX(-12.5deg) skewY(-12.5deg);
                                     transform: skewX(-12.5deg) skewY(-12.5deg);
                                     }
                                     
                                     33.3% {
                                     -webkit-transform: skewX(6.25deg) skewY(6.25deg);
                                     transform: skewX(6.25deg) skewY(6.25deg);
                                     }
                                     
                                     44.4% {
                                     -webkit-transform: skewX(-3.125deg) skewY(-3.125deg);
                                     transform: skewX(-3.125deg) skewY(-3.125deg);
                                     }
                                     
                                     55.5% {
                                     -webkit-transform: skewX(1.5625deg) skewY(1.5625deg);
                                     transform: skewX(1.5625deg) skewY(1.5625deg);
                                     }
                                     
                                     66.6% {
                                     -webkit-transform: skewX(-0.78125deg) skewY(-0.78125deg);
                                     transform: skewX(-0.78125deg) skewY(-0.78125deg);
                                     }
                                     
                                     77.7% {
                                     -webkit-transform: skewX(0.390625deg) skewY(0.390625deg);
                                     transform: skewX(0.390625deg) skewY(0.390625deg);
                                     }
                                     
                                     88.8% {
                                     -webkit-transform: skewX(-0.1953125deg) skewY(-0.1953125deg);
                                     transform: skewX(-0.1953125deg) skewY(-0.1953125deg);
                                     }
                                     }
                                     #btnmap {
                                     background-color: #100F0D;
                                     text-align: center;
                                     cursor: pointer;
                                     outline: none;
                                     color: #fff;
                                     border: none;
                                     border-radius: 15px;
                                     box-shadow: 0 2px #100F0D;
                                     }
                                     
                                     #btnmap:hover{
                                     -webkit-animation-name: jello;
                                     animation-name: jello;
                                     -webkit-animation-duration: 1s;
                                     animation-duration: 1s;
                                     background-color: #100F0D;
                                     }
                                     
                                     #btnmap:active {
                                     background-color: #100F0D;
                                     box-shadow: 0 2px #100F0D;
                                     transform: translateY(4px);
                                     }                           
                                     #btntmap {
                                     background-color: #100F0D;
                                     text-align: center;
                                     cursor: pointer;
                                     outline: none;
                                     color: #fff;
                                     border: none;
                                     border-radius: 15px;
                                     box-shadow: 0 2px #100F0D;
                                     }
                                     
                                     #btntmap:hover{
                                     -webkit-animation-name: jello;
                                     animation-name: jello;
                                     -webkit-animation-duration: 1s;
                                     animation-duration: 1s;
                                     background-color: #100F0D;
                                     }
                                     
                                     #btntmap:active {
                                     background-color: #100F0D;
                                     box-shadow: 0 2px #100F0D;
                                     transform: translateY(4px);
                                     }  
                                     #btnprod {
                                     background-color: #100F0D;
                                     text-align: center;
                                     cursor: pointer;
                                     outline: none;
                                     color: #fff;
                                     border: none;
                                     border-radius: 15px;
                                     box-shadow: 0 2px #100F0D;
                                     }
                                     
                                     #btnprod:hover{
                                     -webkit-animation-name: jello;
                                     animation-name: jello;
                                     -webkit-animation-duration: 1s;
                                     animation-duration: 1s;
                                     background-color: #100F0D;
                                     }
                                     
                                     #btnprod:active {
                                     background-color: #100F0D;
                                     box-shadow: 0 2px #100F0D;
                                     transform: translateY(4px);
                                     }         
                                     #btnBilan {
                                     background-color: #100F0D;
                                     text-align: center;
                                     cursor: pointer;
                                     outline: none;
                                     color: #fff;
                                     border: none;
                                     border-radius: 15px;
                                     box-shadow: 0 2px #100F0D;
                                     }
                                     
                                     #btnBilan:hover{
                                     -webkit-animation-name: jello;
                                     animation-name: jello;
                                     -webkit-animation-duration: 1s;
                                     animation-duration: 1s;
                                     background-color: #100F0D;
                                     }
                                     
                                     #btnBilan:active {
                                     background-color: #100F0D;
                                     box-shadow: 0 2px #100F0D;
                                     transform: translateY(4px);
                                     }      
                                     #btnBalance {
                                     background-color: #100F0D;
                                                 text-align: center;
                                                 cursor: pointer;
                                                 outline: none;
                                                 color: #fff;
                                                 border: none;
                                                 border-radius: 15px;
                                                 box-shadow: 0 2px #100F0D;
                                                 }
                                                 
                                                 #btnBalance:hover{
                                                 -webkit-animation-name: jello;
                                                 animation-name: jello;
                                                 -webkit-animation-duration: 1s;
                                                 animation-duration: 1s;
                                                 background-color: #100F0D;
                                                 }
                                                 
                                                 #btnBalance:active {
                                                 background-color: #100F0D;
                                                 box-shadow: 0 2px #100F0D;
                                                 transform: translateY(4px);
                                                 }
 #signin {
                                     background-color: #a8120c;
                             text-align: center;
                             cursor: pointer;
                             outline: none;
                             color: #fff;
                             border: none;
                             border-radius: 15px;
                             box-shadow: 0 2px #a8120c;
                             }
                             
                             #signin:hover{
                             -webkit-animation-name: jello;
                             animation-name: jello;
                             -webkit-animation-duration: 1s;
                             animation-duration: 1s;
                             background-color: #a8120c;
                             }
                             
                             #signin:active {
                             background-color: #a8120c;
                             box-shadow: 0 2px #a8120c;
                             transform: translateY(4px);
                             }

 #Login {
                                     background-color: #a8120c;
                             text-align: center;
                             cursor: pointer;
                             outline: none;
                             color: #fff;
                             border: none;
                             border-radius: 15px;
                             box-shadow: 0 2px #a8120c;
                             }
                             
                             #Login:hover{
                             -webkit-animation-name: jello;
                             animation-name: jello;
                             -webkit-animation-duration: 1s;
                             animation-duration: 1s;
                             background-color: #a8120c;
                             }
                             
                             #Login:active {
                             background-color: #a8120c;
                             box-shadow: 0 2px #a8120c;
                             transform: translateY(4px);
                             }

 #logout {
                                     background-color: #a8120c;
                             text-align: center;
                             cursor: pointer;
                             outline: none;
                             color: #fff;
                             border: none;
                             border-radius: 15px;
                             box-shadow: 0 2px #a8120c;
                             }
                             
                             #logout:hover{
                             -webkit-animation-name: jello;
                             animation-name: jello;
                             -webkit-animation-duration: 1s;
                             animation-duration: 1s;
                             background-color: #a8120c;
                             }
                             
                             #logout:active {
                             background-color: #a8120c;
                             box-shadow: 0 2px #a8120c;
                             transform: translateY(4px);
                             }

 #combtn {
                                     background-color: #a8120c;
                             text-align: center;
                             cursor: pointer;
                             outline: none;
                             color: #fff;
                             border: none;
                             border-radius: 15px;
                             box-shadow: 0 2px #a8120c;
                             }
                             
                             #combtn:hover{
                             -webkit-animation-name: jello;
                             animation-name: jello;
                             -webkit-animation-duration: 1s;
                             animation-duration: 1s;
                             background-color: #a8120c;
                             }
                             
                             #combtn:active {
                             background-color: #a8120c;
                             box-shadow: 0 2px #a8120c;
                             transform: translateY(4px);
                             }

                                                 /* logo */
                                                 .skin-red .main-header .logo {
                                                 background-color: #a8120c;
                                                 }
                                                 
                                                 /* logo when hovered */
                                                 .skin-red .main-header .logo:hover {
                                                 background-color: #a8120c;
                                                 }
                                                 
                                                 /* navbar (rest of the header) */
                                                 .skin-red .main-header .navbar {
                                                 background-color: #a8120c;
                                                 }        
                                                 
                                                 /* main sidebar */
                                                 .skin-red .main-sidebar {
                                                 background:#100F0D;

                                                 }
                                                 
                                                 /* active selected tab in the sidebarmenu */
                                                 .skin-red .main-sidebar .sidebar .sidebar-menu .active a{
                                                 background-color: #484848;
                                                 color:white;

                                                 }
                                                 
                                                 /* other links in the sidebarmenu */
                                                 .skin-red .main-sidebar .sidebar .sidebar-menu a{
                                                 color: white;
                                                 }
                                                 
                                                 /* other links in the sidebarmenu when hovered */
                                                 .skin-red .main-sidebar .sidebar .sidebar-menu a:hover{
                                                 background-color: #484848;
opacity: 0.25;

                                                 }
                                                 /* toggle button when hovered  */                    
                                                 .skin-red .main-header .navbar .sidebar-toggle:hover{
                                                 background-color: #484848;
                                                 }


.bolt2 {color:black}
.bolt1 {color:black}
.fa-compass {color:black}
.bolt3 {color:#a8120c}
#dpLog {
                                                 background-color: white;
                             }
                             
                             #dpLog:hover{
                             -webkit-animation-name: jello;
                             animation-name: jello;
                             -webkit-animation-duration: 1s;
                             animation-duration: 1s;
                             background-color: #0084b4;
                             }
                             
                             #dpLog:active {
                             background-color: White;
                             
                             }


                                                 #fb {
                                                 background-color: white;
                                                 }
                                                 
                                                 #fb:hover{
                                                 -webkit-animation-name: jello;
                                                 animation-name: jello;
                                                 -webkit-animation-duration: 1s;
                                                 animation-duration: 1s;
                                                 background-color: #0084b4;
                                                 }
                                                 
                                                 #fb:active {
                                                 background-color: White;
                                                 
                                                 }

                                                 #in1 {
                                                 background-color: white;
                                                 }
                                                 
                                                 #in1:hover{
                                                 -webkit-animation-name: jello;
                                                 animation-name: jello;
                                                 -webkit-animation-duration: 1s;
                                                 animation-duration: 1s;
                                                 background-color: #00aced;
                                                 }
                                                 
                                                 #in1:active {
                                                 background-color: White;
                                                 
                                                 }
                                                 #twit {
                                                 background-color: white;
                                                 }
                                                 
                                                 #twit:hover{
                                                 -webkit-animation-name: jello;
                                                 animation-name: jello;
                                                 -webkit-animation-duration: 1s;
                                                 animation-duration: 1s;
                                                 background-color:  #1dcaff;
                                                 }
                                                 
                                                 #twit:active {
                                                 background-color: White;
                                                 
                                                 }
                                                 #ggleplus {
                                                 background-color: white;
                                                 }
                                                 
                                                 #ggleplus:hover{
                                                 -webkit-animation-name: jello;
                                                 animation-name: jello;
                                                 -webkit-animation-duration: 1s;
                                                 animation-duration: 1s;
                                                 background-color: #CC3333;
                                                 }
                                                 
                                                 #ggleplus:active {
                                                 background-color: White;
                                                 
                                                 }

                                                 #dpBalance {
                                                 background-color: white;
                                                 }
                             
                             #dpBalance:hover{
                             -webkit-animation-name: jello;
                             animation-name: jello;
                             -webkit-animation-duration: 1s;
                             animation-duration: 1s;
                             background-color: White;
                             }
                             
                             #dpBalance:active {
                             background-color: White;
                             
                             }
#dpBilan {
background-color: white;
                             }
                             
                             #dpBilan:hover{
                             -webkit-animation-name: jello;
                             animation-name: jello;
                             -webkit-animation-duration: 1s;
                             animation-duration: 1s;
                             background-color: White;
                             }
                             
                             #dpBilan:active {
                             background-color: White;
                             
                             }
#dpPdt {
background-color: white;
                             }
                             
                             #dpPdt:hover{
                             -webkit-animation-name: jello;
                             animation-name: jello;
                             -webkit-animation-duration: 1s;
                             animation-duration: 1s;
                             background-color: White;
                             }
                             
                             #dpPdt:active {
                             background-color: White;
                             
                             }
#dpTmap {
background-color: white;
                             }
                             
                             #dpTmap:hover{
                             -webkit-animation-name: jello;
                             animation-name: jello;
                             -webkit-animation-duration: 1s;
                             animation-duration: 1s;
                             background-color: White;
                             }
                             
                             #dpTmap:active {
                             background-color: White;
                             
                             }
#dpMap {
background-color: white;
                             }
                             
                             #dpMap:hover{
                             -webkit-animation-name: jello;
                             animation-name: jello;
                             -webkit-animation-duration: 1s;
                             animation-duration: 1s;
                             background-color: White;
                             }
                             
                             #dpMap:active {
                             background-color: White;
                             
                             }
#dpOkBalance {
background-color: #CC3333;
                             }
                             
                             #dpOkBalance:hover{
                             -webkit-animation-name: jello;
                             animation-name: jello;
                             -webkit-animation-duration: 1s;
                             animation-duration: 1s;
                             background-color: #CC3333;
                             }
                             
                             #dpOkBalance:active {
                             background-color: #CC3333;
                             
                             }
#dpOkBilan {
background-color: #CC3333;
}

#dpOkBilan:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: #CC3333;
}

#dpOkBilan:active {
background-color: #CC3333;

}
#dpOkPdt {
background-color: #CC3333;
}

#dpOkPdt:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: #CC3333;
}

#dpOkPdt:active {
background-color: #CC3333;

}
#dpOkTmap {
background-color: #CC3333;
}

#dpOkTmap:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: #CC3333;
}

#dpOkTmap:active {
background-color: #CC3333;

}
#dpOkMap {
background-color: #CC3333;
}

#dpOkMap:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: #CC3333;
}

#dpOkMap:active {
background-color: #CC3333;

}
#dpHelpBalance {
background-color: white;
}

#dpHelpBalance:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: White;
}

#dpHelpBalance:active {
background-color: White;

}
#dpHelpBilan {
background-color: white;
}

#dpHelpBilan:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: White;
}

#dpHelpBilan:active {
background-color: White;

}
#dpHelpPdt {
background-color: white;
}

#dpHelpPdt:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: White;
}

#dpHelpPdt:active {
background-color: White;

}
#dpHelpTmap {
background-color: white;
}

#dpHelpTmap:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: White;
}

#dpHelpTmap:active {
background-color: White;

}
#dpHelpMap {
background-color: white;
}

#dpHelpMap:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: White;
}

#dpHelpMap:active {
background-color: White;

}
::-webkit-input-placeholder { /* WebKit browsers */
    color:    red;
opacity: 1 !important;
}

#DnldBilan {
                                    background-color: #100F0D;
text-align: center;
cursor: pointer;
outline: none;
color: #fff;
border: none;
border-radius: 15px;
box-shadow: 0 2px #100F0D;
}

#DnldBilan:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: #100F0D;
}

#DnldBilan:active {
background-color: #100F0D;
box-shadow: 0 2px #100F0D;
transform: translateY(4px);
}   

#DnldImport {
background-color: #100F0D;
text-align: center;
cursor: pointer;
outline: none;
color: #fff;
border: none;
border-radius: 15px;
box-shadow: 0 2px #100F0D;
}

#DnldImport:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: #100F0D;
}

#DnldImport:active {
background-color: #100F0D;
box-shadow: 0 2px #100F0D;
transform: translateY(4px);
}   
#DnldExport {
background-color: #100F0D;
text-align: center;
cursor: pointer;
outline: none;
color: #fff;
border: none;
border-radius: 15px;
box-shadow: 0 2px #100F0D;
}

#DnldExport:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: #100F0D;
}

#DnldExport:active {
background-color: #100F0D;
box-shadow: 0 2px #100F0D;
transform: translateY(4px);
}

#btn {
background-color: #100F0D;
text-align: center;
cursor: pointer;
outline: none;
color: #fff;
border: none;
border-radius: 15px;
box-shadow: 0 2px #100F0D;
}

#btn:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: #100F0D;
}

#btn:active {
background-color: #100F0D;
box-shadow: 0 2px #100F0D;
transform: translateY(4px);
}


#DnldBilanLy {
background-color: #100F0D;
text-align: center;
cursor: pointer;
outline: none;
color: #fff;
border: none;
border-radius: 15px;
box-shadow: 0 2px #100F0D;
}

#DnldBilanLy:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: #100F0D;
}

#DnldBilanLy:active {
background-color: #100F0D;
box-shadow: 0 2px #100F0D;
transform: translateY(4px);
}   

#DnldImportLy {
background-color: #100F0D;
text-align: center;
cursor: pointer;
outline: none;
color: #fff;
border: none;
border-radius: 15px;
box-shadow: 0 2px #100F0D;
}

#DnldImportLy:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: #100F0D;
}

#DnldImportLy:active {
background-color: #100F0D;
box-shadow: 0 2px #100F0D;
transform: translateY(4px);
}   
#DnldExportLy {
background-color: #100F0D;
text-align: center;
cursor: pointer;
outline: none;
color: #fff;
border: none;
border-radius: 15px;
box-shadow: 0 2px #100F0D;
}

#DnldExportLy:hover{
-webkit-animation-name: jello;
animation-name: jello;
-webkit-animation-duration: 1s;
animation-duration: 1s;
background-color: #100F0D;
}

#DnldExportLy:active {
background-color: #100F0D;
box-shadow: 0 2px #100F0D;
transform: translateY(4px);
}


                                                 
                           ")), tags$link(rel = "shortcut icon", href = "open.png")),
  tabItems(
              tabItem(tabName =  "Balance",chooseSliderSkin("Flat", color = "white")
                      , fluidPage(useSweetAlert(),
                                  box(width = 2,solidHeader = F, collapsible = T, status = "danger",background = "red",title ='Paramètres',
                                      div(style="display: inline-block;vertical-align:top; width: 50px;",div(style="color:black;",
                                                                                                             dropdownButton(inputId = "dpBalance",
                                                                                                       column(width = 12,style='border-right: 1px solid red;border-left: 1px solid red;border-top: 1px solid red;border-bottom: 1px solid red;',              
                                                                                                               awesomeRadio(status = "danger","choixPaysBalance","Pays",choices = c("Tous","Choisir Pays")),
                                                                                                               conditionalPanel(condition = "input.choixPaysBalance == 'Choisir Pays'",
                                                                                                                                selectInput("paysSelectBalance",NULL,choices = paysSelect, selected = paysSelect[1])
                                                                                                               ),
                                                                                                               awesomeRadio(status = "danger","choixProduitBalance","Produits",choices = c("Tous","Choisir Produits")),
                                                                                                               conditionalPanel(condition = "input.choixProduitBalance == 'Choisir Produits'",
                                                                                                                                awesomeCheckboxGroup(status = "danger","produitSelectBalance",NULL,choices = c('Sections ( NSH2 )','Chapitres ( NSH4 )','Produits ( NSH6 )')),
                                                                                                                                conditionalPanel(condition = "input.produitSelectBalance == 'Sections ( NSH2 )'",
                                                                                                                                                 selectInput('produitSelectBalanceNSH2', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])),
                                                                                                                                conditionalPanel(condition = "input.produitSelectBalance == 'Chapitres ( NSH4 )'",
                                                                                                                                                 selectInput('produitSelectBalanceNSH4', 'Chapitres ( NSH4 )', choices = sectionNames,selected =sectionNames[1])),
                                                                                                                                conditionalPanel(condition = "input.produitSelectBalance == 'Produits ( NSH6 )'",
                                                                                                                                                 selectInput('produitSelectBalanceNSH6', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])),
                                                                                                                                
                                                                                                                                conditionalPanel(condition = "input.produitSelectBalance.indexOf('Sections ( NSH2 )') == 0 & input.produitSelectBalance.indexOf('Chapitres ( NSH4 )') == 1 & input.produitSelectBalance.indexOf('Produits ( NSH6 )') != 2",
                                                                                                                                                 selectInput('produitSelectBalanceNSH22', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1]),
                                                                                                                                                 selectInput('produitSelectBalanceNSH44', 'Chapitres ( NSH4 )', choices = sectionNames,selected =sectionNames[1])
                                                                                                                                ),
                                                                                                                                conditionalPanel(condition = "input.produitSelectBalance.indexOf('Sections ( NSH2 )') == 0 & input.produitSelectBalance.indexOf('Chapitres ( NSH4 )') != 1 & input.produitSelectBalance.indexOf('Produits ( NSH6 )') == 1",
                                                                                                                                                 selectInput('produitSelectBalanceNSH26', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1]),
                                                                                                                                                 selectInput('produitSelectBalanceNSH626', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
                                                                                                                                ),
                                                                                                                                conditionalPanel(condition = "input.produitSelectBalance.indexOf('Sections ( NSH2 )') != 0 & input.produitSelectBalance.indexOf('Chapitres ( NSH4 )') == 0 & input.produitSelectBalance.indexOf('Produits ( NSH6 )') == 1",
                                                                                                                                                 selectInput('produitSelectBalanceNSH46', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1]),
                                                                                                                                                 selectInput('produitSelectBalanceNSH646', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
                                                                                                                                ),
                                                                                                                                conditionalPanel(condition = "input.produitSelectBalance.indexOf('Sections ( NSH2 )') == 0 & input.produitSelectBalance.indexOf('Chapitres ( NSH4 )') == 1 & input.produitSelectBalance.indexOf('Produits ( NSH6 )') == 2",
                                                                                                                                                 selectInput('produitSelectBalanceNSH266', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1]),
                                                                                                                                                 selectInput('produitSelectBalanceNSH466', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1]),
                                                                                                                                                 selectInput('produitSelectBalanceNSH666', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
                                                                                                                                )
                                                                                                               ),
                                                                                                               
                                                                                                               
                                                                                                               column(12,circleButton(size = "sm","dpOkBalance",icon = icon("check",class="bolt2")),align="right")),
                                                                                                               icon = icon("gear",class = "bolt2"),
                                                                                                               status =  "danger",width = "300px")
                                                                                                             )),
                                      div(style="display: inline-block;vertical-align:top; width: 10px;",h2()),
                                      div(style="display: inline-block;vertical-align:top; width: 50px;",dropdownButton(inputId = "dpHelpBalance", icon = icon("question-circle",class = "bolt2"))),
                                      hr(),div(actionButton("btnBalance","Visualiser",styleclass = "primary"),
                                                         shinyjs::hidden(
                                                           span(id = "jsbtnBalance",tags$img(src = "loader-gif-300-spinner-white.gif", width = "25px", height = "25px")))
                                      )),
                                  column(10,column(12,
                                                   uiOutput("boxImport"),
                                                   uiOutput("boxExport"),
                                                   uiOutput("boxBalance"),hr()),
                                         shinyjs::hidden(
                                           span(id = "jsbtnAnneeBalance",
                                         column(2),        
                                         column(10,htmlOutput("txtAnneeBalance"),align = "right"),       
                                         column(12,    
                                                
sliderTextInput("selectAnneeBalance",NULL,choices = c(1999:2017),grid = T, animate =  animationOptions(interval = 1000, loop = TRUE),width = "90%")))
                                        ),
                                         align = "center"),hr(),   
                                  column(12,hr(),plotlyOutput('BalancePlot',height = 600),align="center")
                      )
              ),
              tabItem(tabName =  "bilan",useShinyjs()
                                 , fluidPage(useSweetAlert(),
                                             box(width = 2,solidHeader = F, collapsible = T, status = "danger",background = "red",title ='Paramètres',
                                                 div(style="display: inline-block;vertical-align:top; width: 50px;",div(style="color:black;",
                                                                      dropdownButton(inputId = "dpBilan",
                                                                         column(width = 12,style='border-right: 1px solid red;border-left: 1px solid red;border-top: 1px solid red;border-bottom: 1px solid red;',
                                                                            awesomeRadio(status = "danger","choixPays","Pays",choices = c("Tous","Choisir Pays")),
                                                                            conditionalPanel(condition = "input.choixPays == 'Choisir Pays'",
                                                                                             selectInput("paysSelect",NULL,choices = paysSelect,multiple = T, selected = paysSelect[1])
                                                                            ),
                                                                            awesomeRadio(status = "danger","choixProduit","Produits",choices = c("Tous","Choisir Produits")),
                                                                            conditionalPanel(condition = "input.choixProduit == 'Choisir Produits'",
                                                                                             awesomeCheckboxGroup(status = "danger","produitSelect",NULL,choices = c('Sections ( NSH2 )','Chapitres ( NSH4 )','Produits ( NSH6 )')),
                                                                                             conditionalPanel(condition = "input.produitSelect == 'Sections ( NSH2 )'",
                                                                                                              selectInput('produitSelectNSH2', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1])),
                                                                                             conditionalPanel(condition = "input.produitSelect == 'Chapitres ( NSH4 )'",
                                                                                                              selectInput('produitSelectNSH4', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected =sectionNames[1])),
                                                                                             conditionalPanel(condition = "input.produitSelect == 'Produits ( NSH6 )'",
                                                                                                              selectInput('produitSelectNSH6', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])),
                                                                                             
                                                                                             conditionalPanel(condition = "input.produitSelect.indexOf('Sections ( NSH2 )') == 0 & input.produitSelect.indexOf('Chapitres ( NSH4 )') == 1 & input.produitSelect.indexOf('Produits ( NSH6 )') != 2",
                                                                                                              selectInput('produitSelectNSH22', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1]),
                                                                                                              selectInput('produitSelectNSH44', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected =sectionNames[1])
                                                                                             ),
                                                                                             conditionalPanel(condition = "input.produitSelect.indexOf('Sections ( NSH2 )') == 0 & input.produitSelect.indexOf('Chapitres ( NSH4 )') != 1 & input.produitSelect.indexOf('Produits ( NSH6 )') == 1",
                                                                                                              selectInput('produitSelectNSH26', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1]),
                                                                                                              selectInput('produitSelectNSH626', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])
                                                                                             ),
                                                                                             conditionalPanel(condition = "input.produitSelect.indexOf('Sections ( NSH2 )') != 0 & input.produitSelect.indexOf('Chapitres ( NSH4 )') == 0 & input.produitSelect.indexOf('Produits ( NSH6 )') == 1",
                                                                                                              selectInput('produitSelectNSH46', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected = sectionNames[1]),
                                                                                                              selectInput('produitSelectNSH646', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])
                                                                                             ),
                                                                                             conditionalPanel(condition = "input.produitSelect.indexOf('Sections ( NSH2 )') == 0 & input.produitSelect.indexOf('Chapitres ( NSH4 )') == 1 & input.produitSelect.indexOf('Produits ( NSH6 )') == 2",
                                                                                                              selectInput('produitSelectNSH266', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1]),
                                                                                                              selectInput('produitSelectNSH466', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected = sectionNames[1]),
                                                                                                              selectInput('produitSelectNSH666', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])
                                                                                             )
                                                                            ),column(12,circleButton(size = "sm","dpOkBilan",icon = icon("check",class="bolt2")),align="right")),
                                                                            icon = icon("gear",class = "bolt2"),
                                                                            status =  "danger", width = "300px"))),
                                                 div(style="display: inline-block;vertical-align:top; width: 10px;",h2()),
                                                 div(style="display: inline-block;vertical-align:top; width: 50px;",dropdownButton(inputId = "dpHelpBilan", icon = icon("question-circle",class = "bolt2"),
                                                                                                                             status =  "danger", width = "300px")),
                                                   hr(),div(actionButton("btnBilan","Visualiser",styleclass = "primary"),
                                                                       shinyjs::hidden(
                                                                         span(id = "jsbtnbilan",tags$img(src = "loader-gif-300-spinner-white.gif", width = "25px", height = "25px")))
                                                    )),
                                             ###Bar chart showing the calories in the leaf of the partition chart
                                             column(width = 10 ,plotlyOutput('BilanPlot',height = 600),
                                                    align = "center")
                                 )
                        ),
              tabItem(tabName = "pdt",useShinyjs()
                                 , fluidPage(useSweetAlert(),
                                             box(width = 2,solidHeader = F, collapsible = T, status = "danger",background = "red",title ='Paramètres',
                                                 div(style="display: inline-block;vertical-align:top; width: 50px;",div(style="color : black;",
                                                                         dropdownButton(inputId = "dpPdt",
                                                                           column(width = 12,style='border-right: 1px solid red;border-left: 1px solid red;border-top: 1px solid red;border-bottom: 1px solid red;',
                                                                            awesomeRadio(status = "danger","pdtRadio","Produits",choices = c("Tous","Choisir Produits")),
                                                                            conditionalPanel(condition = "input.pdtRadio == 'Choisir Produits'",
                                                                            awesomeCheckboxGroup(status = "danger",'produitGlobal',label = NULL,choices = c('Sections ( NSH2 )','Chapitres ( NSH4 )','Produits ( NSH6 )')),
                                                                            conditionalPanel(condition = "input.produitGlobal == 'Sections ( NSH2 )'",
                                                                                             selectInput('produitSpecNSH2', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1])),
                                                                            conditionalPanel(condition = "input.produitGlobal == 'Chapitres ( NSH4 )'",
                                                                                             selectInput('produitSpecNSH4', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected =sectionNames[1])),
                                                                            conditionalPanel(condition = "input.produitGlobal == 'Produits ( NSH6 )'",
                                                                                             selectInput('produitSpecNSH6', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])),
                                                                            
                                                                            conditionalPanel(condition = "input.produitGlobal.indexOf('Sections ( NSH2 )') == 0 & input.produitGlobal.indexOf('Chapitres ( NSH4 )') == 1 & input.produitGlobal.indexOf('Produits ( NSH6 )') != 2",
                                                                                             selectInput('produitSpecNSH22', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1]),
                                                                                             selectInput('produitSpecNSH44', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected =sectionNames[1])
                                                                            ),
                                                                            conditionalPanel(condition = "input.produitGlobal.indexOf('Sections ( NSH2 )') == 0 & input.produitGlobal.indexOf('Chapitres ( NSH4 )') != 1 & input.produitGlobal.indexOf('Produits ( NSH6 )') == 1",
                                                                                             selectInput('produitSpecNSH26', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1]),
                                                                                             selectInput('produitSpecNSH626', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])
                                                                            ),
                                                                            conditionalPanel(condition = "input.produitGlobal.indexOf('Sections ( NSH2 )') != 0 & input.produitGlobal.indexOf('Chapitres ( NSH4 )') == 0 & input.produitGlobal.indexOf('Produits ( NSH6 )') == 1",
                                                                                             selectInput('produitSpecNSH46', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected = sectionNames[1]),
                                                                                             selectInput('produitSpecNSH646', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])
                                                                            ),
                                                                            conditionalPanel(condition = "input.produitGlobal.indexOf('Sections ( NSH2 )') == 0 & input.produitGlobal.indexOf('Chapitres ( NSH4 )') == 1 & input.produitGlobal.indexOf('Produits ( NSH6 )') == 2",
                                                                                             selectInput('produitSpecNSH266', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1]),
                                                                                             selectInput('produitSpecNSH466', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected = sectionNames[1]),
                                                                                             selectInput('produitSpecNSH666', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])
                                                                            )),
                                                                            
                                                                            column(12,circleButton(size = "sm","dpOkPdt",icon = icon("check",class="bolt2")),align="right")),
                                                                             icon = icon("gear",class = "bolt2"),
                                                                            status =  "danger", width = "300px"))),
                                                 div(style="display: inline-block;vertical-align:top; width: 10px;",h2()),
                                                 div(style="display: inline-block;vertical-align:top; width: 50px;",dropdownButton(inputId = "dpHelpPdt", icon = icon("question-circle",class = "bolt2"),
                                                                                                                             status =  "danger", width = "300px"
                                                                                                                             )),
                                                    hr(),div(actionButton("btnprod","Visualiser",styleclass = "primary")
                                                                       ,
                                                                       shinyjs::hidden(
                                                                         span(id = "jsbtnprod",tags$img(src = "loader-gif-300-spinner-white.gif", width = "25px", height = "25px")))
                                                    )),
                                             column(2),
                                             
                                             shinyjs::hidden(
                                             span(id = "jsAnneePdt",column(9,htmlOutput("txtAnneePdt"),
                                             align="right"),column(1), 
                                             column(10,sliderTextInput("selectAnnee",NULL,choices = c(1999:2017),grid = T, animate =  animationOptions(interval = 3000, loop = TRUE),width = "90%"),align = "center"))),
                                             ###Bar chart showing the calories in the leaf of the partition chart
                                             column(12,hr(),
                                             column(width = 6 ,uiOutput('ProduitPlotBar'),
                                                    align = "center"),
                                             column(width = 6 ,uiOutput('ProduitPlotcam')
                                                    ,align = "center"))
                                             
                                             )
                                 
                        ),
              tabItem(tabName =  "tmap",useShinyjs(),
                                 fluidPage(useSweetAlert(),
                                           box(width = 2,solidHeader = F, collapsible = T, status = "danger",background = "red",title = "Paramètres",
                                               div(style="display: inline-block;vertical-align:top; width: 50px;",div(style="color : black;",
                                                                               dropdownButton(inputId = "dpTmap",
                                                                                 column(width = 12,style='border-right: 1px solid red;border-left: 1px solid red;border-top: 1px solid red;border-bottom: 1px solid red;',
                                                                                  awesomeRadio(status = "danger","tmapsRadio","Produits",choices = c("Tous","Choisir Produits")),
                                                                                  conditionalPanel(condition = "input.tmapsRadio == 'Choisir Produits'",
                                                                                          awesomeCheckboxGroup(status = "danger",'parNSH',label = NULL,choices = c('Sections ( NSH2 )','Chapitres ( NSH4 )','Produits ( NSH6 )')),
                                                                                          conditionalPanel(condition = "input.parNSH == 'Sections ( NSH2 )'",
                                                                                                           selectInput('produitSpecNSH2Tm', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1])),
                                                                                          conditionalPanel(condition = "input.parNSH == 'Chapitres ( NSH4 )'",
                                                                                                           selectInput('produitSpecNSH4Tm', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected = sectionNames[1])),
                                                                                          conditionalPanel(condition = "input.parNSH == 'Produits ( NSH6 )'",
                                                                                                           selectInput('produitSpecNSH6Tm', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected = allProduits[1])),
                                                                                          
                                                                                          conditionalPanel(condition = "input.parNSH.indexOf('Sections ( NSH2 )') == 0 & input.parNSH.indexOf('Chapitres ( NSH4 )') == 1 & input.parNSH.indexOf('Produits ( NSH6 )') != 2",
                                                                                                           selectInput('produitSpecNSH22Tm', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1]),
                                                                                                           selectInput('produitSpecNSH44Tm', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected =sectionNames[1])
                                                                                          ),
                                                                                          
                                                                                          conditionalPanel(condition = "input.parNSH.indexOf('Sections ( NSH2 )') == 0 & input.parNSH.indexOf('Chapitres ( NSH4 )') != 1 & input.parNSH.indexOf('Produits ( NSH6 )') == 1",
                                                                                                           selectInput('produitSpecNSH26Tm', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1]),
                                                                                                           selectInput('produitSpecNSH626Tm', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])
                                                                                          ),
                                                                                          conditionalPanel(condition = "input.parNSH.indexOf('Sections ( NSH2 )') != 0 & input.parNSH.indexOf('Chapitres ( NSH4 )') == 0 & input.parNSH.indexOf('Produits ( NSH6 )') == 1",
                                                                                                           selectInput('produitSpecNSH46Tm', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected = sectionNames[1]),
                                                                                                           selectInput('produitSpecNSH646Tm', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])
                                                                                          ),
                                                                                          conditionalPanel(condition = "input.parNSH.indexOf('Sections ( NSH2 )') == 0 & input.parNSH.indexOf('Chapitres ( NSH4 )') == 1 & input.parNSH.indexOf('Produits ( NSH6 )') == 2",
                                                                                                           selectInput('produitSpecNSH266Tm', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1]),
                                                                                                           selectInput('produitSpecNSH466Tm', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected = sectionNames[1]),
                                                                                                           selectInput('produitSpecNSH666Tm', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])
                                                                                          )),awesomeRadio(status = "danger",'chart_type_cal', 'Type Treemap', c('sunburst','circle_treemap','treemap','icicle','partition_chart'),selected = 'treemap'),
                                                                                  column(12,circleButton(size = "sm","dpOkTmap",icon = icon("check",class="bolt2")),align="right")),       
                                                                                  icon = icon("gear",class = "bolt2"),
                                                                                          status =  "danger", width = "300px"
                                                                                          ))),
                                               div(style="display: inline-block;vertical-align:top; width: 10px;",h2()),
                                               div(style="display: inline-block;vertical-align:top; width: 50px;",dropdownButton(inputId = "dpHelpTmap", icon = icon("question-circle",class = "bolt2"),
                                                                                                                           status =  "danger", width = "300px"
                                                                                                                          ),align = "center"),
                                                                  hr(),div(actionButton("btntmap","Visualiser",styleclass = "primary"),
                                                                                     shinyjs::hidden(
                                                                                       span(id = "jsbtntmap",tags$img(src = "loader-gif-300-spinner-white.gif", width = "25px", height = "25px")))
                                                                  )),
                                           column(2),
                                           shinyjs::hidden(
                                             span(id = "jsAnneeTMap",column(9,htmlOutput("txtAnneeTMap"),
                                                  align="right"),column(1),
                                           column(2),
                                           column(10,sliderTextInput("selectAnneeTMap",NULL,choices = c(1999:2017),grid = T, animate =  animationOptions(interval = 3000, loop = TRUE),width = "90%"),align = "center")))
                                           ,
                                           ###Bar chart showing the calories in the leaf of the partition chart
                                           column(12,htmlOutput("uniteTmap"), D3partitionROutput('tmapPlot',height = 600))
                                            )
                        ),
              tabItem(tabName = "map",useShinyjs(),
                                 fluidPage(useSweetAlert(),
                                           box(width = 2,solidHeader = TRUE, collapsible = T, status = "danger",background = "red",title ="Paramètres",
                                               div(style="display: inline-block;vertical-align:top; width: 50px;",div(style="color : black;",
                                                                              dropdownButton(inputId = "dpMap",
                                                                                 column(width = 12,style='border-right: 1px solid red;border-left: 1px solid red;border-top: 1px solid red;border-bottom: 1px solid red;',
                                                                                  awesomeRadio(status = "danger","mapsRadio","Produits",choices = c("Tous","Choisir Produits")),
                                                                                  conditionalPanel(condition = "input.mapsRadio == 'Tous'",awesomeRadio(status = "danger","mapRadio","Type de visualisation",choices = c("Couleurs","Icones"),selected = "Couleurs",inline = T)),
                                                                                  conditionalPanel(condition = "input.mapsRadio == 'Choisir Produits'",
                                                                                          awesomeCheckboxGroup(status = "danger",'parNSHmap',label = NULL,choices = c('Sections ( NSH2 )','Chapitres ( NSH4 )','Produits ( NSH6 )')),
                                                                                          conditionalPanel(condition = "input.parNSHmap == 'Sections ( NSH2 )'",
                                                                                                           selectInput('produitSpecNSH2map', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1])),
                                                                                          conditionalPanel(condition = "input.parNSHmap == 'Chapitres ( NSH4 )'",
                                                                                                           selectInput('produitSpecNSH4map', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected = sectionNames[1])),
                                                                                          conditionalPanel(condition = "input.parNSHmap == 'Produits ( NSH6 )'",
                                                                                                           selectInput('produitSpecNSH6map', 'Produits ( NSH6 )', choices = sectionNames,multiple = T,selected = sectionNames[1])),
                                                                                          
                                                                                          conditionalPanel(condition = "input.parNSHmap.indexOf('Sections ( NSH2 )') == 0 & input.parNSHmap.indexOf('Chapitres ( NSH4 )') == 1 & input.parNSHmap.indexOf('Produits ( NSH6 )') != 2",
                                                                                                           selectInput('produitSpecNSH22map', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1]),
                                                                                                           selectInput('produitSpecNSH44map', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected =sectionNames[1])
                                                                                          ),
                                                                                          
                                                                                          conditionalPanel(condition = "input.parNSHmap.indexOf('Sections ( NSH2 )') == 0 & input.parNSHmap.indexOf('Chapitres ( NSH4 )') != 1 & input.parNSHmap.indexOf('Produits ( NSH6 )') == 1",
                                                                                                           selectInput('produitSpecNSH26map', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1]),
                                                                                                           selectInput('produitSpecNSH626map', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])
                                                                                          ),
                                                                                          conditionalPanel(condition = "input.parNSHmap.indexOf('Sections ( NSH2 )') != 0 & input.parNSHmap.indexOf('Chapitres ( NSH4 )') == 0 & input.parNSHmap.indexOf('Produits ( NSH6 )') == 1",
                                                                                                           selectInput('produitSpecNSH46map', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected = sectionNames[1]),
                                                                                                           selectInput('produitSpecNSH646map', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])
                                                                                          ),
                                                                                          conditionalPanel(condition = "input.parNSHmap.indexOf('Sections ( NSH2 )') == 0 & input.parNSHmap.indexOf('Chapitres ( NSH4 )') == 1 & input.parNSHmap.indexOf('Produits ( NSH6 )') == 2",
                                                                                                           selectInput('produitSpecNSH266map', 'Sections ( NSH2 )', choices = chapterNames,multiple = T,selected = chapterNames[1]),
                                                                                                           selectInput('produitSpecNSH466map', 'Chapitres ( NSH4 )', choices = sectionNames,multiple = T,selected = sectionNames[1]),
                                                                                                           selectInput('produitSpecNSH666map', 'Produits ( NSH6 )', choices = allProduits,multiple = T,selected =allProduits[1])
                                                                                          )),
                                                                                          
                                                                                  column(12,circleButton(size = "sm","dpOkMap",icon = icon("check",class="bolt2")),align="right")),
                                                                                          icon = icon("gear",class = "bolt2"),
                                                                                          status =  "danger", width = "300px"
                                                                                          ))),
                                               div(style="display: inline-block;vertical-align:top; width: 10px;",h2()),
                                               div(style="display: inline-block;vertical-align:top; width: 50px;",dropdownButton(inputId = "dpHelpMap", icon = icon("question-circle",class = "bolt2"),
                                                                                                                           status =  "danger", width = "300px"),align="center"),
                                                                  hr(),div(actionButton("btnmap","Visualiser",styleclass = "primary"),
                                                                                     shinyjs::hidden(
                                                                                       span(id = "jsbtnmap",tags$img(src = "loader-gif-300-spinner-white.gif", width = "25px", height = "25px")))
                                                                  )),
                                           ###Bar chart showing the calories in the leaf of the partition chart
                                           
                                           column(2),
                                           shinyjs::hidden(
                                             span(id = "jsAnneeMap",column(9,htmlOutput("txtAnneeMap"),
                                             align="right"),column(1),
                                           column(2),
                                           column(10,fluidPage(
                                             
                                             sliderTextInput("selectAnneeMap",NULL,choices = c(1999:2017),grid = T, animate =  animationOptions(interval = 4000, loop = TRUE),width = "90%")    
                                             
                                           ),align = "center"))),
                                           column(12,fluidPage(htmlOutput("uniteMap"),leafletOutput('mapPlot',height = 600)))
                                 )),

tabItem(tabName =  "gif",
        box(width = 3,solidHeader = TRUE, collapsible = T, status = "danger",title ="Paramètres",
            awesomeRadio("radioYears",NULL,choices = c("Toutes les Années","Par Année"),status = "danger"),
            conditionalPanel(condition = "input.radioYears == 'Par Année'",
                             selectInput("selectYear","Année",choices = c(1999:2017))
                             ),
            awesomeRadio("radioContinent","Par",choices = c("Pays","Continents"),status = "danger",inline = T),
            awesomeCheckboxGroup("produitSelectGif","Paramètres",choices = c('Sections ( NSH2 )','Chapitres ( NSH4 )','Produits ( NSH6 )'),status = "danger"),
            conditionalPanel(condition = "input.produitSelectGif == 'Sections ( NSH2 )'",
                             selectInput('produitSelectGifNSH2', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1])),
            conditionalPanel(condition = "input.produitSelectGif == 'Chapitres ( NSH4 )'",
                             selectInput('produitSelectGifNSH4', 'Chapitres ( NSH4 )', choices = sectionNames,selected =sectionNames[1])),
            conditionalPanel(condition = "input.produitSelectGif == 'Produits ( NSH6 )'",
                             selectInput('produitSelectGifNSH6', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])),
            
            conditionalPanel(condition = "input.produitSelectGif.indexOf('Sections ( NSH2 )') == 0 & input.produitSelectGif.indexOf('Chapitres ( NSH4 )') == 1 & input.produitSelectGif.indexOf('Produits ( NSH6 )') != 2",
                             selectInput('produitSelectGifNSH22', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1]),
                             selectInput('produitSelectGifNSH44', 'Chapitres ( NSH4 )', choices = sectionNames,selected =sectionNames[1])
            ),
            conditionalPanel(condition = "input.produitSelectGif.indexOf('Sections ( NSH2 )') == 0 & input.produitSelectGif.indexOf('Chapitres ( NSH4 )') != 1 & input.produitSelectGif.indexOf('Produits ( NSH6 )') == 1",
                             selectInput('produitSelectGifNSH26', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1]),
                             selectInput('produitSelectGifNSH626', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
            ),
            conditionalPanel(condition = "input.produitSelectGif.indexOf('Sections ( NSH2 )') != 0 & input.produitSelectGif.indexOf('Chapitres ( NSH4 )') == 0 & input.produitSelectGif.indexOf('Produits ( NSH6 )') == 1",
                             selectInput('produitSelectGifNSH46', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1]),
                             selectInput('produitSelectGifNSH646', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
            ),
            conditionalPanel(condition = "input.produitSelectGif.indexOf('Sections ( NSH2 )') == 0 & input.produitSelectGif.indexOf('Chapitres ( NSH4 )') == 1 & input.produitSelectGif.indexOf('Produits ( NSH6 )') == 2",
                             selectInput('produitSelectGifNSH266', 'Sections ( NSH2 )', choices = chapterNames,selected = chapterNames[1]),
                             selectInput('produitSelectGifNSH466', 'Chapitres ( NSH4 )', choices = sectionNames,selected = sectionNames[1]),
                             selectInput('produitSelectGifNSH666', 'Produits ( NSH6 )', choices = allProduits,selected =allProduits[1])
            )
            ,div(style="display: inline-block;width: 100px",actionButton("btn","Visualiser",styleclass = "primary")) ,
            div(style="display: inline-block;",shinyjs::hidden(
              span(id = "jsbtn",tags$img(src = "loader.gif", width = "30px", height = "30px")))
            )
            
            
           
            ),
            box(width = 9,solidHeader = T, collapsible = T, status = "danger",title =tags$b('Import'),
                fluidPage(shinyjs::hidden(
                  span(id = "jsImport",
                       
                       conditionalPanel(
                         condition = "input.radioYears == 'Toutes les Années' ",
                         imageOutput("imageImport"),
                         downloadButton('DnldImport',"Télécharger")
                         ),
                       conditionalPanel(
                         condition = "input.radioYears != 'Toutes les Années' ",
                         imageOutput("imageImportLy"),
                         downloadButton('DnldImportLy',"Télécharger")
                       )))),align = "center"
            ),
            column(3),
            box(width = 9,solidHeader = T, collapsible = T, status = "danger",title =tags$b('Export'),
                fluidPage( shinyjs::hidden(
                  span(id = "jsExport",
                       conditionalPanel(
                         condition = "input.radioYears == 'Toutes les Années' ",
                         imageOutput("imageExport"),
                         downloadButton('DnldExport',"Télécharger")
                       ),
                       conditionalPanel(
                         condition = "input.radioYears != 'Toutes les Années' ",
                         imageOutput("imageExportLy"),
                         downloadButton('DnldExportLy',"Télécharger")
                       )
                       ))),align = "center"
            ),
            column(3),
            box(width = 9,solidHeader = T, collapsible = T, status = "danger",title =tags$b('Bilan'),
                fluidPage( shinyjs::hidden(
                  span(id = "jsBilan",
                    conditionalPanel(
                         condition = "input.radioYears == 'Toutes les Années' ",
                         imageOutput("imageBilan"),
                         downloadButton('DnldBilan',"Télécharger")
                       ),
                    
                    conditionalPanel(
                      condition = "input.radioYears != 'Toutes les Années' ",
                      imageOutput("imageBilanLy"),
                      downloadButton('DnldBilanLy',"Télécharger")
                    )))),align = "center"
            ),
            column(3)
            )
        ,

tabItem(tabName = "home",
        column(6,
               tags$img(src="wordcloud.png"),
               tags$img(src = "homegif.gif",width = "399px",height = "237px")
               ),
        column(6,tags$span(style="color:#CC3333",tags$b(h3("Créer un compte"))),
               column(6,textInput("nom","Nom",NULL,width = "250")),
               column(6,textInput("prenom","Prénom",NULL,width = "250")),
               column(8,
                      column(4,selectInput("date1", "Jour",choices = c(1:31))),
                      column(4,selectInput("date2", "Mois",choices = c("Janv","Fév","Mars","Avril","Mai","Juin","Juil","Aout","Sept","Oct","Nov","Déc"))),
                      column(4,selectInput("date3", "Année",choices = c(1950:1999)))
                      ),
               column(4,selectInput("sexe","Sexe",choices = c("Homme","Femme"))),
               column(12,textInput("mail","E-mail",NULL,width = "250")),
               column(12,textInput("identifiant","Identifiant",NULL,width = "250")),
               column(6,passwordInput("mdp1","Mot de passe",NULL,width = "250")),
               column(6,passwordInput("mdp2","Confirmer Mot de passe",NULL,width = "250")),
               column(6,tags$img(src = "op.png")),
               column(6,actionButton("signin",label =list('Sign-in',icon('sign-in')),styleclass = "primary"),
                      shinyjs::hidden(span(id="jssignin",tags$img(src = "mainloding3.gif", width = "50px", height = "42px"))),align="right")
               
               
               )
        
        ),tabItem(tabName = "comments",
                  box(width=4,title = "Feedback",solidHeader = TRUE, collapsible = F,status = "danger",height = 400,
                      fluidPage(br(),
                                htmlOutput("comnom"),
                                textAreaInput("comtxt",NULL,placeholder = "Laissez votre commentaire ici...",value = "",width = "280",height = "100px"),
                                actionButton("combtn","Commenter"),
                                shinyjs::hidden(
                                  span(id = "submit_msg_comment",
                                       tags$img(src = "mainloding3.gif", width = "50px", height = "42px"))
                                  ,span(id = "error_msg_comment",
                                        tags$span(style="color:red",tags$b("Commentaire"),"est vide !"))
                                )
                                ,br(),br(),
                                tags$b(tags$span(style="color:#B33","Votre Evaluation de l'application")),br(),
                                div(style="display: inline-block;vertical-align:top;",ratingInput("Rating", label=NULL, dataStop=5,dataFractions=2)),
                                div(style="display: inline-block;vertical-align:top;",tags$span(style="color:white",tags$b("kkk"))),
                                div(style="display: inline-block;vertical-align:top;",tags$b(textOutput("ratetxt")))
                                
                                
                                
                      )),
                  box(title = "Anciens commentaires",width = 8,solidHeader = TRUE, collapsible = F,status = "danger",height = 400,
                      DT::dataTableOutput("comentdata"),align="center" )
                  
        )


)
                        
                        
                      ))
                      
))