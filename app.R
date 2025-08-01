#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(shinydashboard)
library(gt)
library(ffscrapr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(webshot2)
library(pagedown)
library(curl)
library(showtext)
library(lubridate)

ui <- fixedPage(
  
  HTML('<meta name="viewport" content="width=770">'),
  
  theme = shinytheme("yeti"),
  
  # CSS ------------------------------------------------------------------------
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Fjalla+One&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Rubik+Mono+One&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=VT323&display=swap');
      
      body {
        font-family: 'Roboto Condensed';
        font-size: 13px;
        line-height: 1;
      }
      
      .navbar-static-top {
        border-radius: 0px;
        border: #e52534 0px solid;
      }
      
      .navbar-brand {
        font-size: 14px;
        font-style: normal;
        color: #FFFFFF!important;
      }
      
      .navbar {
        font-size: 14px;
        font-weight: 300;
        margin-left: 15px;
        margin-right: 15px;
      }
      
      .navbar-toggle {
      margin-right: 30px;
      }
      
      .navbar-default {
      background-color: #000000;
      }
      
      .navbar-default .navbar-collapse, .navbar-default .navbar-form {
    margin-right: 15px;
      }
    
    .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover, .navbar-default .navbar-nav>.active>a:focus {
    color: #ffffff;
    background-color: #e52534;
    }
    
    .navbar-default .navbar-nav>li>a:focus, .navbar-default .navbar-nav>li>a:hover {
    color: #FFFFFF;
    background-color: #222222;
    border: black 0px solid;
    }
    
    .navbar .navbar-toggle:hover .icon-bar {
    background-color: #000000;
    }
      
      .container {
                  margin: 0 auto;
                  padding: 0;
                  min-width: 730px;
                  max-width: 730px;
                  width: auto !important;
                  }
                  .container-fluid {
                  min-width: 730px;
                  max-width: 730px;
                  width: auto !important;
                  }

      h2 {
        font-size: 55px;
        font-family: 'Fjalla One';
        padding-left: 0px;
        margin-top: 15px;
        margin-left: 15px;
        color: #e52534;
        font-weight: normal;
        margin-bottom:5px;
      }
      
      h3 {
        font-size: 20px;
        font-family: 'Fjalla One';
        line-height:18px;
        padding:2px;
        margin-top: -5px;
        margin-bottom: 5px;
        margin-left: 15px;
        margin-right:15px;
        color: #000000;
        font-weight: normal;
      }
      
      h4 {
        font-size: 20px;
        line-height:1;
        font-weight: normal;
        font-family: 'Fjalla One';
        color: #000000;
      }
      
      a {
    color: #000000;
    text-decoration: none;
      }
      
      a:hover, a:focus {
    color: #e52534;
    text-decoration: underline;
      }
    
    h3 a, h3 a:hover, h3 a:focus {
    font-style:italic;
    }
      
      div#p_name {
    font-family: 'Fjalla One';
    color: black;
    font-size: 33px;
      }
      
      div#p_info {
      font-family: 'Fjalla One';
      font-size: 23px;
      color: #000000;
      padding-top: 3px;
      margin-bottom: 5px;
      }
      
      h5 {
      margin-top: 0px;
      margin-bottom: 0px;
      }
      
      .table>thead>tr>th, .table>tbody>tr>th, .table>tfoot>tr>th, .table>thead>tr>td, .table>tbody>tr>td, .table>tfoot>tr>td {
        padding: 3px;
        line-height: 0.9;
        font-size:14px;
      }
      
      .table>thead>tr>th, .table>tbody>tr>th, .table>tfoot>tr>th {
        font-size: 9px;
      }
      
      .table.dataTable tbody td.active, .table.dataTable tbody tr.active td {
        background-color: #9bc1e6 !important;
        color: black!important;
      }
      
      table.dataTable.table-hover>tbody>tr:hover>* {
        box-shadow: inset 0 0 0 9999px #CCCCCC !important;
      }
      
      .input-sm {
        height: 26px;
        padding: 0px 5px;
        font-size: 12px;
        line-height: 1.5;
        border-radius: 0;
      }
      
      select.input-sm {
        height: 26px;
        line-height: 26px;
      }
      
      .dataTables_wrapper .dataTables_length {
      margin-top:10px;
        float: left;
      }

      .dataTables_wrapper .dataTables_filter {
      margin-top:10px;
        float: right;
        text-align: right;
      }
      
      .col-xs-1, .col-sm-1, .col-md-1, .col-lg-1, .col-xs-2, .col-sm-2, .col-md-2, .col-lg-2, .col-xs-3, .col-sm-3, .col-md-3, .col-lg-3, .col-xs-4, .col-sm-4, .col-md-4, .col-lg-4, .col-xs-5, .col-sm-5, .col-md-5, .col-lg-5, .col-xs-6, .col-sm-6, .col-md-6, .col-lg-6, .col-xs-7, .col-sm-7, .col-md-7, .col-lg-7, .col-xs-8, .col-sm-8, .col-md-8, .col-lg-8, .col-xs-9, .col-sm-9, .col-md-9, .col-lg-9, .col-xs-10, .col-sm-10, .col-md-10, .col-lg-10, .col-xs-11, .col-sm-11, .col-md-11, .col-lg-11, .col-xs-12, .col-sm-12, .col-md-12, .col-lg-12 {
        position: relative;
        min-height: 1px;
        padding-left: 0px;
        padding-right: 0px;
      }
      
      table.dataTable tr.dtrg-group.dtrg-level-0 th {
    font-weight: normal!important;
    background-color: #000000;
    text-align: left;
    color: #FFFFFF;
    font-size: 12px;
    padding-top:5px;
    padding-bottom:5px;
    }
    
    .well {
    min-height: 20px;
    padding: 10px;
    height: 80px;
    margin-bottom: 0px;
    margin-top: 10px;
    background-color: #fafafa;
    border-radius: 0;
    -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,0.05);
    box-shadow: inset 0 1px 1px rgba(0,0,0,0.05);
    }
    
    .irs {
      font-family: 'Roboto Condensed';
      font-weight: bold;
    }

    .btn {
      padding: 4px 12px;
      margin-bottom: 10px;
    }
    
    .form-group {
      margin-bottom: -10px;
    }
    
    .row {
      margin-left: 0px;
      margin-right: 0px;
    }
    
    .content-wrapper, .main-footer, .right-side {
    margin-left: 0px;
    margin-top: 10px;
    background-color: #FFFFFF;
    }
    
    .small-box h3 {
      font-family: 'Fjalla One';
      font-size: 25px;
      margin: 3px 0 5px 0;
      font-weight: normal;
      color:white;
      background-color:transparent;
    }
      
      .small-box p {
      font-size: 16px;
      }

    .small-box {height: 70px;
    margin:3px;
    border-radius:10px;}
    
    .small-box>.inner {
    padding: 7px;}
    
    .small-box .icon-large {
    position: absolute;
    top: auto;
    bottom: 0px;
    right: 5px;
    font-size: 60px;
    color: rgba(0, 0, 0, 0.2);}
    
    .content {
    padding: 0px;
    }
    
    .bg-yellow {
      background-color: #e5aa00!important;
    }
    
    .bg-orange {
      background-color: #ff7700!important;
    }
    
    .pagination>.active>a, .pagination>.active>span, .pagination>.active>a:hover, .pagination>.active>span:hover, .pagination>.active>a:focus, .pagination>.active>span:focus {
    z-index: 3;
    color: #FFFFFF;
    background-color: #e52534;
    border-color: transparent;
    cursor: default;
    }

    .pagination>li>a:hover, .pagination>li>span:hover, .pagination>li>a:focus, .pagination>li>span:focus {
    z-index: 2;
    color: #e52534;
    background-color: #eeeeee;
    border-color: transparent;
    }

    .form-control:focus {
    border-color: #9bc1e6;
    outline: 0;
    -webkit-box-shadow: inset 0 1px 1px rgb(0 0 0 / 8%), 0 0 8px rgb(155 193 230 / 60%);
    box-shadow: inset 0 1px 1px rgb(0 0 0 / 8%), 0 0 8px rgb(155 193 230 / 60%);
    }

    /*** Works on common browsers ***/
    ::selection {
    background-color: #9bc1e6;
    color: #000000;
    }

    /*** Mozilla based browsers ***/
    ::-moz-selection {
    background-color: #9bc1e6;
    color: #000000;
    }

    /***For Other Browsers ***/
    ::-o-selection {
    background-color: #9bc1e6;
    color: #000000;
    }

    ::-ms-selection {
    background-color: #9bc1e6;
    color: #000000;
    }

    /*** For Webkit ***/
    ::-webkit-selection {
    background-color: #9bc1e6;
    color: #000000;
    }

    .selectize-control.multi .selectize-input > div.active {
    background: #9bc1e6;
    color: #000000;
    border: 0 solid rgba(0, 0, 0, 0);
    }

    .selectize-input.focus {
    border-color: #9bc1e6;
    outline: 0;
    -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.075), 0 0 8px rgba(155, 193, 230, 0.6);
    box-shadow: inset 0 1px 1px rgba(0,0,0,.075), 0 0 8px rgba(155, 193, 230, 0.6);
    } 

    .gt_table {
    margin-left: 0px!important;
    }
    
    .irs--shiny .irs-handle {
    top: 20px;
    width: 22px;
    height: 22px;
    border: 1px solid #ababab;
    background-color: #dedede;
    box-shadow: 1px 1px 3px rgb(0 0 0 / 30%);
    border-radius: 22px;
    }

    .irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover {
    background: #ffffff;
    }

    .irs--shiny .irs-bar {
    top: 27px;
    height: 8px;
    border-top: 1px solid #e52534;
    border-bottom: 1px solid #e52534;
    background: #e52534;
    }

    .irs--shiny .irs-line {
    top: 27px;
    height: 8px;
    background: linear-gradient(to bottom, #dedede -50%, #fff 150%);
    background-color: #ededed;
    border: 1px solid #cccccc;
    border-radius: 8px;
    }

    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    color: #fff;
    text-shadow: none;
    padding: 1px 3px;
    background-color: #e52534;
    border-radius: 3px;
    font-size: 12px;
    line-height: 1.333;
    }

    .irs--shiny .irs-min, .irs--shiny .irs-max {
    top: 0;
    padding: 1px 3px;
    text-shadow: none;
    background-color: rgba(0, 0, 0, 0.1);
    border-radius: 3px;
    font-size: 11px;
    line-height: 1.333;
    }
    
    .nav-tabs>li.active>a, .nav-tabs>li.active>a:hover, .nav-tabs>li.active>a:focus {
    color: #000000;
    background-color: #ffffff;
    border: 1px solid #000000;
    border-bottom-color: transparent;
    cursor: default;
    }

    .nav-tabs {
    border-bottom: 1px solid #000000;
    }

    .nav-tabs>li>a {
    background-color: #000000;
    color: #FFFFFF;
    }

    .nav>li>a:hover, .nav>li>a:focus {
    text-decoration: none;
    background-color: #222222;
    color: white;
    border: #000000 1px solid;
    }
    
    .info-box-content {
    padding: 5px 10px;
    margin-left: 45px;
    }

    .info-box {
    display: block;
    min-height: 45px;
    background: #fff;
    width: 100%;
    box-shadow: 0 0px 0px rgb(0 0 0 / 10%);
    border-radius: 5px;
    margin-bottom: 16px;
    border: black 2px solid;
    }

    .info-box-icon {
    border-top-left-radius: 5px;
    border-top-right-radius: 0;
    border-bottom-right-radius: 0;
    border-bottom-left-radius: 5px;
    display: block;
    float: left;
    height: 45px;
    width: 45px;
    text-align: center;
    font-size: 25px;
    line-height: 48px;
    background: rgba(0,0,0,.2);
    margin-top: -1px;
    margin-left: -1px;
    }

    .col-sm-3 {
    position: relative;
    min-height: 1px;
    padding-left: 3px;
    padding-right: 3px;
    }
    
     .bar-chart-bar {
          background-color: #FFFFFF;
          background-size: 20px 20px;
          display: block;
          position:relative;
          width: 100%;
          height: 16px;
          border: #000000 1px solid;
     }
      
      .bar {
          background-color: #e52534;
          background-image: repeating-linear-gradient(45deg, transparent, transparent 3px, rgba(255,255,255,.1) 3px, rgba(255,255,255,.1) 6px);
          float: left;
          height: 100%;
          
      }
      
      "))
    
  ),
  

  # Layout ---------------------------------------------------------------------
  
  useShinydashboard(),  
  
  titlePanel("96X8 Draft Results",
             title = tags$head(tags$link(rel="shortcut icon", 
                                         href="favicon.ico"
                                         ))),
  
  h2("96X8 DRAFT RESULTS"),
  
  h3(textOutput("time")),
  
    navbarPage(
      
      title = 'Menu',
      
      tabPanel('Dashboard',
               tabsetPanel(
                 tabPanel("Draft Info",
                          dashboardBody(
                            withSpinner(
                            tagList(valueBoxOutput("qb1",width = 6),
                                    valueBoxOutput("qb2",width = 6),
                                    valueBoxOutput("rb1",width = 6),
                                    valueBoxOutput("rb2",width = 6),
                                    valueBoxOutput("wr1",width = 6),
                                    valueBoxOutput("wr2",width = 6),
                                    valueBoxOutput("te1",width = 6),
                                    valueBoxOutput("te2",width = 6),
                                    valueBoxOutput("pk1",width = 6),
                                    valueBoxOutput("pk2",width = 6),
                                    valueBoxOutput("most",width = 6),
                                    valueBoxOutput("least",width = 6),
                                    valueBoxOutput("adpd",width = 6),
                                    valueBoxOutput("adpv2",width = 6)),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)
                          )
                 ),
                 tabPanel("Draft Progress",
                          br(),
                          withSpinner(
                            DT::dataTableOutput('dprog'),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)
                 )
              )
      ),
      
      tabPanel('ADP', 
               tabsetPanel(
                 tabPanel("ALL",
                          withSpinner(
                            DT::dataTableOutput('adp_all'),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)),
                 tabPanel("QB",
                          withSpinner(
                            DT::dataTableOutput('adp_qb'),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)),
                 tabPanel("RB",
                          withSpinner(
                            DT::dataTableOutput('adp_rb'),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)),
                 tabPanel("WR",
                          withSpinner(
                            DT::dataTableOutput('adp_wr'),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)),
                 tabPanel("TE",
                          withSpinner(
                            DT::dataTableOutput('adp_te'),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)),
                 tabPanel("PK",
                          withSpinner(
                            DT::dataTableOutput('adp_pk'),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3))
               )
      ),
      
      tabPanel('Player Queries', 
               tabsetPanel(
                 tabPanel("Single Player",
                          br(),
                          selectizeInput("single", "Select Player:",
                                         choices = "",
                                         selected = "",
                                         multiple = FALSE
                          ),
                          actionButton('update_single', 'UPDATE'),
                          dashboardBody(
                            withSpinner(
                              tagList(
                                h5(textOutput("p_name")),
                                h5(textOutput("p_info")),
                                infoBoxOutput("p_adp", width = 3),
                                infoBoxOutput("p_high", width = 3),
                                infoBoxOutput("p_low", width = 3),
                                infoBoxOutput("p_count", width = 3),
                                DT::dataTableOutput('sp')
                              ),
                              type = 8, 
                              color = "#e52534", 
                              color.background ="#FFFFFF", 
                              size = 1.3)
                          )
                 ),
                 
                 tabPanel("Compare Players",
                          br(),
                          selectizeInput("comp", "Select Up to 4 Players:",
                                         choices = "",
                                         selected = "",
                                         options = list(maxItems = 4)
                          ),
                          actionButton('update_comp', 'UPDATE'),
                          withSpinner(
                            tagList(
                              DT::dataTableOutput('adp_comp'),
                              plotOutput("player_comp")
                            ),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)
                 ),
                 
                 tabPanel("Multi Player",
                          br(),
                          selectizeInput("multi", "Select Up to 4 Players:",
                                         choices = "",
                                         selected = "",
                                         options = list(maxItems = 4)
                          ),
                          actionButton('update_multi', 'UPDATE'),
                          withSpinner(
                            tagList(
                              DT::dataTableOutput('mp')
                            ),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)
                 )
               )
      ),
      
      tabPanel('My Team/Draftboard',
               tabsetPanel(
                 tabPanel("My Team",
                          br(),
                          selectizeInput("team", "Select Franchise:",
                                         choices = "",
                                         selected = "",
                                         multiple = FALSE
                          ),
                          actionButton('update_team', 'UPDATE'),
                          downloadButton('teamimage','SAVE AS IMAGE'),
                          fluidRow(column(12,withSpinner(
                            gt_output(outputId = "myteam"),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)
                          ))),
                 tabPanel("Draftboard",
                          br(),
                          selectizeInput("div", "Select Division:",
                                         choices = "",
                                         selected = "",
                                         multiple = FALSE
                          ),
                          actionButton('update_draft', 'UPDATE'),
                          downloadButton('dbimage','SAVE AS IMAGE'),
                          fluidRow(column(12,withSpinner(
                            gt_output(outputId = "draftb"),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)
                          )))
                 )
      ),
      
      tabPanel('Frivolities', 
               tabsetPanel(
                 tabPanel("ADP Value",
                          sidebarPanel(
                            sliderInput("slider1", "Filter by Round:", 1, 20, c(1,20),step=1,ticks=FALSE,),
                            width = 12
                          ),
                          withSpinner(
                            DT::dataTableOutput('adpv'),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)),
                 tabPanel("Roster Construction",
                          sidebarPanel(
                            sliderInput("slider2", "Filter by Round:", 1, 20, c(1,20),step=1,ticks=FALSE,),
                            width = 12
                          ),
                          withSpinner(
                            DT::dataTableOutput('roscon'),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)),
                 tabPanel("Time to Draft",
                          withSpinner(
                            DT::dataTableOutput('ttd_table'),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)
                 )
               )
      ),
      
      tabPanel('Projections', 
               tabsetPanel(
                 tabPanel("Divisional",
                          withSpinner(
                            DT::dataTableOutput('stand_div'),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)
                          ),
                 tabPanel("Overall",
                          withSpinner(
                            DT::dataTableOutput('stand_ovr'),
                            type = 8, 
                            color = "#e52534", 
                            color.background ="#FFFFFF", 
                            size = 1.3)
                 )
               )
      ),
      
      collapsible = TRUE
      
    )
)

# End of UI -------------------------------------------------------------------

server <- function(input, output, session) {
  
  # INPUTS ---------------------------------------------------------------------

  fsn96 <- mfl_connect(2025,16055)
  
  js <- '
function(data, type, row, meta) {
  return $("<div></div>", {
    class: "bar-chart-bar"
  })
    .append(
      $("<div></div>", {
        class: "bar"
      }).css({
        width: (100*data) + "%"
      })
    )
    .prop("outerHTML");
}
'
  
  ### Pull Picks and Latest Pick Time ------------------------------------------
  
  #Pull Draft Picks
  draft <- ff_draft(fsn96) %>% 
    mutate_at(c('round'), as.numeric) %>%
    mutate(draft_pick = paste(round, pick, sep = "."),
           player = sub("(^.*),\\s(.*$)","\\2 \\1", player_name)) %>%
    select(timestamp, franchise = franchise_name, div = division_name, draft_pick, round, pick, overall, player, pos, team) %>%
    group_by(player) %>%
    add_count(name = "count") %>%
    mutate(avg = ifelse(is.na(player), NA, (sum(overall) + (241 * (8-count))) / 8),
           high = ifelse(is.na(player), NA, min(overall)),
           low = ifelse(is.na(player), NA, max(overall)),
           stdev = ifelse(is.na(player), NA, ifelse(is.na(sd(overall,na.rm=TRUE)), 0, sd(overall,na.rm=TRUE))),
           adp_value = ifelse(is.na(player), NA, overall - avg)
    ) %>%
    ungroup() %>%
    group_by(div,pos) %>%
    mutate(div_pos_rk = rank(overall, ties.method = "min"),
           pos_and_rk = paste(pos, div_pos_rk, sep = "")) %>%
    group_by(franchise,div,pos) %>%
    mutate(tm_pos_rk = rank(overall, ties.method = "min")) %>%
    ungroup() %>%
    mutate(div_pos_rk = ifelse(is.na(player),NA,div_pos_rk),
           pos_and_rk = ifelse(is.na(player),NA,pos_and_rk),
           tm_pos_rk = ifelse(is.na(player),NA,tm_pos_rk)
    )
  
  #Get Latest Pick Time
  timestamp <- draft %>%
    arrange(desc(timestamp)) %>%
    select(timestamp) %>%
    head(1)
  
  timestamp2 <- paste0("Last Updated: ",format(as.POSIXct(timestamp$timestamp, format="%Y-%m-%d %H:%M:%S %p", tz="America/New_York"),format="%b %d %H:%M:%S %Z"))
  
  
  ### Drop Down List Choices (Player/Franchise/Div) ----------------------------
  
  playerlist <- distinct(draft %>%
                           filter(!is.na(player)) %>%
                           select(player) %>%
                           arrange(player)
  )
  
  updateSelectizeInput(session, 'single', choices = playerlist$player, server = TRUE)
  
  updateSelectizeInput(session, 'multi', choices = playerlist$player, server = TRUE)
  
  updateSelectizeInput(session, 'comp', choices = playerlist$player, server = TRUE)
  
  franchises <- ff_franchises(fsn96) %>%
    arrange(franchise_name) %>%
    select(franchise=franchise_name)
  
  updateSelectizeInput(session, 'team', choices = franchises$franchise, server = TRUE)
  
  divisions <- distinct(ff_franchises(fsn96) %>%
                          arrange(division) %>%
                          select(div=division_name))
  
  updateSelectizeInput(session, 'div', choices = divisions$div, server = TRUE)
  
  updateSelectizeInput(session, 'proj', choices = c("ALL",divisions$div), server = TRUE)
  
  ### Draft Info ---------------------------------------------------------------
 
  #Quarterbacks
  count_qb <- draft %>%
    filter(pos == "QB") %>%
    group_by(pos) %>%
    summarize(count = n()) %>%
    mutate(perteam = count / 96)
  
  latest_qb <- draft %>%
    filter(pos == "QB" & tm_pos_rk == 1) %>%
    arrange(-overall,-div_pos_rk) %>%
    select(franchise,div,draft_pick) %>%
    head(1)
  
  #Running Backs
  count_rb <- draft %>%
    filter(pos == "RB") %>%
    group_by(pos) %>%
    summarize(count = n()) %>%
    mutate(perteam = count / 96)
  
  latest_rb <- draft %>%
    filter(pos == "RB" & tm_pos_rk == 1) %>%
    arrange(-overall,-div_pos_rk) %>%
    select(franchise,div,draft_pick) %>%
    head(1)
  
  #Wide Receivers
  count_wr <- draft %>%
    filter(pos == "WR") %>%
    group_by(pos) %>%
    summarize(count = n()) %>%
    mutate(perteam = count / 96)
  
  latest_wr <- draft %>%
    filter(pos == "WR" & tm_pos_rk == 1) %>%
    arrange(-overall,-div_pos_rk) %>%
    select(franchise,div,draft_pick) %>%
    head(1)
  
  #Tight Ends
  count_te <- draft %>%
    filter(pos == "TE") %>%
    group_by(pos) %>%
    summarize(count = n()) %>%
    mutate(perteam = count / 96)
  
  latest_te <- draft %>%
    filter(pos == "TE" & tm_pos_rk == 1) %>%
    arrange(-overall,-div_pos_rk) %>%
    select(franchise,div,draft_pick) %>%
    head(1)
  
  #Kickers
  count_pk <- draft %>%
    filter(pos == "PK") %>%
    group_by(pos) %>%
    summarize(count = n()) %>%
    mutate(perteam = count / 96)
  
  earliest_pk <- draft %>%
    filter(pos == "PK" & tm_pos_rk == 1) %>%
    arrange(overall,div_pos_rk) %>%
    select(franchise,div,draft_pick) %>%
    head(1)
  
  #Teams Most Drafted
  most_team <- draft %>%
    filter(!is.na(team)) %>%
    group_by(team) %>%
    summarize(count = n()) %>%
    arrange(-count) %>%
    head(3)
  
  #Teams Least Drafted
  least_team <- draft %>%
    filter(team != "FA" & team != "FA*" & !is.na(team)) %>%
    group_by(team) %>%
    summarize(count = n()) %>%
    arrange(count) %>%
    head(3)
  
  #ADP Disparity Leader
  adp_disp <- unique(
    draft %>%
      filter(count == 8 & !is.na(player)) %>%
      mutate(diff = low - high) %>%
      select(player, high, low, diff) %>%
      arrange(-diff)
  ) %>%
    head(1)
  
  #ADP Value Leader
  adp_v <- distinct(draft %>%
                      filter(!is.na(player)) %>%
                      select(franchise,div,adp_value) %>%
                      group_by(franchise,div) %>%
                      add_count(name = 'picks') %>%
                      mutate(adp_value = sum(adp_value),
                             adp_value_per_pick = adp_value / picks) %>%
                      select(franchise,div,picks,adp_value,adp_value_per_pick) %>%
                      arrange(-adp_value) %>%
                      head(1)
  )
  
  ### Draft Progress -----------------------------------------------------------
  
  prog <- draft %>%
    filter(!is.na(player)) %>%
    select(div,draft_pick,timestamp,overall,franchise,player) %>%
    group_by(div) %>%
    mutate(remaining = 240 - overall,
           rank = rank(-overall),
           pct = overall / 240) %>%
    filter(rank == 1)
  
  prog_f <- left_join(divisions,prog) %>%
    select(div,draft_pick,timestamp,pct) %>%
    arrange(-pct,timestamp) %>%
    mutate(draft_pick = ifelse(is.na(draft_pick),"YTS",draft_pick),
           timestamp = as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%S", tz="America/New_York"),
           timestamp = format(timestamp, format="%b %d %H:%M:%S %Z"),
           pct = ifelse(is.na(pct),0,pct))
  
  ### ADP ----------------------------------------------------------------------
  adp <- distinct(draft %>% select(player,pos,team,avg,high,low,stdev,count)) %>%
    filter(!is.na(player)) %>%
    mutate(rk = rank(avg, ties.method = "min")) %>%
    group_by(pos) %>%
    mutate(pos_rk = rank(avg, ties.method = "min")) %>%
    ungroup() %>%
    arrange(avg, -count, high, player) %>%
    select(rk,player,pos,pos_rk,team:count)
  
  ### Single Player Query ------------------------------------------------------

  #Single Player Query
  single_player <- eventReactive(input$update_single,
                                  {draft %>%
                                      filter(player == input$single) %>%
                                      arrange(overall,div_pos_rk) %>%
                                      select(overall,draft_pick,pos_and_rk,franchise,div)
                                  })
  
  #Single Player Name
  player_name <- eventReactive(input$update_single,
                           {input$single})
  
  #Single Player Pos and Team
  player_info <- eventReactive(input$update_single, {
    paste(adp %>%
      filter(player == input$single) %>%
      mutate(info = paste0(pos,pos_rk," / ",team)) %>%
      select(info))
  })
  
  #Single Player ADP
  player_adp <- eventReactive(input$update_single, {
    paste(adp %>%
            filter(player == input$single) %>%
            mutate(avg = paste(format(avg,digits=2,nsmall=2))) %>%
            select(avg))
  })
  
  #Single Player High
  player_high <- eventReactive(input$update_single, {
    paste(draft %>%
            filter(player == input$single) %>%
            arrange(overall) %>%
            mutate(draft_pick = paste0(draft_pick," (",overall,")")) %>%
            select(draft_pick) %>%
            head(1))
  })
  
  #Single Player Low
  player_low <- eventReactive(input$update_single, {
    paste(draft %>%
            filter(player == input$single) %>%
            arrange(-overall) %>%
            mutate(draft_pick = paste0(draft_pick," (",overall,")")) %>%
            select(draft_pick) %>%
            head(1))
  })
  
  #Single Player Count
  player_count <- eventReactive(input$update_single, {
    adp %>%
      filter(player == input$single) %>%
      select(count)
  })
  
  ### Player Comparison --------------------------------------------------------
  
  font_add_google('Roboto Condensed', 'monospace')
  showtext_auto()
  
  comp_player <- eventReactive(input$update_comp, {
    draft %>%
      filter(player %in% input$comp) %>%
      select(player,overall,div,franchise,draft_pick) %>%
      mutate(player = toupper(player))
  })
  
  comp_count <- eventReactive(input$update_comp,{
    ifelse(length(input$comp) == 1,120,
           ifelse(length(input$comp) == 2,240,
                  ifelse(length(input$comp) == 3,360,480)))
  })
  
  comp_plot <- eventReactive(input$update_comp, {
    comp_player() %>%
      ggplot(
        aes(x = overall, y = div, color = player)) +
      geom_point(size = 5) +
      scale_color_manual(values=c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")) +
      facet_wrap(~ player, ncol = 1) +
      theme_bw() +
      geom_text(aes(label = player), x = 249, y = Inf, hjust = 1, vjust = 1.8, color = alpha('black', .75), size=15,family="monospace", check_overlap = T) +
      theme(plot.margin = margin(t = 1, r = 3, b = 2, l = 2),
            panel.border = element_rect(color = "black", fill=NA, linewidth=1),
            legend.position="none",
            text=element_text(family="monospace",size=32),
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size=24),
            axis.title.y = element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_line(color = "#666666",
                                              linewidth = 0.25,
                                              linetype = 1),
            panel.spacing.y = unit(0,"cm")) +
      scale_x_continuous(breaks=c(6,18,30,42,54,66,78,90,102,114,126,138,150,162,174,186,198,210,222,234),limits=c(0,240),labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),minor_breaks = c(0,12,24,36,48,60,72,84,96,108,120,132,144,156,168,180,192,204,216,228,240)) +
      scale_y_discrete(expand = expansion(mult = c(0.25, 0.25)),
                       limits = rev)
  })
  
  comp_adp <- eventReactive(input$update_comp, {
    adp %>%
      filter(player %in% input$comp)
  })
  
  ### Multi Player Query -------------------------------------------------------
  
  #Count Amount of Players
  multi_count <- eventReactive(input$update_multi,{
    length(input$multi)
  })
  
  #Multi Player Query
  multi_player <- eventReactive(input$update_multi,{
    draft %>%
    filter(player %in% input$multi) %>%
    group_by(franchise) %>%
    add_count(name = "fran_count") %>%
    filter(fran_count == multi_count()) %>%
    mutate(draft_pick = paste(draft_pick," / ",pos_and_rk)) %>%
    select(Franchise = franchise,Division = div,player,draft_pick) %>%
    pivot_wider(names_from = player, values_from = draft_pick)
    })
  
  ### My Team Data -------------------------------------------------------------
  
  #First Half of Team
  myteam1 <- eventReactive(input$update_team,{
    draft %>%
                 filter(franchise == input$team) %>%
                 filter(round <= 10) %>%
                 select(franchise,div,draft_pick,pos_and_rk,team,player) %>%
      mutate_at(c(4,5,6), ~replace_na(.,""))
  })
  
  #Second Half of Team
  myteam2 <- eventReactive(input$update_team,{
    draft %>%
                 filter(franchise == input$team) %>%
                 filter(round > 10) %>%
                 select(franchise,div,draft_pick,pos_and_rk,team,player) %>%
      mutate_at(c(4,5,6), ~replace_na(.,""))
  })
  
  ### My Team Tables -----------------------------------------------------------
  tabfunc <- function(data, ...){
    data %>% 
      gt() %>% 
      opt_table_font(
        font = google_font(name = "Roboto Condensed")) %>%
      tab_options(column_labels.hidden = TRUE,
                  table.margin.left = "0px",
                  table.margin.right = "0px",
                  table.border.top.width = px(0),
                  table.border.top.color = "black",
                  table.border.bottom.width = px(0),
                  table.border.bottom.color = "black",
                  table_body.border.top.width = px(0),
                  table_body.border.top.color = "black",
                  table_body.border.bottom.width = px(0),
                  table_body.border.bottom.color = "black",
                  table_body.hlines.width = px(2),
                  table_body.hlines.color = "black",
                  data_row.padding = px(3),
                  container.padding.x = px(0),
                  container.padding.y = px(0),
                  ) %>%
      cols_width(
        starts_with("player") ~ px(185),
        everything() ~ px(50)
      ) %>%
      tab_style(
        style = list(
          cell_text(color = "black"),
          cell_text(align = "left"),
          cell_text(size = "17px")
        ),
        locations = cells_body()
      ) %>%
      tab_style(
        style = cell_fill(color = "#FFFFFF"),
        locations = cells_body()
      ) %>%
      tab_style(
        style = list(cell_text(color = "white"),
                     cell_fill(color = "#dd4b39")),
        locations = cells_body(rows = grepl('QB', pos_and_rk))
      ) %>%
      tab_style(
        style = list(cell_text(color = "white"),
                     cell_fill(color = "#0073b7")),
        locations = cells_body(rows = grepl('RB', pos_and_rk))
      ) %>%
      tab_style(
        style = list(cell_text(color = "white"),
                     cell_fill(color = "#00a65a")),
        locations = cells_body(rows = grepl('WR', pos_and_rk))
      ) %>%
      tab_style(
        style = list(cell_text(color = "white"),
                     cell_fill(color = "#e5aa00")),
        locations = cells_body(rows = grepl('TE', pos_and_rk))
      ) %>%
      tab_style(
        style = list(cell_text(color = "white"),
                     cell_fill(color = "#ff7700")),
        locations = cells_body(rows = grepl('PK', pos_and_rk))
      ) %>%
      as_raw_html()
  }
  
  mt1 <- eventReactive(input$update_team,{myteam1() %>%
      select(draft_pick,pos_and_rk,team,player) %>%
      tabfunc()
  })
  
  mt2 <- eventReactive(input$update_team,{myteam2() %>%
      select(draft_pick,pos_and_rk,team,player) %>%
      tabfunc()
  })
  
  mt <- eventReactive(input$update_team,{data.frame(mt1(), mt2())})
  
  mtfinal <- eventReactive(input$update_team,{mt() %>%
      gt() %>%
      fmt_markdown(columns = everything()) %>%
      tab_options(column_labels.hidden = TRUE,
                  table.background.color = "#000000",
                  heading.align = "right",
                  heading.title.font.size = "40px",
                  heading.title.font.weight = "normal",
                  heading.subtitle.font.size = "20px",
                  heading.padding = "0px",
                  heading.background.color = "#222222",
                  heading.border.bottom.width = px(1),
                  heading.border.bottom.color = "black",
                  table.border.top.width = px(3),
                  table.border.top.color = "black",
                  table.border.bottom.width = px(3),
                  table.border.bottom.color = "black",
                  data_row.padding = px(0),
                  data_row.padding.horizontal = px(0),
                  table_body.border.top.width = px(0),
                  table_body.border.top.color = NULL,
                  table_body.border.bottom.width = px(0),
                  table_body.border.bottom.color = NULL,
      ) %>%
      opt_table_font(
        font = google_font(name = "Roboto Condensed")
      ) %>%
      tab_header(
        title = unique(myteam1()$franchise),
        subtitle = paste("96X8 - ",unique(myteam1()$div)," Division")
      ) %>%
      tab_style(
        style = cell_text(font = google_font("Fjalla One")),
        location = cells_title()
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "left", color = "black", weight = px(3)
        ),
        location = list(
          cells_title(),
          cells_body(columns = 1)
        )
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "left", color = "black", weight = px(2)
        ),
        location = list(
          cells_title(),
          cells_body(columns = 2)
        )
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "right", color = "black", weight = px(3)
        ),
        location = list(
          cells_title(),
          cells_body(columns = 2)
        )
      ) %>%
      opt_css(
        css = "
   .gt_heading .gt_title {
      background-image: url(https://i.imgur.com/PPaGH4S.png);
      background-size: 205px;
      background-repeat: no-repeat;
      background-position-y: 0px;
    }

.gt_heading .gt_subtitle {
      background-image: url(https://i.imgur.com/PPaGH4S.png);
      background-size: 205px;
      background-repeat: no-repeat;
      background-position-y: -50px;
  }
    "
      )
  })
  
  ### Draftboard Data ----------------------------------------------------------
  draftboard <- eventReactive(input$update_draft,{
    pivot_wider(draft %>%
                             filter(div == input$div) %>%
                             mutate(dpe = ifelse(is.na(player),
                                                 draft_pick,
                                                 paste0(player," (",draft_pick," / ",pos_and_rk," / ",team,")"))) %>%
                             select(franchise,round,dpe),
                names_from = franchise,
                values_from = dpe)
    })
  
  dnames <- c("round","tm1","tm2","tm3","tm4","tm5","tm6","tm7","tm8","tm9","tm10","tm11","tm12")
  
  draftboard2 <- eventReactive(input$update_draft,{
    db <- draftboard()
    colnames(db) <- c(dnames)
    db
    })
  
  ### Draftboard Table ---------------------------------------------------------
  dbfinal <- eventReactive(input$update_draft,{
    draftboard2() %>%
      select(tm1:tm12) %>%
      gt() %>%
      opt_table_font(
        font = google_font(name = "Roboto Condensed")) %>%
      cols_width(
        everything() ~ px(55)
      ) %>%
      tab_style(
        style = "height:25px;",
        locations = cells_body()
      ) %>%
      cols_label(
        tm1 = paste(colnames(draftboard())[2]),
        tm2 = paste(colnames(draftboard())[3]),
        tm3 = paste(colnames(draftboard())[4]),
        tm4 = paste(colnames(draftboard())[5]),
        tm5 = paste(colnames(draftboard())[6]),
        tm6 = paste(colnames(draftboard())[7]),
        tm7 = paste(colnames(draftboard())[8]),
        tm8 = paste(colnames(draftboard())[9]),
        tm9 = paste(colnames(draftboard())[10]),
        tm10 = paste(colnames(draftboard())[11]),
        tm11 = paste(colnames(draftboard())[12]),
        tm12 = paste(colnames(draftboard())[13])
      ) %>%
      tab_style(
        style = list(
          cell_text(color = "white"),
          cell_text(align = "left"),
          cell_text(size = "7px")
        ),
        locations = list(
          cells_body(),
          cells_column_labels()
        )
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#FFFFFF"),
          cell_text(color = "#000000"),
          cell_borders(sides = "all", color = "#000000", style = "solid", weight = px(1))
        ),
        locations = list(
          cells_body()
        )
      ) %>%
      tab_style(
        style = list(
          cell_text(color = "white"),
          cell_fill(color = "#dd4b39"),
          cell_borders(sides = "all", color = "#000000", style = "solid", weight = px(1))
        ),
        locations = list(
          cells_body(columns = tm1, rows = grepl("QB[[:digit:]]",tm1)),
          cells_body(columns = tm2, rows = grepl("QB[[:digit:]]",tm2)),
          cells_body(columns = tm3, rows = grepl("QB[[:digit:]]",tm3)),
          cells_body(columns = tm4, rows = grepl("QB[[:digit:]]",tm4)),
          cells_body(columns = tm5, rows = grepl("QB[[:digit:]]",tm5)),
          cells_body(columns = tm6, rows = grepl("QB[[:digit:]]",tm6)),
          cells_body(columns = tm7, rows = grepl("QB[[:digit:]]",tm7)),
          cells_body(columns = tm8, rows = grepl("QB[[:digit:]]",tm8)),
          cells_body(columns = tm9, rows = grepl("QB[[:digit:]]",tm9)),
          cells_body(columns = tm10, rows = grepl("QB[[:digit:]]",tm10)),
          cells_body(columns = tm11, rows = grepl("QB[[:digit:]]",tm11)),
          cells_body(columns = tm12, rows = grepl("QB[[:digit:]]",tm12))
        )
      ) %>%
      tab_style(
        style = list(
          cell_text(color = "white"),
          cell_fill(color = "#0073b7"),
          cell_borders(sides = "all", color = "#000000", style = "solid", weight = px(1))
        ),
        locations = list(
          cells_body(columns = tm1, rows = grepl("RB[[:digit:]]",tm1)),
          cells_body(columns = tm2, rows = grepl("RB[[:digit:]]",tm2)),
          cells_body(columns = tm3, rows = grepl("RB[[:digit:]]",tm3)),
          cells_body(columns = tm4, rows = grepl("RB[[:digit:]]",tm4)),
          cells_body(columns = tm5, rows = grepl("RB[[:digit:]]",tm5)),
          cells_body(columns = tm6, rows = grepl("RB[[:digit:]]",tm6)),
          cells_body(columns = tm7, rows = grepl("RB[[:digit:]]",tm7)),
          cells_body(columns = tm8, rows = grepl("RB[[:digit:]]",tm8)),
          cells_body(columns = tm9, rows = grepl("RB[[:digit:]]",tm9)),
          cells_body(columns = tm10, rows = grepl("RB[[:digit:]]",tm10)),
          cells_body(columns = tm11, rows = grepl("RB[[:digit:]]",tm11)),
          cells_body(columns = tm12, rows = grepl("RB[[:digit:]]",tm12))
        )
      ) %>%
      tab_style(
        style = list(
          cell_text(color = "white"),
          cell_fill(color = "#00a65a"),
          cell_borders(sides = "all", color = "#000000", style = "solid", weight = px(1))
        ),
        locations = list(
          cells_body(columns = tm1, rows = grepl("WR[[:digit:]]",tm1)),
          cells_body(columns = tm2, rows = grepl("WR[[:digit:]]",tm2)),
          cells_body(columns = tm3, rows = grepl("WR[[:digit:]]",tm3)),
          cells_body(columns = tm4, rows = grepl("WR[[:digit:]]",tm4)),
          cells_body(columns = tm5, rows = grepl("WR[[:digit:]]",tm5)),
          cells_body(columns = tm6, rows = grepl("WR[[:digit:]]",tm6)),
          cells_body(columns = tm7, rows = grepl("WR[[:digit:]]",tm7)),
          cells_body(columns = tm8, rows = grepl("WR[[:digit:]]",tm8)),
          cells_body(columns = tm9, rows = grepl("WR[[:digit:]]",tm9)),
          cells_body(columns = tm10, rows = grepl("WR[[:digit:]]",tm10)),
          cells_body(columns = tm11, rows = grepl("WR[[:digit:]]",tm11)),
          cells_body(columns = tm12, rows = grepl("WR[[:digit:]]",tm12))
        )
      ) %>%
      tab_style(
        style = list(
          cell_text(color = "white"),
          cell_fill(color = "#e5aa00"),
          cell_borders(sides = "all", color = "#000000", style = "solid", weight = px(1))
        ),
        locations = list(
          cells_body(columns = tm1, rows = grepl("TE[[:digit:]]",tm1)),
          cells_body(columns = tm2, rows = grepl("TE[[:digit:]]",tm2)),
          cells_body(columns = tm3, rows = grepl("TE[[:digit:]]",tm3)),
          cells_body(columns = tm4, rows = grepl("TE[[:digit:]]",tm4)),
          cells_body(columns = tm5, rows = grepl("TE[[:digit:]]",tm5)),
          cells_body(columns = tm6, rows = grepl("TE[[:digit:]]",tm6)),
          cells_body(columns = tm7, rows = grepl("TE[[:digit:]]",tm7)),
          cells_body(columns = tm8, rows = grepl("TE[[:digit:]]",tm8)),
          cells_body(columns = tm9, rows = grepl("TE[[:digit:]]",tm9)),
          cells_body(columns = tm10, rows = grepl("TE[[:digit:]]",tm10)),
          cells_body(columns = tm11, rows = grepl("TE[[:digit:]]",tm11)),
          cells_body(columns = tm12, rows = grepl("TE[[:digit:]]",tm12))
        )
      ) %>%
      tab_style(
        style = list(
          cell_text(color = "white"),
          cell_fill(color = "#ff7700"),
          cell_borders(sides = "all", color = "#000000", style = "solid", weight = px(1))
        ),
        locations = list(
          cells_body(columns = tm1, rows = grepl("PK[[:digit:]]",tm1)),
          cells_body(columns = tm2, rows = grepl("PK[[:digit:]]",tm2)),
          cells_body(columns = tm3, rows = grepl("PK[[:digit:]]",tm3)),
          cells_body(columns = tm4, rows = grepl("PK[[:digit:]]",tm4)),
          cells_body(columns = tm5, rows = grepl("PK[[:digit:]]",tm5)),
          cells_body(columns = tm6, rows = grepl("PK[[:digit:]]",tm6)),
          cells_body(columns = tm7, rows = grepl("PK[[:digit:]]",tm7)),
          cells_body(columns = tm8, rows = grepl("PK[[:digit:]]",tm8)),
          cells_body(columns = tm9, rows = grepl("PK[[:digit:]]",tm9)),
          cells_body(columns = tm10, rows = grepl("PK[[:digit:]]",tm10)),
          cells_body(columns = tm11, rows = grepl("PK[[:digit:]]",tm11)),
          cells_body(columns = tm12, rows = grepl("PK[[:digit:]]",tm12))
        )
      ) %>%
      tab_options(
        stub.background.color = "#111111",
        column_labels.background.color = "#111111",
        table.margin.left = "0px",
        table.margin.right = "0px",
        container.padding.x = px(0),
        container.padding.y = px(0),
        data_row.padding = px(2),
        table_body.border.bottom.width = px(3),
        table_body.border.bottom.color = "black",
        column_labels.border.bottom.width = px(3),
        column_labels.border.bottom.color = "black",
        column_labels.border.top.width = px(3),
        column_labels.border.top.color = "black"
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "left",
          color = "black",
          weight = px(3)
        ),
        location = list(
          cells_title(),
          cells_column_labels(columns = 1),
          cells_body(columns = 1)
        )
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "right",
          color = "black",
          weight = px(3)
        ),
        location = list(
          cells_title(),
          cells_column_labels(columns = 12),
          cells_body(columns = 12)
        )
      )
  })

  ### ADP Value ----------------------------------------------------------------
  adp_value <- reactive({distinct(draft %>%
                          filter(round >= input$slider1[1] & round <= input$slider1[2] & !is.na(player)) %>%
                          select(franchise,div,adp_value) %>%
                          group_by(franchise,div) %>%
                          add_count(name = 'picks') %>%
                          mutate(adp_value = sum(adp_value),
                                 adp_value_per_pick = adp_value / picks) %>%
                          select(franchise,div,picks,adp_value,adp_value_per_pick) %>%
                          arrange(-adp_value))
  })
  
  ### Roster Construction ------------------------------------------------------
  roster_cons <- reactive({draft %>%
    filter(round >= input$slider2[1] & round <= input$slider2[2] & !is.na(player)) %>%
    select(franchise,pos) %>%
    mutate(qb = ifelse(pos == "QB", 1, 0),
           rb = ifelse(pos == "RB", 1, 0),
           wr = ifelse(pos == "WR", 1, 0),
           te = ifelse(pos == "TE", 1, 0),
           pk = ifelse(pos == "PK", 1, 0)) %>%
      group_by(franchise) %>%
      summarize(across(qb:pk,sum)) %>%
      ungroup() %>%
      select(qb:pk) %>%
      group_by(qb,rb,wr,te,pk) %>%
      summarize(count = n()) %>%
      mutate(total = qb + rb + wr + te + pk,
             pct = count / 96 ) %>%
      select(total,qb:pk,count,pct) %>%
      arrange(-count,-qb,-rb,-wr,-te,-pk)
  })
  
  ### Time to Draft ------------------------------------------------------------
  
  ttd <- draft %>%
    filter(!is.na(player)) %>%
    select (timestamp,franchise,div,overall) %>%
    group_by(div) %>%
    mutate(ttd = ifelse(overall == 1,
                        difftime(timestamp, as.POSIXct("2025-08-01 14:00:18", format="%Y-%m-%d %H:%M:%S", tz="UTC"), units="sec"),
                        difftime(timestamp, lag(timestamp, default = first(timestamp)), units="sec")
    )
    ) %>%
    group_by(franchise,div) %>%
    summarize(count = n(),
              ttd = sum(ttd),
              ttd_per_pick = ttd / count) %>%
    arrange(ttd_per_pick,ttd,franchise) %>%
    mutate(ttd = hms::as_hms(ttd),
           ttd_per_pick = round(ttd_per_pick,0),
           ttd_per_pick = hms::as_hms(ttd_per_pick))
  
  ### Projections Tab ----------------------------------------------------------
  
  proj_stand_div <- read.csv("stand.csv") %>%
    arrange(division)
  
  proj_stand_ovr <- read.csv("stand.csv")

  # OUTPUTS --------------------------------------------------------------------
  
  message(curl::curl_version()) # check curl is installed
  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    chromote::set_default_chromote_object(
      chromote::Chromote$new(chromote::Chrome$new(
        args = c("--disable-gpu", 
                 "--no-sandbox", 
                 "--disable-dev-shm-usage", # required bc the target easily crashes
                 c("--force-color-profile", "srgb"))
      ))
    )
  }
  
  output$time = renderText({timestamp2})
  
  ### General Info Tab ---------------------------------------------------------
  
  output$qb1 <- renderValueBox({
    valueBox("TOTAL QB", 
             paste0(ifelse(nrow(count_qb) == 0,0,count_qb$count)," (",format(ifelse(nrow(count_qb) == 0,0,count_qb$perteam), digits = 2, nsmall = 2)," per team)"), 
             icon = icon("layer-group"), 
             color = "red")
  })
  
  output$qb2 <- renderValueBox({
    valueBox("LATEST QB1", 
             ifelse(nrow(latest_qb) == 0,"N/A",paste0(latest_qb$draft_pick," (",latest_qb$franchise,")")), 
             icon = icon("clock", class="fas"), 
             color = "red")
  })
  
  output$rb1 <- renderValueBox({
    valueBox("TOTAL RB", 
             paste0(ifelse(nrow(count_rb) == 0,0,count_rb$count)," (",format(ifelse(nrow(count_rb) == 0,0,count_rb$perteam), digits = 2, nsmall = 2)," per team)"), 
             icon = icon("layer-group"), 
             color = "blue")
  })
  
  output$rb2 <- renderValueBox({
    valueBox("LATEST RB1", 
             ifelse(nrow(latest_rb) == 0,"N/A",paste0(latest_rb$draft_pick," (",latest_rb$franchise,")")),
             icon = icon("clock", class="fas"), 
             color = "blue")
  })
  
  output$wr1 <- renderValueBox({
    valueBox("TOTAL WR", 
             paste0(ifelse(nrow(count_wr) == 0,0,count_wr$count)," (",format(ifelse(nrow(count_wr) == 0,0,count_wr$perteam), digits = 2, nsmall = 2)," per team)"), 
             icon = icon("layer-group"), 
             color = "green")
  })
  
  output$wr2 <- renderValueBox({
    valueBox("LATEST WR1", 
             ifelse(nrow(latest_wr) == 0,"N/A",paste0(latest_wr$draft_pick," (",latest_wr$franchise,")")),
             icon = icon("clock", class="fas"), 
             color = "green")
  })
  
  output$te1 <- renderValueBox({
    valueBox("TOTAL TE", 
             paste0(ifelse(nrow(count_te) == 0,0,count_te$count)," (",format(ifelse(nrow(count_te) == 0,0,count_te$perteam), digits = 2, nsmall = 2)," per team)"), 
             icon = icon("layer-group"), 
             color = "yellow")
  })
  
  output$te2 <- renderValueBox({
    valueBox("LATEST TE1", 
             ifelse(nrow(latest_te) == 0,"N/A",paste0(latest_te$draft_pick," (",latest_te$franchise,")")),
             icon = icon("clock", class="fas"), 
             color = "yellow")
  })
  
  output$pk1 <- renderValueBox({
    valueBox("TOTAL PK", 
             paste0(ifelse(nrow(count_pk) == 0,0,count_pk$count)," (",format(ifelse(nrow(count_pk) == 0,0,count_pk$perteam), digits = 2, nsmall = 2)," per team)"), 
             icon = icon("layer-group"), 
             color = "orange")
  })
  
  output$pk2 <- renderValueBox({
    valueBox("EARLIEST PK1", 
             ifelse(nrow(earliest_pk) == 0,"N/A",paste0(earliest_pk$draft_pick," (",earliest_pk$franchise,")")),
             icon = icon("clock", class="fas"), 
             color = "orange")
  })
  
  output$most <- renderValueBox({
    valueBox("MOST DRAFTED TEAMS", 
             ifelse(nrow(most_team) == 0,
                    "N/A",
                    paste0(paste0(most_team$team,collapse = ", ")," (",paste0(most_team$count,collapse = ", "),")")),
             icon = icon("thumbs-up", class="fas"), 
             color = "purple")
  })
  
  output$least <- renderValueBox({
    valueBox("LEAST DRAFTED TEAMS", 
             ifelse(nrow(least_team) == 0,
                    "N/A",
                    paste0(paste0(least_team$team,collapse = ", ")," (",paste0(least_team$count,collapse = ", "),")")),
             icon = icon("thumbs-down", class="fas"), 
             color = "purple")
  })
  
  output$adpd <- renderValueBox({
    valueBox("LARGEST PICK +/-", 
             ifelse(nrow(adp_disp) == 0,"N/A",paste0(adp_disp$player," - ",adp_disp$diff," (",adp_disp$high,"/",adp_disp$low,")")), 
             icon = icon("arrows-left-right"), 
             color = "purple")
  })
  
  output$adpv2 <- renderValueBox({
    valueBox("HIGHEST ADP VALUE", 
             ifelse(nrow(adp_v) == 0,
                    "N/A",
                    paste0(
                      ifelse(
                        adp_v$adp_value > 0,
                        paste0("+",format(adp_v$adp_value, digits = 2, nsmall = 2)),
                        paste0(format(adp_v$adp_value, digits = 2, nsmall = 2))),
                      " (",adp_v$franchise,")")
             ),
             icon = icon("money-bill-wave"), 
             color = "purple")
  })

  ### Draft Progress Tab -------------------------------------------------------
  
  output$dprog = DT::renderDataTable({
    DT::datatable(prog_f,
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('Responsive'),
                  options = list(pageLength = 8, 
                                 info = TRUE,
                                 lengthMenu = list(c(25,50,100), c("25","50","100")),
                                 autoWidth = TRUE,
                                 responsive = FALSE,
                                 dom = "t",
                                 columnDefs = list(list(className = 'dt-left', targets = c(0,2,3)),
                                                   list(width = '55px', targets = c(1)),
                                                   list(width = '300px', targets = c(3)),
                                                   list(targets = 3, render = DT::JS(js)
                                                   )
                                 )),
                  rownames = FALSE,
                  colnames = c("Division","Latest Pick","Time","Progress")
    ) %>%
      DT::formatStyle(columns = c(1, 2, 3), fontSize = '16px')
  })
  
  ### ADP Pages ----------------------------------------------------------------
  output$adp_all = DT::renderDataTable({
    DT::datatable(adp,
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','FixedColumns','Responsive'),
                  options = list(pageLength = 25, 
                                 info = TRUE,
                                 lengthMenu = list(c(25,50,100), c("25","50","100")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 dom = "lftp",
                                 columnDefs = list(list(className = 'dt-center', targets = c(0,2:9)),
                                                   list(width = '35px', targets = c(4:9)),
                                                   list(width = '25px', targets = c(0)),
                                                   list(width = '20px', targets = c(2:3)))),
                  rownames = FALSE,
                  colnames = c("Ovr Rk","Player","Pos","PRk","Team","Avg","High","Low","StDev","Count")
    ) %>%
      DT::formatRound(c(6,9), digits = 2) %>%
      DT::formatStyle(columns = c(3,4),
                      valueColumns = c(3),
                      backgroundColor = DT::styleEqual(c("QB", "RB", "WR", "TE", "PK"), c("#dd4b39","#0073b7","#00a65a","#e5aa00","#ff7700")),
                      color = "#FFFFFF")
  })
  
  output$adp_qb = DT::renderDataTable({
    DT::datatable((adp %>% filter(pos == "QB")),
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','FixedColumns','Responsive'),
                  options = list(pageLength = 25, 
                                 info = TRUE,
                                 lengthMenu = list(c(25,50,100), c("25","50","100")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 dom = "lftp",
                                 columnDefs = list(list(className = 'dt-center', targets = c(0,2:9)),
                                                   list(width = '35px', targets = c(4:9)),
                                                   list(width = '25px', targets = c(0)),
                                                   list(width = '20px', targets = c(2:3)))),
                  rownames = FALSE,
                  colnames = c("Ovr Rk","Player","Pos","PRk","Team","Avg","High","Low","StDev","Count")
    ) %>%
      DT::formatRound(c(6,9), digits = 2)  %>%
      DT::formatStyle(columns = c(3,4),
                      valueColumns = c(3),
                      backgroundColor = DT::styleEqual(c("QB", "RB", "WR", "TE", "PK"), c("#dd4b39","#0073b7","#00a65a","#e5aa00","#ff7700")),
                      color = "#FFFFFF")
  })
  
  output$adp_rb = DT::renderDataTable({
    DT::datatable((adp %>% filter(pos == "RB")),
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','FixedColumns','Responsive'),
                  options = list(pageLength = 25, 
                                 info = TRUE,
                                 lengthMenu = list(c(25,50,100), c("25","50","100")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 dom = "lftp",
                                 columnDefs = list(list(className = 'dt-center', targets = c(0,2:9)),
                                                   list(width = '35px', targets = c(4:9)),
                                                   list(width = '25px', targets = c(0)),
                                                   list(width = '20px', targets = c(2:3)))),
                  rownames = FALSE,
                  colnames = c("Ovr Rk","Player","Pos","PRk","Team","Avg","High","Low","StDev","Count")
    ) %>%
      DT::formatRound(c(6,9), digits = 2)  %>%
      DT::formatStyle(columns = c(3,4),
                      valueColumns = c(3),
                      backgroundColor = DT::styleEqual(c("QB", "RB", "WR", "TE", "PK"), c("#dd4b39","#0073b7","#00a65a","#e5aa00","#ff7700")),
                      color = "#FFFFFF")
  })
  
  output$adp_wr = DT::renderDataTable({
    DT::datatable((adp %>% filter(pos == "WR")),
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','FixedColumns','Responsive'),
                  options = list(pageLength = 25, 
                                 info = TRUE,
                                 lengthMenu = list(c(25,50,100), c("25","50","100")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 dom = "lftp",
                                 columnDefs = list(list(className = 'dt-center', targets = c(0,2:9)),
                                                   list(width = '35px', targets = c(4:9)),
                                                   list(width = '25px', targets = c(0)),
                                                   list(width = '20px', targets = c(2:3)))),
                  rownames = FALSE,
                  colnames = c("Ovr Rk","Player","Pos","PRk","Team","Avg","High","Low","StDev","Count")
    ) %>%
      DT::formatRound(c(6,9), digits = 2)  %>%
      DT::formatStyle(columns = c(3,4),
                      valueColumns = c(3),
                      backgroundColor = DT::styleEqual(c("QB", "RB", "WR", "TE", "PK"), c("#dd4b39","#0073b7","#00a65a","#e5aa00","#ff7700")),
                      color = "#FFFFFF")
  })
  
  output$adp_te = DT::renderDataTable({
    DT::datatable((adp %>% filter(pos == "TE")),
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','FixedColumns','Responsive'),
                  options = list(pageLength = 25, 
                                 info = TRUE,
                                 lengthMenu = list(c(25,50,100), c("25","50","100")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 dom = "lftp",
                                 columnDefs = list(list(className = 'dt-center', targets = c(0,2:9)),
                                                   list(width = '35px', targets = c(4:9)),
                                                   list(width = '25px', targets = c(0)),
                                                   list(width = '20px', targets = c(2:3)))),
                  rownames = FALSE,
                  colnames = c("Ovr Rk","Player","Pos","PRk","Team","Avg","High","Low","StDev","Count")
    ) %>%
      DT::formatRound(c(6,9), digits = 2)  %>%
      DT::formatStyle(columns = c(3,4),
                      valueColumns = c(3),
                      backgroundColor = DT::styleEqual(c("QB", "RB", "WR", "TE", "PK"), c("#dd4b39","#0073b7","#00a65a","#e5aa00","#ff7700")),
                      color = "#FFFFFF")
  })
  
  output$adp_pk = DT::renderDataTable({
    DT::datatable((adp %>% filter(pos == "PK")),
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','FixedColumns','Responsive'),
                  options = list(pageLength = 25, 
                                 info = TRUE,
                                 lengthMenu = list(c(25,50,100), c("25","50","100")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 dom = "lftp",
                                 columnDefs = list(list(className = 'dt-center', targets = c(0,2:9)),
                                                   list(width = '35px', targets = c(4:9)),
                                                   list(width = '25px', targets = c(0)),
                                                   list(width = '20px', targets = c(2:3)))),
                  rownames = FALSE,
                  colnames = c("Ovr Rk","Pos Rk","Player","Pos","Team","Avg","High","Low","StDev","Count")
    ) %>%
      DT::formatRound(c(6,9), digits = 2)  %>%
      DT::formatStyle(columns = c(3,4),
                      valueColumns = c(3),
                      backgroundColor = DT::styleEqual(c("QB", "RB", "WR", "TE", "PK"), c("#dd4b39","#0073b7","#00a65a","#e5aa00","#ff7700")),
                      color = "#FFFFFF")
  })
  
  ### Single Player Query Tab --------------------------------------------------
  
  output$sp = DT::renderDataTable({
    DT::datatable(single_player(),
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','Responsive'),
                  options = list(pageLength = 25, 
                                 info = TRUE,
                                 lengthMenu = list(c(25,50,100), c("25","50","100")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 dom = "t",
                                 columnDefs = list(list(className = 'dt-center', targets = c(0:2)),
                                                   list(width = '60px', targets = c(0:2)),
                                                   list(width = '220px', targets = 3))),
                  rownames = FALSE,
                  colnames = c("Overall","Draft Pick","Pos","Franchise","Division")
    )
  })
  
  output$p_name = renderText({player_name()})
  
  output$p_info = renderText({player_info()})
  
  output$p_adp = renderInfoBox({
    infoBox("ADP",
            paste(player_adp()),
            icon = icon("calculator"),
            color = "black")
  })
  
  output$p_high = renderInfoBox({
    infoBox("HIGH",
            paste(player_high()),
            icon = icon("fire"),
            color = "black")
  })
  
  output$p_low = renderInfoBox({
    infoBox("LOW",
            paste(player_low()),
            icon = icon("clock",class="fas"),
            color = "black")
  })
  
  output$p_count = renderInfoBox({
    infoBox("COUNT",
            paste(player_count()),
            icon = icon("layer-group"),
            color = "black")
  })
  
  ### Player Comparison Tab ----------------------------------------------------
  
  output$player_comp = renderPlot(comp_plot(),
               width ="auto",
               height = eventReactive(input$update_comp,{comp_count()}),
               res = 60)
  
  output$adp_comp = DT::renderDataTable({
    DT::datatable(comp_adp(),
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','FixedColumns','Responsive'),
                  options = list(pageLength = 25, 
                                 info = TRUE,
                                 lengthMenu = list(c(25,50,100), c("25","50","100")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 dom = "t",
                                 columnDefs = list(list(className = 'dt-center', targets = c(0,2:9)),
                                                   list(width = '35px', targets = c(4:9)),
                                                   list(width = '25px', targets = c(0)),
                                                   list(width = '20px', targets = c(2:3)))),
                  rownames = FALSE,
                  colnames = c("Ovr Rk","Player","Pos","PRk","Team","Avg","High","Low","StDev","Count")
    ) %>%
      DT::formatRound(c(6,9), digits = 2)
  })
  
  ### Multi Player Query Tab --------------------------------------------------
  
  output$mp = DT::renderDataTable({
    DT::datatable(multi_player(),
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','Responsive'),
                  options = list(pageLength = 25, 
                                 info = TRUE,
                                 lengthMenu = list(c(25,50,100), c("25","50","100")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 dom = "t"
                                 ),
                  rownames = FALSE
    )
  })
  
  ### My Team Tab -------------------------------------------------------------
  output$myteam = render_gt(mtfinal())
  
  output$teamimage <- downloadHandler(
    filename = function() {
      paste0("myteam-",Sys.Date(),".png")
    },
    content = function(file){
      gtsave(mtfinal(),
             file,
             expand = 5)
    },
    contentType = "image/png"
  )
  
  outputOptions(output, "myteam", suspendWhenHidden = FALSE)
  
  ### Draftboard Tab -----------------------------------------------------------
  output$draftb = render_gt(dbfinal())
  
  output$dbimage <- downloadHandler(
    filename = function() {
      paste0("draftboard-",Sys.Date(),".png")
    },
    content = function(file){
      gtsave(dbfinal(),
             file,
             expand = 5)
    },
    contentType = "image/png"
  )
  
  outputOptions(output, "draftb", suspendWhenHidden = FALSE)
  
  ### ADP Value Tab ------------------------------------------------------------
  output$adpv = DT::renderDataTable({
    DT::datatable(adp_value(),
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','Responsive'),
                  options = list(pageLength = 25, 
                                 info = TRUE,
                                 lengthMenu = list(c(25,50,100), c("25","50","100")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 dom = "lftp",
                                 columnDefs = list(list(className = 'dt-center', targets = c(2:4)),
                                                   list(width = '50px', targets = c(2)),
                                                   list(width = '65px', targets = c(3:4)),
                                                   list(width = '220px', targets = 0))),
                  rownames = FALSE,
                  colnames = c("Franchise","Div","Picks","ADP Value","Value/Pick")
    ) %>%
      DT::formatRound(c(4:5), digits = 2)
  })
  
  ### Roster Construction Tab  -------------------------------------------------
  output$roscon = DT::renderDataTable({
    DT::datatable(roster_cons(),
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','Responsive'),
                  options = list(pageLength = 25, 
                                 info = TRUE,
                                 lengthMenu = list(c(25,50,100), c("25","50","100")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 dom = "ltp",
                                 columnDefs = list(list(className = 'dt-center', targets = c(0:7)))),
                  rownames = FALSE,
                  colnames = c("TTL","QB","RB","WR","TE","PK","Amnt","Pct")
    ) %>%
      DT::formatPercentage(c(8), digits = 2)
  })
  
  ### Time to Draft Tab --------------------------------------------------------
  
  output$ttd_table = DT::renderDataTable({
    DT::datatable(ttd,
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','Responsive'),
                  options = list(pageLength = 25, 
                                 info = TRUE,
                                 lengthMenu = list(c(25,50,100), c("25","50","100")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 dom = "lftp",
                                 columnDefs = list(list(className = 'dt-center', targets = c(2:4)),
                                                   list(width = '55px', targets = c(2)),
                                                   list(width = '75px', targets = c(3:4)))),
                  rownames = FALSE,
                  colnames = c("Franchise","Div","Picks","TTD","TTD/Pick")
    )
  })
  
  ### Projections Tab ----------------------------------------------------------
  
  output$stand_div = DT::renderDataTable({
    DT::datatable(proj_stand_div,
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','Responsive','RowGroup'),
                  options = list(pageLength = 96, 
                                 info = FALSE,
                                 lengthMenu = list(c(96), c("All")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 rowGroup = list(dataSrc = 3),
                                 dom = "ft",
                                 ordering=F,
                                 columnDefs = list(list(targets = c(3), visible = FALSE),
                                                   list(className = 'dt-center', targets = c(0:1,4:8)),
                                                   list(width = '45px', targets = c(0:1,4:5,8)),
                                                   list(width = '75px', targets = c(6:7))
                                 )
                  ),
                  rownames = FALSE,
                  colnames = c("Ovr Rk",
                               "Div Rk",
                               "Franchise",
                               "Division",
                               "Proj W",
                               "Proj L",
                               "Proj Win Pct",
                               "Proj PF",
                               "Proj P-Odds"
                               ) 
    ) %>%
      DT::formatRound(c(5,6), digits = 1) %>%
      DT::formatRound(c(8), digits = 2) %>%
      DT::formatRound(c(7), digits = 3) %>%
      DT::formatPercentage(c(9), digits = 0)
  })
  
  output$stand_ovr = DT::renderDataTable({
    DT::datatable(proj_stand_ovr,
                  filter = 'none',
                  style = "bootstrap",
                  extensions = c('FixedHeader','Responsive','RowGroup'),
                  options = list(pageLength = 96, 
                                 info = FALSE,
                                 lengthMenu = list(c(96), c("All")),
                                 autoWidth = TRUE,
                                 fixedHeader = TRUE,
                                 responsive = FALSE,
                                 rowGroup = FALSE,
                                 ordering = F,
                                 dom = "ft",
                                 columnDefs = list(list(className = 'dt-center', targets = c(0:1,4:8)),
                                                   list(width = '45px', targets = c(0:1,4:5,8)),
                                                   list(width = '75px', targets = c(6:7))
                                 )
                  ),
                  rownames = FALSE,
                  colnames = c("Ovr Rk",
                               "Div Rk",
                               "Franchise",
                               "Division",
                               "Proj W",
                               "Proj L",
                               "Proj Win Pct",
                               "Proj PF",
                               "Proj P-Odds"
                  ) 
    ) %>%
      DT::formatRound(c(5,6), digits = 1) %>%
      DT::formatRound(c(8), digits = 2) %>%
      DT::formatRound(c(7), digits = 3) %>%
      DT::formatPercentage(c(9), digits = 0)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
