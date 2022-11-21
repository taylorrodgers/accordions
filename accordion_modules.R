library(shiny)
library(shiny.semantic)
library(tidyverse)
library(readr)

# Javascript for Accordion
accordion_js <- "
$(document).ready(function() {
  $('.ui.accordion').accordion();
})"

household_estimates <- read_csv("https://raw.githubusercontent.com/taylorrodgers/public_datasets/main/household_carbon_estimates.csv")
total_estimates <- read_csv("https://raw.githubusercontent.com/taylorrodgers/public_datasets/main/total_household_estimates.csv")

Totals <- function(category,period) {
  
  total_estimates %>%
    select(total_type,contains(period)) %>%
    rename(emissions=2) %>%
    filter(total_type==category) %>%
    transmute(emissions=round(emissions,2)) %>%
    paste0(.,ifelse(period=="year"," /yr",
                    ifelse(period=="month"," /mo"," /wk")))
  
}

SubTotals <- function(category,period) {
  
  household_estimates %>%
    select(categories,subcategories,contains(period)) %>%
    rename(emissions=3) %>%
    filter(categories==category) %>%
    arrange(subcategories) %>%
    group_by(subcategories) %>%
    summarize(emissions=sum(round(emissions,2))) %>%
    rename(Subcategories=subcategories,Emissions=emissions)
  
}

accordionTitleContentUI <- function (id,active_ind=FALSE) {
  tagList(
    div(class = ifelse(active_ind==TRUE,"title active","title"),
        h4(class="ui dividing header",
           div(class="ui grid",
               div(class="two column row",
                   div(class="left floated column",
                       textOutput(NS(id,"TitleHeader"),inline=TRUE)),
                   div(class="right floated column",
                       style="text-align: right",
                       textOutput(NS(id,"Total")),inline=TRUE))
           )
         )
    ),
    div(class = ifelse(active_ind==TRUE,"content active","content"),
        div(class="ui teal inverted segment",
            tableOutput(NS(id,"SubTotal"))
        )
    )
  )
}

accordionTitleContentServer <- function(id,category,period) {
  moduleServer(id, function(input,output,session) {
    
    output$TitleHeader <- renderText({
      paste0("Total ",category,": ")
    })
    
    output$Total <- renderText({
      Totals(category=category,period=period)
    })
    
    output$SubTotal <- renderTable(width = "100%",colnames=FALSE,{
      SubTotals(category,period)
    })
    
    outputOptions(output, "SubTotal", suspendWhenHidden = FALSE)
  })
}

accordionMasterUI <- function(id,period) {
  tagList(
    shiny::tags$script(accordion_js),
    div(class = "ui segment",
        style="min-width: 250px; max-width: 275px",
        h3(paste(ifelse(period=="year","Yearly",
                        ifelse(period=="month","Monthly","Weekly")),
                 "Carbon Estimates")),
        h3(class="ui grey sub header","Total CO2 Emissions"),
        div(class="ui grid",
            div(class="four column row",
                div(class="column",
                    h2(class="ui dividing header",textOutput(NS(id,"GrandTotal")))),
                div(class="column",
                    strong(ifelse(period=="year"," /year",
                                  ifelse(period=="month"," /month"," /week")))
                )
            )),
        br(),
        div(class="ui accordion",
            accordionTitleContentUI(NS(id,"Travel"),active_ind=TRUE),
            accordionTitleContentUI(NS(id,"Housing"),active_ind=FALSE),
            accordionTitleContentUI(NS(id,"Food"),active_ind=FALSE),
            accordionTitleContentUI(NS(id,"Goods"),active_ind=FALSE),
            accordionTitleContentUI(NS(id,"Services"),active_ind=FALSE)
        )
    )
  )
}

accordionMasterServer <- function(id,period) {
  moduleServer(id,function(input,output,session) {
    
    output$GrandTotal <- renderText({
      total_estimates %>%
        select(total_type,contains(period)) %>%
        rename(emissions=2) %>%
        filter(total_type=="Grand") %>%
        transmute(grandtotal=round(emissions,2)) %>%
        paste0(.)
    })
    
    accordionTitleContentServer("Travel",category="Travel",period)
    accordionTitleContentServer("Housing",category="Housing",period)
    accordionTitleContentServer("Food",category="Food",period)
    accordionTitleContentServer("Goods",category="Goods",period)
    accordionTitleContentServer("Services",category="Services",period)
    
  })
  
}

ui <- function() {
  semanticPage(
    h2("Our Accordions"),
    div(class="ui stackable grid",
        div(class="four wide computer eight wide tablet sixteen wide mobile column",
            accordionMasterUI("year",period="year")
        ),
        div(class="four wide computer eight wide tablet sixteen wide mobile column",
            accordionMasterUI("month",period="month")
        ),
        div(class="four wide computer eight wide tablet sixteen wide mobile column",
            accordionMasterUI("week",period="week")
        ),
        div(class="two column row",
            div(class="four wide column",
                h3("Totals by Category"),
                div(class="ui raised segment",
                    style="min-width: 550px;",
                    tableOutput("TestTable")
                )
            )
        )
    )
  )
}

server <- function(input, output, session) {
  
  accordionMasterServer("year",period="year")
  accordionMasterServer("month",period="month")
  accordionMasterServer("week",period="week")
  
  output$TestTable <- renderTable({
    total_estimates
  })
  
}

shinyApp(ui = ui(), server = server)
