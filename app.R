#-----------------------------------------------# 
# R Shiny Web App
# 
# Idaho Chapter of The Wildlife Society
# Interactive Annual Meeting Agenda 
# and Scheduler
# 
#
# By: Robert Ritson
# May 15, 2026
#-----------------------------------------------# 
library(shiny)
library(dplyr)
library(lubridate)
library(DT)
library(bslib)
library(stringr)
library(markdown)
library(rmarkdown)
# ---------------------------------------------------------
# CONFERENCE AGENDA & ABSTRACTS
# ---------------------------------------------------------

agenda <- data.table::fread("data/schedule.csv") 
agenda <- agenda %>%
  mutate(
    date = paste0(year,"-",month,"-",day),
    start_datetime = ymd_hm(paste(date, start_time)),
    end_datetime = ymd_hm(paste(date, end_time))
  )

abstracts <- data.table::fread("data/abstracts.csv") 
# ---------------------------------------------------------
# HELPER FUNCTIONS
# ---------------------------------------------------------

has_conflict <- function(new_talk, selected_talks){
  
  if(nrow(selected_talks) == 0){
    return(NULL)
  } 
  
  conflicts <- selected_talks %>%
    filter(
      new_talk$start_datetime < end_datetime &
      new_talk$end_datetime > start_datetime
    )
  
  if(nrow(conflicts) == 0){
    return(NULL)
  }
  
  return(conflicts)
}

suggest_alternatives <- function(new_talk, master_schedule, selected_talks){
  
  unavailable_ids <- selected_talks$session_id
  
  alternatives <- master_schedule %>%
    filter(
      start_datetime >= new_talk$start_datetime - minutes(30),
      start_datetime <= new_talk$start_datetime + minutes(30),
      session_id != new_talk$session_id,
      !session_id %in% unavailable_ids
    )
  
  alternatives
}

# ---------------------------------------------------------
# UI
# ---------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(version = 5,preset = 'flatly'),
  tags$a(href="https://www.idaho-tws.org/",
         tags$img(src="chapter_logo.png",height="150px",style = "margin: 10px;")),
  tags$footer(
    "© 2026 Idaho Chapter of The Wildlife Society by Robert Ritson",
    align="left",
    style="
    position: fixed;
    bottom: 0;
    width: 100%;
    height: 35px;
    color: black;
    padding: 10px;
    background-color: #FFFFFF;
    "
  ),
  titlePanel("Conference Agenda Builder- DEMO"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Filter Talks"),
      
      selectInput(
        "day",
        "Day",
        choices = c("All", unique(agenda$date)),
        selected = "All"
      ),
      
      selectInput(
        "track",
        "Session",
        choices = c("All", unique(agenda$session)),
        selected = "All"
      ),
      
      selectInput(
        "room",
        "Room",
        choices = c("All", unique(agenda$room)),
        selected = "All"
      ),
      
      selectInput(
        "org",
        "Organization",
        choices = c("All", unique(agenda$org)),
        selected = "All"
      ),
      
      textInput(
        "keyword",
        "Search Title/Speaker"
      ),
      
      hr(),
      
      h4("My Agenda Stats"),
      
      verbatimTextOutput("agenda_stats"),
      
      downloadButton(
        "download_agenda",
        "Download My Agenda"
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          
          "Browse Talks",
          
          br(),
          
          DTOutput("agenda_table")
          
          
        ),
        
        tabPanel(
          
          "My Personalized Agenda",
          
          br(),
          
          DTOutput("my_agenda_table"),
          
          br(),
          
          h4("Timeline View"),
          
          uiOutput("timeline_view")
          
        )
        
      )
      
    )
    
  )
)

# ---------------------------------------------------------
# SERVER
# ---------------------------------------------------------

server <- function(input, output, session){
  
  rv <- reactiveValues(
    my_agenda = tibble()
  )
  
  # -------------------------------------------------------
  # FILTERED DATA
  # -------------------------------------------------------
  
  filtered_agenda <- reactive({
    
    dat <- agenda
    
    if(input$day != "All"){
      dat <- dat %>% filter(date == input$day)
    }
    
    if(input$track != "All"){
      dat <- dat %>% filter(session == input$track)
    }
    
    if(input$room != "All"){
      dat <- dat %>% filter(room == input$room)
    }
    
    if(input$org != "All"){
      dat <- dat %>% filter(org == input$org)
    }
    
    if(input$keyword != ""){
      
      dat <- dat %>%
        filter(
          str_detect(
            str_to_lower(title),
            str_to_lower(input$keyword)
          ) |
            str_detect(
              str_to_lower(speaker),
              str_to_lower(input$keyword)
            )
        )
    }
    
    dat
  })
  
  # -------------------------------------------------------
  # MAIN AGENDA TABLE
  # -------------------------------------------------------
  
  output$agenda_table <- renderDT({
    
    dat <- filtered_agenda() %>%
      mutate(
        Add = paste0(
          '<button class="btn btn-primary btn-sm" ',
          'onclick="Shiny.setInputValue(',
          "'add_talk', ",
          session_id,
          ', {priority: \'event\'})">',
          'Add</button>'
        ),
        Abstract = paste0(
          '<button class="btn btn-warning btn-sm" ',
          'onclick="Shiny.setInputValue(',
          "'btn_click', ",
          session_id,
          ', {priority: \'event\'})">',
          'Abstract</button>'
        ),
      ) %>%
      select(
        Add,
        Abstract,
        date,
        title,
        speaker,
        session,
        room,
        start_time,
        end_time,
        org
      )
    
    datatable(
      dat,
      escape = FALSE,
      selection = "none",
      options = list(pageLength = 15)
    )
  })
  
  # -------------------------------------------------------
  # READ OBSERVED ABSTRACT
  # -------------------------------------------------------
  observeEvent(input$btn_click, {
    rid <- input$btn_click

    talk_sel <- abstracts %>%
      filter(session_id == rid)
    rmarkdown::render(input="rmd_code/print_abstract_sel.Rmd",
                      params=list(talk_sel=talk_sel),
                      output_format = "md_document")

    showModal(modalDialog(
      title = paste(talk_sel$title),
      tags$div(
        style = "overflow-y: auto; max-height: 100vh;",
        includeMarkdown("rmd_code/print_abstract_sel.md")
      ),
      easyClose = TRUE,
      footer = modalButton("Dismiss"),
      size = "xl"
    ))

  },ignoreInit = T)

  # -------------------------------------------------------
  # DYNAMIC BUTTON OBSERVERS
  # -------------------------------------------------------
  
      
  observeEvent(input$add_talk, {
    
    id <- input$add_talk
    
    # prevent duplicates
    if(id %in% rv$my_agenda$session_id){
      showNotification("Talk aready added.",type="warning")
      return()
    }
    
    new_talk <- agenda %>%
      filter(session_id == id)
    
    conflicts <- has_conflict(
      new_talk,
      rv$my_agenda
    )
    
    # -------------------------------------------------
    # NO CONFLICT
    # -------------------------------------------------
    
    if(is.null(conflicts)){
      
      rv$my_agenda <- bind_rows(
        rv$my_agenda,
        new_talk
      )
      
      showNotification(
        "Talk added to your agenda.",
        type = "message"
      )
      
    } else {
      
      
      # -----------------------------------------------
      # CONFLICT EXISTS
      # -----------------------------------------------
      
      
      rv$pending_talk <- new_talk
      rv$pending_conflicts <- conflicts
      
      alternatives <- suggest_alternatives(
        new_talk,
        agenda,
        rv$my_agenda
      )
      
      alt_text <- if(nrow(alternatives) == 0){
        
        "No alternative sessions available."
        
      } else {
        
        paste0(
          "<ul>",
          paste0(
            "<li><b>",
            alternatives$title,
            "</b> (",
            alternatives$start_time,
            ")</li>",
            collapse = ""
          ),
          "</ul>"
        )
      }
      
      showModal(
        
        modalDialog(
          
          title = "Schedule Conflict Detected",
          
          HTML(
            paste0(
              "<p><b>",
              new_talk$title,
              "</b> conflicts with:</p>",
              "<ul>",
              paste0(
                "<li>",
                conflicts$title,
                " (",
                conflicts$start_time,
                "-",
                conflicts$end_time,
                ")</li>",
                collapse = ""
              ),
              "</ul>",
              "<hr>",
              "<h4>Suggested Alternatives</h4>",
              alt_text
            )
          ),
          
          footer = tagList(
            
            modalButton("Cancel"),
            
            actionButton(
              "confirm_replace",
              "Replace Existing Talk",
              class = "btn-danger"
            )
            
          ),
          
          easyClose = TRUE
          
        )
        
      )

      
    }
  }, ignoreInit = T)
        
  # -----------------------------------------------
  # REPLACE CONFLICTING TALK
  # -----------------------------------------------
  
  observeEvent(input$confirm_replace, {
    
    req(rv$pending_talk)
    req(rv$pending_conflicts)
    
    rv$my_agenda <- rv$my_agenda %>%
      filter(
        !session_id %in% rv$pending_conflicts$session_id
      )
    
    rv$my_agenda <- bind_rows(
      rv$my_agenda,
      rv$pending_talk
    )
    
    removeModal()
    
    showNotification(
      "Conflicting talk replaced.",
      type = "warning"
    )
  }, ignoreInit = T)
          
  
  # -------------------------------------------------------
  # PERSONALIZED AGENDA TABLE
  # -------------------------------------------------------
  
  output$my_agenda_table <- renderDT({
    dat <- rv$my_agenda %>%
      arrange(start_datetime) %>%
      mutate(
        Remove = paste0(
          '<button class="btn btn-danger btn-sm" ',
          'onclick="Shiny.setInputValue(',
          "'remove_talk', ",
          session_id,
          ', {priority: \'event\'})">',
          'Remove</button>'
        )
      ) %>%
      select(
        Remove,
        date,
        title,
        speaker,
        session,
        room,
        start_time,
        end_time,
        org
      )
    datatable(
      dat,
      escape = FALSE,
      selection = "none",
      options = list(pageLength = 15)
    )
  })
    
  # -------------------------------------------------------
  # REMOVE TALKS
  # -------------------------------------------------------
  observeEvent(input$remove_talk,{
    rv$my_agenda <- rv$my_agenda %>%
      filter(session_id != input$remove_talk)
    
    showNotification(
      "Talk removed from agenda.",
      type="warning"
    )
  }, ignoreInit = T)
  
  # -------------------------------------------------------
  # TIMELINE VIEW
  # -------------------------------------------------------
  
  output$timeline_view <- renderUI({
    
    talks <- rv$my_agenda %>%
      arrange(start_datetime)
    
    if(nrow(talks) == 0){
      
      return(h4("No talks selected yet."))
      
    }
    
    tagList(
      
      lapply(1:nrow(talks), function(i){
        
        talk <- talks[i, ]
        
        div(
          style = "
            border-left: 6px solid #2C7FB8;
            background-color: #F4F6F7;
            padding: 10px;
            margin-bottom: 10px;
            border-radius: 5px;
          ",
          
          h4(talk$title),
          
          p(
            strong("Speaker: "),
            talk$speaker
          ),
          
          p(
            strong("Session: "),
            talk$session
          ),
          
          p(
            strong("Room: "),
            talk$room
          ),
          
          p(
            strong("Time: "),
            paste(
              talk$start_time,
              "-",
              talk$end_time
            )
          )
          
        )
        
      })
      
    )
    
  })
  
  # -------------------------------------------------------
  # STATS
  # -------------------------------------------------------
  
  output$agenda_stats <- renderPrint({
    
    cat(
      "Selected Talks:",
      nrow(rv$my_agenda),
      "\n\n"
    )
    
    if(nrow(rv$my_agenda) > 0){
      
      total_hours <- sum(
        as.numeric(
          difftime(
            rv$my_agenda$end_datetime,
            rv$my_agenda$start_datetime,
            units = "hours"
          )
        )
      )
      
      cat(
        "Total Scheduled Time:",
        round(total_hours, 1),
        "hours"
      )
      
    }
    
  })
  
  # -------------------------------------------------------
  # DOWNLOAD
  # -------------------------------------------------------
  
  output$download_agenda <- downloadHandler( #Rmarkdown file creating pdf from selected talks!
    filename = "my_conference_agenda.html",
    content = function(file){
      temp <- file.path(tempdir(),"rmd_code/personal_agenda.Rmd")
      file.copy("rmd_code/personal_agenda.Rmd",temp,overwrite = T)
      rmarkdown::render(
        input = "rmd_code/personal_agenda.Rmd",
        output_file = file,
        params = list(
          my_agenda = rv$my_agenda
        )
      )
    }
  )
  
}

# ---------------------------------------------------------
# RUN APP
# ---------------------------------------------------------

shinyApp(ui, server)