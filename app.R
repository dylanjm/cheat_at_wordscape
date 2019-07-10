#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(glue)
library(DT)

word_dict <- read_lines(here::here("unix_word_file.txt"))

options(DT.options = list(pageLength = 100, 
                          order = list(list(2, 'desc')),
                          dom = 't',
                          initComplete = JS("function(settings, json) {",
                                            "$(this.api().table().header()).css({",
                                            "'font-size': '20px',",
                                            "'background-color': '#000',",
                                            "'color': '#fff'});}")))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How to Cheat at Wordscape"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "src_letter",
                      label = "Put letters here:"),
            HTML("<center>"),
            checkboxInput(inputId = "three_letter",
                          label = "No Three Letter Words"),
            HTML("</center>")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("thank_you"),
            dataTableOutput("Tables")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    input_cleaner <- function(src_letters){
        clean_letters <- src_letters %>% 
            str_remove_all("\\s+") %>% 
            str_to_lower()
        
        reg_exp <- glue("^[{clean_letters}]+$")
        return(reg_exp)
    }
    
    sort_word <- compose(partial(str_c, collapse = ""),
                         sort,
                         partial(str_split, pattern = "", simplify = T))
    
    sort_regex <- as_mapper(~ str_remove_all(.x, "\\s+") %>% 
                                str_to_lower %>% 
                                str_split("", simplify = T) %>% 
                                sort() %>% 
                                str_c("?", collapse = "") %>% 
                                str_c("^", ., "$"))
    
    observe({
        if(input$src_letter != ""){
            reg_exp <- input_cleaner(input$src_letter)
            
            print(reg_exp)
            
            filter_pred <- ifelse(input$three_letter, 4, 3)
            
            final_table <-  word_dict[str_detect(word_dict, reg_exp)] %>%
                tibble(words = .) %>%
                filter(nchar(words) >= filter_pred) %>%
                mutate(sorted_word = map_chr(words, sort_word),
                       word_length = nchar(words)) %>%
                filter(str_detect(sorted_word, sort_regex(input$src_letter))) %>%
                select(words, word_length) %>%
                arrange(desc(word_length))

            if(nrow(final_table) == 0){
                output$thank_you = renderText({
                    paste0("<p style = 'color:red; font-size:22px; text-align: center;'><b>",
                           "Please enter the letters you want to unjumble<br></p>")
                })
            } else {
                removeUI(selector = "#thank_you")
                output$Tables <- renderDataTable({datatable(final_table) %>%
                        formatStyle(columns = colnames(.$x$data), `font-size` = "20px")})
            }
            
            
        } else{
            output$thank_you = renderText({
                paste0("<p style = 'color:red; font-size:22px; text-align: center;'><b>",
                       "Please enter the letters you want to unjumble<br></p>")
            })
        }
        
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
