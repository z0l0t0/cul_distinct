#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(dbplyr)
library(RSQLite)
library(DT)
library(rvest)

#cul_acq_from_rds <- readRDS("data/cul_acq_circ_oclc_2001-2018.rds")
#dbWriteTable(conn = cul_acq_db_con, name = "OCLCdataset", value = cul_acq_from_rds, overwrite = TRUE)


cul_acq_db_file <- "data/cul_acq_db.sqlite"

cul_acq_db_con <- dbConnect(SQLite(), dbname = cul_acq_db_file)





######################################  ADAM RPUB DOC #######################################
#circ <- dbReadTable(cul_acq_db_con, "OCLCdataset")
circ <- tbl(cul_acq_db_con, "OCLCdataset")
languagelookup <- tbl(cul_acq_db_con, "languagelookup")
#circ <- collect(circ1)


#rawcodes <- read_html("https://www.loc.gov/marc/languages/language_code.html")






######################################  ADAM RPUB DOC #######################################



# Define UI for application
ui <- dashboardPage(skin = "red",

    # Application title
    dashboardHeader(title = "Distinctive collections"),

    # Sidebar
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Introduction", icon = icon("info"), tabName = "intro_tab"
                     ),
            menuItem("Cornell holdings by language", icon = icon("language"), tabName = "cu_only_tab"
                     ),
            menuItem("Case Study: Khmer", icon = icon("globe-asia"), tabName = "khmer_tab"
                     ),
            menuItem("Appendix 1", icon = icon("info"), tabName = "lang_code_lcclass"
                     ),
            menuItem("Appendix 2", icon = icon("info"), tabName = "lang_lcclass_borrower"
                     )
            )
        ),

        # Body
        dashboardBody(
            tabItems(
                tabItem("intro_tab",
                        h3("Cornell Monograph Acquisitions 2001 - 2018"),
                        br(),
                        br(),
                        p("The purpose of this experiment is to link a 1.487 million record dataset of Cornell's 2001 - 2018 monograph acquisitions with corresponding OCLC Worldcat holdings, then group the holdings by language of the material, to measure the extent to which the collection we are building (in each language) is distinctive. A great deal of effort was put into compiling this dataset. This is the first time we've used it to inform our conversation around distinctive collections."),
                        br(),
                        h3("Variables:"),
                        br(),
                        p("language: language of the material as coded in the MARC record"),
                        br(),
                        p("n = number of monograph titles collected by Cornell in the 2001 - 2018 time period"),
                        br(),
                        p("cornell_only = number of titles collected that are owned only by Cornell"),
                        br(),
                        p("cornell_only_per1000 = (cornell_only / number of items collected) * 1000"),
                        br(),
                        p("percent_has_circulated = percentage of titles that have circulated at least once"),
                        br(),
                        p("mean_ivy = average number of copies across Ivy Plus. For a group of titles we take the total number of Ivy Plus libraries that hold titles in the set (for example, Khmer language books) and divide by the number of titles. An average that is close to 1 means that few other libraries hold the same titles; a high average means many of libraries hold the same titles."),
                        br(),
                        p("mean_oclc = average number of copies across all OCLC libraries. For a group of titles we take the total number of OCLC WorldCat libraries that hold titles in the set (for example, Khmer language books) and divide by the number of titles. An average that is close to 1 means that few other libraries hold the same titles; a high average means many of libraries hold the same titles.")
                        
                        ),
                tabItem("cu_only_tab",
                        dataTableOutput("cornell_only_dt")
                        ),
                tabItem("khmer_tab",
                        p("During these two decades, TABLE 1 shows that Cornell added 1232 Khmer language books to the collection.  817 of these are only held by Cornell, which means for every 1000 books we acquired in this language, 663 are unique to Cornell. Continuing with this example, 11% of them circulated, the average holdings across Ivy Plus libraries is 1.12 and the average number of libraries in Worldcat that hold these titles is 1.78."),
                        br(),
                        p("How many Khmer books have circulated?"),
                        tableOutput("khmer_circ"),
                        br(),
                        p("How many Khmer books that have not circulated are held only by Cornell?"),
                        tableOutput("khmer_no_circ")
                    ),
                tabItem("lang_code_lcclass",
                        h3("Appendix 1: Language code and LC class"),
                        br(),
                        dataTableOutput("appendix_one")
                    ),
                tabItem("lang_lcclass_borrower",
                        h3("Appendix 2: Language and LC class circulation by borrower category"),
                        br(),
                        dataTableOutput("appendix_two")
                    )
                )
        )
)


server <- function(input, output) {


    
    ######################################  ADAM RPUB DOC #######################################
    
    
    # languagelookup <- rawcodes %>%
    #     html_node("table") %>%
    #     html_table()
    
    #languagelookup <- read_rds("data/languagelookup.rds")
    
    
    
    holdings_by_lang_code <- circ %>%
        filter(!is.na(oclc_id_norm)) %>%
        group_by(lang_code) %>%
        summarize(n = n(),
                  mean_oclc = mean(oclc_inst_cnt),
                  mean_ivy = mean(ivy_plus_count),
                  percent_has_circulated = mean(has_circulated) * 100 ) %>%
        mutate_if(is.numeric, round, digits = 2) %>%
        left_join(languagelookup, by =c("lang_code" = "code")) %>%
        select(language, everything())
    
    
    oneholding_by_lang_code <- circ %>%
        filter(!is.na(oclc_id_norm)) %>%
        filter(oclc_inst_cnt == 1) %>%
        group_by(lang_code) %>%
        summarize(cornell_only = n()) %>%
        arrange(desc(cornell_only))
    
    cornell_only_per1000 <- holdings_by_lang_code %>%
        left_join(oneholding_by_lang_code, by = "lang_code") %>%
        mutate(cornell_only_ratio = cornell_only / n) %>%
        mutate(cornell_only_per1000 = cornell_only_ratio * 1000) %>%
        mutate_if(is.numeric, round, digits = 2) %>%
        filter(n > 100) %>%
        arrange(desc(cornell_only_per1000)) %>%
        select(-cornell_only_ratio)
    
    
    output$cornell_only_dt <- renderDataTable({
        
        cornell_only_per1000 %>%
            select(language, n, cornell_only, cornell_only_per1000, percent_has_circulated, mean_ivy, mean_oclc) %>%
            mutate_if(is.numeric, round, digits = 1) %>%
            collect() %>% 
            datatable()
        
    })
    
    output$khmer_circ <- renderTable({
        
        circ %>%
            filter(lang_code == "khm") %>%
            group_by(has_circulated) %>%
            count()
        
    })
    
    output$khmer_no_circ <- renderTable({
        
        circ %>%
            filter(lang_code == "khm",
                   has_circulated == 0,
                   oclc_inst_cnt == 1) %>%
            count()
        
    })
    
    
    
    ######################################  ADAM RPUB DOC #######################################  
    
    
    
    ###################################### Appendix One  ########################################
    
    holdings_by_lang_code_lc <- circ %>%
        filter(!is.na(oclc_id_norm)) %>%
        group_by(lang_code, lcclass) %>%
        summarize(n = n(),
                  mean_oclc = mean(oclc_inst_cnt),
                  mean_ivy = mean(ivy_plus_count),
                  percent_has_circulated = mean(has_circulated) * 100 ) %>%
        mutate_if(is.numeric, round, digits = 2) %>%
        left_join(languagelookup, by =c("lang_code" = "code")) %>%
        select(language, everything())
    
    oneholding_by_lang_code_lc <- circ %>%
        filter(!is.na(oclc_id_norm)) %>%
        filter(oclc_inst_cnt == 1) %>%
        group_by(lang_code, lcclass) %>%
        summarize(cornell_only = n()) %>%
        arrange(desc(cornell_only))
    
    cornell_only_per1000_lc <- holdings_by_lang_code_lc %>%
        #left_join(oneholding_by_lang_code_lc, by = "lang_code") %>%
        left_join(oneholding_by_lang_code_lc, by = c("lang_code" = "lang_code", "lcclass" = "lcclass")) %>%
        #by = c("x" = "x2", "y" = "y2")
        mutate(cornell_only_ratio = cornell_only / n) %>%
        mutate(cornell_only_per1000 = cornell_only_ratio * 1000) %>%
        mutate_if(is.numeric, round, digits = 2) %>%
        filter(n > 50) %>%
        arrange(desc(cornell_only_per1000)) %>%
        select(-cornell_only_ratio)
    
    output$appendix_one <- renderDataTable({ 
        
       cornell_only_per1000_lc %>%
            ungroup() %>%
            select(language, lcclass, n, cornell_only, cornell_only_per1000, percent_has_circulated, mean_ivy, mean_oclc) %>%
            mutate_if(is.numeric, round, digits = 1) %>%
            
            collect() %>% 
            
            datatable(filter = 'top', options = list(
                pageLength = 15, autoWidth = TRUE
            ))
        
    })
    
    ###################################### Appendix Two  ########################################
    
    
    output$appendix_two <- renderDataTable({
        
        circ %>%
            group_by(lang_code, lcclass) %>%
            summarize(n = n(),
                      titles_circulated = sum(has_circulated), 
                      total_charges = sum(sum_total_of_charge_count),
                      total_borrow_direct = sum(sum_borrow_direct), 
                      total_ill = sum(sum_interlibrary_loan)) %>% 
                      #total_cornell = total_charges - (total_borrow_direct + total_ill),
                      #charges_per_titlesacquired = total_charges / n) %>% 
            mutate(total_cornell = total_charges - (total_borrow_direct + total_ill),
                   charges_per_titlesacquired = total_charges / n) %>% 
            left_join(cornell_only_per1000_lc, by = c("lang_code" = "lang_code", "lcclass" = "lcclass")) %>%
            filter(!is.na(language)) %>%
            ungroup() %>%
            select(language, lcclass, n.x, titles_circulated, total_charges, charges_per_titlesacquired, total_cornell, total_borrow_direct, total_ill) %>%
            mutate_if(is.numeric, round, digits = 2) %>%
            rename(n = n.x) %>%
            arrange(desc(charges_per_titlesacquired)) %>%

            collect() %>%

            datatable(filter = 'top', options = list(
                pageLength = 15, autoWidth = TRUE
            ))
        
    })
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)
