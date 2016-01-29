library(shiny)
library(leaflet)
library(dplyr)
library(labmanager)

data_path <- file.path("www", "data")

voucher <- get_lab("sample_data", path = data_path)
with_img <-  voucher[["voucher_number"]][as.logical(voucher[["has_photo"]])]

sequencing_data <- get_lab("sequencing_plate_data", path = data_path) %>%
    filter(!is.na(.$"bold_phylum_id")) %>%
    filter(bold_phylum_id != "")
lst_bold_species <- paste(sequencing_data[["bold_phylum_id"]], "--",
                          sequencing_data[["bold_genus_id"]],
                          sequencing_data[["bold_species_id"]],
                          paste0("(", sequencing_data[["bold_bin_id"]], ")")) %>%
    sort %>%
    .[nchar(gsub("\\s", "", .)) > 0]

esus <- get_lab("sample_esu")
lst_esu <- paste(esus$phylum, esus$group_esu, sep = "-") %>% unique %>% sort

navbarPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "lightbox.css")
    ),
    "Florida plankton",
    tabPanel("By voucher",
             sidebarLayout(
                 sidebarPanel("Specimen information",
                              selectInput('voucher_id', "Voucher ID",
                                          choices = with_img),
                              leafletOutput("station_map")
                              ),
                 mainPanel(
                     h2(textOutput("voucher_selected")),
                     textOutput("number_images"),
                     textOutput("voucher_phylum"),
                     textOutput("voucher_taxa"),
                     textOutput("voucher_has_sequence"),
                     textOutput("voucher_bold_id"),
                     uiOutput("list_img")
                     )
             )
             ),
    tabPanel("By species",
             sidebarLayout(
                 sidebarPanel("Choose the species",
                              selectInput('species', "Species",
                                          choices = lst_bold_species,
                                          selected = TRUE),
                              leafletOutput("species_station_map")
                              ),
                 mainPanel(
                     textOutput("voucher_list"),
                     uiOutput("list_img_species", style = "height: 255px;")
                 )
             )
             ),
    tabPanel("By ESU",
             sidebarLayout(
                 sidebarPanel("Choose the species",
                              selectInput('esu', "ESU",
                                          choices = lst_esu,
                                          selected = TRUE),
                              leafletOutput("species_station_map2")
                              ),
                 mainPanel(
                     textOutput("voucher_list_esu"),
                     uiOutput("list_img_esu", style = "height: 255px;")
                 )
             )
             ),
    tags$script(src = "lightbox.js")
)
