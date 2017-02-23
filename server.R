library(shiny)
library(leaflet)
library(labmanager)
library(dplyr)

## Generate the exif information
## info <- system('exiftool -T -r -FileName -Directory -ImageSize "/home/francois/plankton-images/archive_photos/"',
##                inter=TRUE)
## saveRDS(info, file = "inst/shiny-examples/limsr/photo_info.rds")
## img_df <- read.delim2(textConnection(info),
##                       stringsAsFactors = FALSE,
##                       header = FALSE,
##                       col.names = c("file_name", "file_path", "image_size"))
## library(dplyr)
## img_df <- img_df %>%
##     mutate(image_width = unlist(strsplit(image_size, "x"))[1],
##            image_height = unlist(strsplit(image_size, "x"))[2]) %>%
##     mutate(full_path = file.path(file_path, file_name))
## saveRDS(img_df, file = "inst/shiny-examples/limsr/img_df.rds")
##img_df <- readRDS("img_df.rds")


shinyServer(function(input, output) {
    data_path <- file.path("www", "data")
    photo_path <- file.path("www", "app_photos")

    smpl <- get_lab("sample_data", path = data_path)
    seq <- get_lab("sequencing_plate_data", path = data_path)
    sta <- get_lab("station_data", path = data_path)
    esus <- get_lab("sample_esu", path = data_path)

    get_voucher_path <- function(voucher) {
        file.path(photo_path, voucher)
    }

    voucher_path <- reactive({
        get_voucher_path(input$voucher_id)
    })

    large_thumb_path <- function(voucher_path) {
        file.path(voucher_path, "large")
    }

    thumb_path <- function(voucher_path) {
        file.path(voucher_path, "thumbs")
    }

    species_info <- function(species) {
        if (nchar(species) > 0) {
            sp <- strsplit(species, " ")[[1]]
            list(phylum = sp[[1]],
                 genus = sp[[3]],
                 species = sp[[4]])
        } else list(phylum = "", genus = "", species = "")
    }

    esu_info <- function(esu) {
        if (nchar(esu) > 0) {
            tt <- strsplit(esu, "-")[[1]]
            list(phylum = tt[[1]],
                 esu_id = tt[[2]])
        } else
            list(phylum = "",
                 esu_id = "")
    }

    species_voucher <- function(input_species) {
        sp_info <- species_info(input_species)
        ids <- filter(seq,
                      seq[["bold_phylum_id"]] == sp_info$phylum,
                      seq[["bold_genus_id"]] == sp_info$genus,
                      seq[["bold_species_id"]] == sp_info$species) %>%
            dplyr::select(voucher_number) %>%
            .[[1]]
    }

    esu_voucher <- function(input_esu) {
        es_info <- esu_info(input_esu)
        esus[esus[["phylum"]] == es_info$phylum &
               esus[["group_esu"]] == es_info$esu_id, ] %>%
            dplyr::select(voucher_number) %>%
            .[[1]]
    }

    list_thumbs_voucher <- function(vchr) {
        vchr_pth <- get_voucher_path(vchr)
        vchr_pth <- vchr_pth[file.exists(vchr_pth)]
        if (length(vchr_pth) > 0)
            list.files(path = thumb_path(vchr_pth),
                       pattern = "JPG$", full.names = TRUE)
        else character(0)
    }

    lst_esu_from_phylum <- function(phylum) {
        esus[esus[["phylum"]] == phylum, ] %>%
            dplyr::select(phylum, group_esu) %>%
            distinct(phylum, group_esu) %>%
            apply(1, function(x) paste(x[1], x[2], sep = "-")) %>%
            gsub("\\s", "", .)
    }

    ## for the gallery, only returns 1 image per esu
    thumb_from_esu <- function(esu) {
        vchrs <- esu_voucher(esu)
        vchr <- sample(vchrs, 1)
        sample(list_thumbs_voucher(vchr), 1)
    }

    output$voucher_selected <- renderText({
        paste("Information about", input$voucher_id)
    })

    output$voucher_phylum <- renderText({
        phylum <- smpl[smpl[["voucher_number"]] == input$voucher_id, "phylum"]
        paste("Phylum:", phylum)
    })

    output$voucher_taxa <- renderText({
        taxa <- smpl[smpl[["voucher_number"]] == input$voucher_id, "field_identification"]
        paste("Taxa:", taxa)
    })

    output$voucher_has_sequence <- renderText({
        seq_info <- seq[seq[["voucher_number"]] == input$voucher_id, ]
        if (nrow(seq_info) == 1 && seq_info[["success"]] == 1L)
            has_seq <- "yes"
        else
            has_seq <- "no"
        paste("Has sequence:", has_seq)
    })

    output$number_images <- renderText({
        paste("Number of images for specimen",
              length(list.files(path = thumb_path(voucher_path()), pattern = "JPG$"))
              )
    })


    render_img <- function(lst_files, voucher) {
        img_links <- gsub("thumbs/", "large/", lst_files)
        img_links <- gsub("www/", "", img_links)
        lst_files <- gsub("www/", "", lst_files)
        captions <- gsub("(.+)_(.+)_(.+)", "\\1_\\2", basename(img_links))

        div(tags$div(id = "test"),
            lapply(seq_along(lst_files), function(x) {
                div(style = "padding-top: 25px;",
                    p(captions[x]),
                    a(href = img_links[x], `data-lightbox` = voucher,
                      `data-title` = captions[x],
                      tags$img(src = lst_files[x], height = "255px",
                               style = "padding-top: -5px; padding-bottom: 10px;")
                      )
                )
            }),
            div(style = "height: 70px;")
            )
    }

    render_gallery <- function(lst_files, voucher) {
        img_links <- gsub("thumbs/", "large/", lst_files)
        img_links <- gsub("www/", "", img_links)
        lst_files <- gsub("www/", "", lst_files)
        div(tags$div(id = "test"),
            lapply(seq_along(lst_files), function(x) {
                div(style = "padding-top: 25px;",
                    p(voucher[x]),
                    a(href = img_links[x], `data-lightbox` = voucher,
                      tags$img(src = lst_files[x], height = "255px",
                               style = "padding-top: -5px; padding-bottom: 10px;")
                      )
                )
            }),
            div(style = "height: 70px;")
            )
    }

    bold_id_from_voucher <- function(voucher) {
        res <- seq %>%
            filter(voucher_number == voucher) %>%
            filter(success == 1) %>%
            dplyr::select(bold_genus_id, bold_species_id) %>%
            paste(collapse = " ")
        if (identical(res, "NA NA") || identical(res, "character(0) character(0)")) {
            res <- "no identification"
        }
        paste("BOLD ID: ", res)
    }

    output$list_img <- renderUI({
        lst_files <- list.files(path = file.path(voucher_path(), 'thumbs'),
                                pattern = "JPG$", full.names = TRUE)
        render_img(lst_files, input$voucher_id)
    })

    output$list_img_species <- renderUI({
        vchr <- species_voucher(input$species)
        lst_files <- lapply(vchr, list_thumbs_voucher)
        lst_files <- unlist(lst_files)
        render_img(lst_files, paste0(vchr, collapse = ""))
    })

    output$list_img_esu <- renderUI({
        vchr <- esu_voucher(input$esu)
        lst_files <- lapply(vchr, list_thumbs_voucher)
        lst_files <- unlist(lst_files)
        render_img(lst_files, paste0(vchr, collapse = ""))
    })

    output$gallery_esu <- renderUI({
        esus <- lst_esu_from_phylum(input$selected_phylum)
        lst_img <- lapply(esus, function(esu) {
            vchr <- esu_voucher(esu)
            lst_files <- lapply(vchr,
                                function(x) {
                list_thumbs_voucher(x)
            })
            lst_files <- unlist(lst_files)
            if (length(lst_files) < 1) {
                ##"www/no_thumb.png"
                character(0)
            } else {
                sample(unlist(lst_files), 1)
            }
        })
        esus <- esus[sapply(lst_img, length) > 0]
        lst_img <- lst_img[sapply(lst_img, length) > 0]
        render_gallery(lst_img, esus)
    })

    output$voucher_list <- renderText({
        vchr <- species_voucher(input$species)
        n_photos <- vapply(vchr, function(x) length(list_thumbs_voucher(x)),
                           numeric(1))
        paste("Voucher:", paste0(vchr, " (", n_photos, ")", collapse = ", "))
    })

    output$voucher_list_esu <- renderText({
        vchr <- esu_voucher(input$esu)
        paste("Voucher:", paste(vchr, collapse = ", "))
    })

    output$select_phylum <- renderText({
        paste("Phylum: ", input$selected_phylum)
    })

    output$voucher_bold_id <- renderText({
        bold_id_from_voucher(input$voucher_id)
    })

    output$esu_bold_id <- renderText({
        vchrs <- esu_voucher(input$esu)
        bold_id <- sapply(vchrs, bold_id_from_voucher)
        paste(unique(bold_id), collapse = ", ")
    })

    ## Map
    points <- reactive({
        which_smpl <- smpl[smpl[["voucher_number"]] == input$voucher_id, ]
        which_smpl <- dplyr::left_join(which_smpl, sta, by = "station_number")[, c("latitude_start", "longitude_start")]
        names(which_smpl) <- c("latitude", "longitude")
        which_smpl

    })

    output$station_map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addMarkers(data = points())
    })

    species_points <- function(vchr) {
        filter(smpl, voucher_number %in% vchr) %>%
            left_join(sta, by = "station_number") %>%
            dplyr::select(latitude = latitude_start,
                   longitude = longitude_start) %>%
            distinct
    }

    sp_points <- reactive({
        species_points(species_voucher(input$species))
    })

    esu_points <- reactive({
        species_points(esu_voucher(input$esu))
    })

    output$species_station_map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addMarkers(data = sp_points())
    })

    output$species_station_map2 <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addMarkers(data = esu_points())
    })

    observe({
        leafletProxy("species_station_map", data = sp_points())
    })

    observe({
        leafletProxy("species_station_map", data = esu_points())
    })

})
