##Libraries

library(bslib)
library(shiny)
library(shinyMobile)
library(apexcharter)
library(shinyWidgets)
library(cowplot)
library(httr)
library(apexcharter)
library(RcppArmadillo)
library(dashboardthemes)
library(sortable)
library(flexdashboard)
library(scales)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(devtools)
library(kableExtra)
library(data.table)
library(DT)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(ggdark)
library(ggpubr)
library(ggimage)
library(readr)
library(gsisdecoder)
library(RMySQL)
library(stringr)
library(glue)
library(tidyr)
library(kableExtra)
library(webshot)
library(formattable)
library(gt)
library(ggExtra)
library(gtExtras)
library(reactable)
library(reactablefmtr)
library(baseballr)
library(RDS)
library(depmixS4)
library(tidyr)
library(highcharter)
library(wehoop)
library(hms)
library(rsvg)
library(fontawesome)
library(glue)
library(gganimate)
library(ggimage)

player_stats <- wehoop::load_wbb_player_box(seasons=2022)

##Read in Data (fix this with sourcing)



###


f7Icon <- function(..., lib = NULL, color = NULL, style = NULL, old = NULL) {
  call_ <- as.list(match.call())
  if (!is.null(call_$old)) {
    warning(
      "Deprecated. This was to handle old and new icons. ",
      "This parameter will be removed in a future release."
    )
  }
  if (!is.null(lib)) {
    if (identical(lib, "ios")) {
      iconCl <- "icon f7-icons ios-only"
    }
    if (identical(lib, "md")) {
      iconCl <- "icon material-icons md-only"
    }
  } else {
    # class icon is necessary so that icons with labels render well,
    # for instance
    iconCl <- "icon f7-icons"
  }

  if (!is.null(color)) {
    iconCl <- paste0(iconCl, " color-", "#168fa0")
  }

  iconTag <- add_f7icons_dependencies(shiny::tags$i(class = iconCl,
                                                    style = style, ...))
  htmltools::browsable(iconTag)
}







cell_style <- function(data,
                       rows = NULL,
                       values = NULL,
                       font_color = NULL,
                       font_size = NULL,
                       font_style = "normal",
                       font_weight = "normal",
                       horizontal_align = "right",
                       vertical_align = "top",
                       text_decoration = NULL,
                       border_width = NULL,
                       border_style = NULL,
                       border_color = NULL,
                       background_color = NULL,
                       animation = "1s ease") {

  '%notin%' <- Negate('%in%')

  if (!is.null(horizontal_align) && horizontal_align %notin% c("left", "right", "center") == TRUE) {

    stop("horizontal_align must be either 'left', 'right', or 'center'")
  }

  if (!is.null(vertical_align) && vertical_align %notin% c("top", "bottom", "center") == TRUE) {

    stop("vertical_align must be either 'top', 'bottom', or 'center'")
  }

  # assign vertical align
  if (vertical_align == "top") {

    vertical_align_css <- "start"

  } else if (vertical_align == "bottom") {

    vertical_align_css <- "end"

  } else vertical_align_css <- "center"

  # assign horizontal align
  if (horizontal_align == "left") {

    horizontal_align_css <- "flex-start"

  } else if (horizontal_align == "right") {

    horizontal_align_css <- "flex-end"

  } else horizontal_align_css <- "center"

  style <- function(value, index, name) {

    if (!is.null(values) && values %in% data[[name]] == FALSE) {

      stop("values do not exist in dataset")

      if (!is.null(border_style) & grepl("solid | dashed | dotted | double | groove | ridge | inset | outset | none | hidden", border_style) == FALSE) {

        stop("border_style type must be either solid, dashed, dotted, double, groove, ridge, inset, outside, none, or hidden.")
      }

      if (!is.null(font_weight) & !is.numeric(font_weight) & grepl("normal | bold | bolder | lighter", font_weight) == FALSE) {

        stop("font_weight must either be a numeric value between 100 and 900 or one of normal, bold, bolder, or lighter.")
      }

      if (font_style %notin% c("normal", "italic") == TRUE) {

        stop("font_style must be either 'normal' or 'italic'")
      }

      if (!is.null(text_decoration) && text_decoration %notin% c("underline", "overline", "underline overline", "line-through") == TRUE) {

        stop("text_decoration must be either 'underline', 'overline', 'underline overline', or 'line-through'")
      }

    } else if (value %in% values | index %in% rows) {

      list(transition = animation,
           borderColor = border_color,
           borderWidth = border_width,
           borderStyle = border_style,
           color = font_color,
           background = background_color,
           textDecoration = text_decoration,
           fontStyle = font_style,
           fontWeight = font_weight,
           display = "flex",
           alignItems = vertical_align_css,
           justifyContent = horizontal_align_css,
           fontSize = font_size)

    } else if (is.null(values) & is.null(rows)) {

      list(transition = animation,
           borderColor = border_color,
           borderWidth = border_width,
           borderStyle = border_style,
           color = font_color,
           background = background_color,
           textDecoration = text_decoration,
           fontStyle = font_style,
           fontWeight = font_weight,
           display = "flex",
           alignItems = vertical_align_css,
           justifyContent = horizontal_align_css,
           fontSize = font_size)
    }
  }
}

shinyMobile_options <- list(
  theme = "auto",
  dark = TRUE,
  filled = FALSE,
  color = "#3c51d5",
  iosTranslucentBars = FALSE,
  navbar = list(
    iosCenterTitle = TRUE,
    hideOnPageScroll = TRUE
  ),
  toolbar = list(
    hideOnPageScroll = FALSE
  )
)



shinyApp(
  ui = f7Page(
    options = shinyMobile_options,
    title = "",

    f7TabLayout(
      panels = tagList(
        f7Panel(title = "wehoop NCAAW Analytics", side = "left", theme = "dark",
                div(img(src = 'https://a.espncdn.com/i/teamlogos/ncaa/500-dark/52.png',
                        style = "max-width:100%;max-height:100px;display:flex;align-items:center;"),
                    style = "text-align:center;"),
                div(img(src = 'https://a.espncdn.com/i/headshots/womens-college-basketball/players/full/4280885.png',
                        style = "max-width:100%;max-height:225px;display:flex;align-items:center;"),
                    style = "text-align:center;"),
                f7SmartSelect("team",
                              "Team",
                              choices=player_stats %>% pull(team_short_display_name) %>% unique() %>% sort(),
                              selected="Florida State",
                              openIn="popover",
                              multiple=FALSE,
                              searchbar=TRUE),

                f7SmartSelect("player",
                              "Player",
                              choices=player_stats %>% filter(team_short_display_name=="Florida State") %>%
                                pull(athlete_display_name) %>%
                                unique() %>%sort(),
                              selected="Amaya Brown",
                              openIn="popover",
                              multiple=FALSE,
                              searchbar=TRUE),

                f7DatePicker(
                  inputId = "dates1",
                  label = "Start Date",
                  value = c(today()-13),
                  multiple = FALSE
                ),
                f7DatePicker(
                  inputId = "dates2",
                  label = "End Date",
                  value = c(today()),
                  multiple = FALSE
                ),
                f7Link(label = "SDV Site", "https://google.com", icon = f7Icon("user")),
                f7Link(label = "WNBA Site", "https://google.com", icon = f7Icon("user")),
                f7Link(label = "SwishAppeal", "https://www.swishappeal.com/", icon = f7Icon("user")),
                f7Link(label = "Test Site 4", "https://google.com", icon = f7Icon("user")),
                f7Link(label = "Test Site 5", "https://google.com", icon = f7Icon("user")),
                effect = "cover"),
        f7Panel(title = "wehoop Analytics", side = "right", theme = "dark",
                div(img(src = 'https://images.vexels.com/media/users/3/229713/isolated/preview/7fd0256e8d02af6f516b5a582e0ad977-female-basketball-player-playing-cut-out.png',style = "max-width:100%;max-height:175px;display:flex;align-items:center;"),
                    style = "text-align:center;"),
                "Welcome to wehoop Mobile! This is a working demonstration of WNBA using shinyMobile and wehoop. wehoop is an R package for working with women’s college and professional basketball data. The package has functions to access live play by play and box score data from ESPN with shot locations when available.
A scraping and aggregating interface for ESPN’s women’s college basketball and WNBA statistics. It provides users with the capability to access the API’s game play-by-plays, box scores, standings and results to analyze the data for themselves.",
                f7Link(label = "Follow Saiem on Twitter", "https://twitter.com/SaiemGilani", icon = f7Icon("logo_twitter")),
                f7Link(label = "Follow Saiem on LinkedIn", "   https://www.linkedin.com/in/saiem-gilani/", icon = f7Icon("logo_linkedin")),
                effect = "cover")
      ),
      navbar =   f7Navbar(# Navbar ----
                           title = div(img(src = 'https://js.sportsdataverse.org/img/logo.png',style = "max-width:100%;max-height:40px;display:flex;align-items:center;"),
                                              style = "text-align:center;"),
                          # subtitle = "Trade Calculator",
                          transparent = TRUE,
                          hairline = TRUE,
                          leftPanel = TRUE,
                          rightPanel = TRUE),
      f7Tabs(
        animated = TRUE,
        f7Tab(
          tags$head(
            tags$script(
              'Shiny.addCustomMessageHandler("ui-tweak", function(message) {
                var os = message.os;
                var skin = message.skin;

                  $("html").addClass("ios");
                  $("html").removeClass("md");
                  $(".tab-link-highlight").hide();

                  $("html").addClass("theme-dark");



               });
              '
            ),
            tags$style(type = 'text/css',
                       HTML('.navbar { background-color: #3c51d5;}
                          .navbar-default .navbar-brand{color: white;}
                          .tab-panel{ background-color: #3c51d5; color: white}
                          .navbar-default .navbar-nav > .active > a,
                           .navbar-default .navbar-nav > .active > a:focus,
                           .navbar-default .navbar-nav > .active > a:hover {
                                color: #3c51d5;
                                background-color: #3c51d5;
                           }
                    ')
            )
          ),
          f7Panel(title = "NewPan", side = "left", theme = "dark",
                  "On here you will find.......", effect = "cover"),
          tabName = "Player Info",
          icon = f7Icon("person_alt"),
          active = FALSE,

          f7BlockTitle(title = "Player Information") %>% f7Align(side = "center"),
          f7Tabs(
            style = "strong",
            animated = TRUE,
            swipeable = FALSE,
            f7Tab(
              tabName = "Stats",
              active = TRUE,
              highchartOutput("pts")
            ),
            f7Tab(
              tabName = "Comparison Tool",

            )
          )

        ),
        f7Tab(
          tabName = "Team Info",
          icon = f7Icon("person_3_fill"),
          active = FALSE,
          f7BlockTitle(title = "Team Information") %>% f7Align(side = "center"),

          f7Tabs(
            style = "strong",
            animated = TRUE,
            swipeable = FALSE,
            f7Tab(
              tabName = "Schedule",
              active = TRUE,
              gt::gt_output("schedule")
            ),
            f7Tab(
              tabName = "Standings",
            gt::gt_output("standings")
            ),
            f7Tab(
              tabName = "Season Stats",
            apexcharter::apexchartOutput("heat")
            )
            ))
      )
    )
  ),

  server = function(input, output, session) {


    output$schedule <- render_gt({

      wehoop::load_wbb_schedule(seasons=2022) %>% dplyr::select(
        home.logo,
        away.logo,
        date) %>%
        mutate(` `=home.logo,
               `  `=away.logo,
               Date=gsub('character', '', as.character(date)))%>%
        dplyr::select(4,5,6) %>%
        dplyr::arrange(desc(Date)) %>%
        gt()%>%
        gt_img_rows(` `) %>%
        gt_img_rows(`  `) %>%
        tab_options(heading.align = "left", table.background.color = "#000000",
                    column_labels.background.color = "#000000", table.font.color.light = "#3c51d5",
                    column_labels.border.top.style = "none", table.border.top.style = "none",
                    table.border.bottom.color = "#000000", column_labels.border.bottom.width = 3,
                    column_labels.border.bottom.color = "#3c51d5", table_body.border.top.style = "none",
                    table_body.border.bottom.color = "#000000", table.border.left.color = "#000000",
                    table.border.right.color = "#000000", heading.border.bottom.style = "none",
                    data_row.padding = px(7))

    })





    output$standings <- render_gt({

      merge(wehoop::espn_wbb_standings(2022) %>%
              dplyr::select(team,total,avgpointsfor,avgpointsagainst) %>%
              dplyr::rename(Team=team,
                            Record=total,
                            AvgPts=avgpointsfor,
                            AvgPtsAgainst=avgpointsagainst) %>%
              dplyr::mutate(AvgPts=round(as.numeric(AvgPts),2),
                            AvgPtsAgainst=round(as.numeric(AvgPtsAgainst),2)),

            wehoop::load_wbb_schedule(seasons=2022) %>% dplyr::select(home.displayName,
                                                                      home.logo) %>%
              group_by(home.displayName) %>%
              summarise(home.logo=home.logo[1]),by.x="Team",by.y="home.displayName") %>%
        dplyr::mutate(` `=home.logo) %>%
        dplyr::select(` `,Team:AvgPtsAgainst) %>%
        gt() %>%
        gt_img_rows(` `) %>%
     tab_options(heading.align = "left", table.background.color = "#000000",
                        column_labels.background.color = "#000000", table.font.color.light = "#3c51d5",
                        column_labels.border.top.style = "none", table.border.top.style = "none",
                        table.border.bottom.color = "#000000", column_labels.border.bottom.width = 3,
                        column_labels.border.bottom.color = "#3c51d5", table_body.border.top.style = "none",
                        table_body.border.bottom.color = "#000000", table.border.left.color = "#000000",
                        table.border.right.color = "#000000", heading.border.bottom.style = "none",
                        data_row.padding = px(7))

    })

   hc <-  renderHighchart({

      wehoop::load_wbb_team_box(seasons=2022) %>%
        filter(team_display_name=="LSU Tigers") %>%
        hchart("column", hcaes(x = game_date, y = assists))
    })



   pts <- renderHighchart({

     player_stats %>% filter(team_short_display_name=="Florida State",
                             athlete_display_name=="Bianca Jackson") %>%

       arrange(game_date) %>%
       mutate(seasonpoints=cumsum(pts))  %>%
       hchart("column", hcaes(x = game_date, y = as.numeric(pts)),color = "#782F40") %>%
       hc_add_theme(hc_theme_darkunica())%>% hc_title(
         text = "<b>Points (Game-Level)</b>",
         margin = 20,
         align = "center",
         style = list(color = "white", useHTML = TRUE)
       ) %>% hc_xAxis(title=list(text="Date"))%>% hc_yAxis(title=list(text="Points"))
   }
   )

    observe({
      session$sendCustomMessage(
        type = "ui-tweak",
        message = list(os = input$theme, skin = input$color)
      )
    })

  }
)

