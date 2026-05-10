# =============================================================================
# server.R — ABSVA Dashboard
# =============================================================================
# Fixes from v1:
#   - Renamed "/100" column to "Per100" (slashes break DT JS init)
#   - Removed initComplete JS callback (was throwing [object Object])
#   - formatStyle uses column NAMES not indices (avoids hidden-column drift)
#   - Numeric columns kept numeric so styleInterval works
# =============================================================================

server <- function(input, output, session) {

  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a


  # ── 1. Kicker / KPI strip ──────────────────────────────────────────────────

  output$kicker <- renderText({
    sprintf("2026 season · through %s · %d games · %d qualifying catchers",
            format(max(events_raw$game_date, na.rm = TRUE), "%b %d"),
            length(unique(events_raw$game_pk)),
            nrow(leaderboard))
  })

  kpi_card <- function(label, value, color = THEME$text_primary) {
    div(class = "stat-card",
        div(class = "label", label),
        div(class = "value", style = sprintf("color: %s;", color), value))
  }

  output$kpi_total_events <- renderUI({
    kpi_card("Total events", format(nrow(events_raw), big.mark = ","))
  })
  output$kpi_challenges <- renderUI({
    kpi_card("Challenges",
             format(sum(events_raw$event_type %in%
                          c("challenge_win","bad_challenge","overturned_frame")),
                    big.mark = ","))
  })
  output$kpi_overturn_rate <- renderUI({
    kpi_card("Overturn rate", "53.2%")
  })
  output$kpi_top_absva <- renderUI({
    top <- max(leaderboard$absva, na.rm = TRUE)
    kpi_card("Top ABSVA", fmt_signed(top, 1), color = "#E24B4A")
  })


  # ── 2. Filtered leaderboard reactive ───────────────────────────────────────

  filtered_lb <- reactive({
    df <- leaderboard %>%
      filter(pitches_caught >= (input$min_pitches %||% 50))

    if (nzchar(input$search %||% "")) {
      df <- df %>%
        filter(grepl(input$search, catcher_name, ignore.case = TRUE))
    }

    sort_col <- switch(input$sort_by %||% "absva",
                       "absva"   = "absva",
                       "per100"  = "absva_per_100",
                       "framing" = "rv_framing")

    df %>% arrange(desc(.data[[sort_col]]))
  })


  # ── 3. Leaderboard DataTable ───────────────────────────────────────────────

  output$leaderboard_table <- renderDT({
    df <- filtered_lb()

    # Build display frame — keep ABSVA and Per100 NUMERIC for styleInterval
    df_display <- df %>%
      mutate(
        Rank      = row_number(),
        Catcher   = catcher_name,
        Pitches   = pitches_caught,
        FS        = n_framing_steal,
        CW        = n_challenge_win,
        FU        = n_overturned_frame,
        MP        = n_miss_penalty,
        BC        = n_bad_challenge,
        ABSVA     = round(absva, 1),
        Per100    = round(absva_per_100, 2),
        FielderID = fielder_2
      ) %>%
      select(Rank, Catcher, Pitches, FS, CW, FU, MP, BC, ABSVA, Per100, FielderID)

    datatable(
      df_display,
      rownames  = FALSE,
      selection = list(mode = "single", target = "row"),
      escape    = FALSE,
      class     = "compact stripe",
      options   = list(
        pageLength = 25,
        dom        = "tip",
        columnDefs = list(
          list(visible = FALSE,
               targets = which(names(df_display) == "FielderID") - 1),
          list(className = "dt-right",
               targets = which(names(df_display) %in%
                                 c("Pitches","ABSVA","Per100")) - 1),
          list(className = "dt-center",
               targets = which(names(df_display) %in%
                                 c("FS","CW","FU","MP","BC")) - 1)
        )
      )
    ) %>%
      formatRound(c("ABSVA"),  digits = 1) %>%
      formatRound(c("Per100"), digits = 2) %>%
      formatStyle("Pitches",     color = THEME$text_muted) %>%
      formatStyle(c("FS","CW"),  color = "#E24B4A") %>%
      formatStyle("FU",          color = THEME$text_dim) %>%
      formatStyle(c("MP","BC"),  color = "#378ADD") %>%
      formatStyle("ABSVA",
                  color      = styleInterval(c(-0.001, 0.001),
                                             c("#378ADD", THEME$text_muted, "#E24B4A")),
                  fontWeight = 500) %>%
      formatStyle("Per100",
                  color = styleInterval(c(-0.001, 0.001),
                                        c("#378ADD", THEME$text_muted, "#E24B4A")))
  })


  # ── 4. Modal opening on row click ──────────────────────────────────────────

  observeEvent(input$leaderboard_table_rows_selected, {
    sel <- input$leaderboard_table_rows_selected
    if (length(sel) == 0) return()

    df  <- filtered_lb()
    row <- df[sel, ]
    fielder_id <- row$fielder_2

    open_catcher_modal(row, fielder_id, output, session)
  })


  # ── 5. Reactive: filters inside the modal ──────────────────────────────────

  modal_filters <- reactiveValues(event = "all", count = "all", game = "all")

  observeEvent(input$flt_event_all,        { modal_filters$event <- "all" })
  observeEvent(input$flt_event_framing,    { modal_filters$event <- "framing_steal" })
  observeEvent(input$flt_event_challenges, { modal_filters$event <- "challenges" })
  observeEvent(input$flt_event_misses,     { modal_filters$event <- "miss_penalty" })

  observeEvent(input$flt_count_all,    { modal_filters$count <- "all" })
  observeEvent(input$flt_count_2strk,  { modal_filters$count <- "two_strikes" })
  observeEvent(input$flt_count_hilev,  { modal_filters$count <- "high_leverage" })

  observeEvent(input$modal_game, {
    modal_filters$game <- input$modal_game %||% "all"
  })


  # ── 6. Strike zone plotly renderer ─────────────────────────────────────────

  current_catcher_id <- reactiveVal(NULL)

  output$strike_zone <- renderPlotly({
    fid <- current_catcher_id()
    req(fid)

    df <- events_for_catcher(fid)

    if (modal_filters$event == "framing_steal") {
      df <- df %>% filter(event_type == "framing_steal")
    } else if (modal_filters$event == "challenges") {
      df <- df %>% filter(event_type %in%
                            c("challenge_win","bad_challenge","overturned_frame"))
    } else if (modal_filters$event == "miss_penalty") {
      df <- df %>% filter(event_type == "miss_penalty")
    }

    if (modal_filters$count == "two_strikes") {
      df <- df %>% filter(strikes == 2)
    } else if (modal_filters$count == "high_leverage") {
      df <- df %>% filter(outs_when_up == 2 | !is.na(on_2b) | !is.na(on_3b))
    }

    if (!is.null(modal_filters$game) && modal_filters$game != "all") {
      df <- df %>% filter(as.character(game_pk) == modal_filters$game)
    }

    if (nrow(df) == 0) {
      empty_msg <- if (!is.null(modal_filters$game) && modal_filters$game != "all") {
        "No pitches in this game match the current filters"
      } else {
        "No pitches match the current filters"
      }
      return(plot_ly() %>%
               layout(annotations = list(
                 list(text = empty_msg,
                      x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                      showarrow = FALSE,
                      font = list(color = THEME$text_muted))),
                 paper_bgcolor = THEME$bg_card,
                 plot_bgcolor  = THEME$bg_card))
    }

    sz_top_med <- median(df$sz_top, na.rm = TRUE)
    sz_bot_med <- median(df$sz_bot, na.rm = TRUE)

    df <- df %>%
      mutate(
        size_px      = pmax(6, pmin(18, 6 + abs(event_run_value) * 50)),
        color_hex    = EVENT_COLORS[event_type],
        event_label  = EVENT_LABELS[event_type],
        count_label  = paste0(balls, "-", strikes),
        leverage     = case_when(
          outs_when_up == 2 & (!is.na(on_2b) | !is.na(on_3b)) ~ "High",
          outs_when_up == 2                                    ~ "Med-high",
          !is.na(on_2b) | !is.na(on_3b)                        ~ "Medium",
          TRUE                                                  ~ "Low"
        ),
        rv_label = vapply(event_run_value, fmt_signed, character(1), digits = 3),
        hover    = paste0(
          "<b>", event_label, "</b><br>",
          "Count: ", count_label, "<br>",
          "Leverage: ", leverage, "<br>",
          "Run value: ", rv_label
        )
      )

    plot_ly(
      data    = df,
      x       = ~plate_x,
      y       = ~plate_z,
      type    = "scatter",
      mode    = "markers",
      marker  = list(
        size  = ~size_px,
        color = ~color_hex,
        line  = list(width = 0)
      ),
      text      = ~hover,
      hoverinfo = "text"
    ) %>%
      add_segments(x = -ABS_HALF_WIDTH, xend =  ABS_HALF_WIDTH,
                   y = sz_bot_med,      yend = sz_bot_med,
                   line = list(color = THEME$text_muted, width = 1.5),
                   inherit = FALSE, hoverinfo = "skip", showlegend = FALSE) %>%
      add_segments(x = -ABS_HALF_WIDTH, xend =  ABS_HALF_WIDTH,
                   y = sz_top_med,      yend = sz_top_med,
                   line = list(color = THEME$text_muted, width = 1.5),
                   inherit = FALSE, hoverinfo = "skip", showlegend = FALSE) %>%
      add_segments(x = -ABS_HALF_WIDTH, xend = -ABS_HALF_WIDTH,
                   y = sz_bot_med,      yend = sz_top_med,
                   line = list(color = THEME$text_muted, width = 1.5),
                   inherit = FALSE, hoverinfo = "skip", showlegend = FALSE) %>%
      add_segments(x =  ABS_HALF_WIDTH, xend =  ABS_HALF_WIDTH,
                   y = sz_bot_med,      yend = sz_top_med,
                   line = list(color = THEME$text_muted, width = 1.5),
                   inherit = FALSE, hoverinfo = "skip", showlegend = FALSE) %>%
      layout(
        xaxis = list(
          range = c(1.5, -1.5),
          title = "",
          zeroline = FALSE, showgrid = FALSE, showticklabels = FALSE,
          fixedrange = TRUE
        ),
        yaxis = list(
          range = c(0, 5),
          title = "",
          zeroline = FALSE, showgrid = FALSE, showticklabels = FALSE,
          fixedrange = TRUE,
          scaleanchor = "x", scaleratio = 1
        ),
        paper_bgcolor = THEME$bg_card,
        plot_bgcolor  = THEME$bg_card,
        showlegend    = FALSE,
        margin        = list(l = 10, r = 10, t = 30, b = 10),
        annotations   = list(
          list(text = "CATCHER'S VIEW", x = 0.5, y = 1.08,
               xref = "paper", yref = "paper", showarrow = FALSE,
               font = list(color = THEME$text_dim, size = 9)),
          list(text = "RHB", x = 0.95, y = 0.5,
               xref = "paper", yref = "paper", showarrow = FALSE,
               font = list(color = THEME$text_faint, size = 9)),
          list(text = "LHB", x = 0.05, y = 0.5,
               xref = "paper", yref = "paper", showarrow = FALSE,
               font = list(color = THEME$text_faint, size = 9))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })


  # ── 7. Modal builder ───────────────────────────────────────────────────────

  open_catcher_modal <- function(row, fielder_id, output, session) {
    current_catcher_id(fielder_id)

    # Build per-catcher game choices for the modal's game dropdown.
    # Catcher's defense team = home if inning_topbot == "Top", else away,
    # so the opponent is the OTHER team. Take first row per game (consistent
    # within a game for a single catcher).
    catcher_games <- events_for_catcher(fielder_id) %>%
      group_by(game_pk) %>%
      summarise(
        game_date     = first(game_date),
        home_team     = first(home_team),
        away_team     = first(away_team),
        inning_topbot = first(inning_topbot),
        .groups = "drop"
      ) %>%
      mutate(
        opponent = if_else(inning_topbot == "Top", away_team, home_team),
        label    = sprintf("%s vs %s", format(game_date, "%b %d"),
                           ifelse(is.na(opponent), "?", opponent))
      ) %>%
      arrange(desc(game_date))

    game_choices <- c("All games" = "all",
                      setNames(as.character(catcher_games$game_pk),
                               catcher_games$label))

    pct_color <- function(p) {
      if (is.na(p)) "#6B7280"
      else if (p >= 75) "#E24B4A"
      else if (p >= 25) "#888780"
      else "#378ADD"
    }

    pct_bar <- function(label, pct) {
      color <- pct_color(pct)
      tagList(
        span(label, style = sprintf("color: %s;", THEME$text_muted)),
        div(class = "pct-bar-bg",
            div(class = "pct-bar-fill",
                style = sprintf("width: %d%%; background: %s;",
                                as.integer(pct), color)),
            div(class = "pct-marker",
                style = sprintf("left: %d%%;", as.integer(pct)))),
        span(as.character(pct),
             style = sprintf("color: %s; font-weight: 500; font-variant-numeric: tabular-nums;",
                             color))
      )
    }

    breakdown_row <- function(et, n, rv, vs_avg) {
      tags$tr(style = sprintf("border-top: 0.5px solid %s;", THEME$border),
        tags$td(style = "padding: 7px 0;",
           span(style = sprintf("display:inline-block;width:7px;height:7px;background:%s;border-radius:50%%;margin-right:6px;",
                                EVENT_COLORS[et])),
           EVENT_LABELS[et]),
        tags$td(style = sprintf("text-align: right; color: %s; font-variant-numeric: tabular-nums;",
                                THEME$text_muted),
                format(n, big.mark = ",")),
        tags$td(style = sprintf("text-align: right; color: %s; font-variant-numeric: tabular-nums;",
                                if (rv > 0) "#E24B4A" else if (rv < 0) "#378ADD" else THEME$text_dim),
                fmt_signed(rv, 1)),
        tags$td(style = sprintf("text-align: right; color: %s; font-variant-numeric: tabular-nums;",
                                if (vs_avg > 0) "#E24B4A" else if (vs_avg < 0) "#378ADD" else THEME$text_dim),
                fmt_signed(vs_avg, 1))
      )
    }

    pct100  <- as.numeric(row$pitches_caught) / 100

    showModal(modalDialog(
      size      = "l",
      easyClose = TRUE,
      footer    = NULL,
      title     = NULL,

      div(style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 14px;",
        div(
          h2(row$catcher_name,
             style = sprintf("font-size: 22px; font-weight: 500; margin: 0; color: %s;",
                             THEME$text_primary)),
          div(sprintf("%s pitches caught · %d ABSVA events",
                      format(row$pitches_caught, big.mark = ","),
                      row$n_framing_steal + row$n_challenge_win + row$n_overturned_frame +
                      row$n_miss_penalty + row$n_bad_challenge),
              style = sprintf("font-size: 12px; color: %s; margin-top: 3px;",
                              THEME$text_muted))
        )
      ),

      div(style = "display: grid; grid-template-columns: 1fr 1fr 1fr 1fr; gap: 8px; margin-bottom: 16px;",
        div(class = "stat-card",
            div(class = "label", "ABSVA"),
            div(class = "value",
                style = sprintf("color: %s;",
                                if (row$absva > 0) "#E24B4A" else "#378ADD"),
                fmt_signed(row$absva, 1)),
            div(class = "sub", sprintf("%dth pct", row$pct_absva))),
        div(class = "stat-card",
            div(class = "label", "per 100"),
            div(class = "value",
                style = sprintf("color: %s;",
                                if (row$absva_per_100 > 0) "#E24B4A" else "#378ADD"),
                fmt_signed(row$absva_per_100, 2)),
            div(class = "sub", sprintf("%dth pct", row$pct_absva_per100))),
        div(class = "stat-card",
            div(class = "label", "framing only"),
            div(class = "value",
                style = sprintf("color: %s;",
                                if (row$rv_framing > 0) "#E24B4A" else "#378ADD"),
                fmt_signed(row$rv_framing, 1)),
            div(class = "sub", sprintf("%dth pct", row$pct_framing))),
        div(class = "stat-card",
            div(class = "label", "challenge net"),
            div(class = "value",
                style = sprintf("color: %s;",
                                if ((row$rv_challenge_win + row$rv_bad_challenge) > 0)
                                  "#E24B4A" else "#378ADD"),
                fmt_signed(row$rv_challenge_win + row$rv_bad_challenge, 1)),
            div(class = "sub", sprintf("%dth pct", row$pct_challenge_rate)))
      ),

      div(style = "display: grid; grid-template-columns: 1fr 1.1fr; gap: 14px;",

        div(
          div(class = "uppercase-label", "strike zone · catcher's view"),
          div(style = "margin-bottom: 8px;",
              selectizeInput(
                "modal_game", label = NULL,
                choices  = game_choices,
                selected = "all",
                width    = "100%",
                options  = list(
                  searchField = c("label", "value"),
                  placeholder = "Filter by game"
                )
              )),
          div(style = sprintf("background: %s; border-radius: 8px; padding: 10px;", THEME$bg_card),
              plotlyOutput("strike_zone", height = "320px")),

          div(style = sprintf("background: %s; border-radius: 8px; padding: 12px; margin-top: 10px;", THEME$bg_card),
            div(class = "uppercase-label", style = "margin-bottom: 6px;", "filter pitches"),
            div(style = "display: flex; flex-wrap: wrap; gap: 5px; margin-bottom: 8px;",
              actionLink("flt_event_all",        "All",        class = "pill active"),
              actionLink("flt_event_framing",    "Framing",    class = "pill"),
              actionLink("flt_event_challenges", "Challenges", class = "pill"),
              actionLink("flt_event_misses",     "Misses",     class = "pill")
            ),
            div(class = "uppercase-label", style = "margin: 8px 0 6px;", "count situation"),
            div(style = "display: flex; flex-wrap: wrap; gap: 5px;",
              actionLink("flt_count_all",   "All counts",    class = "pill active"),
              actionLink("flt_count_2strk", "2 strikes",     class = "pill"),
              actionLink("flt_count_hilev", "High leverage", class = "pill")
            )
          )
        ),

        div(
          div(class = "uppercase-label", "event breakdown"),
          div(style = sprintf("background: %s; border-radius: 8px; padding: 12px 14px; margin-bottom: 14px;",
                              THEME$bg_card),
            tags$table(style = "width: 100%; border-collapse: collapse; font-size: 12px;",
              tags$thead(
                tags$tr(style = sprintf("font-size: 9px; color: %s; text-transform: uppercase; letter-spacing: 1px;",
                                        THEME$text_dim),
                  tags$th("event",  style = "text-align: left; padding-bottom: 6px;"),
                  tags$th("n",      style = "text-align: right; padding-bottom: 6px;"),
                  tags$th("RV",     style = "text-align: right; padding-bottom: 6px;"),
                  tags$th("vs avg", style = "text-align: right; padding-bottom: 6px;")
                )
              ),
              tags$tbody(
                breakdown_row("framing_steal", row$n_framing_steal, row$rv_framing,
                              row$rv_framing       - LEAGUE_AVG_PER100$framing_steal * pct100),
                breakdown_row("challenge_win", row$n_challenge_win, row$rv_challenge_win,
                              row$rv_challenge_win - LEAGUE_AVG_PER100$challenge_win * pct100),
                breakdown_row("overturned_frame", row$n_overturned_frame, 0, 0),
                breakdown_row("miss_penalty",  row$n_miss_penalty,  row$rv_miss_penalty,
                              row$rv_miss_penalty  - LEAGUE_AVG_PER100$miss_penalty  * pct100),
                breakdown_row("bad_challenge", row$n_bad_challenge, row$rv_bad_challenge,
                              row$rv_bad_challenge - LEAGUE_AVG_PER100$bad_challenge * pct100),
                tags$tr(style = sprintf("border-top: 1px solid %s;", THEME$text_faint),
                  tags$td("Total", style = "padding: 8px 0; font-weight: 500;"),
                  tags$td(format(row$n_framing_steal + row$n_challenge_win + row$n_overturned_frame +
                                 row$n_miss_penalty + row$n_bad_challenge, big.mark = ","),
                          style = sprintf("text-align: right; color: %s; font-weight: 500; font-variant-numeric: tabular-nums;",
                                          THEME$text_muted)),
                  tags$td(fmt_signed(row$absva, 1),
                          style = sprintf("text-align: right; color: %s; font-weight: 500; font-variant-numeric: tabular-nums;",
                                          if (row$absva > 0) "#E24B4A" else "#378ADD")),
                  tags$td(fmt_signed(row$absva - LEAGUE_AVG_PER100$total * pct100, 1),
                          style = sprintf("text-align: right; color: %s; font-weight: 500; font-variant-numeric: tabular-nums;",
                                          if ((row$absva - LEAGUE_AVG_PER100$total * pct100) > 0)
                                            "#E24B4A" else "#378ADD"))
                )
              )
            )
          ),

          div(class = "uppercase-label", "league percentile · per-100 basis"),
          div(style = sprintf("background: %s; border-radius: 8px; padding: 14px;", THEME$bg_card),
            div(style = "display: grid; grid-template-columns: auto 1fr auto; gap: 6px 10px; font-size: 11px; align-items: center;",
              pct_bar("ABSVA",      row$pct_absva_per100),
              pct_bar("Framing",    row$pct_framing),
              pct_bar("Challenge%", row$pct_challenge_rate),
              pct_bar("Avoid bad",  row$pct_avoid_bad),
              pct_bar("Miss avoid", row$pct_avoid_miss)
            ),
            div(style = sprintf("font-size: 10px; color: %s; line-height: 1.7; margin-top: 12px; padding-top: 10px; border-top: 0.5px solid %s;",
                                THEME$text_dim, THEME$border),
              sprintf("Bars show %s's rank among %d qualifying catchers (min %d pitches). Higher is always better — \"Avoid bad\" and \"Miss avoid\" are inverted so 99th = fewest negative events.",
                      row$catcher_name, nrow(leaderboard), MIN_PITCHES_QUALIFY))
          )
        )
      )
    ))

    modal_filters$event <- "all"
    modal_filters$count <- "all"
    modal_filters$game  <- "all"
  }
}
