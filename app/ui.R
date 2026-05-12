# =============================================================================
# ui.R — ABSVA Dashboard
# =============================================================================
# Layout: bs4Dash with one main page (Leaderboard).
# Custom CSS pushes the look toward the dark athletic aesthetic from the mockup.
# Modal is constructed server-side via showModal() — see server.R.
# =============================================================================

custom_css <- HTML(sprintf("
  body, .content-wrapper { background: %s !important; color: %s; }
  .main-header.navbar { background: %s !important; border-bottom: 1px solid %s !important; }
  .main-sidebar { background: %s !important; }
  .nav-sidebar > .nav-item > .nav-link.active { background: %s !important; color: %s !important; }
  .card { background: %s !important; border: 1px solid %s !important; color: %s !important; }
  .card-header { border-bottom: 1px solid %s !important; }
  .small-box { background: %s !important; color: %s !important; }
  .table { color: %s !important; }
  .table thead th { color: %s !important; border-bottom: 1px solid %s !important; font-size: 10px !important; letter-spacing: 1px; text-transform: uppercase; }
  .table tbody tr { border-top: 1px solid %s !important; cursor: pointer; }
  .table tbody tr:hover { background: %s !important; }
  .modal-content { background: %s !important; color: %s !important; border: 1px solid %s !important; }
  .modal-header { border-bottom: 1px solid %s !important; }
  .modal-footer { border-top: 1px solid %s !important; }
  .form-control, .form-select { background: %s !important; color: %s !important; border: 1px solid %s !important; }
  .pill { font-size: 11px; padding: 4px 9px; border-radius: 12px; background: %s; color: %s; border: 1px solid %s; cursor: pointer; user-select: none; }
  .pill.active { background: #2A1A1F; color: #E24B4A; border: 1px solid #4A2A2F; }
  .pct-bar-bg { background: %s; border-radius: 4px; height: 8px; position: relative; }
  .pct-bar-fill { position: absolute; left: 0; top: 0; height: 100%%; border-radius: 4px; }
  .pct-marker { position: absolute; top: -3px; width: 2px; height: 14px; background: %s; }
  .stat-card { background: %s; border-radius: 8px; padding: 10px 12px; }
  .stat-card .label { font-size: 9px; color: %s; text-transform: uppercase; letter-spacing: 1px; }
  .stat-card .value { font-size: 20px; font-weight: 500; }
  .stat-card .sub   { font-size: 10px; color: %s; margin-top: 2px; }
  .uppercase-label { font-size: 10px; color: %s; text-transform: uppercase; letter-spacing: 1.5px; margin-bottom: 8px; }
",
  THEME$bg_app, THEME$text_primary,
  THEME$bg_app, THEME$border,
  THEME$bg_app,
  THEME$bg_card_hi, THEME$text_primary,
  THEME$bg_card, THEME$border, THEME$text_primary,
  THEME$border,
  THEME$bg_card, THEME$text_primary,
  THEME$text_primary,
  THEME$text_muted, THEME$border,
  THEME$border,
  THEME$bg_card_hi,
  THEME$bg_card, THEME$text_primary, THEME$border,
  THEME$border,
  THEME$border,
  THEME$bg_card, THEME$text_primary, THEME$border,
  THEME$bg_card, THEME$text_muted, THEME$border,
  THEME$border,
  THEME$text_primary,
  THEME$bg_card,
  THEME$text_dim,
  THEME$text_muted,
  THEME$text_dim
))


ui <- bs4DashPage(
  title = "ABSVA — Catcher Run Value Dashboard",
  dark  = TRUE,
  header = bs4DashNavbar(
    title = bs4DashBrand(
      title = "ABSVA",
      color = "primary"
    ),
    skin = "dark"
  ),
  sidebar = bs4DashSidebar(
    skin = "dark",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Leaderboard", tabName = "leaderboard", icon = icon("trophy"))
    )
  ),
  body = bs4DashBody(
    tags$head(tags$style(custom_css)),

    bs4TabItems(
      bs4TabItem(
        tabName = "leaderboard",

        # ─── Header / kicker ─────────────────────────────────────────────────
        fluidRow(
          column(12,
            div(style = sprintf("font-size: 11px; letter-spacing: 1.5px; color: %s; text-transform: uppercase;",
                                THEME$text_dim),
                "ABSVA Dashboard"),
            h2("Catcher Leaderboard",
               style = sprintf("font-size: 22px; font-weight: 500; margin: 4px 0 2px; color: %s;",
                               THEME$text_primary)),
            div(textOutput("kicker", inline = TRUE),
                style = sprintf("font-size: 12px; color: %s; margin-bottom: 14px;",
                                THEME$text_muted))
          )
        ),

        # ─── KPI strip ───────────────────────────────────────────────────────
        fluidRow(
          column(3, uiOutput("kpi_total_events")),
          column(3, uiOutput("kpi_challenges")),
          column(3, uiOutput("kpi_overturn_rate")),
          column(3, uiOutput("kpi_top_absva"))
        ),

        # ─── Filter bar ──────────────────────────────────────────────────────
        fluidRow(
          column(12,
            div(style = "display: flex; gap: 10px; align-items: center; margin: 16px 0 12px;",
              textInput("search", NULL, placeholder = "Search catcher...",
                        width = "260px"),
              selectInput("sort_by", NULL,
                          choices = c("Sort: ABSVA"      = "absva",
                                      "Sort: ABSVA / 100" = "per100",
                                      "Sort: Frame only"   = "frame"),
                          width = "180px", selected = "absva"),
              span("Min pitches", style = sprintf("font-size: 11px; color: %s;", THEME$text_dim)),
              numericInput("min_pitches", NULL, value = 50, min = 0, step = 25,
                           width = "80px")
            )
          )
        ),

        # ─── Main leaderboard table ──────────────────────────────────────────
        fluidRow(
          column(12,
            DTOutput("leaderboard_table")
          )
        )
      )
    )
  )
)
