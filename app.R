# Weight Loss Trend
library(shiny)
library(shinydashboard)
library(reactable)
library(shinyjs)
library(shinyWidgets)

GOALWEIGHT <- 185

############################## UI ##############################################

ui <- dashboardPage(
    title = "Weight Loss Tracking Dashboard",
    dashboardHeader(
        title = "Weight Loss Tracking Dashboard"
    ),
    {dashboardSidebar(
        useShinyjs(),
        tags$head(
            tags$style(
                HTML(".sidebar { height: 90vh; overflow-y: auto; }")   
            ),
            tags$link(rel = "shortcut icon", href = "favicon.ico")
        ),
        fluidRow(
            style = "margin: 3px",
            actionBttn(
                "test",
                label = "View on Github",
                onclick ="window.open('https://github.com/cluffa/weight-loss-dash', '_blank')",
                style = "simple",
                color = "warning"
            ),
            radioButtons(
                "drType",
                label = "Date Range Type:",
                choices = c(
                    "Preset",
                    "Range Input",
                    "Date Range Selector"
                ),
                selected = "Preset",
                inline = TRUE
            ),
            numericInput(
                "drNum",
                label = "Date Range:",
                value = 3,
                width = "100px"
            ),
            radioButtons(
                "drUnit",
                label = NULL,
                choices = c(
                    "Days",
                    "Weeks",
                    "Months",
                    "Years"
                ),
                selected = "Months",
                inline = TRUE
            ),
            radioButtons(
                "drSimple",
                label = "Date Range:",
                choices = c(
                    `30 Days` = Sys.Date() - 30,
                    `90 Days` = Sys.Date() - 90,
                    `6 Months` = Sys.Date() - 180,
                    `1 Year` = Sys.Date() - 365,
                    `2 Years` = Sys.Date() - 730,
                    `3 Years` = Sys.Date() - 1095
                ),
                # selected = c(`90 Days` = Sys.Date() - 90),
                selected = c(`6 Months` = Sys.Date() - 180),
                inline = TRUE
            ),
            dateRangeInput(
                "drSelector",
                label = "Date Range:",
                start = as.Date("2023-01-01", "%Y-%m-%d"),
                end = Sys.Date()
            ),
            sliderInput(
                "smoothing",
                label = "Spline Smoothing:",
                min = 0,
                max = 1,
                step = 0.05,
                value = 0.5,
            ),
            awesomeCheckbox(
                "showGoal",
                label = "Show Projected Goal on Body Weight Plot",
                status = "primary"
            ),
            awesomeCheckbox(
                "showMM",
                label = "Show Min/Max Weights",
                status = "primary"
            ),
            awesomeCheckbox(
                "model30",
                value = FALSE,
                label = "Fit Linear Model on Last 30 Days Only",
                status = "primary"
            ),
            # sliderInput(
            numericInput(
                "fitdays",
                label = "Adjust the Number of Days (if box checked above):",
                min = 5,
                # max = 90,
                step = 5,
                value = 30,
            )
        ),
        collapsed = FALSE
    )}, # sidebar
    {dashboardBody(
        fluidRow(
            tabBox(
                tabPanel(
                    title = "Body Weight",
                    plotOutput(
                        "plot1",
                        height = "500px",
                    ),
                ),
                tabPanel(
                    title = "Caloric Deficit/Excess",
                    plotOutput("calPlot", height = "500px"),
                ),
                height = "580px"
            ),
            tabBox(
                id = "tabs",
                height = "580px",
                tabPanel(
                    "Stats",
                    h4("Weight Stats"),
                    verbatimTextOutput("stats"),
                    h4("Trends and Goal Projection"),
                    sliderInput(
                        "goalwt",
                        label = "Goal Weight:",
                        min = GOALWEIGHT - 25,
                        max = GOALWEIGHT + 25,
                        value = GOALWEIGHT,
                        step = 5
                    ),
                    verbatimTextOutput("summary")
                ),
                tabPanel(
                    "Model Stats",
                    verbatimTextOutput("models"),
                    tags$head(tags$style("#models{font-size: 12px;}"))
                ),
                tabPanel(
                    "Table",
                    reactableOutput("table")
                ),
                tabPanel(
                    "Info",
                    strong("BMR/TDEE Formula used"),
                    p("Mifflin-St Jeor equation", a(href = "https://pubmed.ncbi.nlm.nih.gov/2305711/", "https://pubmed.ncbi.nlm.nih.gov/2305711/")),
                    p("Men: (10 × weight in kg) + (6.25 × height in cm) - (5 × age in years) + 5"),
                    p("Women: (10 × weight in kg) + (6.25 × height in cm) - (5 × age in years) - 161"),
                    strong("Background"),
                    p("This dashboard was created so that I could easily track my weight over time.
                        I aim for a specific average pounds lost per week over the last 30 or 90 days.
                        With this dashboard I can easily view those trends and adjust accordingly."),
                    p("Because the observations are not spread evenly, I created a spline to
                        approximate the weight with values spread exactly 24 hours apart.
                        The linear regression line is fit using those points. The slope is then used
                        to predict when I will meet my goal weight.
                        ")
                )
            ),

        )
    )} # body
)


################################## SERVER ######################################

server <- function(input, output) {
    # library(Cairo)
    # options(shiny.usecairo=TRUE)
    shinyjs::hide("drSelector")
    shinyjs::hide("drNum")
    shinyjs::hide("drUnit")
    
    #library(tidyverse)
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    
    # read in data
    source("data.R")
    df <- get_weight()
    loseit <- get_loseit()
    
    df_desc <- arrange(df, desc(date))

    # create a spline
    spl <- reactive({
        smooth.spline(df$date, df$weight, spar = input$smoothing)
    }) # |> bindCache(input$smoothing)
    
    pred <- reactive({
        rng_start <- floor_date(df$date[1], "days")
        rng_stop <- ceiling_date(tail(df$date, 1), "days")
        rng <- seq(rng_start, rng_stop, by = "24 hours")
        predict(spl(), as.numeric(rng))
    }) # |> bindCache(spl())
    
    # wait for changes in the date range type, then show/hide the appropriate inputs
    observeEvent(input$drType, {
        type = input$drType
        if(type == "Date Range Selector") {
            shinyjs::hide("drSimple")
            shinyjs::show("drSelector")
            shinyjs::hide("drNum")
            shinyjs::hide("drUnit")
        } else if(type == "Range Input") {
            shinyjs::hide("drSimple")
            shinyjs::hide("drSelector")
            shinyjs::show("drNum")
            shinyjs::show("drUnit")
        } else {
            shinyjs::show("drSimple")
            shinyjs::hide("drSelector")
            shinyjs::hide("drNum")
            shinyjs::hide("drUnit")
        }
    });

    # get the data in the range
    get_date_range <- reactive({
        type <- input$drType
        
        if(type == "Date Range Selector") {
            return(as.POSIXct(input$drSelector))

        } else if(type == "Range Input") {
            return(
                as.POSIXct(c(
                    Sys.Date() - period(input$drNum, tolower(input$drUnit)),
                    Sys.Date()
                ))
            )

        } else {
            return(
                c(
                    as.POSIXct(input$drSimple),
                    as.POSIXct(Sys.Date())
                )
            )
          
        }
    }) # |> bindCache(input$drType, input$drSelector, input$drNum, input$drUnit, input$drSimple)
    
    get_df <- reactive({
        range <- get_date_range()
        df <- df_desc
        df <- df[df$date >= range[1] & df$date <= range[2] + 86400,]
        df
    }) # |> bindCache(get_date_range())
    
    get_loseit_in_range <- reactive({
        range <- get_date_range()
        loseit[loseit$date >= range[1] & loseit$date <= range[2] + 86400,]
    }) # |> bindCache(get_date_range())
    
    get_model <- reactive({
        shorten <- shorten()
        pred <- get_spline_pred_in_range() |> shorten()
        lm(weight ~ date, pred)
    }) # |> bindCache(shorten(), get_spline_pred_in_range())
    
    get_cals <- reactive({
        pred <- pred()
        c(diff(pred$y), NA) * 3500
    }) # |> bindCache(pred())
    
    get_spline_pred_in_range <- reactive({
        pred <- pred()
        range <- get_date_range()
        cals <- get_cals()
    
        in_rng <- pred$x >= range[1] & pred$x <= range[2] + 86400
        
        data.frame(
            date = as.POSIXct(pred$x[in_rng], origin = "1970-1-1"),
            weight = pred$y[in_rng],
            cals = cals[in_rng]
            )
    }) # |> bindCache(pred(), get_date_range(), get_cals())
    
    get_gw <- reactive({
        input$goalwt
    })
    
    output$calPlot <- renderPlot({
        df <- get_spline_pred_in_range()
        loseit <- get_loseit_in_range()
        ylimits <- c(
            min(
                min(loseit$weekdiff, na.rm = TRUE),
                min(df$cals, na.rm = TRUE)
            ),
            max(
                max(loseit$weekdiff, na.rm = TRUE),
                max(df$cals, na.rm = TRUE)
            )
        )

        
        p <- ggplot(df) +
            geom_point(
                aes(date, diff),
                data = loseit,
                color = "lightgray",
                alpha = 0.5
                ) +
            geom_line(
                aes(date, weekdiff, linetype = tdee_method),
                data = loseit,
                color = "darkgray"
                ) +
            geom_hline(
                yintercept = 0,
                color = "green"
                ) +
            geom_line(
                aes(date, cals),
                color = "blue"
                ) +
            theme_bw() +
            ylab("calories per day") +
            scale_y_continuous(
                sec.axis = sec_axis(~ . / 500, name = "pounds per week", breaks = seq(-5, 5, by = 1)),
                breaks = seq(-5000, 5000, by = 500)
            ) + 
            theme(legend.position = "bottom") +
            coord_cartesian(
                ylim = ylimits
            )

        
        return(p)
    }) # |> bindCache(get_loseit_in_range(), get_spline_pred_in_range(), sizePolicy = sizeGrowthRatio(width = 400, height = 400, growthRate = 1.1))
    
    get_mm <- reactive({
        df <- get_df()
        mm <- df[c(which.max(df$weight), which.min(df$weight)),]
        mm$label <- c("max", "min")
        
        return(mm)
    }) # |> bindCache(get_df())
    
    output$plot1 <- renderPlot({
        shorten <- shorten()
        range <- get_date_range()
        df <- get_df()
        gw_df <- get_goal_date()
        spl <- get_spline_pred_in_range()
        
        mm <- get_mm()
        
        leg <- c(
            "Observed Weight" = "darkgray",
            "Spline Fit" = "blue",
            "Linear Model" = "red",
            "Projected Goal Date" = "green"
        )
        
        p <- ggplot() +
            geom_point(
                aes(y = weight, x = date, color = "Observed Weight"),
                data = df,
                ) +
            geom_line(
                aes(date, weight, color = "Spline Fit"),
                data = spl,
                linewidth = 1
                ) +
            scale_color_manual(
                name = NULL,
                breaks = names(leg),
                values = leg
                ) +
            scale_y_continuous(
                breaks = seq(0, 500, by = 5)
                ) +
            theme_bw() +
            theme(
                legend.position = "bottom",
                legend.background = element_rect(fill = "transparent")
                )
        
        if (input$showGoal) {
            p <- p + geom_point(
                aes(y = weight, x = date, color = "Projected Goal Date"),
                data = gw_df,
                size = 3
                ) + 
            geom_smooth(
                aes(x = date, y = weight, color = "Linear Model"),
                data = bind_rows(spl, gw_df) |> shorten(extra = 1),
                method = "lm",
                linetype = 2,
                linewidth = 1,
                formula = y ~ x,
                se = FALSE
                ) +
            geom_text(
                aes(
                    x = date,
                    y = weight,
                    label = format(date, "%Y-%m-%d"),
                    fontface = "bold"
                    ),
                data = gw_df,
                vjust = "outward",
                hjust = "inward",
                nudge_y = -1
                )
        } else {
            p <- p +
                geom_smooth(
                    aes(x = date, y = weight, color = "Linear Model"),
                    data = spl |> shorten(),
                    method = "lm",
                    linetype = 2,
                    linewidth = 1,
                    formula = y ~ x,
                    se = FALSE
                )
        }
        
        if (input$showMM) {
            p <- p +
                geom_text(
                    aes(
                        x = date,
                        y = weight,
                        label = paste(" ", weight, label, " "), 
                        fontface = "bold"
                    ),
                    data = mm,
                    hjust = "inward",
                    vjust = "outward",
                ) +
                geom_point(
                        aes(
                            x = date,
                            y = weight,
                        ),
                        data = mm,
                        size = 2
                ) 
        }
        
        return(p)
    }) # |> bindCache(shorten(), get_date_range(), get_df(), get_goal_date(), get_spline_pred_in_range(), get_mm(), shorten(), input$showGoal, input$showMM, sizePolicy = sizeGrowthRatio(width = 400, height = 400, growthRate = 1.1))
    
    shorten <- reactive({
        function(df, extra = 0) {
            if (input$model30) {
                return(tail(df, n = input$fitdays + extra))
            } else {
                return(df)
            }
        }
    }) # |> bindCache(input$model30, input$fitdays)
    
    get_goal_date <- reactive({
        gw <- get_gw()
        model <- get_model()
        gwdate <- (gw - model$coefficients[1])/model$coefficients[2]
        
        data.frame(
            weight = gw,
            date = as.POSIXct(gwdate, origin = "1970-1-1")
        )
    }) # |> bindCache(get_gw(), get_model())
    
    get_cw <- reactive({
        round(tail(get_spline_pred_in_range()$weight, n = 1), 1)
    }) # |> bindCache(get_spline_pred_in_range())
    
    output$summary <- renderPrint({
        model <- get_model()
        coef <- unname(model$coefficients[2])*86400
        gw <- get_gw()
        
        if (input$model30) {
            loseit <- get_loseit_in_range()
            loseit <- loseit[loseit$date > (Sys.Date() - input$fitdays),]
        } else {
            loseit <- get_loseit_in_range()
        }
            
        cals <- coef * 3500
        
        gwdate <- (gw - model$coefficients[1])/model$coefficients[2]
        gwdate <- as.Date(as.POSIXct.numeric(gwdate, origin = "1970-1-1"))
        weeks <- round((gwdate - Sys.Date()) / 7, 1)
        
        food_mean <- mean(loseit$food)
        tdee_mean <- mean(loseit$tdee)
        est_act <- (food_mean - cals)/(tdee_mean/input$mult)
        
        trend <- ifelse(coef < 0, "Losing", "Gaining")
        coef <- abs(coef)
        
        cat(
            ifelse(
                input$model30,
                paste("Linear Model Fit on Last", input$fitdays, "days"),
                "Linear Model Fit on Selected Date Range"
            ),
            "\nDaily Trend:", trend, coef |> round(2), "lbs/day",
            "\nWeekly Trend:", trend, (coef*7) |> round(2), "lbs/week",
            "\nGoal Projection:", as.character(gwdate),
            paste0("(", as.character(weeks), " Weeks)"),
            "\nAvg Daily Diff Based on Trend:", round(cals, 0),
            "\nAvg Daily Intake:", food_mean |> round(),
            "\nAvg Est. TDEE:", tdee_mean |> round(),
            "\nAvg Daily Diff Based on Intake:", mean(loseit$diff) |> round()
            )
    }) # |> bindCache(get_model(), get_gw(), input$model30, input$fitdays, input$mult)
    
    output$stats <- renderPrint({
        df <- get_df() |> 
            mutate(
                date = format(date, "%Y-%m-%d")
            )
        
        df$date <- as.character(df$date)
    
        suppressWarnings(
            cat(
                "Current Weight:", get_cw(), "lbs",
                "\nRange Low:", min(df$weight), "on", df$date[which.min(df$weight)],
                "\nRange High:", max(df$weight), "on", df$date[which.max(df$weight)],
                "\nRange Mean:", round(mean(df$weight, na.rm = TRUE),1)
            )
        )
    }) # |> bindCache(get_df(), get_date_range(), get_cw())

    output$models <- renderPrint({
        model <- get_model()
        spl <- spl()
        print(spl)
        summary(model)
    }) # |> bindCache(get_model(), spl())
    
    output$table <- renderReactable({
        data_frame <- get_spline_pred_in_range()
        
        data_frame[nrow(data_frame):1,] |>
            mutate(
                date = as_date(date),
                weight = round(weight, 1),
                cals = round(cals)
            ) |>
            reactable(
                striped = TRUE
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

