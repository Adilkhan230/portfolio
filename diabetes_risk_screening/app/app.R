# ── Пакеты ─────────────────────────────────────────────────────────────────────
library(shiny)
library(shinyjs)
library(xgboost)
library(dplyr)
library(bslib)
library(grid)          # для PDF-отчёта
library(randomForest)  # важно для predict.randomForest
library(e1071)         # если SVM обучен через e1071
library(caret)         # если модели сохранены как caret::train

# ── Модели ─────────────────────────────────────────────────────────────────────
model <- readRDS("xgb_selection_model2.rds")
lgb_model <- readRDS("light_model.rds")
svm_model <- readRDS("svm_model.rds")
short_xgb_model <- readRDS("short_xgb_model.model")
rf_model <- readRDS("rf_model.rds")

# Полный набор фич (как при обучении)
model_cols <- c(
  "RIDAGEYR","DMDEDUC2","INDHHIN2","DMDHHSIZ","BMXHT","BMXBMI","BPXSY1","BPXDI1",
  "HUQ010","HUQ030","DBQ700","SLQ060","DIQ175U.x_Thirst","DIQ175V.x_No","CBQ505_Yes",
  "RIAGENDR_Male","RIDRETH1_Non.Hispanic.Black","RIDRETH1_Non.Hispanic.White",
  "RIDRETH1_Other.Hispanic","RIDRETH1_Other.Race...Including.Multi.Racial",
  "DMDMARTL_Living.with.partner","DMDMARTL_Married","DMDMARTL_Never.married",
  "DMDMARTL_Separated","DMDMARTL_Widowed","MCQ010_Yes","MCQ080_Yes","MCQ160A_Yes",
  "MCQ160B_Yes","MCQ160C_Yes","MCQ160E_Yes","MCQ160F_Yes","MCQ300C_Yes","SMQ020_Yes",
  "ALQ101_Yes","PAQ605_Yes","PAQ620_Yes","PAQ635_Yes","SLQ050_Yes",
  "DPQ020_Nearly.every.day","DPQ020_Not.at.all","DPQ020_Several.days",
  "DPQ040_Nearly.every.day","DPQ040_Not.at.all","DPQ040_Several.days","PFQ020_Yes"
)

# Укороченные фичи для short XGB
short_model_cols <- c(
  "RIDAGEYR","HUQ010","MCQ300C_Yes","BMXBMI","MCQ080_Yes",
  "HUQ030","BPXSY1","SLQ050_Yes","BPXDI1","MCQ160A_Yes"
)

# ── Якорные точки распределения (из базы) ──────────────────────────────────────
anchors_prob  <- c(0.02881, 0.02934, 0.03396, 0.06674, 0.4021982, 0.6911009, 0.91068)
anchors_pct   <- c(0,       25,      50,      75,      90,       95,       100)

prob_to_percentile <- function(p) {
  p <- pmin(pmax(p, min(anchors_prob)), max(anchors_prob))
  as.numeric(approx(x = anchors_prob, y = anchors_pct, xout = p, ties = "ordered")$y)
}

# ── Цвет по перцентилю (новые зоны) ────────────────────────────────────────────
get_color_by_perc <- function(perc) {
  if (perc < 75) {
    "#10B981" # зелёный
  } else if (perc < 90) {
    "#FBBF24" # жёлтый
  } else {
    "#EF4444" # красный
  }
}

# ── Хелперы вероятностей (универсальные) ───────────────────────────────────────
pick_positive_col <- function(prob_df) {
  candidates <- c("Yes","1","TRUE","Positive","positive","Diabetes","Case")
  cols <- colnames(prob_df)
  hit <- which(cols %in% candidates)
  if (length(hit) >= 1) return(hit[1])
  if (ncol(prob_df) >= 2) return(2) # часто порядок "No","Yes"
  return(1)
}

prob_from_rf <- function(model, newdata) {
  if (inherits(model, "train")) {
    p <- predict(model, newdata = newdata, type = "prob")
    p[, pick_positive_col(p), drop = TRUE]
  } else if (inherits(model, "randomForest")) {
    p <- predict(model, newdata = newdata, type = "prob")
    if (is.matrix(p) || is.data.frame(p)) {
      p[, pick_positive_col(p), drop = TRUE]
    } else as.numeric(p)
  } else stop("RF model class unsupported: ", paste(class(model), collapse = ", "))
}

prob_from_svm <- function(model, newdata) {
  if (inherits(model, "train")) {
    p <- predict(model, newdata = newdata, type = "prob")
    p[, pick_positive_col(p), drop = TRUE]
  } else if (inherits(model, "svm")) {
    preds <- predict(model, newdata = newdata, probability = TRUE)
    probs <- attr(preds, "probabilities")
    if (is.null(probs))
      stop("SVM обучен без probability=TRUE. Переобучите SVM с probability=TRUE.")
    probs[, pick_positive_col(probs), drop = TRUE]
  } else stop("SVM model class unsupported: ", paste(class(model), collapse = ", "))
}

# ── Тема и стили ───────────────────────────────────────────────────────────────
app_theme <- bs_theme(
  bootswatch = "minty",
  base_font = font_google("Inter"),
  heading_font = font_google("Playfair Display"),
  code_font = font_google("JetBrains Mono")
)

custom_css <- HTML("
  .app-title { font-weight:700; letter-spacing:.3px; }
  .help-subtle { color:#6b7280; font-size:0.9rem; margin-top:-6px; }
  .card-ghost { border:0; box-shadow:0 10px 25px rgba(0,0,0,.06); border-radius:20px; }
  .pill { display:inline-block; padding:.25rem .6rem; border-radius:999px; background:#eaf6f1; color:#059669; font-size:.85rem; }
  .progress-wrap { margin-top:.75rem; background:#eef2f7; border-radius:999px; height:16px; overflow:hidden; position:relative; }
  .progress-bar-soft { height:100%; width:0%; border-radius:999px; background:#10B981; transition: width .8s ease, background .3s ease; }
  .big-percent { font-size:2.6rem; font-weight:800; line-height:1.1; }
  .small-note { color:#6b7280; font-size:.95rem; }
  .model-table td, .model-table th { padding:.5rem .75rem; }
  .footer-note { color:#6b7280; font-size:.85rem; }
  .btn-primary { border-radius:12px; font-weight:600; }
  .btn-ghost { border-radius:12px; }
  .ruler { position:relative; height:44px; margin-top:10px; }
  .ruler .axis { position:absolute; top:24px; left:0; right:0; height:2px; background:#e5e7eb; border-radius:999px; }
  .tick { position:absolute; top:14px; width:2px; height:22px; background:#9ca3af; }
  .tick-label { position:absolute; top:0px; transform:translateX(-50%); font-size:.8rem; color:#6b7280; white-space:nowrap; }
  .marker { position:absolute; top:0px; transform:translateX(-50%); text-align:center; }
  .marker .dot { width:12px; height:12px; border-radius:50%; background:#10b981; box-shadow:0 0 0 4px rgba(16,185,129,.15); margin:0 auto; }
  .marker .lbl { font-size:.8rem; color:#10b981; margin-top:4px; text-align:center; white-space:nowrap; }
")

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = div(class="app-title","Оценка риска СД2 (ансамбль моделей)"),
  theme = app_theme,
  sidebar = sidebar(
    useShinyjs(),
    tags$style(custom_css),
    
    # Блок ввода
    tagList(
      numericInput("RIDAGEYR", "Возраст (лет):", value = NA, min = 10, max = 100, step = 1),
      numericInput("BMXBMI", "Индекс массы тела (ИМТ, кг/м²):", value = NA, min = 10, max = 70, step = 0.1),
      numericInput("BMXHT",  "Рост (см):", value = NA, min = 100, max = 230, step = 0.1),
      div(class = "help-subtle", "*Указывать вес необязательно — расчёт возможен по ИМТ"),
      
      tags$hr(),
      h5("Артериальное давление"),
      numericInput("BPXSY1", "Систолическое (верхнее) давление, мм рт. ст.:", value = NA, min = 70, max = 260, step = 1),
      numericInput("BPXDI1", "Диастолическое (нижнее) давление, мм рт. ст.:", value = NA, min = 40, max = 150, step = 1),
      
      tags$hr(),
      h5("Сведения о здоровье"),
      selectInput("RIAGENDR", "Пол:", choices = c("Мужской" = 1, "Женский" = 2)),
      selectInput("HUQ010","Как Вы оцениваете своё общее состояние здоровья?",
                  choices = c("Отлично"=5,"Очень хорошо"=4,"Хорошо"=3,"Удовлетворительно"=2,"Плохо"=1)),
      selectInput("HUQ030","Есть ли у Вас постоянное место (врач или поликлиника), куда Вы обращаетесь при необходимости?",
                  choices = c("Да"=1,"Нет"=0)),
      selectInput("MCQ010","Ставил ли Вам врач диагноз «астма»?", choices=c("Да"=1,"Нет"=0)),
      selectInput("MCQ080","Сообщал ли Вам врач о наличии лишнего веса?", choices=c("Да"=1,"Нет"=0)),
      selectInput("MCQ160A","Ставил ли Вам врач диагноз «артрит»?", choices=c("Да"=1,"Нет"=0)),
      selectInput("MCQ160B","Ставил ли Вам врач диагноз «сердечная недостаточность»?", choices=c("Да"=1,"Нет"=0)),
      selectInput("MCQ160C","Ставил ли Вам врач диагноз «ишемическая болезнь сердца»?", choices=c("Да"=1,"Нет"=0)),
      selectInput("MCQ300C","Был ли у Ваших близких родственников сахарный диабет?", choices=c("Да"=1,"Нет"=0)),
      selectInput("SLQ050_Yes","Замечали ли Вы у себя проблемы со сном?", choices=c("Да"=1,"Нет"=0))
    ),
    
    div(class="mt-3",
        actionButton("submit", "Рассчитать риск (перцентиль)", class = "btn btn-primary"),
        actionButton("reset", "Сбросить", class = "btn btn-outline-secondary ms-2 btn-ghost")
    ),
    div(class="mt-3 help-subtle",
        HTML("⚠ Это не диагноз. Модель обучена на популяционных данных и служит для <b>первичного скрининга</b>.")
    ),
    
    tags$hr(),
    downloadButton("dl_pdf", "Скачать паспорт риска (PDF)", class = "btn btn-success")
  ),
  
  layout_columns(
    col_widths = c(6,6),
    
    card(
      class = "card-ghost",
      card_header(div("Ваш ориентировочный риск", span(class="pill","по перцентилю"))),
      card_body(
        uiOutput("ensemble_text"),
        div(class="small-note", uiOutput("prob_note")),
        div(class="progress-wrap", div(id="progbar", class="progress-bar-soft")),
        # Линейка-легенда (обновлённая: только 75 и 90)
        div(
          class="ruler",
          div(class="axis"),
          div(class="tick", style="left:75%;"),
          div(class="tick-label", style="left:75%;", "75-й"),
          div(class="tick", style="left:90%;"),
          div(class="tick-label", style="left:90%;", "90-й"),
          div(id="marker", class="marker", style="left:0%;",
              div(class="dot"),
              div(id="marker_lbl", class="lbl",""))
        ),
        div(class="mt-3", uiOutput("risk_tip"))
      )
    ),
    
    card(
      class = "card-ghost",
      card_header("Детализация по моделям"),
      card_body(
        tableOutput("model_table"),
        div(class="footer-note mt-3",
            "Значения в таблице — вероятности; итоговый риск на карточке — перцентиль по базе результатов."
        )
      )
    )
  )
)

# ── SERVER ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  # Храним последний расчёт для PDF
  last <- reactiveValues(
    perc = NA_real_, prob = NA_real_,
    by_model = NULL, inputs = NULL, ts = NULL
  )
  
  # Сброс
  observeEvent(input$reset, {
    updateNumericInput(session, "RIDAGEYR", value = NA)
    updateNumericInput(session, "BMXBMI", value = NA)
    updateNumericInput(session, "BMXHT",  value = NA)
    updateNumericInput(session, "BPXSY1", value = NA)
    updateNumericInput(session, "BPXDI1", value = NA)
    updateSelectInput(session, "RIAGENDR", selected = 2)
    updateSelectInput(session, "HUQ010", selected = 3)
    updateSelectInput(session, "HUQ030", selected = 1)
    updateSelectInput(session, "MCQ010", selected = 0)
    updateSelectInput(session, "MCQ080", selected = 0)
    updateSelectInput(session, "MCQ160A", selected = 0)
    updateSelectInput(session, "MCQ160B", selected = 0)
    updateSelectInput(session, "MCQ160C", selected = 0)
    updateSelectInput(session, "MCQ300C", selected = 0)
    updateSelectInput(session, "SLQ050_Yes", selected = 0)
    runjs('document.getElementById("progbar").style.width = "0%";')
    runjs('document.getElementById("progbar").style.background = "#10B981";')
    runjs('document.getElementById("marker").style.left = "0%";')
    runjs('document.getElementById("marker_lbl").innerText = "";')
    output$ensemble_text <- renderUI({ HTML("<div class='big-percent'>—</div>") })
    output$prob_note <- renderUI({ HTML("<div class='help-subtle'>Введите данные и нажмите «Рассчитать риск (перцентиль)»</div>") })
    output$model_table <- renderTable(NULL)
    output$risk_tip <- renderUI(NULL)
  })
  
  observeEvent(input$submit, {
    req(input$RIDAGEYR, input$BMXBMI, input$BPXSY1, input$BPXDI1, input$BMXHT)
    
    # 1 = Мужской → RIAGENDR_Male=1; 2 = Женский → 0
    ri_male <- ifelse(as.numeric(input$RIAGENDR) == 1, 1, 0)
    
    new_data <- list(
      RIDAGEYR = as.numeric(input$RIDAGEYR),
      BMXBMI = as.numeric(input$BMXBMI),
      BPXSY1 = as.numeric(input$BPXSY1),
      BPXDI1 = as.numeric(input$BPXDI1),
      BMXHT  = as.numeric(input$BMXHT),
      RIAGENDR_Male = ri_male,
      HUQ010 = as.numeric(input$HUQ010),
      HUQ030 = as.numeric(input$HUQ030),
      MCQ010_Yes = ifelse(input$MCQ010 == 1, 1, 0),
      MCQ080_Yes = ifelse(input$MCQ080 == 1, 1, 0),
      MCQ160A_Yes = ifelse(input$MCQ160A == 1, 1, 0),
      MCQ160B_Yes = ifelse(input$MCQ160B == 1, 1, 0),
      MCQ160C_Yes = ifelse(input$MCQ160C == 1, 1, 0),
      MCQ300C_Yes = ifelse(input$MCQ300C == 1, 1, 0),
      SLQ050_Yes = as.numeric(input$SLQ050_Yes)
    )
    new_data_df <- as.data.frame(new_data)
    
    # добиваем отсутствующие фичи нулями и упорядочиваем
    missing_cols <- setdiff(model_cols, names(new_data_df))
    for (col in missing_cols) new_data_df[[col]] <- 0
    new_data_df <- new_data_df[, model_cols, drop = FALSE]
    
    # модели
    new_matrix <- as.matrix(new_data_df)
    dnew <- xgb.DMatrix(data = new_matrix, missing = NA)
    pred_xgb <- as.numeric(predict(model, dnew))
    pred_lgb <- as.numeric(predict(lgb_model, new_matrix))
    pred_svm_prob <- as.numeric(prob_from_svm(svm_model, new_data_df))
    new_short_df <- new_data_df[, short_model_cols, drop = FALSE]
    dnew_short <- xgb.DMatrix(data = as.matrix(new_short_df), missing = NA)
    pred_short_xgb <- as.numeric(predict(short_xgb_model, dnew_short))
    pred_prob_rf <- as.numeric(prob_from_rf(rf_model, new_data_df))
    
    pred_ensemble <- mean(c(pred_xgb, pred_lgb, pred_svm_prob, pred_short_xgb, pred_prob_rf))
    
    # перцентиль и цвет
    perc <- prob_to_percentile(pred_ensemble)
    perc_round <- round(perc, 0)
    prob_pct <- round(pred_ensemble * 100, 1)
    bar_color <- get_color_by_perc(perc)
    
    # вывод
    output$ensemble_text <- renderUI({
      HTML(sprintf("<div class='big-percent'>%s-й перцентиль <span class='small-note'>(вероятность: %s%%)</span></div>",
                   perc_round, prob_pct))
    })
    output$prob_note <- renderUI({
      HTML("Калибровка по базе: min=0.0288; Q1=0.0293; median=0.0340; Q3=0.0667; 90%=0.4022; 95%=0.6911; max=0.9107")
    })
    
    # прогресс-бар + маркер
    runjs(sprintf('
      var bar = document.getElementById("progbar");
      bar.style.width = "%s%%";
      bar.style.background = "%s";
    ', perc, bar_color))
    
    runjs(sprintf('
      var marker = document.getElementById("marker");
      marker.style.left = "%s%%";
      var dot = marker.querySelector(".dot");
      dot.style.background = "%s";
      dot.style.boxShadow = "0 0 0 4px " + "%s" + "33";
      var lbl = document.getElementById("marker_lbl");
      lbl.style.color = "%s";
      lbl.innerText = "%s-й";
    ', perc, bar_color, bar_color, bar_color, perc_round))
    
    # Рекомендации по новым зонам
    tip <- if (perc < 75) {
      paste(
        "Зелёная зона (<75-й). Хороший ориентир — удерживать баланс:",
        "• Сон 7–9 ч; шаги 8–10k/день; 2–3 силовые тренировки/нед;",
        "• Белок 1.2–1.6 г/кг, клетчатка ≥25–30 г/день, насыщенные жиры ↓;",
        "• Контроль АД, стресса и гигиены сна. Профилактика — твой суперсиловой режим.", sep="<br/>"
      )
    } else if (perc < 90) {
      paste(
        "Жёлтая зона (75–89-й). Усиль профилактику:",
        "• Пересмотри рацион (дефицит 10–15% при избытке веса, сахар/рафинированные ↓);",
        "• Тренинг: силовые 3 р/нед + кардио 150–300 мин/нед;",
        "• Обсуди с врачом базовый скрининг: глюкоза натощак, HbA1c, липидный профиль, мониторинг АД.", sep="<br/>"
      )
    } else {
      paste(
        "Красная зона (≥90-й). Нужны точные шаги:",
        "• Прицельный чек-ап: HbA1c, ФГ, ОГТТ при необходимости, липиды, АД (домашний мониторинг);",
        "• Ранняя консультация специалиста и персональный план коррекции веса, питания и активности;",
        "• Краткосрочный план с ежемесячной переоценкой метрик.", sep="<br/>"
      )
    }
    output$risk_tip <- renderUI(HTML(sprintf("<div>%s</div>", tip)))
    
    # таблица
    tbl <- data.frame(
      Модель = c("XGBoost (полная)","XGBoost (short)","Random Forest","LightGBM","SVM"),
      Вероятность = sprintf("%.2f%%", c(pred_xgb, pred_short_xgb, pred_prob_rf, pred_lgb, pred_svm_prob) * 100),
      stringsAsFactors = FALSE
    )
    output$model_table <- renderTable(tbl, striped = TRUE, hover = TRUE, bordered = FALSE,
                                      width = "100%", spacing = "s", align = "lc")
    
    # сохранить для PDF
    last$perc <- perc_round
    last$prob <- prob_pct
    last$by_model <- tbl
    last$inputs <- as.list(input)
    last$ts <- format(Sys.time(), "%Y-%m-%d %H:%M")
  })
  
  # ── PDF «Паспорт риска» (с теми же зонами) ────────────────────────────────────
  output$dl_pdf <- downloadHandler(
    filename = function() {
      ts <- if (is.null(last$ts)) "now" else last$ts
      sprintf("Risk_Passport_%s.pdf", gsub("[: ]","_", ts))
    },
    content = function(file) {
      perc <- last$perc; prob <- last$prob
      if (is.na(perc) || is.na(prob)) { perc <- 0; prob <- 0 }
      color <- get_color_by_perc(perc)
      
      grDevices::pdf(file, width = 8.27, height = 11.69) # A4
      grid::grid.newpage()
      
      # Заголовок
      grid::grid.text("Паспорт риска СД2", x=.5, y=.95, gp=grid::gpar(fontsize=24, fontface="bold"))
      grid::grid.text(sprintf("Перцентиль: %s-й   (вероятность: %.1f%%)", perc, prob),
                      x=.5, y=.91, gp=grid::gpar(fontsize=14))
      
      # Линейка (только 75 и 90)
      x0 <- .1; x1 <- .9; y <- .85
      grid::grid.lines(x=c(x0,x1), y=c(y,y), gp=grid::gpar(col="#E5E7EB", lwd=6, lineend="round"))
      ticks <- c(75,90)
      for (t in ticks) {
        xt <- x0 + (x1-x0)*(t/100)
        grid::grid.lines(x=c(xt,xt), y=c(y-0.02,y+0.02), gp=grid::gpar(col="#9CA3AF", lwd=3))
        grid::grid.text(sprintf("%d-й", t), x=xt, y=y+0.04, gp=grid::gpar(col="#6B7280", cex=.8))
      }
      # Маркер пользователя
      xu <- x0 + (x1-x0)*(perc/100)
      grid::grid.circle(x=xu, y=y, r=0.012, gp=grid::gpar(fill=color, col=NA))
      grid::grid.text(sprintf("%d-й", perc), x=xu, y=y-0.04, gp=grid::gpar(col=color, cex=.85, fontface="bold"))
      
      # Рекомендация по зонам
      tip <- if (perc < 75) {
        "Зелёная зона (<75-й): поддерживай профилактику — сон 7–9 ч, шаги 8–10k/день, силовые 2–3 р/нед, белок 1.2–1.6 г/кг, клетчатка ≥25–30 г/д, контроль АД и стресса."
      } else if (perc < 90) {
        "Жёлтая зона (75–89-й): усили профилактику; обсуди с врачом скрининг (глюкоза натощак, HbA1c, липидный профиль), увеличь долю силовых и аэробной нагрузки."
      } else {
        "Красная зона (≥90-й): прицельный клинико-лабораторный чек-ап в ближайшее время и персональный план коррекции."
      }
      grid::grid.text(paste0("Рекомeндация: ", tip), x=.5, y=.78, gp=grid::gpar(cex=0.95))
      
      # Таблица моделей
      if (!is.null(last$by_model)) {
        tbl <- last$by_model
        y0 <- .7; row_h <- .04
        grid::grid.text("Детализация по моделям", x=.5, y=y0+row_h, gp=grid::gpar(fontface="bold"))
        headers <- c("Модель","Вероятность")
        grid::grid.text(headers[1], x=.25, y=y0, gp=grid::gpar(fontface="bold"))
        grid::grid.text(headers[2], x=.75, y=y0, gp=grid::gpar(fontface="bold"))
        for (i in seq_len(nrow(tbl))) {
          grid::grid.text(tbl$Модель[i], x=.25, y=y0 - i*row_h)
          grid::grid.text(tbl$Вероятность[i], x=.75, y=y0 - i*row_h)
        }
      }
      
      # Ключевые введённые данные
      grid::grid.text("Ключевые введённые данные", x=.5, y=.45, gp=grid::gpar(fontface="bold"))
      lines <- c(
        sprintf("Возраст: %s", input$RIDAGEYR),
        sprintf("ИМТ: %s", input$BMXBMI),
        sprintf("АД: %s/%s мм рт. ст.", input$BPXSY1, input$BPXDI1),
        sprintf("Самооценка здоровья: %s", input$HUQ010),
        sprintf("Родственники с диабетом: %s", c("Нет","Да")[as.integer(input$MCQ300C)+1])
      )
      for (i in seq_along(lines)) {
        grid::grid.text(lines[i], x=.5, y=.45 - i*0.03, gp=grid::gpar(col="#374151"))
      }
      
      # Сноска
      grid::grid.text("Примечание: это скрининговая оценка по ансамблю ML-моделей; не является диагнозом.",
                      x=.5, y=.08, gp=grid::gpar(col="#6B7280", cex=.85))
      ts <- if (is.null(last$ts)) format(Sys.time(), "%Y-%m-%d %H:%M") else last$ts
      grid::grid.text(sprintf("Сгенерировано: %s", ts),
                      x=.5, y=.05, gp=grid::gpar(col="#9CA3AF", cex=.8))
      
      grDevices::dev.off()
    }
  )
}

shinyApp(ui,server)