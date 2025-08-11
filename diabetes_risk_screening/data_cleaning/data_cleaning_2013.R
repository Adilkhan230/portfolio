library(nhanesA)
library(tidyverse)
demo_h <- nhanes("DEMO_H")

blood_pressure <- nhanes("BPX_H")

body_measures <- nhanes("BMX_H")
diabetes_questionnaire <- nhanes("DIQ_H")


all_tables <- nhanesSearch("") 
diet_tables <- subset(all_tables, grepl("^DR", all_tables$Data.File.Name))
diet_data <- nhanes("DR1TOT_H")   # Total Nutrients (1st day)

exam_tables <- subset(all_tables, grepl("^BMX|^BPX|^OHX", all_tables$Data.File.Name))
oral_exam <- nhanes("OHXPER_H")

questionnaire_tables <- c(
  "DLQ_H", "DEQ_H", "OSQ_H", "IMQ_H", "SXQ_H", "CDQ_H", "BPQ_H", "MCQ_H", "HIQ_H",
  "HUQ_H", "PAQ_H", "PFQ_H", "HEQ_H", "ECQ_H", "DIQ_H", "SMQFAM_H", "SMQ_H", 
  "SMQRTU_H", "HOQ_H", "PUQMEC_H", "SMQSHS_H", "INQ_H", "CSQ_H", "DBQ_H", "CBQ_H", 
  "HSQ_H", "SLQ_H", "RXQASA_H", "DUQ_H", "WHQMEC_H", "ALQ_H", "DPQ_H", "ACQ_H", 
  "WHQ_H", "RHQ_H", "FSQ_H", "OHQ_H", "OCQ_H", "RXQ_RX_H", "KIQ_U_H", "CKQ_H", 
  "VTQ_H", "CFQ_H"
)
questionnaire_data <- questionnaire_tables %>%
  map(~ nhanes(.x)) %>%
  reduce(full_join, by = "SEQN")

# Check duplicates based on all columns except one
#for (col in colnames(questionnaire_data)) {
 # temp <- questionnaire_data[ , !(names(questionnaire_data) %in% col)]
  #if (any(duplicated(temp))) {
   # cat("Duplicates remain without", col, "\n")
  #} else {
    #cat("Duplicates resolved when", col, "is removed — it may be the issue\n")
  #}
#}
questionnaire_data2 <- questionnaire_data %>%
  select(-c(RXDDRGID, RXDDAYS, RXDDRGID,RXDDRUG, RXDRSD2 ,RXDRSD3))


questionnaire_data3 <- questionnaire_data2 %>% filter(RXDRSC1 == "E11" | RXDRSC1 == "")
questionnaire_data4 <- distinct(questionnaire_data3)

data1 <- inner_join(questionnaire_data4, demo_h, by = "SEQN")
data2 <- inner_join(data1, blood_pressure, by = "SEQN")
data3 <- inner_join(data2, body_measures, by = "SEQN")
data4 <- inner_join(data3, diabetes_questionnaire, by = "SEQN")
data5 <- inner_join(data4, diet_data, by = "SEQN")
data6 <- left_join(data5, oral_exam, by = "SEQN")



data7 <- data6 %>% mutate(target = ifelse(RXDRSC1 == "E11",1,0))
id_to_sub <- c(73557 ,74861, 74958, 75654, 75938, 76164, 76655, 77142, 77874, 78134, 78394, 80483, 80804)
data7 <- data7 %>%
  mutate(target = ifelse(SEQN %in% id_to_sub, 1, target))
data8 <- data7 %>%
  distinct(SEQN, target, .keep_all = TRUE)
library(dplyr)

data8 <- data8 %>% mutate(year = 2013)
selected_data <- data8 %>%
  select(
    year,
    DIQ175U.x,
    DIQ175V.x,
    CBQ505,
    # 1. ДЕМОГРАФИЧЕСКИЕ ДАННЫЕ
    # ======================
    RIAGENDR,     # Пол
    RIDAGEYR,     # Возраст
    RIDRETH1,     # Раса/этническая принадлежность
    DMDMARTL,     # Семейное положение
    DMDEDUC2,     # Образование
    INDHHIN2,     # Доход
    DMDHHSIZ,     # Кол-во человек в домохозяйстве
    
    # =============================
    # 2. АНТРОПОМЕТРИЯ И ФИЗИОЛОГИЯ
    # =============================
    BMXHT,        # Рост
    BMXWT,        # Вес
    BMXBMI,       # ИМТ
    BMXWAIST,     # Окружность талии
    #MGDCGSZ,      # Сила хвата
    
    # =============================
    # 3. АРТЕРИАЛЬНОЕ ДАВЛЕНИЕ
    # =============================
    BPXSY1,       # Систолическое давление
    BPXDI1,       # Диастолическое давление
    
    # =============================
    # 4. САМООЦЕНКА ЗДОРОВЬЯ
    # =============================
    #HSD010,       # Самооценка здоровья
    HUQ010,       # Наличие медучреждения
    HUQ030,       # Последний визит к врачу
    
    # =============================
    # 5. ХРОНИЧЕСКИЕ ЗАБОЛЕВАНИЯ
    # =============================
    MCQ010,       # Астма
    MCQ080,       # Артрит
    MCQ160A,      # Сердечная недостаточность
    MCQ160B,      # ИБС
    MCQ160C,      # Стенокардия
    MCQ160D,      # Инфаркт
    MCQ160E,      # Инсульт
    MCQ160F,      # Эмфизема
    #DIQ010,       # Диабет диагностирован
    MCQ300C,      # Родственники с диабетом
    
    # =============================
    # 6. ОБРАЗ ЖИЗНИ
    # =============================
    SMQ020,       # Курил ≥100 сигарет
    SMQ040,       # Статус курения
    #SMD057,       # Сигарет в день
    ALQ101,       # Употребление алкоголя
    ALQ110,       # Дни употребления алкоголя
    #ALQ130,       # Напитков в день
    
    # =============================
    # 7. ПИТАНИЕ
    # =============================
    DBQ700,       # Оценка питания
    #DBQ197,       # Сладкие напитки
    #starts_with("DBQ233"),# Потребление воды (разные варианты вопроса:
    # DBQ233A - обычная вода
    # DBQ233B - минеральная вода
    # и т.д.)
    #DBD100,	#Добавление соли
    
    # =============================
    # 8. ФИЗИЧЕСКАЯ АКТИВНОСТЬ
    # =============================
    PAQ605,       # Пешие/велопрогулки ≥10 мин
    PAQ620,       # Умеренная активность
    PAQ635,       # Дни активности за 30 дней
    
    # =============================
    # 9. СОН И ПСИХИКА
    # =============================
    SLQ050, 
    SLQ060, # Проблемы со сном      # Сколько спите в сутки
    DPQ020,       # Подавленность
    DPQ040,       # Ангедония
    
    # =============================
    # 10. ФУНКЦИОНАЛЬНЫЕ ОГРАНИЧЕНИЯ
    # =============================
    PFQ020,       # Проблемы со ступенями
    #PFQ030,       # Проблемы при ходьбе 400 м
    
    # =============================
    # 11. ЦЕЛЕВАЯ ПЕРЕМЕННАЯ
    # =============================
    target        # Целевая переменная
  )
library(summarytools)


selected_data[] <- lapply(selected_data, function(x) {
  if (is.character(x) | is.factor(x)) {
    x[grepl("^do(n't| not) know$", x, ignore.case = TRUE)] <- NA
  }
  return(x)
})
library(summarytools)
summary_output <- dfSummary(selected_data, title.freq = "Summary of data", style = "grid",plain.ascii = FALSE) 
view(summary_output, file = "summary_2013.html")
write.csv(selected_data, "selected_2013.csv")


