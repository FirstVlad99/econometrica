library(dplyr)
library(ggplot2)
library(readxl)
# читаю данные
setwd("/home/user/builds/Github/econometrica/Induvidual1")
data <- read_excel("data.xlsx")

# обработка данных
data$income <- as.numeric(gsub(",", ".", data$income))
data$outcome <- as.numeric(gsub(",", ".", data$outcome))

# удаляю строки с нулями (отсутствие данных)
data_clean <- data %>% filter(income != 0 & outcome != 0)

# убираю выбросы по квантилям
lower_income <- quantile(data_clean$income, 0.1)
upper_income <- quantile(data_clean$income, 0.95)
lower_outcome <- quantile(data_clean$outcome, 0.1)
upper_outcome <- quantile(data_clean$outcome, 0.95)

data_filtered <- data_clean %>%
  filter(income >= lower_income & income <= upper_income &
         outcome >= lower_outcome & outcome <= upper_outcome)
# визуализация данных (общая)
ggplot(data_filtered, aes(x = income, y = outcome)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_y_continuous(labels = scales::comma_format(big.mark = " ", suffix = " млн")) +
  scale_x_continuous(labels = scales::comma_format(big.mark = " ")) +
  labs(title = "Зависимость инвестиций от доходов",
       x = "Среднедушевые доходы (руб.)",
       y = "Инвестиции в жилища")

# коэффициент корреляции
correlation <- cor(data_filtered$income, data_filtered$outcome, use = "complete.obs")
correlation

# пусть уровень значимости 0.05 (то есть вероятность, что связь случайна, практически нулевая.)
# проверка значимости
cor_test <- cor.test(data_filtered$income, data_filtered$outcome)
cor_test

# Вывод: r = 0.394, p-value < 0.05, зависимость положительная и значимая
# Связь слабая, но статистически значимая

# Линейная модель для всех регионов
model_all <- lm(outcome ~ income, data = data_filtered)
summary(model_all)

#если доход увеличивается на 1 рубль, то инвестиции в жильё в среднем увеличиваются на 0,96 рубля.
#R^2 = 0.155, значит модель объясняет только 15% разброса инвестиций.
# Это не очень много. Значит, большая часть инвестиций зависит от других факторов, не учтённых в твоей модели.
# прогнозы
data_filtered$pred_all <- predict(model_all, newdata = data_filtered)

# MAPE для общей модели
mape_all <- mean(abs(data_filtered$outcome - data_filtered$pred_all) / data_filtered$outcome) * 100
mape_all
# # MAPE = 265%, значит линейная модель без логарифмов слишком грубая для прогноза
# добавляем логарифмы
data_filtered <- data_filtered %>%
  mutate(log_income = log(income),
         log_outcome = log(outcome))

# общая модель с логарифмами
model_log_all <- lm(log_outcome ~ log_income, data = data_filtered)
summary(model_log_all)

# прогноз и MAPE
data_filtered$pred_log_all <- exp(predict(model_log_all, newdata = data_filtered))
mape_log_all <- mean(abs(data_filtered$outcome - data_filtered$pred_log_all) / data_filtered$outcome) * 100
mape_log_all

# визуализация логарифмических данных (общая)
ggplot(data_filtered, aes(x = log(income), y = log(outcome))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Логарифическая зависимость инвестиций от доходов",
       x = "log(Среднедушевые доходы) (руб.)",
       y = "log(Инвестиции в жилища)")
# MAPE = 138% лучше, но все еще очень высоко для общей модели
# модель для Карелии
karelia <- data_filtered %>% filter(region == "Республика Карелия")
model_log_karelia <- lm(log_outcome ~ log_income, data = karelia)
karelia$pred_log <- exp(predict(model_log_karelia, newdata = karelia))
mape_log_karelia <- mean(abs(karelia$outcome - karelia$pred_log) / karelia$outcome) * 100
mape_log_karelia

# визуализация для Республики Карелия (логарифм)
ggplot(karelia, aes(x = log_income, y = log_outcome)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Логарифмированная зависимость инвестиций от доходов (Карелия)",
       x = "log(Среднедушевые доходы)",
       y = "log(Инвестиции в жилища)") +
  theme_minimal()

# визуализация для Республики Карелия (исходные)
ggplot(karelia,  aes(x = income/1000, y = outcome)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_y_continuous(labels = scales::comma_format(big.mark = " ", suffix = " млн")) +
  scale_x_continuous(labels = scales::comma_format(big.mark = " ")) +
  labs(title = "Зависимость инвестиций от доходов (Карелия)",
       x = "Среднедушевые доходы (тыс. руб)",
       y = "Инвестиции в жилища (млн. руб)") +
  theme_minimal()

# построить прогноз для РК для 2012
karelia_point <- data_filtered %>%
  filter(region == "Республика Карелия", year == 2012)

income_real <- karelia_point$income
outcome_real <- karelia_point$outcome

# прогноз по log модели
# создаю данные для предсказания
pred_data <- data.frame(log_income = log(income_real))

# прогноз + доверительный интервал
log_pred <- predict(model_log_all,
                    newdata = pred_data,
                    interval = "prediction",
                    level = 0.95)

# возвращаем из логов
pred_outcome <- exp(log_pred[1])
pred_lower <- exp(log_pred[2])
pred_upper <- exp(log_pred[3])

#Визуализация 
ggplot(data_filtered, aes(x = income, y = outcome)) +
  geom_point(aes(color = "Регионы"), alpha = 0.3) +
  
  stat_smooth(aes(color = "Линия регрессии"),
              method = "lm",
              formula = y ~ x,
              se = FALSE) +
  
  # факт Карелии
  geom_point(aes(x = income_real, y = outcome_real, color = "Карелия (факт)"),
             size = 3) +
  
  # прогноз
  geom_point(aes(x = income_real, y = pred_outcome, color = "Карелия (прогноз)"),
             size = 3, shape = 17) +
  
  scale_color_manual(values = c("Регионы" = "blue",
                                "Линия регрессии" = "red",
                                "Карелия (факт)" = "green",
                                "Карелия (прогноз)" = "orange")) +
  
  scale_y_continuous(labels = scales::comma_format(big.mark = " ", suffix = " млн")) +
  scale_x_continuous(labels = scales::comma_format(big.mark = " ", suffix = " руб.")) +
  
  labs(title = "Прогноз инвестиций (Карелия, 2012)",
       x = "Среднедушевые доходы",
       y = "Инвестиции в жилища",
       color = "") +
  
  theme_minimal()

#Экономические выводы
# ошибки прогноза
abs_error <- abs(outcome_real - pred_outcome)
rel_error <- abs_error / outcome_real * 100

# Связь между доходами и инвестициями слабая
# Коэффициент корреляции r = 0.3939 зависимость положительная, но слабая

# Логарифмическая модель лучше линейной
# (ошибка ниже, модель адекватнее описывает зависимость)

# Сравнение прогноза и факта для Карелии (2012)
# Фактические инвестиции НИЖЕ прогнозных
# Отклонение: 8698.39 млн руб.
# Относительная ошибка: 306.61%
#  модель сильно переоценивает инвестиции для данного региона

# Эластичность
# бета = 0.9708

# Интерпретация эластичности
# Инвестиции НЕЭЛАСТИЧНЫ по доходу (бета < 1)
# Это означает:
# при росте доходов на 1% инвестиции увеличиваются примерно на 0.97%
# влияние доходов почти пропорциональное, но немного слабее

# Для увеличения инвестиций на 1% доход должен вырасти примерно на 1.03%

# Общий вывод
# Доходы оказывают влияние на инвестиции, но не являются ключевым фактором

# Экономическая интерпретация
# Высокая ошибка прогноза (более 300%) показывает:
# - модель по всем регионам плохо описывает отдельный регион (Карелию)
# - инвестиции зависят от дополнительных факторов

# Итог
# Для повышения качества модели необходимо учитывать дополнительные переменные
