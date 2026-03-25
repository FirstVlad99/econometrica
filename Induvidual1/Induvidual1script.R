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

