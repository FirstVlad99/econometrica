library(dplyr)
library(ggplot2)
library(readxl)
library(forecast)
library(lm.beta)
library(car)
library(corrplot)
# читаю данные
data <- read_excel("dataFor2lab.xlsx")

num_vars <- data %>% 
  select(income, outcome, population, distance_to_moscow, latitude) %>%
  mutate(across(everything(), ~as.numeric(.)))

data$latitude <- as.numeric(data$latitude)

# Корреляционная матрица для числовых переменных
cor_matrix <- cor(num_vars, use="complete.obs")
corrplot(cor_matrix, method = "color", addCoef.col = "black")

# Проверка гипотезы о значимости корреляции (p-value)
cor_test_matrix <- function(df){
  n <- ncol(df)
  res <- matrix(NA, n, n)
  for(i in 1:n){
    for(j in 1:n){
      res[i,j] <- cor.test(df[[i]], df[[j]])$p.value
    }
  }
  colnames(res) <- rownames(res) <- colnames(df)
  return(res)
}
p_matrix <- cor_test_matrix(num_vars)
p_matrix

nrow(data)
length(unique(data$latitude))
length(unique(data$federal_district))

# Диаграммы рассеяния
plot(data$income, data$outcome, main="Зависимость инвестиций от доходов",
     xlab="Доход", ylab="Инвестиции")

# создаем фиктивные переменные для федеральных округов
data$federal_district <- as.factor(data$federal_district)


# Множественная линейная регрессия
model_multi <- lm(
  outcome ~ income * federal_district +
    population +
    distance_to_moscow +
    latitude,
  data = data
)

summary(model_multi)
accuracy(model_multi)

# Стандартизованные коэффициенты
model_std <- lm.beta(model_multi)
model_std

# Проверка мультиколлинеарности
vif(model_multi, type = "predictor")

# Значимость всей регрессии
summary(model_multi)$fstatistic

# Корреляционный анализ (cor_matrix и corrplot) показал:
# Самая сильная корреляция наблюдается между population и outcome (инвестиции), 
#   а также между population и latitude (коэффициенты положительные и значимые по p-value).
# income имеет слабую корреляцию с outcome, p-value ~ 0.09, что говорит о слабой статистической значимости.
# distance_to_moscow и latitude показывают слабую отрицательную зависимость с outcome.

# Множественная регрессия (model_multi) показала:
# - population является статистически значимым фактором (p < 0.001) и имеет наибольший стандартизованный коэффициент (0.73), 
#   что говорит о сильном влиянии численности населения на инвестиции.
# - Остальные переменные (income, distance_to_moscow, latitude, federal_district) статистически не значимы на уровне 0.05.
# - R-squared ~0.67, скорректированный R-squared ~0.62 — модель объясняет большую часть вариации outcome, 
#   но часть влияния остаётся необъяснённой.

# Мультиколлинеарность:
# - VIF показывает высокую коллинеарность для distance_to_moscow (VIF ~27) и federal_district (VIF ~136 для набора категориальных переменных),
#   что может искажать оценки отдельных коэффициентов, но модель в целом остаётся интерпретируемой для главных факторов.

# Частные уравнения регрессии
coef(model_multi)

cat("Частное уравнение по доходу:\n")
cat("Y = const +", coef(model_multi)["income"], "* income\n\n")

cat("Частное уравнение по населению:\n")
cat("Y = const +", coef(model_multi)["population"], "* population\n\n")

cat("Частное уравнение по расстоянию:\n")
cat("Y = const", coef(model_multi)["distance_to_moscow"], "* distance_to_moscow\n\n")

cat("Частное уравнение по широте:\n")
cat("Y = const", coef(model_multi)["latitude"], "* latitude\n\n")

# Оценка коэффициентов эластичности (лог-лог модель)
data <- data %>%
  mutate(log_outcome = log(outcome),
         log_income = log(income),
         log_population = log(population))

model_log <- lm(
  log_outcome ~ log_income * federal_district +
    log_population +
    distance_to_moscow +
    latitude,
  data = data
)
summary(model_log)

# Значимость логарифмической модели
summary(model_log)$fstatistic

# Эластичности
elasticities <- sapply(names(coef(model_log))[-1], function(var){
  if(var %in% c("log_income","log_population")){
    coef(model_log)[var]   # прямые эластичности
  } else {
    coef(model_log)[var]*mean(data[[var]])/mean(data$outcome)
  }
})
elasticities

# Оценка коэффициентов эластичности

# Лог-лог модель (model_log) позволяет оценить прямые эластичности:
# log_income: 1.34 - увеличение дохода на 1% увеличивает инвестиции примерно на 1.34%
# log_population: 0.87 - увеличение численности населения на 1% увеличивает инвестиции на 0.87%
# Эти результаты подтверждают выводы множественной регрессии: население и доходы — ключевые факторы.

# Для непрямых переменных (distance_to_moscow, latitude) расчёт эластичности через средние значения данных:
# - distance_to_moscow: ~ -3.6e-5 - влияние очень слабое, практически нулевое
# - latitude: ~ -1.86e-4 - также практически не влияет на outcome
# - федеральные округа дали NA, так как они категориальные переменные и их среднее число нельзя использовать для эластичности

# Вывод:
# - Эластичность инвестиций по доходам и населению положительная и достаточно высокая.
# - Географические характеристики и федеральные округа оказывают минимальное влияние.
# - Полезно для прогнозирования: рост доходов и населения напрямую повышает инвестиции.

# Прогноз для Карелии
karelia <- data %>% filter(region == "Республика Карелия", year==2012)
pred_karelia <- predict(model_log, newdata=karelia, interval="prediction", level=0.95)
pred_karelia <- exp(pred_karelia)

pred_karelia

# Точечный и интервальный прогноз
fact <- karelia$outcome
forecast_value <- pred_karelia[1,"fit"]
lower <- pred_karelia[1,"lwr"]
upper <- pred_karelia[1,"upr"]

cat("Факт:", fact, "\n")
cat("Прогноз:", forecast_value, "\n")
cat("Нижняя граница:", lower, "\n")
cat("Верхняя граница:", upper, "\n")

# Ошибка прогноза
abs_error <- abs(fact - forecast_value)
rel_error <- abs_error / fact * 100

cat("Абсолютная ошибка:", abs_error, "\n")
cat("Относительная ошибка (%):", rel_error, "\n")

# Визуализация прогноза для Карелии
ggplot() +
  
  # Все регионы (ФО)
  geom_point(
    data = data,
    aes(x = income / 1000,
        y = outcome,
        color = federal_district),
    alpha = 0.3
  ) +
  
  # Линии регрессии по ФО
  geom_smooth(
    data = data,
    aes(x = income / 1000,
        y = outcome,
        color = federal_district,
        group = federal_district),
    method = "lm",
    se = FALSE
  ) +
  
  # Факт Карелии
  geom_point(
    data = karelia,
    aes(x = income / 1000,
        y = outcome,
        color = "Карелия (факт)"),
    size = 3
  ) +
  
  # Прогноз Карелии
  geom_point(
    data = karelia,
    aes(x = income / 1000,
        y = forecast_value,
        color = "Карелия (прогноз)"),
    size = 3,
    shape = 17
  ) +
  
  scale_color_manual(
    values = c(
      "Центральный федеральный округ" = "#1f77b4",
      "Северо-Западный федеральный округ" = "#ff7f0e",
      "Южный федеральный округ" = "#2ca02c",
      "Северо-Кавказский федеральный округ" = "#d62728",
      "Приволжский федеральный округ" = "#9467bd",
      "Уральский федеральный округ" = "#8c564b",
      "Сибирский федеральный округ" = "#e377c2",
      "Дальневосточный федеральный округ" = "#7f7f7f",
      
      "Карелия (факт)" = "black",
      "Карелия (прогноз)" = "orange"
    )
  ) +
  
  scale_y_continuous(
    labels = scales::comma_format(big.mark = " ", suffix = " млн")
  ) +
  
  scale_x_continuous(
    labels = scales::comma_format(big.mark = " ", suffix = " тыс. руб.")
  ) +
  
  labs(
    title = "Регрессии по федеральным округам + Карелия",
    x = "Доход, тыс. руб.",
    y = "Инвестиции, млн",
    color = "Объект"
  )


# Исследование неоднородности данных

# Остатки модели
plot(residuals(model_multi),
     main="Остатки множественной регрессии",
     ylab="Остатки",
     xlab="Наблюдения")

abline(h=0, col="red")

# Ящики с усами 
boxplot(data$outcome,
        main = "Проверка выбросов инвестиций",
        ylab = "Инвестиции (млн руб.)")


boxplot(data$population / 1e6,
        main = "Проверка выбросов населения",
        ylab = "Население (млн человек)")

