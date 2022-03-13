# Работа с dataframe. Циклы
#1. Загрузить данные из evals.csv в новую переменную dats2.
options(stringsAsFactors = FALSE) 
dats2 <- read.csv("evals.csv", header = TRUE)
#2. В таблице dats2 создать столбец Number нумерации строк.
dats2$Number <- 1:nrow(dats2)
#3. Отсортировать значения столбца Number в порядке убывания.
dats2 <- dats2[order(-dats2$Number),]
#4. Сделать столбец Number первым, остальные столбцы сместить вправо.
dats2<- dats2[,c(22,1:21)]
#5. Удалить из набора данных dats2 переменную Number.
dats2$Number <- NULL
#6. Добавить в код комментарий о том, какую именно переменную удалили.
### Number
#7. Узнать размерность таблицы dats2.
dim(dats2)
#8. В наборе данных dats2 отобрать всех, у кого оценка (score) выше четырёх.
dats2[dats2$score > 4,]
#9. Отобрать из dats2 значения оценок, полученных женщинами.
dats2[dats2$gender=="female","score"]
#10. Получить три колонки данных (на выбор студента), которые касаются только женщин.
dats2[dats2$gender=="female",1:3]
#11. Данные, которые принадлежат женщинам сохранить их в переменную dat_f.
dat_f <- dats2[dats2$gender == 'female',]
#12. Отобрать данные, которые принадлежат мужчинам, сохранить в dat_m.
dat_m <- dats2[dats2$gender == 'male',]
#13.Изучить функции rbind(), cbind().
help(rbind)
help(cbind)
#14.Соединить переменные dat_f и dat_m по строкам и сохранить в переменную dat_fm.
dat_fm <- rbind(dat_f,dat_m)
#15. Создать переменную dats3, содержащую со второго по седьмой столбцы таблицы dats2.
dats3 <- dats2[,2:7]
#16. Создать переменную dats4 состоящую из столбцов набора данных dat: с восьмого по одиннадцатый.
dats4 <- dats2[8:11]
#17. Соединить переменные dats3 и dats4 по столбцам, сохранить значение в переменную dats5.
dats5 <- cbind(dats3,dats4)
#18. Удалить dats5 из памяти.
rm(dats5)
#19. Одной командой удалить из памяти данные dats3, dats4.
rm(dats3,dats4)
#20. Заменить маленькие буквы на большие в названии любой переменной из таблицы данных dats2.
(tmp <- sample(ncol(dats2), 1))
names(dats2)[tmp] <- toupper(names(dats2)[tmp])
#21. Вывести на экран все названия переменных из набора данных dats2.
names(dats2)
#22.Создать переменную dats6 из 1,4,5,6,7,10 столбцов переменной dats2.
dats6 <- dats2[,c(1,4,5,6,7,10)]
#23.Добавить в таблицу dats6 строку с произвольными переменными.
dats6 <- rbind(dats6, list(1, 'female', 'english', 0, 13, 'lvl'))
#24.Посмотреть последние 6 строк dats6.
tail(dats6)
#25.Избавиться от нечисловых переменных набора данных dats6.
dats6 <- dats6[, which(sapply(dats6,is.numeric))]
#26.Создать диаграммы парных зависимостей таблицы dats6.
pairs(dats6)
#27.Изменить имя пятой строки в таблице dats6.
rownames(dats6)[5] <- "temp"
#28. Удалить из памяти все данные.
rm(list=ls())
# 2 циклы
#1 Загрузить в переменную cik таблицу данных evals.csv.
cik <- read.csv("evals.csv", header = TRUE)
#2. В наборе данных cik создать новую числовую переменную cik_new, которая в строках содержит единицы, если оценка (score) выше 4. В строках, где условие не выполняется, поставить нули. Задачу решить с использованием конструкции ifelse.
cik$cik_new <- ifelse(cik$score > 4, 1, 0)
# 3. Рассчитать скользящее среднее для  переменной score набора данных cik с интервалом сглаживания равным  7  (сначала вычисляется   среднее для элементов  с  1  по  7,  затем  –  среднее для элементов со  2  по  8  и т.д. ). Результат значений средних сохранить в переменную score_mov. 
# Вектор score_mov можно создать заранее: 
score_mov <- c() 
for (i in 1:(nrow(cik)-6)){ 
  score_mov <- append(score_mov, mean(cik$score[seq(i,i+6)]))
}
score_mov

# Манипулирование данными. Набор пакетов “tidyverse”
# 1. Установить пакет tidyverse.
install.packages("tidyverse")
# 2. В среду R загрузить пакет readr.
library(readr)
# 3. В переменную company загрузить данные из таблицы company2.csv, созданной на практической работе №1, используя функции пакета readr.
company <- read_csv2("company2.csv")
# 4. Вывести в отдельном окне редактора кода данные переменной company.
View(company)
# 5. Посмотреть тип данных переменной company.
class(company)
# 6. Установить и загрузить пакет reshape2.
install.packages("reshape2")
library(reshape2)
# 7. Присвоить новой переменной company1 "расплавленные" в "длинный" формат данные, указав в качестве идентификационных переменных возраст и общий стаж работы.
company1 <- melt(company, id.vars=c("age","year_service"))
# 8. Посмотреть первые 7 строк таблицы company1.
head(company1, 7,)
# 9. Сохранить в переменную company2 "собранные" в "широкий" формат данные переменной company1.
company2 <- dcast(company1, Age + yer_service ~ variable)
# 10. Посмотреть первые 7 строк таблицы company2.
head(company2, 7,)
# 11. Сравнить наборы данных company1 и company2.
all.equal(company1,company2)
# 12. Загрузить пакет dplyr.
library(dplyr)
# 13. Работа с оператором pipe %>%.
# 13.1. Вывести на экран с 4 по 8 строки набора данных company с помощью команды head().
company %>% head(4,8)
# 13.2. Посмотреть с 4 по 8 строки набора данных company, используя оператор pipe %>%.
company %>% slice(4:8)
# ? 13.3. Сравнить полученные результаты, используя логическое равенство.
company %>% head(4,8) == company %>% slice(4:8)
# 14. Работа с функцией фильтрации строк: filter().
# 14.1. В качестве переменной, по которой необходимо выполнить фильтрацию, взять стаж работы в данной компании (в наборе данных company пять вариантов стажа: 1-5, выбрать по варианту, данному преподавателем).
# 5
# 14.2. Отфильтровать таблицу company двумя способами (с использованием и без использования оператора pipe %>%). Сравнить являются ли, полученные в результате фильтрации, таблицы одинаковыми.
company %>% filter(yer_service_ooo == 5) %>% all.equal(filter(company, yer_service_ooo == 5))
# 14.3. Отфильтровать набор данных company используя 2 условия: стаж работы сотрудников компании и возраст (критерий для возраста выбрать самостоятельно). Показать первые 4 строки полученной таблицы данных.
company %>% filter(yer_service_ooo == 5 & Age > 30) %>% head(4,)
# 14.4. Выбрать из таблицы company со 2 по 6 строки с помощью оператора pipe %>% и функции slice().
company %>% slice(2:6)
# 15. Работа с функцией упорядочения строк: arrange().
# 15.1. Упорядочить строки набора данных company по двум, самостоятельно выбранным, переменным, используя функцию arrange() и оператор pipe %>%.
company %>% arrange(Name, Martial)
# 15.2. Упорядочить строки таблицы company по убыванию переменной (переменную выбрать самостоятельно), используя функцию desc().
company %>% arrange(desc(Age))
# 16. Работа с функцией выбора колонок: select().
# 16.1. Выбрать самостоятельно любые 3 колонки из таблицы данных company.
company %>% select(c(Name, Age, Martial))
# 16.2. Изучить вспомогательные функции contains(), ends_with(), starts_with() и matches(), привести примеры работы с ними.
help(contains)
company %>% select(contains('year_service'))
help(ends_with)
company %>% select(ends_with('on'))
help(starts_with)
company %>% select(starts_with('ag'))
help(matches)
company %>% select(matches('year_service\\B'))
# 17. Работа с функцией создания новых колонок: mutate().
# 17.1. Используя функцию mutate(), создать в наборе данных company новый столбец, который будет содержать процент стажа работы в данной компании от общего стажа работы.
company <- company %>% mutate(stolbec = year_service_ooo/year_service*100)
# 18. Установить пакет gapminder и загрузить его в среду программирования R.
install.packages("gapminder")
library(gapminder)
# 19. Присвоить новой переменной vvp данные таблицы gapminder (входит в пакет gapminder).
vvp <- gapminder
# 20. Выбрать из таблицы vvp данные за 1952 год (переменная year) со средней продолжительностью жизни больше 60 лет (переменная lifeExp). Сохранить эти данные в таблицу vvp1952.
vvp1952 <- vvp %>% filter(year == 1952 & lifeExp > 60)
# 21. Данные таблицы vvp1952 отсортировать по убыванию населения (переменная pop).
vvp1952 %>% arrange(desc(pop))
# 22. Из таблицы данных vvp выбрать переменные: ВВП (gdpPercap), континент (continent), год (year).
vvp %>% select(c(gdpPercap, continent, year))
# 23. В набор данных vvp добавить столбец с продолжительностью жизни, указанной в днях (принять год равным 365 дней).
vvp <- vvp %>% mutate(stolbec = lifeExp*365)
# 24. Для каждой страны (country) посчитать среднее значение ВВП. Отсортировать результаты по убыванию.
vvp %>% group_by(country) %>% summarise(mean = mean(gdpPercap)) %>% arrange(desc(mean))
# ? 25. Разбить набор данных vvp на две таблицы: до 1980 г., после 1980 г.
# group_by()
do1980 <- vvp %>% filter(year < 1980)
posle1980 <- vvp %>% filter(year > 1980)
# 26. Выбрать переменные lifeExp, country.
do1980 %>% select(c(lifeExp, country))
posle1980 %>% select(c(lifeExp, country))
# 27. Вычислить для каждой страны среднюю продолжительность жизни.
do1980 %>% group_by(country) %>% summarise(mean = mean(lifeExp))
posle1980 %>% group_by(country) %>% summarise(mean = mean(lifeExp))
# 28. Используя inner_join объединить две таблицы по стране.
inner_join(do1980, posle1980, by = c('country' = 'country'))