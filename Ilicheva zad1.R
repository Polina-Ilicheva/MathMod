#Ильичева П.И.  – для региона 33 Владимирская область
#ЗАДАНИЕ: для столицы 33 региона скачайте все имеющиеся климатические данные (ежедневные осадки и среднедневные температуры) для 50 близлежайших метеостанций. 
#Рассчитайте урожайность пшеницы в 2006 году, взяв для рассчета средние суммы активных температур за предыдущие 4 год 
#установка (и проверка) рабочей директории
setwd("D:/polina/Ilicheva/1")
getwd()
#очистим рабочую память
rm(list=ls())
#загрузим пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)
#сохраним список метеостанций
station_data = ghcnd_stations()
station_data = read.csv("station_data2020.rdata")
#После получения списка всех станций, получите список станций ближайших к столице вашего региона,создав таблицу с именем региона и координатами его столицы
vla = data.frame(id = "VLADIMIR", latitude = 56.1366,  longitude = 40.3966)
vla_around = meteo_nearby_stations(lat_lon_df = vla, station_data = station_data,
                                    limit = 50, var = c("PRCP", "TAVG"),
                                    year_min = 2002, year_max = 2005)
#vla_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
#удалленности от Владимира, очевидно что первым элементом таблицы будет идентификатор метеостанции Владимира, его то мы и попытаемся получить
vla_table = vla_around[["VLADIMIR"]][["id"]][1]
#переведем в вектор
vla_id = vla_table[[1]]
#смотрим, что получилось
summary (vla_id)
vla_id2 = vla_id[1]
#смотрим, что получилось
summary (vla_id2)
#в таблице vla_table оказались объекты, ранжированные по расстоянию от Владимира
#нужно убедиться, что этот список включает нужные по условию задачи метеостанции
# отфильтруем все станции, на расстоянии 50 м 
vla_stations = vla_around[[1]] %>% filter(distance < 50)
#Таким образом, мы сформировали список необходимых станций, посмотрим, что он содержит
str(vla_stations)
vla_stations$id
#Создадим объект, куда скачаем все данные с 50 ближайших метеостанций
all_vla_meteodata = data.frame()
#Цикл для всех метеостанций
for(i in 1:50) 
  {
  vla_table = vla_around[["VLADIMIR"]][["id"]][i]
  vla_around = meteo_tidy_ghcnd(stationid = vla_table, var = "TAVG", date_min = "2002-01-01", date_max = "2005-12-31")
#с помощью команды rbind соединяем данные, полученные на предыдущих и данном этапах цикла
  all_vla_meteodata1=rbind(all_vla_meteodata,vla_around)
print(all_vla_meteodata1)
}
#записываем полученные результаты
write.csv(all_vla_meteodata1,"all_vla_meteodata1.csv")
#для расчета урожайности в 2006 году, необходимо ввести переменные и константы
#коэффициент для экспозиции склона - считаем что все поля идеально ровные
y = 1
#Константа afi, берется из табл. 1
afi = c (0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00)
#Константа bfi, берется из табл. 1
bfi = c (0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00)
#отношение числа дней i-го месяца, входящих в период вегетации культуры, к общему числу дней в месяце, берется из табл. 1.
di = c (0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00,0.00)
#коэффициент использования ФАР посевом
Kf = 300
#калорийность урожая культуры
Qj = 1600
#коэффициент «Сумма частей основной и побочной продукции»
Lj = 2.2
#коэффициент «Стандартная влажность культуры»
Ej = 25
#выбираем данные года, добавив year, month, day
all_vla_data = all_vla_meteodata1 %>% mutate(year = year(date), month = month(date), day = day(date))%>%
filter(year > 2001 & year < 2006) %>%
  #группируем с учетом id
  group_by (year, month, id) %>%
  #выбираем активные температуры, превышающие 5 градусов
  mutate (tavg=tavg/10) %>% filter(tavg>5) %>%
  #вычисляем сумму активных температур
  summarise(sum = sum(tavg, na.rm = TRUE)) %>%
  #вычисляем средние активные температуры по месяцам перед этим группируем наши данные
  group_by (month) %>% summarise(S = mean(sum, na.rm = TRUE)) %>%
#вычисляем урожайность для каждого месяца
mutate (F=((afi+bfi*y*S*di)*Kf)/(Qj*Lj*(100-Ej)))
#вычисляем суммарную урожайность
yield = sum(all_vla_data$F)
yield

#В результате расчетов было получено, что урожайность в 2006 году составляла 16,43068 ц/га



