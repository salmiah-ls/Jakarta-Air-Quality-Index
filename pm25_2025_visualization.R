library(tidyverse)
library(lubridate)
library(ggplot2)
library(leaflet)

### Bundaran HI ###
jkt1 <- read.csv("jakarta/ispu_dki1.csv")
head(jkt1, 5)
tail(jkt1, 5)

glimpse(jkt1)

# convert datatype in column 'tanggal' from char into datetime
jkt1$tanggal <- as.Date(jkt1$tanggal)

jkt1_2025 <- jkt1 %>% filter(tanggal > '2024-12-31')


# Kelapa Gading
jkt2 <- read.csv("jakarta/ispu_dki2.csv")
glimpse(jkt2)
jkt2$tanggal <- as.Date(jkt2$tanggal)

jkt2_2025 <- jkt2 %>% filter(tanggal > '2024-12-31')
View(jkt2_2025)


# Jagakarsa
jkt3 <- read.csv("jakarta/ispu_dki3.csv")
glimpse(jkt3)
jkt3$tanggal <- as.Date(jkt3$tanggal)

jkt3_2025 <- jkt3 %>% filter(tanggal > '2024-12-31')
View(jkt3_2025)


# Lubang Buaya
jkt4 <- read.csv("jakarta/ispu_dki4.csv")
glimpse(jkt4)
jkt4$tanggal <- as.Date(jkt4$tanggal)

jkt4_2025 <- jkt4 %>% filter(tanggal > '2024-12-31')
View(jkt4_2025)


# Kebon Jeruk
jkt5 <- read.csv("jakarta/ispu_dki5.csv")
glimpse(jkt5)
jkt5$tanggal <- as.Date(jkt5$tanggal)

jkt5_2025 <- jkt5 %>% filter(tanggal > '2024-12-31')
View(jkt5_2025)


# merge
jktall_2025 <- rbind(jkt1_2025, jkt2_2025, jkt3_2025, jkt4_2025, jkt5_2025)
View(jktall_2025)
write.csv(jktall_2025, "jakarta/jktall_2025.csv")

# visualize with scatter and line
ggplot(jktall_2025, aes(tanggal, pm25, colour = stasiun)) + geom_point() + 
  ggtitle ("Level of PM2.5 in air measured at 5 stations in Jakarta in early 2025") + xlab("Date (in 2025)") + ylab ("PM2.5 Level") +
  theme(plot.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12))

ggplot(jktall_2025, aes(tanggal, pm25, colour = stasiun)) + geom_line() +
  ggtitle ("Level of PM2.5 in air measured at 5 stations in Jakarta in early 2025") + xlab("Date (in 2025)") + ylab ("PM2.5 Level") +
  theme(plot.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12))

# calculate mean of PM25 for each station
list_pm25 <- jktall_2025 %>% group_by(stasiun) %>% summarise(avg_pm25 = mean(pm25, na.rm = T))
df_pm25 <- as.data.frame(list_pm25)
View(df_pm25)

# assign latitude and longitude to stasiun
latitude <- c(-6.19, -6.15, -6.33, -6.29, -6.19)
df_pm25['latitude'] <- latitude

longitude <- c(106.82, 106.9, 106.81, 106.9, 106.77)
df_pm25['longitude'] <- longitude

glimpse(df_pm25)

# visualize with map
DKI_map <- leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  setView(lat = -6.24, lng = 106.82, zoom = 11)
DKI_map

DKI_map %>% addCircleMarkers(data = df_pm25, 
                             lat = ~latitude,
                             lng = ~longitude,
                             radius = ~avg_pm25/2,
                             color = 'red',
                             fillOpacity = 0.5,
                             stroke = FALSE,
                             popup = ~paste("Station: ", df_pm25$stasiun, "<br />PM25 Level: ", round(df_pm25$avg_pm25, 2)))

