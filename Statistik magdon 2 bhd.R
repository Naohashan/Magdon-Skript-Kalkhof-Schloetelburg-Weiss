#-------------------------------------------------------------------------------
#22.07.2026
#Magdon Sensorik
#Gruppenarbeit. Auswertung der Aufnahmen
#Benedikt Kalkhof, Jona Schlötelburg, Jonas Weiß
#dieses Skript behandelt BHD Statistik
#-------------------------------------------------------------------------------
#wir machen hier spezifisch statistik zum BHD
library(dplyr)
library(ggplot2)
library(stats)
library(tidyr)

#Einlesen der Daten
einzelbaum <- read.csv2("Daten/Einzelbaumvariablen.csv")
plotdaten <- read.csv2("Daten/Plotvariablen.csv")
daten <- left_join(
  einzelbaum,
  plotdaten,
  by = c("aufnahme_id", "plot_id", "baum_id")
)

#aufräumen
daten$hoehe <- as.numeric(daten$hoehe)
daten$bhd <- as.numeric(daten$bhd)
daten$methode <- as.factor(daten$methode)
daten$baumart <- as.factor(daten$baumart)

bhd <- na.omit(daten)
str(bhd)
levels(bhd$methode)
bhd <- droplevels(bhd)
#-------------------------------------------------------------------------------
#statistik

bhd %>%
  group_by(methode) %>%
  summarise(
    mean = mean(bhd),
    sd = sd(bhd),
    var = var(bhd),
    min = min(bhd),
    max = max(bhd),
    median = median(bhd)
  )
#methode    mean    sd   var   min   max median
#Arboreal   47.0  9.38  87.9  30.6  63.5   47.5
#Klassisch  49.7 10.1  102.   30.1  65.2   49.3
#individuelle shapiro tests für bhd ohne moti
shapiro.test(daten$bhd[daten$methode == "Klassisch"]) #nicht normalverteilt p=0.004267
shapiro.test(daten$bhd[daten$methode == "Arboreal"]) #nicht normalverteilt p=0.004994
#homogenität
leveneTest(bhd ~ methode, data = bhd) #homogenität gegeben f 1.1114 p 0.2932
#wir entscheiden uns für das linear gemischte Modell
mod_bhd <- lmer(
  bhd ~ methode + (1|baum_id),
  data = bhd
)
summary(mod_bhd)
emmeans(mod_bhd, pairwise ~ methode)
res <- residuals(mod_bhd)

shapiro.test(res)

sd_res <- sd(res)
d <- abs(49.7 - 47.0) / sd_res
d #1.44877 cohens d. Das heißt ein großer Effekt von Residuen. BHD Unterschiede
#sind signifikant und praktisch bedeutsam
#-------------------------------------------------------------------------------
#wir machen daraus einen Boxplot um das zu visualisieren
plot3 <- ggplot(bhd, aes(x = methode, y = bhd)) +
  geom_boxplot(fill = "grey80") +
  geom_point(position = position_jitter(width = 0.15), alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3)+
  labs(title = "Boxplot der BHD-Werte nach Methoden", x = "Messmethode", y = "BHD [cm]")
plot3
#-------------------------------------------------------------------------------
#wiederholung der gesamten Testreihe mit mittelwerten
bhd_mittel <- bhd %>%
  group_by(plot_id, baum_id, methode) %>%
  summarise(
    bhd = mean(bhd),
    .groups = "drop"
  )
shapiro.test(bhd_mittel$bhd[bhd_mittel$methode == "Klassisch"]) #normalverteilt p=0.3479
shapiro.test(bhd_mittel$bhd[bhd_mittel$methode == "Arboreal"]) #normalverteilt p=0.0.3671
leveneTest(bhd ~ methode, data = bhd_mittel) #varianzhomogen
#linear gemischtes Modell
mod_bhd2 <- lmer(
  bhd ~ methode + (1|baum_id),
  data = bhd_mittel
)
summary(mod_bhd2)
emmeans(mod_bhd2, pairwise ~ methode)
shapiro.test(residuals(mod_bhd2)) #residuen normalverteilt
#methode   emmean  SE   df lower.CL upper.CL
#Arboreal    47.0 1.8 31.2     43.4     50.7
#Klassisch   49.7 1.8 31.2     46.0     53.3
#contrast             estimate   SE df t.ratio p.value
# Arboreal - Klassisch    -2.65 0.69 29  -3.848  0.0006
#das heißt dass es HOCHsignifikante Unterschiede gibt!! Klassisch ist im schnitt größer
#-------------------------------------------------------------------------------
#Wird für die Arbeit nicht verwendet
plot4 <- ggplot( 
  bhd,
  aes(x = factor(baum_id),y = bhd, color = methode)) +
  stat_summary(
    fun = mean,
    geom = "point"
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = methode)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1
    )
  )+
  labs(title = "Messwerte als Mittelwerte pro Baum", x = "Baum", y = "BHD [cm]")

