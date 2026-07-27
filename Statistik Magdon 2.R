#-------------------------------------------------------------------------------
#22.07.2026
#Magdon Sensorik
#Gruppenarbeit. Auswertung der Aufnahmen
#Benedikt Kalkhof, Jona Schlötelburg, Jonas Weiß
#wir gehen hier die Höhen-Statistik durch. Am Ende wurden erstellte Plots
#zusammengefügt.
#-------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stats)
library(tidyr)
library(car)
library(lme4)
library(emmeans)
library(rstatix)
library(patchwork)
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
write.csv(daten, "Ausgabe/Daten.csv", row.names = FALSE)
#-------------------------------------------------------------------------------
#Boxplots höhe
plot1 <- ggplot(daten, aes(x = methode, y = hoehe)) +
  geom_boxplot(fill = "grey80") +
  geom_point(position = position_jitter(width = 0.15), alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  labs(title = "Boxplot sortiert nach Methoden", x = "Messmethode", y = "Höhe [m]")
plot1
#plot 2 wird nicht verwendet
plot2 <- ggplot(
  daten,
  aes(x = factor(baum_id),y = hoehe, color = methode)) +
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
  labs(title = "Messwerte als Mittelwerte pro Baum", x = "Baum", y = "Höhe [m]")
plot2
#-------------------------------------------------------------------------------
#statistik. Wir machen hier primär erstmal die hoehe
# Normalverteilung für jede Methode prüfen hoehe
shapiro_results <- daten %>%
  group_by(methode) %>%
  summarise(p_value = shapiro.test(hoehe)$p.value)
print(shapiro_results)
#arboreal 0.0869, klassisch 0.236, MOTI 0.278. Normalverteilt!
# Test auf homogene Varianzen (Levene-Test)
leveneTest(hoehe ~ methode, data = daten)
#Homogenität verletzt

#linear gemischtes modell anstatt anova für hoehe

# Fit eines linearen gemischten Modells
modellh <- lmer(hoehe ~ methode + (1 | baum_id), data = daten)
# Zusammenfassung
summary(modellh)
shapiro.test(residuals(modellh)) #residuen nicht normalverteilt
# Für Post-hoc-Vergleiche:
emm <- emmeans(modellh, pairwise ~ methode)
summary(emm)
#ERGEBNISSE
#Vergleich Unterschied p-wert Signifikanz
#Arboreal vs Klassisch -0.898 0.1926 nicht signifikant(p > 0.05)
#Arboreal vs MOTI -2.380 <0.0001 hochsignifikanz (p < 0.001)
#Klassisch vs MOTI -1.483 0.0123 signifikant (p < 0.05)
#-------------------------------------------------------------------------------
#wir machen das nochmal mit gemittelten Werten:
daten_mittel <- daten %>%
  group_by(plot_id, baum_id, methode) %>%
  summarise(
    hoehe = mean(hoehe),
    .groups = "drop"
  )
shapiro_results2 <- daten_mittel %>%
  group_by(methode) %>%
  summarise(p_value = shapiro.test(hoehe)$p.value)
print(shapiro_results2)
modellh2 <- lmer(hoehe ~ methode + (1 | baum_id), data = daten_mittel)
summary(modellh2) #varianzhomogenität 31.83, res. var. 12.03
emm2 <- emmeans(modellh2, pairwise ~ methode)
summary(emm2)
shapiro.test(residuals(modellh2)) #normalverteilung! p = 0.4337
#ergebnisse
#contrast             estimate    SE df t.ratio p.value
#Arboreal - Klassisch   -0.898 0.896 58  -1.002  0.5783 n. signifikant
#Arboreal - MOTI        -2.380 0.896 58  -2.658  0.0270 signifikant!
#Klassisch - MOTI       -1.483 0.896 58  -1.655  0.2311 n. signifikant
#methode   emmean   SE   df lower.CL upper.CL
#Arboreal    32.2 1.21 42.4     29.7     34.6
#Klassisch   33.1 1.21 42.4     30.6     35.5
#MOTI        34.5 1.21 42.4     32.1     37.0
#-------------------------------------------------------------------------------
#weitere statistik

#Streuung

daten %>%
  group_by(methode) %>%
  summarise(
    mean = mean(hoehe),
    sd = sd(hoehe),
    var = var(hoehe),
    min = min(hoehe),
    max = max(hoehe),
    median = median(hoehe)
  )
#  methode    mean    sd   var   min   max median
#1 Arboreal   32.2  6.00  36.0  18.9  44.1   32.9
#2 Klassisch  33.1  5.80  33.7  21.8  47.9   33.6
#3 MOTI       34.5  8.22  67.5  16.9  56.5   33.4
 

#Streuungsmodell
streuung <- daten %>%
  group_by(baum_id, methode) %>%
  summarise(
    sd_hoehe = sd(hoehe),
    .groups = "drop"
  )
shapiro_results_streu <- streuung %>%
  group_by(methode) %>%
  summarise(p_value = shapiro.test(sd_hoehe)$p.value)
print(shapiro_results)
leveneTest(sd_hoehe ~ methode, data = streuung)
#-------------------------------------------------------------------------------
#modell funktioniert nicht
mod_streu <- lmer(
  sd_hoehe ~ methode + (1|baum_id),
  data = streuung
)
summary(mod_streu)
shapiro.test(residuals(mod_streu))
#-------------------------------------------------------------------------------
#wir machen kruskall

kruskal.test(sd_hoehe ~ methode, data = streuung)
dunn_test(
  streuung,
  sd_hoehe ~ methode,
  p.adjust.method = "bonferroni"
)
#.y.      group1    group2       n1    n2 statistic            p        p.adj p.adj.signif
#1sd_hoehe Arboreal  Klassisch    30    30     -1.47 0.142        0.427        ns          
#2sd_hoehe Arboreal  MOTI         30    30      4.12 0.0000376    0.000113     ***         
#3 sd_hoehe Klassisch MOTI         30    30      5.59 0.0000000228 0.0000000684 ****   
#-------------------------------------------------------------------------------
#Boxplot
plot7 <- ggplot(streuung, aes(x = methode, y = sd_hoehe)) +
  geom_boxplot(fill = "grey80") +
  geom_point(position = position_jitter(width = 0.15), alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  labs(title = "Boxplot der Streuung sortiert nach Methoden", x = "Messmethode", y = "Standartabweichung der Höhenmessung [m]")
plot7
#plot 8 wird nicht verwendet
plot8 <- ggplot( 
  streuung,
  aes(x = factor(baum_id),y = sd_hoehe, color = methode)) +
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
  labs(title = "Streuung als Mittelwerte pro Baum", x = "Baum", y = "Standartabweichung der Höhenmessung [m]")
plot8
#-------------------------------------------------------------------------------
#hier sollten alle Plots zusammengeklebt werden um sie besser zu visualisieren

boxplots <- (plot1 | plot3) /  (plot5 | plot7)
boxplots
sumplots <- (plot2 | plot4) /
  (plot6 | plot8)
boxplots
sumplots
ggsave("Ausgabe/Boxplots.png",
       boxplots,
       width = 20,
       height = 15,
       units = "cm",
       dpi = 300)

ggsave("Ausgabe/Sumplots.png",
       sumplots,
       width = 20,
       height = 15,
       units = "cm",
       dpi = 300)
install.packages("ggplot2")
figure1 <- (plot1 | plot2) / (plot3 | plot4)
ggsave("Ausgabe/alleplots.png",
       figure1,
       width = 20,
       height = 15,
       units = "cm",
       dpi = 300)
figure1
figure2 <- (plot5 | plot6) /
  (plot7 | plot8)