#-------------------------------------------------------------------------------
#22.07.2026
#Magdon Sensorik
#Gruppenarbeit. Auswertung der Aufnahmen
#Benedikt Kalkhof, Jona Schlötelburg, Jonas Weiß
#Dieses Skript behandelt Zeit-Statistik und speichert am Ende die plots ab.
#-------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stats)
library(tidyr)
library(car)
library(lme4)
library(emmeans)
library(rstatix)
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
#Fehlerhafte Zeitwerte rausschmeißen
zeit_bereinigt <- daten %>%
  filter(!( zeit >= 10))
#-------------------------------------------------------------------------------
plot5 <- ggplot(zeit_bereinigt, aes(x = methode, y = zeit)) +
  geom_boxplot(fill = "grey80") +
  geom_point(position = position_jitter(width = 0.15), alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3)+
  labs(title = "Boxplot der Zeitwerte nach Methoden", x = "Messmethode", y = "Zeit [min]")

#folgender plot wird für die Arbeit nicht verwendet
plot6 <- ggplot( 
  zeit_bereinigt,
  aes(x = factor(baum_id),y = zeit, color = methode)) +
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
  labs(title = "Messwerte als Mittelwerte pro Baum", x = "Baum", y = "Zeit [min]")
plot6
#-------------------------------------------------------------------------------
#wir machen hier wieder Mittelwerte draus.
zeit_mittel <- zeit_bereinigt %>%
  group_by(baum_id, methode) %>%
  summarise(
    zeit = mean(zeit),
    .groups = "drop"
  )
nrow(zeit_mittel)
table(zeit_mittel$methode)
#übliche Statistik
zeit_mittel %>%
  group_by(methode) %>%
  summarise(
    mean = mean(zeit),
    sd = sd(zeit),
    var = var(zeit),
    median = median(zeit),
    min = min(zeit),
    max = max(zeit)
  )
#methode    mean    sd median   min   max
#1 Arboreal   2.57 0.898      2     1     5
#2 Klassisch  4.32 1.44       4     2     7
#3 MOTI       1.8  0.961      2     1     5

#jetzt kommt der Weg zum Modell
shapiro.test(zeit_mittel$zeit[zeit_mittel$methode=="Arboreal"]) #W = 0.80047, p-value = 6.686e-05
shapiro.test(zeit_mittel$zeit[zeit_mittel$methode=="MOTI"]) #W = 0.74722, p-value = 8.213e-06
leveneTest( #2  2.8523 0.06345
  zeit ~ methode,
  data = zeit_mittel
)
#-------------------------------------------------------------------------------
#Das Modell funktioniert nicht da die Residuen nicht normalverteilt sind.
mod_zeit <- lmer(
  zeit ~ methode + (1|baum_id),
  data = zeit_mittel
)
summary(mod_zeit)
#Estimate Std. Error t value
#(Intercept)        2.5667     0.2012  12.755
#methodeKlassisch   1.7533     0.2985   5.875
#methodeMOTI       -0.7667     0.2846  -2.694
shapiro.test(residuals(mod_zeit)) #W = 0.94131, p-value = 0.0007539
emmeans(mod_zeit, pairwise ~ methode)
#Arboreal    2.57 0.201 82     2.17     2.97
#Klassisch   4.32 0.221 82     3.88     4.76
#MOTI        1.80 0.201 82     1.40     2.20
#contrast             estimate    SE   df t.ratio p.value
#Arboreal - Klassisch   -1.753 0.299 56.9  -5.861 <0.0001
#Arboreal - MOTI         0.767 0.285 53.5   2.694  0.0251
#Klassisch - MOTI        2.520 0.299 56.9   8.423 <0.0001
#-------------------------------------------------------------------------------
#Kruskal wallis test als nicht parametrisches Modell
kruskal.test(
  zeit ~ methode,
  data = zeit_mittel
)
#Kruskal-Wallis chi-squared = 40.254, df = 2, p-value = 1.816e-09
#nullhypothese verworfen
#signifikant, also dunn test zeit ad hoc
dunn_test(
  zeit_mittel,
  zeit ~ methode,
  p.adjust.method = "bonferroni"
)
#y.group1    group2       n1    n2 statistic        p    p.adj p.adj.signif
#1 zeit  Arboreal  Klassisch    30    25      3.72 1.96e- 4 5.88e- 4 ***         
#2 zeit  Arboreal  MOTI         30    30     -2.74 6.15e- 3 1.84e- 2 *           
#3 zeit  Klassisch MOTI         25    30     -6.34 2.35e-10 7.05e-10 ****  
#klassisch -> arboreal -> MOTI
#-------------------------------------------------------------------------------
#Speichere hier die ganzen plots als Bilder.
ggsave(plot=plot1, filename="Ausgabe/boxplot_hoehe.png", device = "png")
ggsave(plot=plot2, filename="Ausgabe/sumplot_hoehe.png", device = "png")
ggsave(plot=plot3, filename="Ausgabe/boxplot_bhd.png", device = "png")
ggsave(plot=plot4, filename="Ausgabe/sumplot_bhd.png", device = "png")
ggsave(plot=plot5, filename="Ausgabe/boxplot_zeit.png", device = "png")
ggsave(plot=plot6, filename="Ausgabe/sumplot_zeit.png", device = "png")
ggsave(plot=plot7, filename="Ausgabe/boxplot_streuung.png", device = "png")
ggsave(plot=plot8, filename="Ausgabe/sumplot_streuung.png", device = "png")
