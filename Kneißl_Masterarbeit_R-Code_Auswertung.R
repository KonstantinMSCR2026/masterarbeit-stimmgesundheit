##################################################
# Masterarbeit Stimmgesundheit – Datenauswertung
##################################################

# -----------------------------------------------
# 1. Pakete laden
# -----------------------------------------------
library(tidyverse)
library(haven)
library(janitor)
library(psych)
library(readxl)

# Browser für den PNG-Export mit gtsave festlegen
Sys.setenv(
  CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
)

# -----------------------------------------------
# 2. Daten einlesen
# -----------------------------------------------
daten <- read_excel("data_trainerstimmgesundheit_bearbeitet.xlsx")

variablen <- read_delim(
  "variables_trainerstimmgesundheit.csv",
  delim = "\t",
  locale = locale(encoding = "UTF-16LE")
)

werte <- read_delim(
  "values_trainerstimmgesundheit.csv",
  delim = "\t",
  locale = locale(encoding = "UTF-16LE")
)

codebook <- read_excel("codebook_trainerstimmgesundheit.xlsx")

# -----------------------------------------------
# 3. Frage 1: Geschlecht
# -----------------------------------------------

# -----------------------------------------------
# 3.1 Geschlecht identifizieren
# -----------------------------------------------
variablen %>%
  filter(str_detect(LABEL, "Geschlecht"))

werte %>%
  filter(VAR == "P001")

# -----------------------------------------------
# 3.2 Geschlecht deskriptiv auswerten
# -----------------------------------------------
geschlecht_tab <- daten %>%
  filter(!is.na(P001)) %>%
  count(P001) %>%
  mutate(
    Prozent = round(n / sum(n) * 100, 2),
    Geschlecht = recode(as.character(P001),
                        "1" = "Weiblich",
                        "2" = "Männlich",
                        "3" = "Divers",
                        "4" = "Keine Angabe"
    )
  ) %>%
  select(Geschlecht, n, Prozent)

geschlecht_tab

# -----------------------------------------------
# 4. Gesamtzahl der Fälle
# -----------------------------------------------
nrow(daten)

# -----------------------------------------------
# 5. Frage 2: Alter
# -----------------------------------------------

# -----------------------------------------------
# 5.1 Alter prüfen und bereinigen
# -----------------------------------------------
summary(daten$P002_01)
sd(daten$P002_01, na.rm = TRUE)
sum(!is.na(daten$P002_01))

# unplausiblen Wert 99 als fehlend setzen
daten$P002_01[daten$P002_01 == 99] <- NA

# Alter nach Bereinigung erneut prüfen
summary(daten$P002_01)
sd(daten$P002_01, na.rm = TRUE)
sum(!is.na(daten$P002_01))

# -----------------------------------------------
# 5.2 Deskriptive Statistik + Normalverteilung
# -----------------------------------------------

# -----------------------------------------------
# 5.2.1 Deskriptive Statistik
# -----------------------------------------------

# Anzahl gültiger Werte
sum(!is.na(daten$P002_01))

# Mittelwert
mean(daten$P002_01, na.rm = TRUE)

# Median
median(daten$P002_01, na.rm = TRUE)

# Standardabweichung
sd(daten$P002_01, na.rm = TRUE)

# Minimum und Maximum
min(daten$P002_01, na.rm = TRUE)
max(daten$P002_01, na.rm = TRUE)

# Gesamte Übersicht
summary(daten$P002_01)

# -----------------------------------------------
# 5.2.2 Q-Q-Plot
# -----------------------------------------------

qqnorm(daten$P002_01)
qqline(daten$P002_01, col = "red")

# -----------------------------------------------
# 6. Frage 3: Haupt- vs. Nebenberuf 
# -----------------------------------------------

# -----------------------------------------------
# 6.1 Haupt- vs. Nebenberuf identifizieren
# -----------------------------------------------
variablen %>%
  filter(str_detect(LABEL, "beruf"))

werte %>%
  filter(VAR == "P003")

# -----------------------------------------------
# 6.2 Haupt- vs. Nebenberuf deskriptiv auswerten
# -----------------------------------------------
table(daten$P003)
prop.table(table(daten$P003)) * 100
sum(!is.na(daten$P003))

trainer_beruf_tab <- daten %>%
  filter(!is.na(P003)) %>%
  count(P003) %>%
  mutate(
    Prozent = round(n / sum(n) * 100, 2),
    Status = recode(as.character(P003),
                    "1" = "Hauptberuflich",
                    "2" = "Nebenberuflich"
    )
  ) %>%
  select(Status, n, Prozent)

trainer_beruf_tab

# -----------------------------------------------
# 7. Frage 4: Erwachsene vs. Jugend 
# -----------------------------------------------

# -----------------------------------------------
# 7.1 Erwachsene vs. Jugend identifizieren
# -----------------------------------------------
variablen %>%
  filter(str_detect(QUESTION, "train"))

werte %>%
  filter(VAR == "P004")

# -----------------------------------------------
# 7.2 Erwachsene vs. Jugend deskriptiv auswerten
# -----------------------------------------------
table(daten$P004)
prop.table(table(daten$P004)) * 100
sum(!is.na(daten$P004))

trainingsgruppe_tab <- daten %>%
  filter(!is.na(P004)) %>%
  count(P004) %>%
  mutate(
    Prozent = round(n / sum(n) * 100, 2),
    Gruppe = recode(as.character(P004),
                    "1" = "Erwachsenenmannschaft",
                    "2" = "Jugendmannschaft"
    )
  ) %>%
  select(Gruppe, n, Prozent)

trainingsgruppe_tab

variablen %>%
  filter(str_detect(LABEL, "Liga") | str_detect(QUESTION, "Mannschaft der"))

# -----------------------------------------------
# 8. Frage 5: Liga
# -----------------------------------------------

library(dplyr)
library(gt)

# -----------------------------------------------
# 8.1 Übersicht der berücksichtigten Spielklassen
# -----------------------------------------------

tabelle_ligen <- tibble::tribble(
  ~Bereich, ~Spielklasse,
  
  "Deutschland", "1. Bundesliga der Herren",
  "Deutschland", "2. Bundesliga der Herren",
  "Deutschland", "3. Liga der Herren",
  "Deutschland", "Regionalligen der Herren (Nord, Nordost, West, Südwest, Bayern)",
  "Deutschland", "Google Pixel Frauen-Bundesliga",
  "Deutschland", "2. Frauen-Bundesliga",
  "Deutschland", "U19-DFB-Nachwuchsliga",
  "Deutschland", "U17-DFB-Nachwuchsliga",
  "Deutschland", "Nachwuchsleistungszentrum",
  
  "Österreich", "Admiral Bundesliga der Herren",
  "Österreich", "2. Liga der Herren",
  "Österreich", "Admiral Frauen-Bundesliga",
  "Österreich", "Frauen Future League",
  "Österreich", "Ausbildungsakademie",
  
  "Schweiz", "Brack Super League der Herren",
  "Schweiz", "Dieci Challenge League der Herren",
  "Schweiz", "AXA Women’s Super League",
  "Schweiz", "Nationalliga B der Frauen",
  
  "Nationalmannschaft", "DFB-Auswahl (m/w)",
  "Nationalmannschaft", "ÖFB-Auswahl (m/w)",
  "Nationalmannschaft", "SFV-Auswahl (m/w)",
  "Nationalmannschaft", "DFB-Jugendauswahl (m/w)",
  "Nationalmannschaft", "ÖFB-Jugendauswahl (m/w)",
  "Nationalmannschaft", "SFV-Jugendauswahl (m/w)"
)

# -----------------------------------------------
# 8.2 Tabelle gestalten
# -----------------------------------------------

tabelle_ligen_gt <- tabelle_ligen %>%
  gt(
    groupname_col = "Bereich"
  ) %>%
  
  # Zunächst alle automatisch erzeugten Tabellenlinien entfernen
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Keine sichtbare Spaltenüberschrift
  cols_label(
    Spielklasse = ""
  ) %>%
  
  # Inhalte linksbündig ausrichten
  cols_align(
    align = "left",
    columns = Spielklasse
  ) %>%
  
  # Gruppenüberschriften fett formatieren
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_row_groups()
  ) %>%
  
  # Spielklassen leicht einrücken
  tab_style(
    style = cell_text(
      indent = px(6)
    ),
    locations = cells_body(
      columns = Spielklasse
    )
  ) %>%
  
  # Schwarze Linie oberhalb jeder Gruppenüberschrift
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_row_groups()
  ) %>%
  
  # Einheitliches Tabellendesign
  tab_options(
    
    # Schrift
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    # Tabellenbreite
    table.width = px(650),
    
    # Leere Kopfzeile möglichst klein halten
    column_labels.padding = px(0),
    
    # Gruppenüberschriften
    row_group.padding = px(7),
    
    # Kompakte Datenzeilen
    data_row.padding = px(4),
    
    # Schwarze Abschlusslinie ganz unten
    table_body.border.bottom.style = "solid",
    table_body.border.bottom.width = px(1),
    table_body.border.bottom.color = "black",
    
    # Keine weiteren äußeren Rahmenlinien
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none"
  )

tabelle_ligen_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_ligen_gt,
  filename = "Tab_Spielklassen.png",
  zoom = 3,
  expand = 5
)

# -----------------------------------------------
# 8.3 Liga identifizieren
# -----------------------------------------------
variablen %>%
  filter(str_detect(LABEL, "Liga") | str_detect(QUESTION, "Mannschaft der"))

werte %>%
  filter(VAR == "P005")

# -----------------------------------------------
# 8.4 Liga deskriptiv auswerten
# -----------------------------------------------
table(daten$P005)
prop.table(table(daten$P005)) * 100
sum(!is.na(daten$P005))

# Tabelle mit n + Prozent + Klartext-Label
liga_tab <- daten %>%
  filter(!is.na(P005)) %>%
  count(P005) %>%
  mutate(
    Prozent = round(n / sum(n) * 100, 2),
    P005 = as.character(P005)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "P005", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("P005" = "RESPONSE")
  ) %>%
  rename(Liga = MEANING) %>%
  select(Liga, n, Prozent)

liga_tab

# -----------------------------------------------
# 8.5 Grafik für Frage 5: Liga
# -----------------------------------------------
ggplot(
  liga_tab %>% mutate(Liga = reorder(Liga, Prozent)),
  aes(x = Liga, y = Prozent)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Verteilung der trainierten Liga",
    x = "Liga",
    y = "Prozent"
  )

# -----------------------------------------------
# 8.6 Schöne Tabelle für Frage 5: Liga
# -----------------------------------------------

# Tabelle erstellen

liga_tab <- daten %>%
  filter(!is.na(P005)) %>%
  count(P005) %>%
  mutate(
    Prozent = n / sum(n) * 100,
    P005 = as.character(P005)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "P005", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("P005" = "RESPONSE")
  ) %>%
  rename(
    Liga = MEANING
  ) %>%
  select(
    Liga,
    n,
    Prozent
  )

# Tabelle gestalten

liga_tab_gt <- liga_tab %>%
  gt() %>%
  
  # Zunächst alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Liga = "Liga bzw. Leistungsbereich",
    n = md("*n*"),
    Prozent = "%"
  ) %>%
  
  # Prozentwerte mit zwei Nachkommastellen
  
  fmt_number(
    columns = Prozent,
    decimals = 2,
    use_seps = FALSE
  ) %>%
  
  # Häufigkeiten ohne Nachkommastellen
  
  fmt_integer(
    columns = n,
    use_seps = FALSE
  ) %>%
  
  # Textspalte linksbündig
  
  cols_align(
    align = "left",
    columns = Liga
  ) %>%
  
  # Zahlenspalten rechtsbündig
  
  cols_align(
    align = "right",
    columns = c(n, Prozent)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Tabellenzeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = nrow(liga_tab)
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    
    # Schrift
    
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    # Tabellenbreite
    
    table.width = px(500),
    
    # Kopfzeile
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    # Datenzeilen
    
    data_row.padding = px(5),
    
    # Keine zusätzlichen Tabellenrahmen
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    # Keine Linien zwischen einzelnen Datenzeilen
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    # Keine zusätzliche automatische Abschlusslinie
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

liga_tab_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = liga_tab_gt,
  filename = "Tab_Liga_Leistungsbereich.png",
  zoom = 3,
  expand = 5
)

# -----------------------------------------------
# 9. Frage 6: Trainerart
# -----------------------------------------------

# Codierung anschauen
werte %>%
  filter(VAR == "P006")

# Deskriptive Übersicht
table(daten$P006_bereinigt)
prop.table(table(daten$P006_bereinigt)) * 100
sum(!is.na(daten$P006_bereinigt))

# Saubere Tabelle erstellen (inkl. Gesamtzeile)
trainerart_tab <- daten %>%
  filter(!is.na(P006_bereinigt)) %>%
  count(P006_bereinigt) %>%
  mutate(
    Prozent = round(n / sum(n) * 100, 2),
    Trainerart = recode(as.character(P006_bereinigt),
                        "1" = "Cheftrainer:in",
                        "2" = "Co-Trainer:in",
                        "3" = "Torwarttrainer:in",
                        "4" = "Andere"
    )
  ) %>%
  select(Trainerart, n, Prozent) %>%
  bind_rows(
    tibble(
      Trainerart = "Gesamt",
      n = sum(.$n),
      Prozent = 100
    )
  )

# -----------------------------------------------
# 9.1 Schöne Tabelle für Frage 6: Trainer:innenrolle
# -----------------------------------------------

library(dplyr)
library(gt)

# Tabelle erstellen
trainerart_tab <- daten %>%
  filter(!is.na(P006_bereinigt)) %>%
  count(P006_bereinigt) %>%
  mutate(
    Prozent = round(n / sum(n) * 100, 2),
    Trainerrolle = recode(
      as.character(P006_bereinigt),
      "1" = "Cheftrainer:in",
      "2" = "Co-Trainer:in",
      "3" = "Torwarttrainer:in",
      "4" = "Andere"
    )
  ) %>%
  select(Trainerrolle, n, Prozent) %>%
  bind_rows(
    tibble(
      Trainerrolle = "Gesamt",
      n = sum(.$n),
      Prozent = 100
    )
  )

# Schöne Tabelle anzeigen
trainerart_tab %>%
  gt() %>%
  tab_header(
    title = "Trainer:innenrolle der Teilnehmenden"
  ) %>%
  cols_label(
    Trainerrolle = "Trainer:innenrolle",
    n = "n",
    Prozent = "%"
  ) %>%
  fmt_number(
    columns = Prozent,
    decimals = 2
  )

# -----------------------------------------------
# 10. Frage 7: Berufserfahrung allgemein
# -----------------------------------------------

# Deskriptive Statistik
summary(daten$P007_07)

mean(daten$P007_07, na.rm = TRUE)
median(daten$P007_07, na.rm = TRUE)
sd(daten$P007_07, na.rm = TRUE)

min(daten$P007_07, na.rm = TRUE)
max(daten$P007_07, na.rm = TRUE)

sum(!is.na(daten$P007_07))

# Q-Q-Plot
qqnorm(daten$P007_07)
qqline(daten$P007_07, col = "red")

# -----------------------------------------------
# 11. Frage 8: Berufserfahrung als Cheftrainer:in
# -----------------------------------------------

# Deskriptive Statistik
summary(daten$P012_01)

mean(daten$P012_01, na.rm = TRUE)
median(daten$P012_01, na.rm = TRUE)
sd(daten$P012_01, na.rm = TRUE)

min(daten$P012_01, na.rm = TRUE)
max(daten$P012_01, na.rm = TRUE)

sum(!is.na(daten$P012_01))

# Q-Q-Plot
qqnorm(daten$P012_01)
qqline(daten$P012_01, col = "red")

# -----------------------------------------------
# 12. Frage 9: Erfahrung mit Stimmtraining
# -----------------------------------------------

werte %>%
  filter(VAR == "P008")

table(daten$P008)
prop.table(table(daten$P008)) * 100
sum(!is.na(daten$P008))

stimmtraining_tab <- daten %>%
  filter(!is.na(P008)) %>%
  count(P008) %>%
  mutate(
    Prozent = round(n / sum(n) * 100, 2),
    Antwort = recode(as.character(P008),
                     "1" = "Ja",
                     "2" = "Nein"
    )
  ) %>%
  select(Antwort, n, Prozent) %>%
  bind_rows(
    tibble(
      Antwort = "Gesamt",
      n = sum(.$n),
      Prozent = 100
    )
  )

stimmtraining_tab

# -----------------------------------------------
# 13. Frage 10: HNO/ Logopädie (mit Begründung)
# -----------------------------------------------

werte %>%
  filter(VAR == "P009")

table(daten$P009)
prop.table(table(daten$P009)) * 100
sum(!is.na(daten$P009))

frage10_tab <- daten %>%
  filter(!is.na(P009)) %>%
  count(P009) %>%
  mutate(
    Prozent = round(n / sum(n) * 100, 2),
    Antwort = recode(as.character(P009),
                     "1" = "Ja (mit Begründung)",
                     "2" = "Nein"
    )
  ) %>%
  select(Antwort, n, Prozent) %>%
  bind_rows(
    tibble(
      Antwort = "Gesamt",
      n = sum(.$n),
      Prozent = 100
    )
  )

frage10_tab

# -----------------------------------------------
# 14. Gemeinsame Kategorien und Reihenfolgen
# -----------------------------------------------

sprechanteil_levels <- c(
  "0–15 Minuten",
  "16–30 Minuten",
  "31–45 Minuten",
  "46–60 Minuten",
  "61–75 Minuten",
  "76–90 Minuten",
  "91–105 Minuten",
  "106–120 Minuten",
  "Mehr als 120 Minuten"
)

rolle_levels <- c(
  "Cheftrainer:in",
  "Co-Trainer:in",
  "Torwarttrainer:in",
  "Andere"
)

# -----------------------------------------------
# 15. Frage 12: Sprechanteil im Training
# -----------------------------------------------

# -----------------------------------------------
# 15.1 Häufigkeiten in der Gesamtstichprobe
# -----------------------------------------------

library(dplyr)
library(tidyr)
library(gt)

# Antwortkategorien und Reihenfolge festlegen
ba02_labels <- tibble(
  BA02 = c("6", "2", "3", "9", "4", "8", "10", "5", "7"),
  Sprechanteil = sprechanteil_levels
)

# Häufigkeiten berechnen
ba02_counts <- daten %>%
  filter(!is.na(BA02)) %>%
  mutate(
    BA02 = as.character(BA02)
  ) %>%
  count(BA02)

# Tabelle mit Häufigkeiten und Prozentwerten erstellen
ba02_tab <- ba02_labels %>%
  left_join(
    ba02_counts,
    by = "BA02"
  ) %>%
  mutate(
    n = replace_na(n, 0),
    Prozent = n / sum(n) * 100
  ) %>%
  select(
    Sprechanteil,
    n,
    Prozent
  ) %>%
  bind_rows(
    tibble(
      Sprechanteil = "Gesamt",
      n = sum(.$n),
      Prozent = 100
    )
  )

# Tabelle gestalten
ba02_tab_gt <- ba02_tab %>%
  gt() %>%
  
  # Alle automatischen Tabellenlinien entfernen
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  cols_label(
    Sprechanteil = "Sprechanteil",
    n = md("*n*"),
    Prozent = "%"
  ) %>%
  
  # Prozentwerte mit zwei Nachkommastellen
  fmt_number(
    columns = Prozent,
    decimals = 2,
    use_seps = FALSE
  ) %>%
  
  # Häufigkeiten ohne Nachkommastellen
  fmt_integer(
    columns = n,
    use_seps = FALSE
  ) %>%
  
  # Textspalte linksbündig
  cols_align(
    align = "left",
    columns = Sprechanteil
  ) %>%
  
  # Zahlenspalten rechtsbündig
  cols_align(
    align = "right",
    columns = c(n, Prozent)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Gesamtzeile fett formatieren
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_body(
      rows = Sprechanteil == "Gesamt"
    )
  ) %>%
  
  # Schwarze Linie oberhalb der Gesamtzeile
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Sprechanteil == "Gesamt"
    )
  ) %>%
  
  # Schwarze Abschlusslinie unter der Gesamtzeile
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Sprechanteil == "Gesamt"
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  tab_options(
    
    # Schrift
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    # Tabellenbreite
    table.width = px(420),
    
    # Kopfzeile
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    # Datenzeilen
    data_row.padding = px(5),
    
    # Keine automatischen Rahmenlinien
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    # Keine Linien zwischen einzelnen Antwortkategorien
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    # Keine zusätzliche automatische Abschlusslinie
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen
ba02_tab_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = ba02_tab_gt,
  filename = "Tab_Sprechanteil_Training_Gesamtstichprobe.png",
  zoom = 3,
  expand = 5
)

# -----------------------------------------------
# 15.2 Daten vorbereiten:
# Sprechanteil im Training nach Trainerrolle
# -----------------------------------------------

sprechanteil_training_testdaten <- daten %>%
  filter(!is.na(P006_bereinigt), !is.na(BA02)) %>%
  mutate(
    P006_bereinigt = as.character(P006_bereinigt),
    BA02 = as.character(BA02),
    
    Trainerrolle = recode(
      P006_bereinigt,
      "1" = "Cheftrainer:in",
      "2" = "Co-Trainer:in",
      "3" = "Torwarttrainer:in",
      "4" = "Andere"
    ),
    
    Trainerrolle = factor(
      Trainerrolle,
      levels = rolle_levels
    ),
    
    Sprechanteil = case_when(
      BA02 == "6"  ~ "0–15 Minuten",
      BA02 == "2"  ~ "16–30 Minuten",
      BA02 == "3"  ~ "31–45 Minuten",
      BA02 == "9"  ~ "46–60 Minuten",
      BA02 == "4"  ~ "61–75 Minuten",
      BA02 == "8"  ~ "76–90 Minuten",
      BA02 == "10" ~ "91–105 Minuten",
      BA02 == "5"  ~ "106–120 Minuten",
      BA02 == "7"  ~ "Mehr als 120 Minuten"
    ),
    
    Sprechanteil = factor(
      Sprechanteil,
      levels = sprechanteil_levels
    ),
    
    BA02_rank = case_when(
      BA02 == "6"  ~ 1,
      BA02 == "2"  ~ 2,
      BA02 == "3"  ~ 3,
      BA02 == "9"  ~ 4,
      BA02 == "4"  ~ 5,
      BA02 == "8"  ~ 6,
      BA02 == "10" ~ 7,
      BA02 == "5"  ~ 8,
      BA02 == "7"  ~ 9
    )
  )


# -----------------------------------------------
# 15.3 Kruskal-Wallis-Test:
# Sprechanteil im Training nach Trainerrolle
# -----------------------------------------------

kruskal_training_rolle <- kruskal.test(
  BA02_rank ~ Trainerrolle,
  data = sprechanteil_training_testdaten
)

kruskal_training_rolle


# -----------------------------------------------
# 15.4 Post-hoc-Tests nach Kruskal-Wallis:
# paarweise Wilcoxon-Rangsummentests
# mit Holm-Korrektur
# -----------------------------------------------

pairwise_training_rolle <- pairwise.wilcox.test(
  sprechanteil_training_testdaten$BA02_rank,
  sprechanteil_training_testdaten$Trainerrolle,
  p.adjust.method = "holm",
  exact = FALSE
)

pairwise_training_rolle

# -----------------------------------------------
# 15.5 Hilfsfunktion für alle Tabellen 
# für Holm-korrigierte Post-hoc-Vergleiche
# -----------------------------------------------

erstelle_posthoc_tabelle <- function(posthoc_objekt) {
  
  # p-Werte formatieren
  
  formatiere_p <- function(p) {
    if (p < .001) {
      return("< .001")
    } else {
      return(sub("^0", "", sprintf("%.3f", p)))
    }
  }
  
  # Post-hoc-Matrix auslesen
  
  posthoc_matrix <- posthoc_objekt$p.value
  
  posthoc_tabelle <- data.frame(
    Gruppe_1 = character(),
    Gruppe_2 = character(),
    p = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Nur vorhandene Werte aus der unteren Dreiecksmatrix übernehmen
  
  for (zeile in rownames(posthoc_matrix)) {
    for (spalte in colnames(posthoc_matrix)) {
      
      p_wert <- posthoc_matrix[zeile, spalte]
      
      if (!is.na(p_wert)) {
        posthoc_tabelle <- rbind(
          posthoc_tabelle,
          data.frame(
            Gruppe_1 = spalte,
            Gruppe_2 = zeile,
            p = p_wert,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
  
  # Tabelle formatieren
  
  posthoc_tabelle <- posthoc_tabelle %>%
    mutate(
      Vergleich = paste(
        Gruppe_1,
        Gruppe_2,
        sep = " – "
      ),
      p = sapply(
        p,
        formatiere_p
      )
    ) %>%
    select(
      Vergleich,
      p
    )
  
  return(posthoc_tabelle)
}

# ---------------------------------------------------------------
# 15.6 Tabellen Holm-korrigierte Post-hoc-Vergleiche
# Sprechanteil im Training
# ---------------------------------------------------------------

tabelle_sprechanteil_training <- erstelle_posthoc_tabelle(
  posthoc_objekt = pairwise_training_rolle
)

tabelle_sprechanteil_training_gt <- tabelle_sprechanteil_training %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Vergleich = "Paarweiser Vergleich",
    p = md("*p*")
  ) %>%
  
  # Vergleich linksbündig
  
  cols_align(
    align = "left",
    columns = Vergleich
  ) %>%
  
  # p-Werte zentrieren
  
  cols_align(
    align = "center",
    columns = p
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = nrow(tabelle_sprechanteil_training)
    )
  ) %>%
  
  # Anmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Die *p*-Werte sind nach Holm korrigiert."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(400),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

tabelle_sprechanteil_training_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_sprechanteil_training_gt,
  filename = "Tab_Posthoc_Sprechanteil_Training.png",
  zoom = 3,
  expand = 5
)

# -----------------------------------------------
# 15.7 Tabellenbasis:
# Sprechanteil im Training nach Trainer:innenrolle
# -----------------------------------------------

sprechanteil_training_tabelle <- sprechanteil_training_testdaten %>%
  count(
    Sprechanteil,
    Trainerrolle,
    .drop = FALSE
  ) %>%
  group_by(Trainerrolle) %>%
  mutate(
    Prozent = n / sum(n) * 100
  ) %>%
  ungroup()

# Gruppengrößen bestimmen
gruppen_n_training <- sprechanteil_training_testdaten %>%
  count(Trainerrolle)

n_chef_training <- gruppen_n_training$n[
  gruppen_n_training$Trainerrolle == "Cheftrainer:in"
]

n_co_training <- gruppen_n_training$n[
  gruppen_n_training$Trainerrolle == "Co-Trainer:in"
]

n_torwart_training <- gruppen_n_training$n[
  gruppen_n_training$Trainerrolle == "Torwarttrainer:in"
]

n_andere_training <- gruppen_n_training$n[
  gruppen_n_training$Trainerrolle == "Andere"
]

# -----------------------------------------------
# 15.8 Tabelle:
# Sprechanteil im Training nach Trainer:innenrolle
# -----------------------------------------------

tabelle_sprechanteil_training_prozent <-
  sprechanteil_training_tabelle %>%
  select(
    Sprechanteil,
    Trainerrolle,
    Prozent
  ) %>%
  pivot_wider(
    names_from = Trainerrolle,
    values_from = Prozent,
    values_fill = 0
  ) %>%
  select(
    Sprechanteil,
    `Cheftrainer:in`,
    `Co-Trainer:in`,
    `Torwarttrainer:in`,
    Andere
  ) %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Sprechanteil = "Sprechanteil",
    
    `Cheftrainer:in` = md(
      paste0(
        "Cheftrainer:innen<br>(*n* = ",
        n_chef_training,
        ")"
      )
    ),
    
    `Co-Trainer:in` = md(
      paste0(
        "Co-Trainer:innen<br>(*n* = ",
        n_co_training,
        ")"
      )
    ),
    
    `Torwarttrainer:in` = md(
      paste0(
        "Torwarttrainer:innen<br>(*n* = ",
        n_torwart_training,
        ")"
      )
    ),
    
    Andere = md(
      paste0(
        "Andere<br>(*n* = ",
        n_andere_training,
        ")"
      )
    )
  ) %>%
  
  # Feste, kompakte Spaltenbreiten
  
  cols_width(
    Sprechanteil ~ px(140),
    `Cheftrainer:in` ~ px(110),
    `Co-Trainer:in` ~ px(105),
    `Torwarttrainer:in` ~ px(125),
    Andere ~ px(80)
  ) %>%
  
  # Prozentwerte mit zwei Nachkommastellen
  
  fmt_number(
    columns = c(
      `Cheftrainer:in`,
      `Co-Trainer:in`,
      `Torwarttrainer:in`,
      Andere
    ),
    decimals = 2,
    use_seps = FALSE,
    pattern = "{x} %"
  ) %>%
  
  # Sprechanteil linksbündig
  
  cols_align(
    align = "left",
    columns = Sprechanteil
  ) %>%
  
  # Prozentwerte mittig unter den Überschriften
  
  cols_align(
    align = "center",
    columns = c(
      `Cheftrainer:in`,
      `Co-Trainer:in`,
      `Torwarttrainer:in`,
      Andere
    )
  ) %>%
  
  # Rollenüberschriften mittig
  
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_column_labels(
      columns = c(
        `Cheftrainer:in`,
        `Co-Trainer:in`,
        `Torwarttrainer:in`,
        Andere
      )
    )
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Kategorie
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Sprechanteil == "Mehr als 120 Minuten"
    )
  ) %>%
  
  # Tabellenanmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Angaben in Prozent innerhalb der jeweiligen Trainer:innenrolle."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    
    # Schrift
    
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    # Kompakte Tabellenbreite
    
    table.width = px(560),
    
    # Kopfzeile
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    # Datenzeilen
    
    data_row.padding = px(5),
    
    # Tabellenanmerkung
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    # Keine automatischen Rahmenlinien
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    # Keine Linien zwischen den Antwortkategorien
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    # Keine zusätzliche automatische Abschlusslinie
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

tabelle_sprechanteil_training_prozent

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_sprechanteil_training_prozent,
  filename = "Tab_Sprechanteil_Training_Trainerrolle.png",
  zoom = 3,
  expand = 5
)

# -----------------------------------------------
# 16. Frage 13: Sprechanteil im Spiel
# -----------------------------------------------

# -----------------------------------------------
# 16.1 Häufigkeiten in der Gesamtstichprobe
# -----------------------------------------------

# Antwortkategorien und Reihenfolge festlegen

ba05_labels <- tibble(
  BA05 = c("7", "2", "3", "10", "4", "8", "9", "5", "6"),
  Sprechanteil = sprechanteil_levels
)

# Häufigkeiten berechnen

ba05_counts <- daten %>%
  filter(!is.na(BA05)) %>%
  mutate(
    BA05 = as.character(BA05)
  ) %>%
  count(BA05)

# Tabelle mit Häufigkeiten und Prozentwerten erstellen

ba05_tab <- ba05_labels %>%
  left_join(
    ba05_counts,
    by = "BA05"
  ) %>%
  mutate(
    n = replace_na(n, 0),
    Prozent = n / sum(n) * 100
  ) %>%
  select(
    Sprechanteil,
    n,
    Prozent
  ) %>%
  bind_rows(
    tibble(
      Sprechanteil = "Gesamt",
      n = sum(.$n),
      Prozent = 100
    )
  )

# Tabelle gestalten

ba05_tab_gt <- ba05_tab %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Sprechanteil = "Sprechanteil",
    n = md("*n*"),
    Prozent = "%"
  ) %>%
  
  # Prozentwerte mit zwei Nachkommastellen
  
  fmt_number(
    columns = Prozent,
    decimals = 2,
    use_seps = FALSE
  ) %>%
  
  # Häufigkeiten ohne Nachkommastellen
  
  fmt_integer(
    columns = n,
    use_seps = FALSE
  ) %>%
  
  # Text linksbündig
  
  cols_align(
    align = "left",
    columns = Sprechanteil
  ) %>%
  
  # Zahlen rechtsbündig
  
  cols_align(
    align = "right",
    columns = c(n, Prozent)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Gesamtzeile fett
  
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_body(
      rows = Sprechanteil == "Gesamt"
    )
  ) %>%
  
  # Linie oberhalb der Gesamtzeile
  
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Sprechanteil == "Gesamt"
    )
  ) %>%
  
  # Abschlusslinie unter der Gesamtzeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Sprechanteil == "Gesamt"
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(420),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

ba05_tab_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = ba05_tab_gt,
  filename = "Tab_Sprechanteil_Spiel_Gesamtstichprobe.png",
  zoom = 3,
  expand = 5
)

# -----------------------------------------------
# 16.2 Daten vorbereiten:
# Sprechanteil im Spiel nach Trainerrolle
# -----------------------------------------------

sprechanteil_spiel_testdaten <- daten %>%
  filter(!is.na(P006_bereinigt), !is.na(BA05)) %>%
  mutate(
    P006_bereinigt = as.character(P006_bereinigt),
    BA05 = as.character(BA05),
    
    Trainerrolle = recode(
      P006_bereinigt,
      "1" = "Cheftrainer:in",
      "2" = "Co-Trainer:in",
      "3" = "Torwarttrainer:in",
      "4" = "Andere"
    ),
    
    Trainerrolle = factor(
      Trainerrolle,
      levels = rolle_levels
    ),
    
    Sprechanteil = case_when(
      BA05 == "7"  ~ "0–15 Minuten",
      BA05 == "2"  ~ "16–30 Minuten",
      BA05 == "3"  ~ "31–45 Minuten",
      BA05 == "10" ~ "46–60 Minuten",
      BA05 == "4"  ~ "61–75 Minuten",
      BA05 == "8"  ~ "76–90 Minuten",
      BA05 == "9"  ~ "91–105 Minuten",
      BA05 == "5"  ~ "106–120 Minuten",
      BA05 == "6"  ~ "Mehr als 120 Minuten"
    ),
    
    Sprechanteil = factor(
      Sprechanteil,
      levels = sprechanteil_levels
    ),
    
    BA05_rank = case_when(
      BA05 == "7"  ~ 1,
      BA05 == "2"  ~ 2,
      BA05 == "3"  ~ 3,
      BA05 == "10" ~ 4,
      BA05 == "4"  ~ 5,
      BA05 == "8"  ~ 6,
      BA05 == "9"  ~ 7,
      BA05 == "5"  ~ 8,
      BA05 == "6"  ~ 9
    )
  )


# -----------------------------------------------
# 16.3 Kruskal-Wallis-Test:
# Sprechanteil im Spiel nach Trainerrolle
# -----------------------------------------------

kruskal_spiel_rolle <- kruskal.test(
  BA05_rank ~ Trainerrolle,
  data = sprechanteil_spiel_testdaten
)

kruskal_spiel_rolle


# -----------------------------------------------
# 16.4 Post-hoc-Tests nach Kruskal-Wallis:
# paarweise Wilcoxon-Rangsummentests
# mit Holm-Korrektur
# -----------------------------------------------

pairwise_spiel_rolle <- pairwise.wilcox.test(
  sprechanteil_spiel_testdaten$BA05_rank,
  sprechanteil_spiel_testdaten$Trainerrolle,
  p.adjust.method = "holm",
  exact = FALSE
)

pairwise_spiel_rolle

# -----------------------------------------------------------
# 16.5 Tabelle Holm-korrigierte Post-hoc-Vergleiche
# Sprechanteil im Spiel
# -----------------------------------------------------------

tabelle_sprechanteil_spiel <- erstelle_posthoc_tabelle(
  posthoc_objekt = pairwise_spiel_rolle
)

tabelle_sprechanteil_spiel_gt <- tabelle_sprechanteil_spiel %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Vergleich = "Paarweiser Vergleich",
    p = md("*p*")
  ) %>%
  
  # Vergleich linksbündig
  
  cols_align(
    align = "left",
    columns = Vergleich
  ) %>%
  
  # p-Werte zentrieren
  
  cols_align(
    align = "center",
    columns = p
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = nrow(tabelle_sprechanteil_spiel)
    )
  ) %>%
  
  # Anmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Die *p*-Werte sind nach Holm korrigiert."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(400),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

tabelle_sprechanteil_spiel_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_sprechanteil_spiel_gt,
  filename = "Tab_Posthoc_Sprechanteil_Spiel.png",
  zoom = 3,
  expand = 5
)

# -----------------------------------------------
# 16.6 Tabellenbasis:
# Sprechanteil im Spiel nach Trainerrolle
# -----------------------------------------------

sprechanteil_spiel_tabelle <- sprechanteil_spiel_testdaten %>%
  count(Sprechanteil, Trainerrolle, .drop = FALSE) %>%
  group_by(Trainerrolle) %>%
  mutate(
    Prozent = round(n / sum(n) * 100, 1),
    Anzeige_n_prozent = paste0(n, " (", Prozent, " %)"),
    Anzeige_prozent = paste0(Prozent, " %")
  ) %>%
  ungroup()

gruppen_n_spiel <- sprechanteil_spiel_testdaten %>%
  count(Trainerrolle)

n_chef_spiel <- gruppen_n_spiel$n[
  gruppen_n_spiel$Trainerrolle == "Cheftrainer:in"
]

n_co_spiel <- gruppen_n_spiel$n[
  gruppen_n_spiel$Trainerrolle == "Co-Trainer:in"
]

n_torwart_spiel <- gruppen_n_spiel$n[
  gruppen_n_spiel$Trainerrolle == "Torwarttrainer:in"
]

n_andere_spiel <- gruppen_n_spiel$n[
  gruppen_n_spiel$Trainerrolle == "Andere"
]

# -----------------------------------------------
# 16.7 Tabelle:
# Sprechanteil im Spiel nach Trainer:innenrolle
# -----------------------------------------------

tabelle_sprechanteil_spiel_prozent <-
  sprechanteil_spiel_tabelle %>%
  select(
    Sprechanteil,
    Trainerrolle,
    Prozent
  ) %>%
  pivot_wider(
    names_from = Trainerrolle,
    values_from = Prozent,
    values_fill = 0
  ) %>%
  select(
    Sprechanteil,
    `Cheftrainer:in`,
    `Co-Trainer:in`,
    `Torwarttrainer:in`,
    Andere
  ) %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Sprechanteil = "Sprechanteil",
    
    `Cheftrainer:in` = md(
      paste0(
        "Cheftrainer:innen<br>(*n* = ",
        n_chef_spiel,
        ")"
      )
    ),
    
    `Co-Trainer:in` = md(
      paste0(
        "Co-Trainer:innen<br>(*n* = ",
        n_co_spiel,
        ")"
      )
    ),
    
    `Torwarttrainer:in` = md(
      paste0(
        "Torwarttrainer:innen<br>(*n* = ",
        n_torwart_spiel,
        ")"
      )
    ),
    
    Andere = md(
      paste0(
        "Andere<br>(*n* = ",
        n_andere_spiel,
        ")"
      )
    )
  ) %>%
  
  # Feste, kompakte Spaltenbreiten
  
  cols_width(
    Sprechanteil ~ px(140),
    `Cheftrainer:in` ~ px(110),
    `Co-Trainer:in` ~ px(105),
    `Torwarttrainer:in` ~ px(125),
    Andere ~ px(80)
  ) %>%
  
  # Prozentwerte mit zwei Nachkommastellen
  
  fmt_number(
    columns = c(
      `Cheftrainer:in`,
      `Co-Trainer:in`,
      `Torwarttrainer:in`,
      Andere
    ),
    decimals = 2,
    use_seps = FALSE,
    pattern = "{x} %"
  ) %>%
  
  # Text linksbündig
  
  cols_align(
    align = "left",
    columns = Sprechanteil
  ) %>%
  
  # Prozentwerte zentrieren
  
  cols_align(
    align = "center",
    columns = c(
      `Cheftrainer:in`,
      `Co-Trainer:in`,
      `Torwarttrainer:in`,
      Andere
    )
  ) %>%
  
  # Überschriften zentrieren
  
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_column_labels(
      columns = c(
        `Cheftrainer:in`,
        `Co-Trainer:in`,
        `Torwarttrainer:in`,
        Andere
      )
    )
  ) %>%
  
  # Schwarze Linien ober- und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Sprechanteil == "Mehr als 120 Minuten"
    )
  ) %>%
  
  # Tabellenanmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Angaben in Prozent innerhalb der jeweiligen Trainer:innenrolle."
    )
  ) %>%
  
  # Einheitliches Design
  
  tab_options(
    
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(560),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

tabelle_sprechanteil_spiel_prozent

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_sprechanteil_spiel_prozent,
  filename = "Tab_Sprechanteil_Spiel_Trainerrolle.png",
  zoom = 3,
  expand = 5
)


# -----------------------------------------------
# Gemeinsame Kategorien für die Lautstärke
# -----------------------------------------------

lautstaerke_levels <- c(
  "Nie",
  "Selten",
  "Gelegentlich",
  "Oft",
  "Immer"
)

# -----------------------------------------------
# 17. Frage 14: Lautstärke im Training
# -----------------------------------------------

# -----------------------------------------------
# 17.1 Häufigkeiten in der Gesamtstichprobe
# -----------------------------------------------

# Antwortkategorien und Reihenfolge festlegen

ba04_labels <- tibble(
  BA04 = c("1", "2", "3", "4", "5"),
  Lautstaerke = lautstaerke_levels
)

# Häufigkeiten berechnen

ba04_counts <- daten %>%
  filter(!is.na(BA04)) %>%
  mutate(
    BA04 = as.character(BA04)
  ) %>%
  count(BA04)

# Tabelle mit Häufigkeiten und Prozentwerten erstellen

ba04_tab <- ba04_labels %>%
  left_join(
    ba04_counts,
    by = "BA04"
  ) %>%
  mutate(
    n = replace_na(n, 0),
    Prozent = n / sum(n) * 100
  ) %>%
  select(
    Lautstaerke,
    n,
    Prozent
  ) %>%
  bind_rows(
    tibble(
      Lautstaerke = "Gesamt",
      n = sum(.$n),
      Prozent = 100
    )
  )

# Tabelle gestalten

ba04_tab_gt <- ba04_tab %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Lautstaerke = "Häufigkeit",
    n = md("*n*"),
    Prozent = "%"
  ) %>%
  
  # Prozentwerte mit zwei Nachkommastellen
  
  fmt_number(
    columns = Prozent,
    decimals = 2,
    use_seps = FALSE
  ) %>%
  
  # Häufigkeiten ohne Nachkommastellen
  
  fmt_integer(
    columns = n,
    use_seps = FALSE
  ) %>%
  
  # Textspalte linksbündig
  
  cols_align(
    align = "left",
    columns = Lautstaerke
  ) %>%
  
  # Zahlenspalten rechtsbündig
  
  cols_align(
    align = "right",
    columns = c(n, Prozent)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Gesamtzeile fett
  
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_body(
      rows = Lautstaerke == "Gesamt"
    )
  ) %>%
  
  # Schwarze Linie oberhalb der Gesamtzeile
  
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Lautstaerke == "Gesamt"
    )
  ) %>%
  
  # Schwarze Abschlusslinie unter der Gesamtzeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Lautstaerke == "Gesamt"
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(420),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

ba04_tab_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = ba04_tab_gt,
  filename = "Tab_Lautstaerke_Training_Gesamtstichprobe.png",
  zoom = 3,
  expand = 5
)

# -----------------------------------------------
# 17.2 Daten vorbereiten:
# Lautstärke im Training nach Trainerrolle
# -----------------------------------------------

lautstaerke_training_testdaten <- daten %>%
  filter(!is.na(P006_bereinigt), !is.na(BA04)) %>%
  mutate(
    P006_bereinigt = as.character(P006_bereinigt),
    BA04 = as.character(BA04),
    
    Trainerrolle = recode(
      P006_bereinigt,
      "1" = "Cheftrainer:in",
      "2" = "Co-Trainer:in",
      "3" = "Torwarttrainer:in",
      "4" = "Andere"
    ),
    
    Trainerrolle = factor(
      Trainerrolle,
      levels = rolle_levels
    ),
    
    Lautstaerke = case_when(
      BA04 == "1" ~ "Nie",
      BA04 == "2" ~ "Selten",
      BA04 == "3" ~ "Gelegentlich",
      BA04 == "4" ~ "Oft",
      BA04 == "5" ~ "Immer"
    ),
    
    Lautstaerke = factor(
      Lautstaerke,
      levels = lautstaerke_levels
    ),
    
    BA04_rank = case_when(
      BA04 == "1" ~ 1,
      BA04 == "2" ~ 2,
      BA04 == "3" ~ 3,
      BA04 == "4" ~ 4,
      BA04 == "5" ~ 5
    )
  )


# -----------------------------------------------
# 17.3 Kruskal-Wallis-Test:
# Lautstärke im Training nach Trainerrolle
# -----------------------------------------------

kruskal_laut_training <- kruskal.test(
  BA04_rank ~ Trainerrolle,
  data = lautstaerke_training_testdaten
)

kruskal_laut_training


# -----------------------------------------------
# 17.4 Post-hoc-Tests nach Kruskal-Wallis:
# paarweise Wilcoxon-Rangsummentests
# mit Holm-Korrektur
# -----------------------------------------------

pairwise_laut_training <- pairwise.wilcox.test(
  lautstaerke_training_testdaten$BA04_rank,
  lautstaerke_training_testdaten$Trainerrolle,
  p.adjust.method = "holm",
  exact = FALSE
)

pairwise_laut_training

# -----------------------------------------------------------
# 17.5 Tabellen Holm-korrigierte Post-hoc-Vergleiche
# Erhöhte Lautstärke im Training
# -----------------------------------------------------------

tabelle_lautstaerke_training <- erstelle_posthoc_tabelle(
  posthoc_objekt = pairwise_laut_training
)

tabelle_lautstaerke_training_gt <- tabelle_lautstaerke_training %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Vergleich = "Paarweiser Vergleich",
    p = md("*p*")
  ) %>%
  
  # Vergleich linksbündig
  
  cols_align(
    align = "left",
    columns = Vergleich
  ) %>%
  
  # p-Werte zentrieren
  
  cols_align(
    align = "center",
    columns = p
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = nrow(tabelle_lautstaerke_training)
    )
  ) %>%
  
  # Anmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Die *p*-Werte sind nach Holm korrigiert."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(400),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

tabelle_lautstaerke_training_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_lautstaerke_training_gt,
  filename = "Tab_Posthoc_Lautstaerke_Training.png",
  zoom = 3,
  expand = 5
)

# -----------------------------------------------
# 17.6 Tabellenbasis:
# Lautstärke im Training nach Trainerrolle
# -----------------------------------------------

lautstaerke_training_tabelle <- lautstaerke_training_testdaten %>%
  count(Lautstaerke, Trainerrolle, .drop = FALSE) %>%
  group_by(Trainerrolle) %>%
  mutate(
    Prozent = round(n / sum(n) * 100, 1),
    Anzeige_n_prozent = paste0(n, " (", Prozent, " %)"),
    Anzeige_prozent = paste0(Prozent, " %")
  ) %>%
  ungroup()

gruppen_n_laut_training <- lautstaerke_training_testdaten %>%
  count(Trainerrolle)

n_chef_laut_training <- gruppen_n_laut_training$n[
  gruppen_n_laut_training$Trainerrolle == "Cheftrainer:in"
]

n_co_laut_training <- gruppen_n_laut_training$n[
  gruppen_n_laut_training$Trainerrolle == "Co-Trainer:in"
]

n_torwart_laut_training <- gruppen_n_laut_training$n[
  gruppen_n_laut_training$Trainerrolle == "Torwarttrainer:in"
]

n_andere_laut_training <- gruppen_n_laut_training$n[
  gruppen_n_laut_training$Trainerrolle == "Andere"
]

# -----------------------------------------------
# 17.7 Tabelle:
# Lautstärke im Training nach Trainer:innenrolle
# -----------------------------------------------

tabelle_laut_training_prozent <-
  lautstaerke_training_tabelle %>%
  select(
    Lautstaerke,
    Trainerrolle,
    Prozent
  ) %>%
  pivot_wider(
    names_from = Trainerrolle,
    values_from = Prozent,
    values_fill = 0
  ) %>%
  select(
    Lautstaerke,
    `Cheftrainer:in`,
    `Co-Trainer:in`,
    `Torwarttrainer:in`,
    Andere
  ) %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Lautstaerke = "Häufigkeit",
    
    `Cheftrainer:in` = md(
      paste0(
        "Cheftrainer:innen<br>(*n* = ",
        n_chef_laut_training,
        ")"
      )
    ),
    
    `Co-Trainer:in` = md(
      paste0(
        "Co-Trainer:innen<br>(*n* = ",
        n_co_laut_training,
        ")"
      )
    ),
    
    `Torwarttrainer:in` = md(
      paste0(
        "Torwarttrainer:innen<br>(*n* = ",
        n_torwart_laut_training,
        ")"
      )
    ),
    
    Andere = md(
      paste0(
        "Andere<br>(*n* = ",
        n_andere_laut_training,
        ")"
      )
    )
  ) %>%
  
  # Feste, kompakte Spaltenbreiten
  
  cols_width(
    Lautstaerke ~ px(140),
    `Cheftrainer:in` ~ px(110),
    `Co-Trainer:in` ~ px(105),
    `Torwarttrainer:in` ~ px(125),
    Andere ~ px(80)
  ) %>%
  
  # Prozentwerte mit zwei Nachkommastellen
  
  fmt_number(
    columns = c(
      `Cheftrainer:in`,
      `Co-Trainer:in`,
      `Torwarttrainer:in`,
      Andere
    ),
    decimals = 2,
    use_seps = FALSE,
    pattern = "{x} %"
  ) %>%
  
  # Text linksbündig
  
  cols_align(
    align = "left",
    columns = Lautstaerke
  ) %>%
  
  # Prozentwerte zentrieren
  
  cols_align(
    align = "center",
    columns = c(
      `Cheftrainer:in`,
      `Co-Trainer:in`,
      `Torwarttrainer:in`,
      Andere
    )
  ) %>%
  
  # Überschriften der Trainer:innenrollen zentrieren
  
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_column_labels(
      columns = c(
        `Cheftrainer:in`,
        `Co-Trainer:in`,
        `Torwarttrainer:in`,
        Andere
      )
    )
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Kategorie
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Lautstaerke == "Immer"
    )
  ) %>%
  
  # Tabellenanmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Angaben in Prozent innerhalb der jeweiligen Trainer:innenrolle."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(560),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

tabelle_laut_training_prozent

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_laut_training_prozent,
  filename = "Tab_Lautstaerke_Training_Trainerrolle.png",
  zoom = 3,
  expand = 5
)

# -----------------------------------------------
# 18. Frage 15: Lautstärke im Spiel
# -----------------------------------------------

# -----------------------------------------------
# 18.1 Häufigkeiten in der Gesamtstichprobe
# -----------------------------------------------

# Antwortkategorien und Reihenfolge festlegen

ba06_labels <- tibble(
  BA06 = c("1", "2", "3", "4", "5"),
  Lautstaerke = lautstaerke_levels
)

# Häufigkeiten berechnen

ba06_counts <- daten %>%
  filter(!is.na(BA06)) %>%
  mutate(
    BA06 = as.character(BA06)
  ) %>%
  count(BA06)

# Tabelle mit Häufigkeiten und Prozentwerten erstellen

ba06_tab <- ba06_labels %>%
  left_join(
    ba06_counts,
    by = "BA06"
  ) %>%
  mutate(
    n = replace_na(n, 0),
    Prozent = n / sum(n) * 100
  ) %>%
  select(
    Lautstaerke,
    n,
    Prozent
  ) %>%
  bind_rows(
    tibble(
      Lautstaerke = "Gesamt",
      n = sum(.$n),
      Prozent = 100
    )
  )

# Tabelle gestalten

ba06_tab_gt <- ba06_tab %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Lautstaerke = "Häufigkeit",
    n = md("*n*"),
    Prozent = "%"
  ) %>%
  
  # Prozentwerte mit zwei Nachkommastellen
  
  fmt_number(
    columns = Prozent,
    decimals = 2,
    use_seps = FALSE
  ) %>%
  
  # Häufigkeiten ohne Nachkommastellen
  
  fmt_integer(
    columns = n,
    use_seps = FALSE
  ) %>%
  
  # Textspalte linksbündig
  
  cols_align(
    align = "left",
    columns = Lautstaerke
  ) %>%
  
  # Zahlenspalten rechtsbündig
  
  cols_align(
    align = "right",
    columns = c(n, Prozent)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Gesamtzeile fett
  
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_body(
      rows = Lautstaerke == "Gesamt"
    )
  ) %>%
  
  # Schwarze Linie oberhalb der Gesamtzeile
  
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Lautstaerke == "Gesamt"
    )
  ) %>%
  
  # Schwarze Abschlusslinie unter der Gesamtzeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Lautstaerke == "Gesamt"
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(420),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

ba06_tab_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = ba06_tab_gt,
  filename = "Tab_Lautstaerke_Spiel_Gesamtstichprobe.png",
  zoom = 3,
  expand = 5
)

# -----------------------------------------------
# 18.2 Daten vorbereiten:
# Lautstärke im Spiel nach Trainerrolle
# -----------------------------------------------

lautstaerke_spiel_testdaten <- daten %>%
  filter(!is.na(P006_bereinigt), !is.na(BA06)) %>%
  mutate(
    P006_bereinigt = as.character(P006_bereinigt),
    BA06 = as.character(BA06),
    
    Trainerrolle = recode(
      P006_bereinigt,
      "1" = "Cheftrainer:in",
      "2" = "Co-Trainer:in",
      "3" = "Torwarttrainer:in",
      "4" = "Andere"
    ),
    
    Trainerrolle = factor(
      Trainerrolle,
      levels = rolle_levels
    ),
    
    Lautstaerke = case_when(
      BA06 == "1" ~ "Nie",
      BA06 == "2" ~ "Selten",
      BA06 == "3" ~ "Gelegentlich",
      BA06 == "4" ~ "Oft",
      BA06 == "5" ~ "Immer"
    ),
    
    Lautstaerke = factor(
      Lautstaerke,
      levels = lautstaerke_levels
    ),
    
    BA06_rank = case_when(
      BA06 == "1" ~ 1,
      BA06 == "2" ~ 2,
      BA06 == "3" ~ 3,
      BA06 == "4" ~ 4,
      BA06 == "5" ~ 5
    )
  )


# -----------------------------------------------
# 18.3 Kruskal-Wallis-Test:
# Lautstärke im Spiel nach Trainerrolle
# -----------------------------------------------

kruskal_laut_spiel <- kruskal.test(
  BA06_rank ~ Trainerrolle,
  data = lautstaerke_spiel_testdaten
)

kruskal_laut_spiel


# -----------------------------------------------
# 18.4 Post-hoc-Tests nach Kruskal-Wallis:
# paarweise Wilcoxon-Rangsummentests
# mit Holm-Korrektur
# -----------------------------------------------

pairwise_laut_spiel <- pairwise.wilcox.test(
  lautstaerke_spiel_testdaten$BA06_rank,
  lautstaerke_spiel_testdaten$Trainerrolle,
  p.adjust.method = "holm",
  exact = FALSE
)

pairwise_laut_spiel

# -----------------------------------------------------------
# 18.5 Tabellen Holm-korrigierte Post-hoc-Vergleiche
# Erhöhte Lautstärke im Spiel
# -----------------------------------------------------------

tabelle_lautstaerke_spiel <- erstelle_posthoc_tabelle(
  posthoc_objekt = pairwise_laut_spiel
)

tabelle_lautstaerke_spiel_gt <- tabelle_lautstaerke_spiel %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Vergleich = "Paarweiser Vergleich",
    p = md("*p*")
  ) %>%
  
  # Vergleich linksbündig
  
  cols_align(
    align = "left",
    columns = Vergleich
  ) %>%
  
  # p-Werte zentrieren
  
  cols_align(
    align = "center",
    columns = p
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = nrow(tabelle_lautstaerke_spiel)
    )
  ) %>%
  
  # Anmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Die *p*-Werte sind nach Holm korrigiert."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(400),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

tabelle_lautstaerke_spiel_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_lautstaerke_spiel_gt,
  filename = "Tab_Posthoc_Lautstaerke_Spiel.png",
  zoom = 3,
  expand = 5
)

# -----------------------------------------------
# 18.6 Tabellenbasis:
# Lautstärke im Spiel nach Trainerrolle
# -----------------------------------------------

lautstaerke_spiel_tabelle <- lautstaerke_spiel_testdaten %>%
  count(Lautstaerke, Trainerrolle, .drop = FALSE) %>%
  group_by(Trainerrolle) %>%
  mutate(
    Prozent = round(n / sum(n) * 100, 1),
    Anzeige_n_prozent = paste0(n, " (", Prozent, " %)"),
    Anzeige_prozent = paste0(Prozent, " %")
  ) %>%
  ungroup()

gruppen_n_laut_spiel <- lautstaerke_spiel_testdaten %>%
  count(Trainerrolle)

n_chef_laut_spiel <- gruppen_n_laut_spiel$n[
  gruppen_n_laut_spiel$Trainerrolle == "Cheftrainer:in"
]

n_co_laut_spiel <- gruppen_n_laut_spiel$n[
  gruppen_n_laut_spiel$Trainerrolle == "Co-Trainer:in"
]

n_torwart_laut_spiel <- gruppen_n_laut_spiel$n[
  gruppen_n_laut_spiel$Trainerrolle == "Torwarttrainer:in"
]

n_andere_laut_spiel <- gruppen_n_laut_spiel$n[
  gruppen_n_laut_spiel$Trainerrolle == "Andere"
]

# -----------------------------------------------
# 18.7 Tabelle:
# Lautstärke im Spiel nach Trainer:innenrolle
# -----------------------------------------------

tabelle_laut_spiel_prozent <-
  lautstaerke_spiel_tabelle %>%
  select(
    Lautstaerke,
    Trainerrolle,
    Prozent
  ) %>%
  pivot_wider(
    names_from = Trainerrolle,
    values_from = Prozent,
    values_fill = 0
  ) %>%
  select(
    Lautstaerke,
    `Cheftrainer:in`,
    `Co-Trainer:in`,
    `Torwarttrainer:in`,
    Andere
  ) %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Lautstaerke = "Häufigkeit",
    
    `Cheftrainer:in` = md(
      paste0(
        "Cheftrainer:innen<br>(*n* = ",
        n_chef_laut_spiel,
        ")"
      )
    ),
    
    `Co-Trainer:in` = md(
      paste0(
        "Co-Trainer:innen<br>(*n* = ",
        n_co_laut_spiel,
        ")"
      )
    ),
    
    `Torwarttrainer:in` = md(
      paste0(
        "Torwarttrainer:innen<br>(*n* = ",
        n_torwart_laut_spiel,
        ")"
      )
    ),
    
    Andere = md(
      paste0(
        "Andere<br>(*n* = ",
        n_andere_laut_spiel,
        ")"
      )
    )
  ) %>%
  
  # Feste, kompakte Spaltenbreiten
  
  cols_width(
    Lautstaerke ~ px(140),
    `Cheftrainer:in` ~ px(110),
    `Co-Trainer:in` ~ px(105),
    `Torwarttrainer:in` ~ px(125),
    Andere ~ px(80)
  ) %>%
  
  # Prozentwerte mit zwei Nachkommastellen
  
  fmt_number(
    columns = c(
      `Cheftrainer:in`,
      `Co-Trainer:in`,
      `Torwarttrainer:in`,
      Andere
    ),
    decimals = 2,
    use_seps = FALSE,
    pattern = "{x} %"
  ) %>%
  
  # Text linksbündig
  
  cols_align(
    align = "left",
    columns = Lautstaerke
  ) %>%
  
  # Prozentwerte zentrieren
  
  cols_align(
    align = "center",
    columns = c(
      `Cheftrainer:in`,
      `Co-Trainer:in`,
      `Torwarttrainer:in`,
      Andere
    )
  ) %>%
  
  # Überschriften der Trainer:innenrollen zentrieren
  
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_column_labels(
      columns = c(
        `Cheftrainer:in`,
        `Co-Trainer:in`,
        `Torwarttrainer:in`,
        Andere
      )
    )
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Kategorie
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Lautstaerke == "Immer"
    )
  ) %>%
  
  # Tabellenanmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Angaben in Prozent innerhalb der jeweiligen Trainer:innenrolle."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(560),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

tabelle_laut_spiel_prozent

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_laut_spiel_prozent,
  filename = "Tab_Lautstaerke_Spiel_Trainerrolle.png",
  zoom = 3,
  expand = 5
)

# =========================================================
# 19. Frage 17: FESS
# =========================================================

# ---------------------------------------------------------
# 19.1 FESS-Items und Subskalen definieren
# ---------------------------------------------------------

fess_items <- c(
  "SB02_01", "SB02_02", "SB02_03", "SB02_04", "SB02_05",
  "SB02_06", "SB02_07", "SB02_08", "SB02_09",
  "SB03_01", "SB03_02", "SB03_03", "SB03_04",
  "SB03_05", "SB03_06", "SB03_07", "SB03_08"
)

fess_beziehung <- c(
  "SB02_05", "SB02_09", "SB03_01",
  "SB03_03", "SB03_06", "SB03_07"
)

fess_bewusstheit <- c(
  "SB02_01", "SB02_04", "SB02_06",
  "SB02_08", "SB03_02", "SB03_04"
)

fess_emotion <- c(
  "SB02_02", "SB02_03", "SB02_07",
  "SB03_05", "SB03_08"
)

# ---------------------------------------------------------
# 19.2 Vollständig beantwortete FESS-Fälle auswählen
#      und Subskalenscores berechnen
# ---------------------------------------------------------

daten_fess <- daten %>%
  filter(if_all(all_of(fess_items), ~ !is.na(.x))) %>%
  mutate(
    FESS_Beziehung = rowSums(pick(all_of(fess_beziehung))),
    FESS_Bewusstheit = rowSums(pick(all_of(fess_bewusstheit))),
    FESS_Emotion = rowSums(pick(all_of(fess_emotion))),
    
    # In Kapitel 22 erzeugte Bezeichnungen bleiben erhalten.
    Geschlecht = recode(
      as.character(P001),
      "1" = "Weiblich",
      "2" = "Männlich",
      "3" = "Divers",
      "4" = "Keine Angabe"
    ),
    Beruf = recode(
      as.character(P003),
      "1" = "Hauptberuflich",
      "2" = "Nebenberuflich"
    ),
    Gruppe = recode(
      as.character(P004),
      "1" = "Erwachsene",
      "2" = "Jugend"
    ),
    Trainerrolle = recode(
      as.character(P006_bereinigt),
      "1" = "Cheftrainer:in",
      "2" = "Co-Trainer:in",
      "3" = "Torwarttrainer:in",
      "4" = "Andere"
    ),
    Stimmtraining = recode(
      as.character(P008),
      "1" = "Ja",
      "2" = "Nein"
    )
  )

# ---------------------------------------------------------
# 19.3 FESS-Tabelle mit den individuellen Subskalenscores
# ---------------------------------------------------------

fess_tabelle <- daten_fess %>%
  select(
    CASE,
    FESS_Beziehung,
    FESS_Bewusstheit,
    FESS_Emotion
  ) %>%
  arrange(CASE)

fess_tabelle

# ---------------------------------------------------------
# 19.4 Deskriptive Auswertung der FESS-Subskalen
# ---------------------------------------------------------

fess_deskriptiv <- daten_fess %>%
  summarise(
    n = n(),
    
    Mittelwert_Beziehung = round(mean(FESS_Beziehung), 2),
    SD_Beziehung = round(sd(FESS_Beziehung), 2),
    Median_Beziehung = median(FESS_Beziehung),
    Minimum_Beziehung = min(FESS_Beziehung),
    Maximum_Beziehung = max(FESS_Beziehung),
    
    Mittelwert_Bewusstheit = round(mean(FESS_Bewusstheit), 2),
    SD_Bewusstheit = round(sd(FESS_Bewusstheit), 2),
    Median_Bewusstheit = median(FESS_Bewusstheit),
    Minimum_Bewusstheit = min(FESS_Bewusstheit),
    Maximum_Bewusstheit = max(FESS_Bewusstheit),
    
    Mittelwert_Emotion = round(mean(FESS_Emotion), 2),
    SD_Emotion = round(sd(FESS_Emotion), 2),
    Median_Emotion = median(FESS_Emotion),
    Minimum_Emotion = min(FESS_Emotion),
    Maximum_Emotion = max(FESS_Emotion)
  )

fess_deskriptiv

# ---------------------------------------------------------
# 19.5 Reliabilität der FESS-Subskalen
# ---------------------------------------------------------

alpha_beziehung <- psych::alpha(
  daten_fess[, fess_beziehung]
)

alpha_bewusstheit <- psych::alpha(
  daten_fess[, fess_bewusstheit]
)

alpha_emotion <- psych::alpha(
  daten_fess[, fess_emotion]
)

alpha_beziehung
alpha_bewusstheit
alpha_emotion

# ---------------------------------------------------------
# 19.6 FESS-Scores in den Hauptdatensatz übernehmen
# ---------------------------------------------------------

# Vor einem erneuten Ausführen vorhandene FESS-Scores entfernen,
# damit durch den Join keine Spalten mit .x und .y entstehen.
daten <- daten %>%
  select(
    -any_of(c(
      "FESS_Beziehung",
      "FESS_Bewusstheit",
      "FESS_Emotion"
    ))
  ) %>%
  left_join(
    daten_fess %>%
      select(
        CASE,
        FESS_Beziehung,
        FESS_Bewusstheit,
        FESS_Emotion
      ),
    by = "CASE"
  )

# -----------------------------------------------

# 19.7 FESS – Boxplot der Subskalen in Prozent

# -----------------------------------------------

# Prozentwerte berechnen

daten_fess_prozent <- daten_fess %>%
  mutate(
    FESS_Beziehung_Prozent = FESS_Beziehung / 30 * 100,
    FESS_Bewusstheit_Prozent = FESS_Bewusstheit / 30 * 100,
    FESS_Emotion_Prozent = FESS_Emotion / 25 * 100
  ) %>%
  select(
    FESS_Beziehung_Prozent,
    FESS_Bewusstheit_Prozent,
    FESS_Emotion_Prozent
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "FESS_Subskala",
    values_to = "Prozentwert"
  ) %>%
  mutate(
    FESS_Subskala = factor(
      FESS_Subskala,
      levels = c(
        "FESS_Beziehung_Prozent",
        "FESS_Bewusstheit_Prozent",
        "FESS_Emotion_Prozent"
      ),
      labels = c(
        "Beziehung zur\nStimme",
        "Bewusstheit im Umgang\nmit der Stimme",
        "Stimme und\nEmotion"
      )
    )
  )

# Boxplot erstellen

boxplot_fess_prozent <- ggplot(
  daten_fess_prozent,
  aes(
    x = FESS_Subskala,
    y = Prozentwert
  )
) +
  geom_boxplot(
    width = 0.50,
    fill = "white",
    colour = "black",
    linewidth = 0.35,
    outlier.shape = 16,
    outlier.size = 1.5
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = NULL,
    y = "Anteil am maximalen\nSkalenwert (%)"
  ) +
  theme_classic(
    base_size = 18,
    base_family = "Times New Roman"
  ) +
  theme(
    
    # Gesamte Schrift in Times New Roman
    
    text = element_text(
      family = "Times New Roman"
    ),
    
    # Linien der x- und y-Achse
    
    axis.line = element_line(
      colour = "black",
      linewidth = 0.35
    ),
    
    # Markierungen der y-Achse
    
    axis.ticks.y = element_line(
      colour = "black",
      linewidth = 0.35
    ),
    
    # Beschriftung der y-Achse
    
    axis.title.y = element_text(
      family = "Times New Roman",
      size = 12,
      lineheight = 0.9,
      margin = margin(r = 10)
    ),
    
    # Zahlen der y-Achse
    
    axis.text.y = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    # Beschriftungen der drei FESS-Subskalen
    
    axis.text.x = element_text(
      family = "Times New Roman",
      size = 12,
      lineheight = 0.9,
      margin = margin(t = 8)
    ),
    
    # Markierungen der x-Achse
    
    axis.ticks.x = element_line(
      colour = "black",
      linewidth = 0.35
    ),
    
    # Genügend Rand, damit nichts abgeschnitten wird
    
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 20,
      l = 25
    )
  )

# Boxplot anzeigen

print(boxplot_fess_prozent)

# Boxplot als hochauflösende PNG-Datei speichern

ggsave(
  filename = "Abb_FESS_Subskalen_Boxplot_Prozent.png",
  plot = boxplot_fess_prozent,
  width = 18,
  height = 10,
  units = "cm",
  dpi = 300,
  bg = "white",
  device = "png"
)

# ---------------------------------------------------------
# 19.8 Liga-Bezeichnungen ergänzen
# ---------------------------------------------------------

# Der Objekt- und Spaltenname aus dem ursprünglichen Kapitel
# wird für mögliche spätere Verwendungen beibehalten.
daten_fess_liga <- daten_fess %>%
  mutate(P005 = as.character(P005)) %>%
  left_join(
    werte %>%
      filter(VAR == "P005", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("P005" = "RESPONSE")
  ) %>%
  rename(Liga = MEANING)

# ---------------------------------------------------------
# 19.9 FESS nach Trainerrolle
# ---------------------------------------------------------

fess_trainerrolle_tab <- daten_fess %>%
  filter(!is.na(Trainerrolle)) %>%
  group_by(Trainerrolle) %>%
  summarise(
    n = n(),
    M_Beziehung = round(mean(FESS_Beziehung), 2),
    SD_Beziehung = round(sd(FESS_Beziehung), 2),
    Median_Beziehung = median(FESS_Beziehung),
    IQR_Beziehung = IQR(FESS_Beziehung),
    M_Bewusstheit = round(mean(FESS_Bewusstheit), 2),
    SD_Bewusstheit = round(sd(FESS_Bewusstheit), 2),
    Median_Bewusstheit = median(FESS_Bewusstheit),
    IQR_Bewusstheit = IQR(FESS_Bewusstheit),
    M_Emotion = round(mean(FESS_Emotion), 2),
    SD_Emotion = round(sd(FESS_Emotion), 2),
    Median_Emotion = median(FESS_Emotion),
    IQR_Emotion = IQR(FESS_Emotion),
    .groups = "drop"
  )

fess_trainerrolle_tab

kruskal_fess_beziehung_trainerrolle <- kruskal.test(
  FESS_Beziehung ~ Trainerrolle,
  data = daten_fess
)

kruskal_fess_bewusstheit_trainerrolle <- kruskal.test(
  FESS_Bewusstheit ~ Trainerrolle,
  data = daten_fess
)

kruskal_fess_emotion_trainerrolle <- kruskal.test(
  FESS_Emotion ~ Trainerrolle,
  data = daten_fess
)

kruskal_fess_beziehung_trainerrolle
kruskal_fess_bewusstheit_trainerrolle
kruskal_fess_emotion_trainerrolle

pairwise_fess_beziehung_trainerrolle <- pairwise.wilcox.test(
  daten_fess$FESS_Beziehung,
  daten_fess$Trainerrolle,
  p.adjust.method = "holm",
  exact = FALSE
)

pairwise_fess_bewusstheit_trainerrolle <- pairwise.wilcox.test(
  daten_fess$FESS_Bewusstheit,
  daten_fess$Trainerrolle,
  p.adjust.method = "holm",
  exact = FALSE
)

pairwise_fess_emotion_trainerrolle <- pairwise.wilcox.test(
  daten_fess$FESS_Emotion,
  daten_fess$Trainerrolle,
  p.adjust.method = "holm",
  exact = FALSE
)

pairwise_fess_beziehung_trainerrolle
pairwise_fess_bewusstheit_trainerrolle
pairwise_fess_emotion_trainerrolle

# ---------------------------------------------------------
# 19.10 FESS und Berufserfahrung
# ---------------------------------------------------------

fess_erfahrung <- daten_fess %>%
  filter(!is.na(P007_07))

spearman_fess_beziehung_erfahrung <- cor.test(
  fess_erfahrung$P007_07,
  fess_erfahrung$FESS_Beziehung,
  method = "spearman",
  exact = FALSE
)

spearman_fess_bewusstheit_erfahrung <- cor.test(
  fess_erfahrung$P007_07,
  fess_erfahrung$FESS_Bewusstheit,
  method = "spearman",
  exact = FALSE
)

spearman_fess_emotion_erfahrung <- cor.test(
  fess_erfahrung$P007_07,
  fess_erfahrung$FESS_Emotion,
  method = "spearman",
  exact = FALSE
)

spearman_fess_beziehung_erfahrung
spearman_fess_bewusstheit_erfahrung
spearman_fess_emotion_erfahrung

# ---------------------------------------------------------
# 19.11 FESS und Sprechanteil im Training
# ---------------------------------------------------------

sprechanteil_levels <- c(
  "0-15 Minuten",
  "16-30 Minuten",
  "31-45 Minuten",
  "46-60 Minuten",
  "61-75 Minuten",
  "76-90 Minuten",
  "91-105 Minuten",
  "106-120 Minuten",
  "Mehr als 120 Minuten"
)

fess_sprech_training_spearman <- daten_fess %>%
  filter(!is.na(BA02)) %>%
  mutate(BA02 = as.character(BA02)) %>%
  left_join(
    werte %>%
      filter(VAR == "BA02", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA02" = "RESPONSE")
  ) %>%
  rename(Sprechanteil = MEANING) %>%
  mutate(
    Sprechanteil = factor(
      Sprechanteil,
      levels = sprechanteil_levels,
      ordered = TRUE
    ),
    BA02_rank = as.numeric(Sprechanteil)
  ) %>%
  filter(!is.na(BA02_rank))

# Der ursprüngliche Objektname bleibt als Alias erhalten.
fess_sprech_training <- fess_sprech_training_spearman

spearman_fess_beziehung_sprech_training <- cor.test(
  fess_sprech_training_spearman$BA02_rank,
  fess_sprech_training_spearman$FESS_Beziehung,
  method = "spearman",
  exact = FALSE
)

spearman_fess_bewusstheit_sprech_training <- cor.test(
  fess_sprech_training_spearman$BA02_rank,
  fess_sprech_training_spearman$FESS_Bewusstheit,
  method = "spearman",
  exact = FALSE
)

spearman_fess_emotion_sprech_training <- cor.test(
  fess_sprech_training_spearman$BA02_rank,
  fess_sprech_training_spearman$FESS_Emotion,
  method = "spearman",
  exact = FALSE
)

spearman_fess_beziehung_sprech_training
spearman_fess_bewusstheit_sprech_training
spearman_fess_emotion_sprech_training

tabelle_fess_sprech <- fess_sprech_training %>%
  group_by(Sprechanteil) %>%
  summarise(
    n = n(),
    Beziehung_M = round(mean(FESS_Beziehung), 2),
    Beziehung_SD = round(sd(FESS_Beziehung), 2),
    Bewusstheit_M = round(mean(FESS_Bewusstheit), 2),
    Bewusstheit_SD = round(sd(FESS_Bewusstheit), 2),
    Emotion_M = round(mean(FESS_Emotion), 2),
    Emotion_SD = round(sd(FESS_Emotion), 2),
    .groups = "drop"
  ) %>%
  arrange(Sprechanteil)

tabelle_fess_sprech

# ---------------------------------------------------------
# 19.12 FESS und Sprechanteil im Spiel
# ---------------------------------------------------------

fess_sprech_spiel_spearman <- daten_fess %>%
  filter(!is.na(BA05)) %>%
  mutate(BA05 = as.character(BA05)) %>%
  left_join(
    werte %>%
      filter(VAR == "BA05", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA05" = "RESPONSE")
  ) %>%
  rename(Sprechanteil = MEANING) %>%
  mutate(
    Sprechanteil = factor(
      Sprechanteil,
      levels = sprechanteil_levels,
      ordered = TRUE
    ),
    BA05_rank = as.numeric(Sprechanteil)
  ) %>%
  filter(!is.na(BA05_rank))

# Der ursprüngliche Objektname bleibt als Alias erhalten.
fess_sprech_spiel <- fess_sprech_spiel_spearman

spearman_fess_beziehung_sprech_spiel <- cor.test(
  fess_sprech_spiel_spearman$BA05_rank,
  fess_sprech_spiel_spearman$FESS_Beziehung,
  method = "spearman",
  exact = FALSE
)

spearman_fess_bewusstheit_sprech_spiel <- cor.test(
  fess_sprech_spiel_spearman$BA05_rank,
  fess_sprech_spiel_spearman$FESS_Bewusstheit,
  method = "spearman",
  exact = FALSE
)

spearman_fess_emotion_sprech_spiel <- cor.test(
  fess_sprech_spiel_spearman$BA05_rank,
  fess_sprech_spiel_spearman$FESS_Emotion,
  method = "spearman",
  exact = FALSE
)

spearman_fess_beziehung_sprech_spiel
spearman_fess_bewusstheit_sprech_spiel
spearman_fess_emotion_sprech_spiel

tabelle_fess_sprech_spiel <- fess_sprech_spiel %>%
  group_by(Sprechanteil) %>%
  summarise(
    n = n(),
    Beziehung_M = round(mean(FESS_Beziehung), 2),
    Beziehung_SD = round(sd(FESS_Beziehung), 2),
    Bewusstheit_M = round(mean(FESS_Bewusstheit), 2),
    Bewusstheit_SD = round(sd(FESS_Bewusstheit), 2),
    Emotion_M = round(mean(FESS_Emotion), 2),
    Emotion_SD = round(sd(FESS_Emotion), 2),
    .groups = "drop"
  ) %>%
  arrange(Sprechanteil)

tabelle_fess_sprech_spiel

# ---------------------------------------------------------
# 19.13 FESS und Lautstärke im Training
# ---------------------------------------------------------

fess_laut_training_spearman <- daten_fess %>%
  filter(!is.na(BA04)) %>%
  mutate(BA04 = as.character(BA04)) %>%
  left_join(
    werte %>%
      filter(VAR == "BA04", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA04" = "RESPONSE")
  ) %>%
  rename(Lautstärke = MEANING) %>%
  mutate(
    Lautstärke = factor(
      Lautstärke,
      levels = lautstaerke_levels,
      ordered = TRUE
    ),
    BA04_rank = as.numeric(Lautstärke)
  ) %>%
  filter(!is.na(BA04_rank))

# Der ursprüngliche Objektname bleibt als Alias erhalten.
fess_laut_training <- fess_laut_training_spearman

spearman_fess_beziehung_laut_training <- cor.test(
  fess_laut_training_spearman$BA04_rank,
  fess_laut_training_spearman$FESS_Beziehung,
  method = "spearman",
  exact = FALSE
)

spearman_fess_bewusstheit_laut_training <- cor.test(
  fess_laut_training_spearman$BA04_rank,
  fess_laut_training_spearman$FESS_Bewusstheit,
  method = "spearman",
  exact = FALSE
)

spearman_fess_emotion_laut_training <- cor.test(
  fess_laut_training_spearman$BA04_rank,
  fess_laut_training_spearman$FESS_Emotion,
  method = "spearman",
  exact = FALSE
)

spearman_fess_beziehung_laut_training
spearman_fess_bewusstheit_laut_training
spearman_fess_emotion_laut_training

tabelle_fess_laut_training <- fess_laut_training %>%
  group_by(Lautstärke) %>%
  summarise(
    n = n(),
    Beziehung_M = round(mean(FESS_Beziehung), 2),
    Beziehung_SD = round(sd(FESS_Beziehung), 2),
    Bewusstheit_M = round(mean(FESS_Bewusstheit), 2),
    Bewusstheit_SD = round(sd(FESS_Bewusstheit), 2),
    Emotion_M = round(mean(FESS_Emotion), 2),
    Emotion_SD = round(sd(FESS_Emotion), 2),
    .groups = "drop"
  ) %>%
  arrange(Lautstärke)

tabelle_fess_laut_training

# ---------------------------------------------------------
# 19.14 FESS und Lautstärke im Spiel
# ---------------------------------------------------------

fess_laut_spiel_spearman <- daten_fess %>%
  filter(!is.na(BA06)) %>%
  mutate(BA06 = as.character(BA06)) %>%
  left_join(
    werte %>%
      filter(VAR == "BA06", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA06" = "RESPONSE")
  ) %>%
  rename(Lautstärke = MEANING) %>%
  mutate(
    Lautstärke = factor(
      Lautstärke,
      levels = lautstaerke_levels,
      ordered = TRUE
    ),
    BA06_rank = as.numeric(Lautstärke)
  ) %>%
  filter(!is.na(BA06_rank))

# Der ursprüngliche Objektname bleibt als Alias erhalten.
fess_laut_spiel <- fess_laut_spiel_spearman

spearman_fess_beziehung_laut_spiel <- cor.test(
  fess_laut_spiel_spearman$BA06_rank,
  fess_laut_spiel_spearman$FESS_Beziehung,
  method = "spearman",
  exact = FALSE
)

spearman_fess_bewusstheit_laut_spiel <- cor.test(
  fess_laut_spiel_spearman$BA06_rank,
  fess_laut_spiel_spearman$FESS_Bewusstheit,
  method = "spearman",
  exact = FALSE
)

spearman_fess_emotion_laut_spiel <- cor.test(
  fess_laut_spiel_spearman$BA06_rank,
  fess_laut_spiel_spearman$FESS_Emotion,
  method = "spearman",
  exact = FALSE
)

spearman_fess_beziehung_laut_spiel
spearman_fess_bewusstheit_laut_spiel
spearman_fess_emotion_laut_spiel

tabelle_fess_laut_spiel <- fess_laut_spiel %>%
  group_by(Lautstärke) %>%
  summarise(
    n = n(),
    Beziehung_M = round(mean(FESS_Beziehung), 2),
    Beziehung_SD = round(sd(FESS_Beziehung), 2),
    Bewusstheit_M = round(mean(FESS_Bewusstheit), 2),
    Bewusstheit_SD = round(sd(FESS_Bewusstheit), 2),
    Emotion_M = round(mean(FESS_Emotion), 2),
    Emotion_SD = round(sd(FESS_Emotion), 2),
    .groups = "drop"
  ) %>%
  arrange(Lautstärke)

tabelle_fess_laut_spiel


# -----------------------------------------------
# 20. Frage 18: VHI-9i 
# -----------------------------------------------

# -----------------------------------------------
# 20.1 Variablen identifizieren und prüfen
# -----------------------------------------------

# Die 9 VHI-Items festlegen
vhi_items <- c(
  "SB13_01", "SB13_02", "SB13_03", "SB13_04", "SB13_05",
  "SB13_06", "SB13_07", "SB13_08", "SB13_09"
)

# Prüfen, ob die Variablen im Datensatz vorhanden sind
vhi_items %in% names(daten)

# Variablenlabels anzeigen
variablen %>%
  filter(VAR %in% vhi_items) %>%
  select(VAR, LABEL)

# Antwortcodierung anzeigen
werte %>%
  filter(VAR %in% vhi_items) %>%
  select(VAR, RESPONSE, MEANING)

# Häufigkeitsverteilungen für alle 9 Items
for (item in vhi_items) {
  cat("\n-----------------------------\n")
  cat("Variable:", item, "\n")
  print(table(daten[[item]], useNA = "ifany"))
}

# -----------------------------------------------
# 20.2 VHI-9i: Daten vorbereiten und Score berechnen
# -----------------------------------------------

# -9 als NA setzen (Sicherheitscheck)
for (item in vhi_items) {
  daten[[item]][daten[[item]] == -9] <- NA
}

# Umcodieren: 1–5 -> 0–4
daten_vhi <- daten %>%
  mutate(across(all_of(vhi_items), ~ . - 1))

# Nur vollständig beantwortete Fälle behalten
daten_vhi <- daten_vhi %>%
  filter(if_all(all_of(vhi_items), ~ !is.na(.)))

# Anzahl gültiger Fälle prüfen
nrow(daten_vhi)

# VHI-Gesamtscore berechnen (0–36)
daten_vhi <- daten_vhi %>%
  mutate(
    VHI_gesamt = rowSums(select(., all_of(vhi_items)))
  )

# Erste Werte anschauen
daten_vhi %>%
  select(CASE, VHI_gesamt) %>%
  head(10)

# -----------------------------------------------
# 20.3 VHI-9i: Deskriptive Auswertung
# -----------------------------------------------

# Übersicht
summary(daten_vhi$VHI_gesamt)

# Mittelwert + SD + Median
mean(daten_vhi$VHI_gesamt)
sd(daten_vhi$VHI_gesamt)
median(daten_vhi$VHI_gesamt)

# Minimum + Maximum
min(daten_vhi$VHI_gesamt)
max(daten_vhi$VHI_gesamt)


# -----------------------------------------------
# 20.4 VHI-9i: Schweregrade bilden
# -----------------------------------------------

daten_vhi <- daten_vhi %>%
  mutate(
    VHI_Schweregrad = case_when(
      VHI_gesamt <= 7 ~ "Keine Beeinträchtigung",
      VHI_gesamt <= 16 ~ "Geringgradige Beeinträchtigung",
      VHI_gesamt <= 26 ~ "Mittelgradige Beeinträchtigung",
      VHI_gesamt <= 36 ~ "Hochgradige Beeinträchtigung"
    )
  )

# Verteilung anzeigen
table(daten_vhi$VHI_Schweregrad)

# Prozent
prop.table(table(daten_vhi$VHI_Schweregrad)) * 100

# Schöne Tabelle
vhi_schweregrad_tab <- daten_vhi %>%
  count(VHI_Schweregrad) %>%
  mutate(
    Prozent = round(n / sum(n) * 100, 1)
  )

vhi_schweregrad_tab

# -----------------------------------------------
# 20.5 VHI-9i: Gesamtstichprobe (Mittelwert, SD, n)
# -----------------------------------------------

vhi_deskriptiv <- daten_vhi %>%
  summarise(
    n = n(),
    Mittelwert = round(mean(VHI_gesamt), 2),
    SD = round(sd(VHI_gesamt), 2),
    Minimum = min(VHI_gesamt),
    Maximum = max(VHI_gesamt)
  )

vhi_deskriptiv

# -----------------------------------------------
# 20.5.1 VHI-9i: Reliabilität prüfen (Cronbach's Alpha)
# -----------------------------------------------

library(psych)

alpha_vhi <- alpha(
  daten_vhi[, vhi_items]
)

# Ergebnis anzeigen
alpha_vhi

# -----------------------------------------------

# 20.5.2 Tabelle:

# Verteilung der VHI-Schweregrade

# -----------------------------------------------

# Reihenfolge der Kategorien festlegen

vhi_levels <- c(
  "Keine Beeinträchtigung",
  "Geringgradige Beeinträchtigung",
  "Mittelgradige Beeinträchtigung",
  "Hochgradige Beeinträchtigung"
)

# Tabellenbasis erstellen

vhi_schweregrad_tab <- daten_vhi %>%
  mutate(
    VHI_Schweregrad = factor(
      VHI_Schweregrad,
      levels = vhi_levels
    )
  ) %>%
  count(
    VHI_Schweregrad,
    .drop = FALSE
  ) %>%
  mutate(
    Prozent = n / sum(n) * 100
  )

# Tabelle gestalten

vhi_schweregrad_gt <- vhi_schweregrad_tab %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    VHI_Schweregrad = "Schweregrad",
    n = md("*n*"),
    Prozent = "%"
  ) %>%
  
  # Prozentwerte mit zwei Nachkommastellen
  
  fmt_number(
    columns = Prozent,
    decimals = 2,
    use_seps = FALSE
  ) %>%
  
  # Häufigkeiten ohne Nachkommastellen
  
  fmt_integer(
    columns = n,
    use_seps = FALSE
  ) %>%
  
  # Textspalte linksbündig
  
  cols_align(
    align = "left",
    columns = VHI_Schweregrad
  ) %>%
  
  # Zahlenspalten rechtsbündig
  
  cols_align(
    align = "right",
    columns = c(n, Prozent)
  ) %>%
  
  # Kompakte Spaltenbreiten
  
  cols_width(
    VHI_Schweregrad ~ px(245),
    n ~ px(60),
    Prozent ~ px(75)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Kategorie
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = VHI_Schweregrad == "Hochgradige Beeinträchtigung"
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(380),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

vhi_schweregrad_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = vhi_schweregrad_gt,
  filename = "Tab_VHI_Schweregrade.png",
  zoom = 3,
  expand = 5
)

# ----------------------------------------------------------
# 20.5.3 VHI-9i-Score zusätzlich in Hauptdatensatz übernehmen
# ohne Originalitems in daten zu verändern
# ----------------------------------------------------------

daten <- daten %>%
  left_join(
    daten_vhi %>%
      select(CASE, VHI_gesamt),
    by = "CASE"
  )

# Kontrolle
"VHI_gesamt" %in% names(daten)

# Anzahl gültiger Werte
sum(!is.na(daten$VHI_gesamt))

# Kurzer Kontrollblick
daten %>%
  select(CASE, VHI_gesamt) %>%
  filter(!is.na(VHI_gesamt)) %>%
  head(10)

# -----------------------------------------------

# 20.5.4 VHI-9i – Boxplot

# -----------------------------------------------

# Boxplot erstellen

boxplot_vhi_gesamt <- ggplot(
  daten_vhi,
  aes(
    x = "",
    y = VHI_gesamt
  )
) +
  geom_boxplot(
    width = 0.35,
    fill = "white",
    colour = "black",
    linewidth = 0.35,
    outlier.shape = 16,
    outlier.size = 1.5
  ) +
  scale_y_continuous(
    limits = c(0, 36),
    breaks = seq(0, 36, by = 4),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = NULL,
    y = "VHI-9i-Gesamtscore"
  ) +
  theme_classic(
    base_size = 18,
    base_family = "Times New Roman"
  ) +
  theme(
    
    # Gesamte Schrift in Times New Roman
    
    text = element_text(
      family = "Times New Roman"
    ),
    
    # Linien der x- und y-Achse
    
    axis.line = element_line(
      colour = "black",
      linewidth = 0.35
    ),
    
    # Markierungen der y-Achse
    
    axis.ticks.y = element_line(
      colour = "black",
      linewidth = 0.35
    ),
    
    # Beschriftung der y-Achse
    
    axis.title.y = element_text(
      family = "Times New Roman",
      size = 12,
      lineheight = 0.9,
      margin = margin(r = 10)
    ),
    
    # Zahlen der y-Achse
    
    axis.text.y = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    # Beschriftung und Markierungen der x-Achse ausblenden
    
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    # Einheitliche Ränder
    
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 20,
      l = 25
    )
  )

# Boxplot anzeigen

print(boxplot_vhi_gesamt)

# Boxplot als hochauflösende PNG-Datei speichern

ggsave(
  filename = "Abb_VHI9i_Gesamtscore_Boxplot.png",
  plot = boxplot_vhi_gesamt,
  width = 12,
  height = 10,
  units = "cm",
  dpi = 300,
  bg = "white",
  device = "png"
)

# -----------------------------------------------
# 20.6 VHI × Alter
# -----------------------------------------------

# Datensatz vorbereiten
vhi_alter <- daten_vhi %>%
  filter(!is.na(P002_01))

# Deskriptive Übersicht
vhi_alter %>%
  summarise(
    n = n(),
    Mittelwert_Alter = round(mean(P002_01), 1),
    SD_Alter = round(sd(P002_01), 1),
    Median_Alter = median(P002_01),
    Mittelwert_VHI = round(mean(VHI_gesamt), 2),
    Median_VHI = median(VHI_gesamt)
  )

# Spearman-Rangkorrelation
cor.test(
  vhi_alter$P002_01,
  vhi_alter$VHI_gesamt,
  method = "spearman"
)

# -----------------------------------------------
# 20.7 VHI × Berufserfahrung gesamt (Frage 7)
# -----------------------------------------------

# Datensatz vorbereiten
vhi_erfahrung <- daten_vhi %>%
  filter(!is.na(P007_07))

# Deskriptive Statistik
vhi_erfahrung %>%
  summarise(
    n = n(),
    Mittelwert_Erfahrung = round(mean(P007_07), 2),
    SD_Erfahrung = round(sd(P007_07), 2),
    Median_Erfahrung = median(P007_07),
    Mittelwert_VHI = round(mean(VHI_gesamt), 2),
    Median_VHI = median(VHI_gesamt)
  )

# Spearman-Rangkorrelation
cor.test(
  vhi_erfahrung$P007_07,
  vhi_erfahrung$VHI_gesamt,
  method = "spearman"
)

# Optional: Scatterplot
library(ggplot2)

ggplot(vhi_erfahrung, aes(x = P007_07, y = VHI_gesamt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Zusammenhang zwischen Berufserfahrung und VHI",
    x = "Berufserfahrung (Jahre)",
    y = "VHI-Gesamtscore"
  ) +
  theme_minimal()

# -----------------------------------------------
# 20.8 VHI × Sprechanteil im Training (BA02)
# -----------------------------------------------

vhi_sprech_training <- daten_vhi %>%
  filter(!is.na(BA02)) %>%
  mutate(
    BA02 = as.character(BA02)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA02", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA02" = "RESPONSE")
  ) %>%
  rename(Sprechanteil = MEANING)

# Reihenfolge der Kategorien festlegen
vhi_sprech_training <- vhi_sprech_training %>%
  mutate(
    Sprechanteil = factor(
      Sprechanteil,
      levels = c(
        "0-15 Minuten",
        "16-30 Minuten",
        "31-45 Minuten",
        "46-60 Minuten",
        "61-75 Minuten",
        "76-90 Minuten",
        "91-105 Minuten",
        "106-120 Minuten",
        "Mehr als 120 Minuten"
      )
    )
  )

# 1) Deskriptive Tabelle
vhi_sprech_training_tab <- vhi_sprech_training %>%
  group_by(Sprechanteil) %>%
  summarise(
    n = n(),
    Mittelwert = round(mean(VHI_gesamt), 2),
    SD = round(sd(VHI_gesamt), 2),
    Minimum = min(VHI_gesamt),
    Maximum = max(VHI_gesamt)
  ) %>%
  arrange(Sprechanteil)

vhi_sprech_training_tab

# -----------------------------------------------
# 20.8.1 Spearman: VHI × Sprechanteil im Training (BA02)
# -----------------------------------------------

vhi_sprech_training_spearman <- daten_vhi %>%
  filter(!is.na(BA02), !is.na(VHI_gesamt)) %>%
  mutate(
    BA02 = as.character(BA02)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA02", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA02" = "RESPONSE")
  ) %>%
  rename(Sprechanteil = MEANING) %>%
  mutate(
    Sprechanteil = factor(
      Sprechanteil,
      levels = c(
        "0-15 Minuten",
        "16-30 Minuten",
        "31-45 Minuten",
        "46-60 Minuten",
        "61-75 Minuten",
        "76-90 Minuten",
        "91-105 Minuten",
        "106-120 Minuten",
        "Mehr als 120 Minuten"
      ),
      ordered = TRUE
    ),
    BA02_rank = as.numeric(Sprechanteil)
  )

# Kontrolle: stimmt die Rangcodierung?
vhi_sprech_training_spearman %>%
  distinct(Sprechanteil, BA02_rank) %>%
  arrange(BA02_rank)

# Spearman-Korrelation
spearman_vhi_sprech_training <- cor.test(
  vhi_sprech_training_spearman$BA02_rank,
  vhi_sprech_training_spearman$VHI_gesamt,
  method = "spearman",
  exact = FALSE
)

spearman_vhi_sprech_training

# -----------------------------------------------
# 20.9 VHI × Sprechanteil im Spiel (BA05)
# -----------------------------------------------

vhi_sprech_spiel <- daten_vhi %>%
  filter(!is.na(BA05)) %>%
  mutate(
    BA05 = as.character(BA05)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA05", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA05" = "RESPONSE")
  ) %>%
  rename(Sprechanteil = MEANING)

# Reihenfolge festlegen
vhi_sprech_spiel <- vhi_sprech_spiel %>%
  mutate(
    Sprechanteil = factor(
      Sprechanteil,
      levels = c(
        "0-15 Minuten",
        "16-30 Minuten",
        "31-45 Minuten",
        "46-60 Minuten",
        "61-75 Minuten",
        "76-90 Minuten",
        "91-105 Minuten",
        "106-120 Minuten",
        "Mehr als 120 Minuten"
      )
    )
  )

# 1) Deskriptive Tabelle
vhi_sprech_spiel_tab <- vhi_sprech_spiel %>%
  group_by(Sprechanteil) %>%
  summarise(
    n = n(),
    Mittelwert = round(mean(VHI_gesamt), 2),
    SD = round(sd(VHI_gesamt), 2),
    Minimum = min(VHI_gesamt),
    Maximum = max(VHI_gesamt)
  ) %>%
  arrange(Sprechanteil)

vhi_sprech_spiel_tab

# -----------------------------------------------
# 20.9.1 Spearman: VHI × Sprechanteil im Spiel (BA05)
# -----------------------------------------------

vhi_sprech_spiel_spearman <- daten_vhi %>%
  filter(!is.na(BA05), !is.na(VHI_gesamt)) %>%
  mutate(
    BA05 = as.character(BA05)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA05", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA05" = "RESPONSE")
  ) %>%
  rename(Sprechanteil = MEANING) %>%
  mutate(
    Sprechanteil = factor(
      Sprechanteil,
      levels = c(
        "0-15 Minuten",
        "16-30 Minuten",
        "31-45 Minuten",
        "46-60 Minuten",
        "61-75 Minuten",
        "76-90 Minuten",
        "91-105 Minuten",
        "106-120 Minuten",
        "Mehr als 120 Minuten"
      ),
      ordered = TRUE
    ),
    BA05_rank = as.numeric(Sprechanteil)
  )

# Kontrolle der Rangcodierung
vhi_sprech_spiel_spearman %>%
  distinct(Sprechanteil, BA05_rank) %>%
  arrange(BA05_rank)

# Spearman-Korrelation
spearman_vhi_sprech_spiel <- cor.test(
  vhi_sprech_spiel_spearman$BA05_rank,
  vhi_sprech_spiel_spearman$VHI_gesamt,
  method = "spearman",
  exact = FALSE
)

spearman_vhi_sprech_spiel

# -----------------------------------------------
# 20.10 VHI × Lautstärke im Training (BA04)
# -----------------------------------------------

vhi_laut_training <- daten_vhi %>%
  filter(!is.na(BA04)) %>%
  mutate(
    BA04 = as.character(BA04)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA04", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA04" = "RESPONSE")
  ) %>%
  rename(Lautstärke = MEANING)

# Reihenfolge festlegen
vhi_laut_training <- vhi_laut_training %>%
  mutate(
    Lautstärke = factor(
      Lautstärke,
      levels = c(
        "Nie",
        "Selten",
        "Gelegentlich",
        "Oft",
        "Immer"
      )
    )
  )


# 1) Deskriptive Tabelle
vhi_laut_training_tab <- vhi_laut_training %>%
  group_by(Lautstärke) %>%
  summarise(
    n = n(),
    Mittelwert = round(mean(VHI_gesamt), 2),
    SD = round(sd(VHI_gesamt), 2),
    Minimum = min(VHI_gesamt),
    Maximum = max(VHI_gesamt)
  ) %>%
  arrange(Lautstärke)

vhi_laut_training_tab

# -----------------------------------------------
# 20.10.1 Spearman: VHI × Lautstärke im Training (BA04)
# -----------------------------------------------

vhi_laut_training_spearman <- daten_vhi %>%
  filter(!is.na(BA04), !is.na(VHI_gesamt)) %>%
  mutate(
    BA04 = as.character(BA04)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA04", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA04" = "RESPONSE")
  ) %>%
  rename(Lautstärke = MEANING) %>%
  mutate(
    Lautstärke = factor(
      Lautstärke,
      levels = c(
        "Nie",
        "Selten",
        "Gelegentlich",
        "Oft",
        "Immer"
      ),
      ordered = TRUE
    ),
    BA04_rank = as.numeric(Lautstärke)
  )

# Kontrolle der Rangcodierung
vhi_laut_training_spearman %>%
  distinct(Lautstärke, BA04_rank) %>%
  arrange(BA04_rank)

# Spearman-Korrelation
spearman_vhi_laut_training <- cor.test(
  vhi_laut_training_spearman$BA04_rank,
  vhi_laut_training_spearman$VHI_gesamt,
  method = "spearman",
  exact = FALSE
)

spearman_vhi_laut_training

# -----------------------------------------------
# 20.11 VHI × Lautstärke im Spiel (BA06)
# -----------------------------------------------

vhi_laut_spiel <- daten_vhi %>%
  filter(!is.na(BA06)) %>%
  mutate(
    BA06 = as.character(BA06)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA06", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA06" = "RESPONSE")
  ) %>%
  rename(Lautstärke = MEANING)

# Reihenfolge festlegen
vhi_laut_spiel <- vhi_laut_spiel %>%
  mutate(
    Lautstärke = factor(
      Lautstärke,
      levels = c(
        "Nie",
        "Selten",
        "Gelegentlich",
        "Oft",
        "Immer"
      )
    )
  )

# 1) Deskriptive Tabelle
vhi_laut_spiel_tab <- vhi_laut_spiel %>%
  group_by(Lautstärke) %>%
  summarise(
    n = n(),
    Mittelwert = round(mean(VHI_gesamt), 2),
    SD = round(sd(VHI_gesamt), 2),
    Minimum = min(VHI_gesamt),
    Maximum = max(VHI_gesamt)
  ) %>%
  arrange(Lautstärke)

vhi_laut_spiel_tab

# -----------------------------------------------
# 20.11.1 Spearman: VHI × Lautstärke im Spiel (BA06)
# -----------------------------------------------

vhi_laut_spiel_spearman <- daten_vhi %>%
  filter(!is.na(BA06), !is.na(VHI_gesamt)) %>%
  mutate(
    BA06 = as.character(BA06)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA06", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA06" = "RESPONSE")
  ) %>%
  rename(Lautstärke = MEANING) %>%
  mutate(
    Lautstärke = factor(
      Lautstärke,
      levels = c(
        "Nie",
        "Selten",
        "Gelegentlich",
        "Oft",
        "Immer"
      ),
      ordered = TRUE
    ),
    BA06_rank = as.numeric(Lautstärke)
  )

# Kontrolle der Rangcodierung
vhi_laut_spiel_spearman %>%
  distinct(Lautstärke, BA06_rank) %>%
  arrange(BA06_rank)

# Spearman-Korrelation
spearman_vhi_laut_spiel <- cor.test(
  vhi_laut_spiel_spearman$BA06_rank,
  vhi_laut_spiel_spearman$VHI_gesamt,
  method = "spearman",
  exact = FALSE
)

spearman_vhi_laut_spiel

# -----------------------------------------------
# 21 VTD 
# -----------------------------------------------

# -----------------------------------------------
# 21.1 Variablen prüfen
# -----------------------------------------------

vtd_freq_items <- paste0("SB05_0", 1:8)
vtd_sev_items  <- paste0("SB06_0", 1:8)

# prüfen ob alle Variablen existieren
vtd_freq_items %in% names(daten)
vtd_sev_items %in% names(daten)

# -----------------------------------------------
# 21.2 VTD: Daten bereinigen und vollständige Fälle
# -----------------------------------------------

# alle VTD-Items zusammenführen
vtd_items <- c(vtd_freq_items, vtd_sev_items)

# -9 als fehlend setzen (falls vorhanden)
for (item in vtd_items) {
  daten[[item]][daten[[item]] == -9] <- NA
}

# nur vollständig beantwortete Fälle behalten
daten_vtd <- daten %>%
  filter(if_all(all_of(vtd_items), ~ !is.na(.)))

# Anzahl gültiger Fälle
nrow(daten_vtd)

# Kontrolle (sollte keine NAs mehr enthalten)
daten_vtd %>%
  select(all_of(vtd_items)) %>%
  summary()

# -----------------------------------------------
# 21.3 VTD-Datencheck: 
# -----------------------------------------------

# -----------------------------------------------
# 21.3.1 vollständige Interviews + fehlende Werte
# Wie viele Interviews wurden vollständig beendet?
# -----------------------------------------------

table(daten$FINISHED)

# Nur vollständig beendete Interviews
daten_finished <- daten %>%
  filter(FINISHED == 1)

# Wie viele davon?
nrow(daten_finished)

# Anzahl gültiger Werte pro VTD-Item innerhalb der fertigen Interviews
sapply(daten_finished[, vtd_items], function(x) sum(!is.na(x)))

# ----------------------------------------------------------
# 21.3.2 Designbedingt fehlende SB06-Werte ergänzen
# Wenn SB05 == 1 ("Nie"),
# dann SB06 = 1 ("Keine")
# ----------------------------------------------------------

for(i in 1:8) {
  
  sb05 <- paste0("SB05_0", i)
  sb06 <- paste0("SB06_0", i)
  
  daten[[sb06]] <- ifelse(
    daten[[sb05]] == 1 & is.na(daten[[sb06]]),
    1,
    daten[[sb06]]
  )
}

# -----------------------------------------------
# 21.4 VTD – Summenscores
# Häufigkeit und Schweregrad GETRENNT
# -----------------------------------------------

# -----------------------------------------------
# 21.4.1 VTD-Häufigkeit
# -----------------------------------------------

# Häufigkeitsitems festlegen
vtd_freq_items <- paste0("SB05_0", 1:8)

# Rohdaten prüfen
summary(daten[, vtd_freq_items])
str(daten[, vtd_freq_items])

# Datensatz für VTD-Häufigkeit
# - nur SB05 verwenden
# - Werte von 1–7 auf 0–6 umcodieren
# - nur vollständige Häufigkeitsfälle behalten
# - Summenscore berechnen

daten_vtd_freq <- daten %>%
  mutate(
    across(
      all_of(vtd_freq_items),
      ~ ifelse(!is.na(.), . - 1, NA_real_)
    )
  ) %>%
  filter(
    if_all(all_of(vtd_freq_items), ~ !is.na(.))
  ) %>%
  mutate(
    VTD_freq = rowSums(select(., all_of(vtd_freq_items)))
  )

# Anzahl gültiger Fälle
nrow(daten_vtd_freq)

# Kontrolle der Umcodierung
summary(daten_vtd_freq[, vtd_freq_items])

# Deskriptive Statistik Summenscore
summary(daten_vtd_freq$VTD_freq)
sd(daten_vtd_freq$VTD_freq)


# -----------------------------------------------
# 21.4.2 VTD-Schweregrad
# -----------------------------------------------

# Schweregraditems festlegen
vtd_sev_items <- paste0("SB06_0", 1:8)

# Rohdaten prüfen
summary(daten[, vtd_sev_items])
str(daten[, vtd_sev_items])

# Datensatz für VTD-Schweregrad
# - nur SB06 verwenden
# - Werte von 1–7 auf 0–6 umcodieren
# - nur vollständige Schweregradfälle behalten
# - Summenscore berechnen

daten_vtd_sev <- daten %>%
  mutate(
    across(
      all_of(vtd_sev_items),
      ~ ifelse(!is.na(.), . - 1, NA_real_)
    )
  ) %>%
  filter(
    if_all(all_of(vtd_sev_items), ~ !is.na(.))
  ) %>%
  mutate(
    VTD_sev = rowSums(select(., all_of(vtd_sev_items)))
  )

# Anzahl gültiger Fälle
nrow(daten_vtd_sev)

# Kontrolle der Umcodierung
summary(daten_vtd_sev[, vtd_sev_items])

# Deskriptive Statistik Summenscore
summary(daten_vtd_sev$VTD_sev)
sd(daten_vtd_sev$VTD_sev)

# -----------------------------------------------
# 21.4.3 VTD-Gesamtscore
# Häufigkeit + Schweregrad
# -----------------------------------------------

# WICHTIG:
# Für den VTD-Gesamtscore werden nur Personen berücksichtigt,
# die sowohl VTD-Häufigkeit als auch VTD-Schweregrad vollständig haben.
# Personen, die nach VTD-Häufigkeit abgebrochen haben,
# werden dadurch automatisch ausgeschlossen.

daten_vtd_gesamt <- daten %>%
  mutate(
    across(
      all_of(c(vtd_freq_items, vtd_sev_items)),
      ~ ifelse(!is.na(.), . - 1, NA_real_)
    )
  ) %>%
  filter(
    if_all(all_of(c(vtd_freq_items, vtd_sev_items)), ~ !is.na(.))
  ) %>%
  mutate(
    VTD_freq = rowSums(select(., all_of(vtd_freq_items))),
    VTD_sev  = rowSums(select(., all_of(vtd_sev_items))),
    VTD_gesamt = VTD_freq + VTD_sev
  )

# Anzahl gültiger Fälle für den VTD-Gesamtscore
nrow(daten_vtd_gesamt)

# Kontrolle: Die beiden Abbruchpersonen sollten hier nicht enthalten sein
daten_vtd_gesamt %>%
  filter(CASE %in% c(96, 128)) %>%
  select(CASE, VTD_freq, VTD_sev, VTD_gesamt)

# Deskriptive Statistik VTD-Gesamtscore
summary(daten_vtd_gesamt$VTD_gesamt)
sd(daten_vtd_gesamt$VTD_gesamt)

# Optional: Kontrolle der beiden Subscores im Gesamtdatensatz
summary(daten_vtd_gesamt$VTD_freq)
summary(daten_vtd_gesamt$VTD_sev)

# Reliabilität VTD-Gesamtscore

alpha(
  daten_vtd_gesamt[, c(vtd_freq_items, vtd_sev_items)]
)

# -----------------------------------------------
# 21.4.4 VTD-Scores zusätzlich in Hauptdatensatz übernehmen
# ohne Originalitems in daten zu verändern
# -----------------------------------------------

daten <- daten %>%
  left_join(
    daten_vtd_gesamt %>%
      select(CASE, VTD_freq, VTD_sev, VTD_gesamt),
    by = "CASE"
  )

# Kontrolle
"VTD_freq" %in% names(daten)
"VTD_sev" %in% names(daten)
"VTD_gesamt" %in% names(daten)

# -----------------------------------------------
# 21.4.5 VTD: Schweregrade bilden
# -----------------------------------------------

daten_vtd_gesamt <- daten_vtd_gesamt %>%
  mutate(
    VTD_Schweregrad = case_when(
      VTD_gesamt <= 13 ~ "Keine stimmliche Beeinträchtigung",
      VTD_gesamt <= 26 ~ "Leichtgradige stimmliche Beeinträchtigung",
      VTD_gesamt <= 40 ~ "Mittelgradige stimmliche Beeinträchtigung",
      VTD_gesamt <= 96 ~ "Schwergradige stimmliche Beeinträchtigung"
    )
  )

# Verteilung anzeigen
table(daten_vtd_gesamt$VTD_Schweregrad)

# Prozent
prop.table(table(daten_vtd_gesamt$VTD_Schweregrad)) * 100

# -----------------------------------------------
# 21.4.6 VTD: Gesamtstichprobe (Mittelwert, SD, Median, Range)
# -----------------------------------------------

vtd_deskriptiv <- daten_vtd_gesamt %>%
  summarise(
    n = n(),
    Mittelwert = round(mean(VTD_gesamt), 2),
    SD = round(sd(VTD_gesamt), 2),
    Median = median(VTD_gesamt),
    Minimum = min(VTD_gesamt),
    Maximum = max(VTD_gesamt)
  )

vtd_deskriptiv

# -----------------------------------------------

# 21.4.7 Tabelle:

# Verteilung der VTD-Schweregrade

# -----------------------------------------------

# Reihenfolge der Kategorien festlegen

vtd_levels <- c(
  "Keine stimmliche Beeinträchtigung",
  "Leichtgradige stimmliche Beeinträchtigung",
  "Mittelgradige stimmliche Beeinträchtigung",
  "Schwergradige stimmliche Beeinträchtigung"
)

# Tabellenbasis erstellen

vtd_schweregrad_tab <- daten_vtd_gesamt %>%
  mutate(
    VTD_Schweregrad = factor(
      VTD_Schweregrad,
      levels = vtd_levels
    )
  ) %>%
  count(
    VTD_Schweregrad,
    .drop = FALSE
  ) %>%
  mutate(
    Prozent = n / sum(n) * 100
  )

# Tabelle gestalten

vtd_schweregrad_gt <- vtd_schweregrad_tab %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    VTD_Schweregrad = "Schweregrad",
    n = md("*n*"),
    Prozent = "%"
  ) %>%
  
  # Prozentwerte mit zwei Nachkommastellen
  
  fmt_number(
    columns = Prozent,
    decimals = 2,
    use_seps = FALSE
  ) %>%
  
  # Häufigkeiten ohne Nachkommastellen
  
  fmt_integer(
    columns = n,
    use_seps = FALSE
  ) %>%
  
  # Textspalte linksbündig
  
  cols_align(
    align = "left",
    columns = VTD_Schweregrad
  ) %>%
  
  # Zahlenspalten rechtsbündig
  
  cols_align(
    align = "right",
    columns = c(n, Prozent)
  ) %>%
  
  # Kompakte Spaltenbreiten
  
  cols_width(
    VTD_Schweregrad ~ px(320),
    n ~ px(60),
    Prozent ~ px(75)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Kategorie
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = VTD_Schweregrad == "Schwergradige stimmliche Beeinträchtigung"
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(455),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

vtd_schweregrad_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = vtd_schweregrad_gt,
  filename = "Tab_VTD_Schweregrade.png",
  zoom = 3,
  expand = 5
)

# -----------------------------------------------
# 21.4.8 VTD – Einzelitems + Reliabilität
# -----------------------------------------------

# Item-Beschriftungen aus dem Fragebogen
vtd_labels <- c(
  "Brennen",
  "Enge",
  "Trockenheit",
  "Schmerzen",
  "Kitzeln",
  "Wundheit",
  "Gereiztheit",
  "Kloßgefühl im Hals"
)


# -----------------------------------------------
# 21.4.8.1 VTD-Häufigkeit – Einzelitems
# -----------------------------------------------

vtd_freq_items_desc <- data.frame(
  Item = vtd_labels,
  Mittelwert = round(
    colMeans(daten_vtd_freq[, vtd_freq_items], na.rm = TRUE),
    2
  ),
  SD = round(
    apply(daten_vtd_freq[, vtd_freq_items], 2, sd, na.rm = TRUE),
    2
  ),
  Minimum = apply(
    daten_vtd_freq[, vtd_freq_items],
    2,
    min,
    na.rm = TRUE
  ),
  Maximum = apply(
    daten_vtd_freq[, vtd_freq_items],
    2,
    max,
    na.rm = TRUE
  )
)

vtd_freq_items_desc

# Nach Mittelwert sortiert
vtd_freq_items_desc_sortiert <- vtd_freq_items_desc %>%
  arrange(desc(Mittelwert))

vtd_freq_items_desc_sortiert


# -----------------------------------------------
# 21.4.8.2 Reliabilität VTD-Häufigkeit
# -----------------------------------------------

library(psych)

alpha(daten_vtd_freq[, vtd_freq_items])


# -----------------------------------------------
# 21.4.8.3 VTD-Schweregrad – Einzelitems
# -----------------------------------------------

vtd_sev_items_desc <- data.frame(
  Item = vtd_labels,
  Mittelwert = round(
    colMeans(daten_vtd_sev[, vtd_sev_items], na.rm = TRUE),
    2
  ),
  SD = round(
    apply(daten_vtd_sev[, vtd_sev_items], 2, sd, na.rm = TRUE),
    2
  ),
  Minimum = apply(
    daten_vtd_sev[, vtd_sev_items],
    2,
    min,
    na.rm = TRUE
  ),
  Maximum = apply(
    daten_vtd_sev[, vtd_sev_items],
    2,
    max,
    na.rm = TRUE
  )
)

vtd_sev_items_desc

# Nach Mittelwert sortiert
vtd_sev_items_desc_sortiert <- vtd_sev_items_desc %>%
  arrange(desc(Mittelwert))

vtd_sev_items_desc_sortiert


# -----------------------------------------------
# 21.4.8.4 Reliabilität VTD-Schweregrad
# -----------------------------------------------

alpha(daten_vtd_sev[, vtd_sev_items])

# -----------------------------------------------

# 21.5 VTD – Boxplot VTD-Gesamtscore

# -----------------------------------------------

# Boxplot erstellen

boxplot_vtd_gesamt <- ggplot(
  daten_vtd_gesamt,
  aes(
    x = "",
    y = VTD_gesamt
  )
) +
  geom_boxplot(
    width = 0.35,
    fill = "white",
    colour = "black",
    linewidth = 0.35,
    outlier.shape = 16,
    outlier.size = 1.5
  ) +
  scale_y_continuous(
    limits = c(0, 96),
    breaks = seq(0, 96, by = 12),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = NULL,
    y = "VTD-Gesamtscore"
  ) +
  theme_classic(
    base_size = 18,
    base_family = "Times New Roman"
  ) +
  theme(
    
    # Gesamte Schrift in Times New Roman
    
    text = element_text(
      family = "Times New Roman"
    ),
    
    # Linien der x- und y-Achse
    
    axis.line = element_line(
      colour = "black",
      linewidth = 0.35
    ),
    
    # Markierungen der y-Achse
    
    axis.ticks.y = element_line(
      colour = "black",
      linewidth = 0.35
    ),
    
    # Beschriftung der y-Achse
    
    axis.title.y = element_text(
      family = "Times New Roman",
      size = 12,
      lineheight = 0.9,
      margin = margin(r = 10)
    ),
    
    # Zahlen der y-Achse
    
    axis.text.y = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    # Beschriftung und Markierungen der x-Achse ausblenden
    
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    # Einheitliche Ränder
    
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 20,
      l = 25
    )
  )

# Boxplot anzeigen

print(boxplot_vtd_gesamt)

# Boxplot als hochauflösende PNG-Datei speichern

ggsave(
  filename = "Abb_VTD_Gesamtscore_Boxplot.png",
  plot = boxplot_vtd_gesamt,
  width = 12,
  height = 10,
  units = "cm",
  dpi = 300,
  bg = "white",
  device = "png"
)

# -----------------------------------------------
# 21.6 VTD × Alter
# Häufigkeit und Schweregrad getrennt
# -----------------------------------------------

# -----------------------------------------------
# 21.6.1 VTD-Häufigkeit × Alter
# Spearman-Korrelation
# -----------------------------------------------

# Spearman-Korrelation
cor_vtd_freq_alter_spearman <- cor.test(
  daten_vtd_freq$P002_01,
  daten_vtd_freq$VTD_freq,
  method = "spearman"
)

cor_vtd_freq_alter_spearman

# -----------------------------------------------
# 21.6.2 VTD-Schweregrad × Alter
# Spearman-Korrelation
# -----------------------------------------------

# Spearman-Korrelation
cor_vtd_sev_alter_spearman <- cor.test(
  daten_vtd_sev$P002_01,
  daten_vtd_sev$VTD_sev,
  method = "spearman"
)

cor_vtd_sev_alter_spearman

# -----------------------------------------------
# 21.6.3 VTD-Gesamtscore × Alter
# Spearman-Korrelation
# -----------------------------------------------

cor_vtd_gesamt_alter_spearman <- cor.test(
  daten_vtd_gesamt$P002_01,
  daten_vtd_gesamt$VTD_gesamt,
  method = "spearman"
)

cor_vtd_gesamt_alter_spearman

# -----------------------------------------------
# 21.7 VTD × Berufserfahrung
# Spearman-Korrelationen:
# Häufigkeit, Schweregrad und Gesamtscore
# -----------------------------------------------

# -----------------------------------------------
# 21.7.1 Hilfsfunktion für Spearman-Korrelation
# -----------------------------------------------

spearman_vtd_beruf <- function(
    daten,
    vtd_variable,
    bezeichnung
) {
  
  # Nur Fälle mit vollständigen Werten verwenden
  analyse_daten <- daten[
    complete.cases(
      daten$P007_07,
      daten[[vtd_variable]]
    ),
  ]
  
  # Spearman-Korrelation berechnen
  test <- cor.test(
    analyse_daten$P007_07,
    analyse_daten[[vtd_variable]],
    method = "spearman",
    exact = FALSE
  )
  
  # Ergebnisse übersichtlich zurückgeben
  ergebnis <- data.frame(
    Zusammenhang = bezeichnung,
    n = nrow(analyse_daten),
    rho = unname(test$estimate),
    p_Wert = test$p.value
  )
  
  return(
    list(
      daten = analyse_daten,
      test = test,
      ergebnis = ergebnis
    )
  )
}


# -----------------------------------------------
# 21.7.2 VTD-Häufigkeit × Berufserfahrung
# -----------------------------------------------

ergebnis_vtd_freq_beruf <- spearman_vtd_beruf(
  daten = daten_vtd_freq,
  vtd_variable = "VTD_freq",
  bezeichnung = "Berufserfahrung × VTD-Häufigkeit"
)

# Spearman-Test vollständig anzeigen
ergebnis_vtd_freq_beruf$test

# Kompakte Ergebnisausgabe
ergebnis_vtd_freq_beruf$ergebnis

# Deskriptive Übersichten
summary(ergebnis_vtd_freq_beruf$daten$P007_07)
summary(ergebnis_vtd_freq_beruf$daten$VTD_freq)

# Scatterplot
plot(
  ergebnis_vtd_freq_beruf$daten$P007_07,
  ergebnis_vtd_freq_beruf$daten$VTD_freq,
  xlab = "Berufserfahrung (Jahre)",
  ylab = "VTD-Häufigkeit",
  main = "Zusammenhang zwischen Berufserfahrung und VTD-Häufigkeit"
)

# Lineare Trendlinie nur zur visuellen Orientierung
abline(
  lm(
    VTD_freq ~ P007_07,
    data = ergebnis_vtd_freq_beruf$daten
  ),
  col = "red"
)


# -----------------------------------------------
# 21.7.3 VTD-Schweregrad × Berufserfahrung
# -----------------------------------------------

ergebnis_vtd_sev_beruf <- spearman_vtd_beruf(
  daten = daten_vtd_sev,
  vtd_variable = "VTD_sev",
  bezeichnung = "Berufserfahrung × VTD-Schweregrad"
)

# Spearman-Test vollständig anzeigen
ergebnis_vtd_sev_beruf$test

# Kompakte Ergebnisausgabe
ergebnis_vtd_sev_beruf$ergebnis

# Deskriptive Übersichten
summary(ergebnis_vtd_sev_beruf$daten$P007_07)
summary(ergebnis_vtd_sev_beruf$daten$VTD_sev)

# Scatterplot
plot(
  ergebnis_vtd_sev_beruf$daten$P007_07,
  ergebnis_vtd_sev_beruf$daten$VTD_sev,
  xlab = "Berufserfahrung (Jahre)",
  ylab = "VTD-Schweregrad",
  main = "Zusammenhang zwischen Berufserfahrung und VTD-Schweregrad"
)

# Lineare Trendlinie nur zur visuellen Orientierung
abline(
  lm(
    VTD_sev ~ P007_07,
    data = ergebnis_vtd_sev_beruf$daten
  ),
  col = "red"
)


# -----------------------------------------------
# 21.7.4 VTD-Gesamtscore × Berufserfahrung
# -----------------------------------------------

ergebnis_vtd_gesamt_beruf <- spearman_vtd_beruf(
  daten = daten_vtd_gesamt,
  vtd_variable = "VTD_gesamt",
  bezeichnung = "Berufserfahrung × VTD-Gesamtscore"
)

# Spearman-Test vollständig anzeigen
ergebnis_vtd_gesamt_beruf$test

# Kompakte Ergebnisausgabe
ergebnis_vtd_gesamt_beruf$ergebnis

# Deskriptive Übersichten
summary(ergebnis_vtd_gesamt_beruf$daten$P007_07)
summary(ergebnis_vtd_gesamt_beruf$daten$VTD_gesamt)

# -----------------------------------------------
# 21.7.5 Gemeinsame Ergebnistabelle
# -----------------------------------------------

tabelle_vtd_beruf <- rbind(
  ergebnis_vtd_freq_beruf$ergebnis,
  ergebnis_vtd_sev_beruf$ergebnis,
  ergebnis_vtd_gesamt_beruf$ergebnis
)

tabelle_vtd_beruf <- tabelle_vtd_beruf %>%
  mutate(
    rho = round(rho, 3),
    p = ifelse(
      p_Wert < .001,
      "< .001",
      sprintf("%.3f", p_Wert)
    )
  ) %>%
  select(
    Zusammenhang,
    n,
    `Spearman-ρ` = rho,
    p
  )

tabelle_vtd_beruf

# -----------------------------------------------
# 21.8 VTD × Sprechanteil im Training (BA02)
# Häufigkeit und Schweregrad getrennt
# -----------------------------------------------

# -----------------------------------------------
# 21.8.1 VTD-Häufigkeit × Sprechanteil im Training
# -----------------------------------------------

vtd_freq_sprech_training <- daten_vtd_freq %>%
  filter(!is.na(BA02)) %>%
  mutate(
    BA02 = as.character(BA02)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA02", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA02" = "RESPONSE")
  ) %>%
  rename(Sprechanteil = MEANING)

# Reihenfolge festlegen
vtd_freq_sprech_training <- vtd_freq_sprech_training %>%
  mutate(
    Sprechanteil = factor(
      Sprechanteil,
      levels = c(
        "0-15 Minuten",
        "16-30 Minuten",
        "31-45 Minuten",
        "46-60 Minuten",
        "61-75 Minuten",
        "76-90 Minuten",
        "91-105 Minuten",
        "106-120 Minuten",
        "Mehr als 120 Minuten"
      ),
      ordered = TRUE
    ),
    BA02_rank = as.numeric(Sprechanteil)
  )

# Deskriptive Tabelle
vtd_freq_sprech_training_tab <- vtd_freq_sprech_training %>%
  group_by(Sprechanteil) %>%
  summarise(
    n = n(),
    Median = median(VTD_freq, na.rm = TRUE),
    Mittelwert = round(mean(VTD_freq, na.rm = TRUE), 2),
    SD = round(sd(VTD_freq, na.rm = TRUE), 2),
    Minimum = min(VTD_freq, na.rm = TRUE),
    Maximum = max(VTD_freq, na.rm = TRUE),
    .groups = "drop"
  )

vtd_freq_sprech_training_tab

# Spearman-Korrelation
spearman_vtd_freq_sprech_training <- cor.test(
  vtd_freq_sprech_training$BA02_rank,
  vtd_freq_sprech_training$VTD_freq,
  method = "spearman",
  exact = FALSE
)

spearman_vtd_freq_sprech_training

# Plot
library(ggplot2)

ggplot(
  vtd_freq_sprech_training,
  aes(x = BA02_rank, y = VTD_freq)
) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_continuous(
    breaks = 1:9,
    labels = c(
      "0-15",
      "16-30",
      "31-45",
      "46-60",
      "61-75",
      "76-90",
      "91-105",
      "106-120",
      ">120"
    )
  ) +
  labs(
    title = "VTD-Häufigkeit × Sprechanteil im Training",
    x = "Sprechanteil im Training",
    y = "VTD-Häufigkeit"
  ) +
  theme_minimal()

# -----------------------------------------------
# 21.8.2 VTD-Schweregrad × Sprechanteil im Training
# -----------------------------------------------

vtd_sev_sprech_training <- daten_vtd_sev %>%
  filter(!is.na(BA02)) %>%
  mutate(
    BA02 = as.character(BA02)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA02", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA02" = "RESPONSE")
  ) %>%
  rename(Sprechanteil = MEANING)

# Reihenfolge festlegen
vtd_sev_sprech_training <- vtd_sev_sprech_training %>%
  mutate(
    Sprechanteil = factor(
      Sprechanteil,
      levels = c(
        "0-15 Minuten",
        "16-30 Minuten",
        "31-45 Minuten",
        "46-60 Minuten",
        "61-75 Minuten",
        "76-90 Minuten",
        "91-105 Minuten",
        "106-120 Minuten",
        "Mehr als 120 Minuten"
      ),
      ordered = TRUE
    ),
    BA02_rank = as.numeric(Sprechanteil)
  )

# Deskriptive Tabelle
vtd_sev_sprech_training_tab <- vtd_sev_sprech_training %>%
  group_by(Sprechanteil) %>%
  summarise(
    n = n(),
    Median = median(VTD_sev, na.rm = TRUE),
    Mittelwert = round(mean(VTD_sev, na.rm = TRUE), 2),
    SD = round(sd(VTD_sev, na.rm = TRUE), 2),
    Minimum = min(VTD_sev, na.rm = TRUE),
    Maximum = max(VTD_sev, na.rm = TRUE),
    .groups = "drop"
  )

vtd_sev_sprech_training_tab

# Spearman-Korrelation
spearman_vtd_sev_sprech_training <- cor.test(
  vtd_sev_sprech_training$BA02_rank,
  vtd_sev_sprech_training$VTD_sev,
  method = "spearman",
  exact = FALSE
)

spearman_vtd_sev_sprech_training


# Plot
ggplot(
  vtd_sev_sprech_training,
  aes(x = BA02_rank, y = VTD_sev)
) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_continuous(
    breaks = 1:9,
    labels = c(
      "0-15",
      "16-30",
      "31-45",
      "46-60",
      "61-75",
      "76-90",
      "91-105",
      "106-120",
      ">120"
    )
  ) +
  labs(
    title = "VTD-Schweregrad × Sprechanteil im Training",
    x = "Sprechanteil im Training",
    y = "VTD-Schweregrad"
  ) +
  theme_minimal()

# -----------------------------------------------
# 21.8.3 VTD-Gesamtscore × Sprechanteil im Training
# -----------------------------------------------

vtd_gesamt_sprech_training <- daten_vtd_gesamt %>%
  filter(!is.na(BA02)) %>%
  mutate(
    BA02 = as.character(BA02)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA02", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA02" = "RESPONSE")
  ) %>%
  rename(Sprechanteil = MEANING)

# Reihenfolge festlegen
vtd_gesamt_sprech_training <- vtd_gesamt_sprech_training %>%
  mutate(
    Sprechanteil = factor(
      Sprechanteil,
      levels = c(
        "0-15 Minuten",
        "16-30 Minuten",
        "31-45 Minuten",
        "46-60 Minuten",
        "61-75 Minuten",
        "76-90 Minuten",
        "91-105 Minuten",
        "106-120 Minuten",
        "Mehr als 120 Minuten"
      ),
      ordered = TRUE
    ),
    BA02_rank = as.numeric(Sprechanteil)
  )

# Deskriptive Tabelle
vtd_gesamt_sprech_training_tab <- vtd_gesamt_sprech_training %>%
  group_by(Sprechanteil) %>%
  summarise(
    n = n(),
    Median = median(VTD_gesamt, na.rm = TRUE),
    Mittelwert = round(mean(VTD_gesamt, na.rm = TRUE), 2),
    SD = round(sd(VTD_gesamt, na.rm = TRUE), 2),
    Minimum = min(VTD_gesamt, na.rm = TRUE),
    Maximum = max(VTD_gesamt, na.rm = TRUE),
    .groups = "drop"
  )

vtd_gesamt_sprech_training_tab

# Spearman-Korrelation
spearman_vtd_gesamt_sprech_training <- cor.test(
  vtd_gesamt_sprech_training$BA02_rank,
  vtd_gesamt_sprech_training$VTD_gesamt,
  method = "spearman",
  exact = FALSE
)

spearman_vtd_gesamt_sprech_training

# -----------------------------------------------
# 21.9 VTD × Sprechanteil im Spiel (BA05)
# Häufigkeit und Schweregrad getrennt
# -----------------------------------------------

# -----------------------------------------------
# 21.9.1 VTD-Häufigkeit × Sprechanteil im Spiel
# -----------------------------------------------

vtd_freq_sprech_spiel <- daten_vtd_freq %>%
  filter(!is.na(BA05)) %>%
  mutate(
    BA05 = as.character(BA05)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA05", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA05" = "RESPONSE")
  ) %>%
  rename(Sprechanteil = MEANING)

# Reihenfolge festlegen und Rangcodierung erstellen
vtd_freq_sprech_spiel <- vtd_freq_sprech_spiel %>%
  mutate(
    Sprechanteil = factor(
      Sprechanteil,
      levels = c(
        "0-15 Minuten",
        "16-30 Minuten",
        "31-45 Minuten",
        "46-60 Minuten",
        "61-75 Minuten",
        "76-90 Minuten",
        "91-105 Minuten",
        "106-120 Minuten",
        "Mehr als 120 Minuten"
      ),
      ordered = TRUE
    ),
    BA05_rank = as.numeric(Sprechanteil)
  )

# Kontrolle der Rangcodierung
vtd_freq_sprech_spiel %>%
  distinct(Sprechanteil, BA05_rank) %>%
  arrange(BA05_rank)

# Deskriptive Tabelle
vtd_freq_sprech_spiel_tab <- vtd_freq_sprech_spiel %>%
  group_by(Sprechanteil) %>%
  summarise(
    n = n(),
    Median = median(VTD_freq, na.rm = TRUE),
    Mittelwert = round(mean(VTD_freq, na.rm = TRUE), 2),
    SD = round(sd(VTD_freq, na.rm = TRUE), 2),
    Minimum = min(VTD_freq, na.rm = TRUE),
    Maximum = max(VTD_freq, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Sprechanteil)

vtd_freq_sprech_spiel_tab

# Spearman-Korrelation
spearman_vtd_freq_sprech_spiel <- cor.test(
  vtd_freq_sprech_spiel$BA05_rank,
  vtd_freq_sprech_spiel$VTD_freq,
  method = "spearman",
  exact = FALSE
)

spearman_vtd_freq_sprech_spiel

# -----------------------------------------------
# 21.9.2 VTD-Schweregrad × Sprechanteil im Spiel
# -----------------------------------------------

vtd_sev_sprech_spiel <- daten_vtd_sev %>%
  filter(!is.na(BA05)) %>%
  mutate(
    BA05 = as.character(BA05)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA05", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA05" = "RESPONSE")
  ) %>%
  rename(Sprechanteil = MEANING)

# Reihenfolge festlegen und Rangcodierung erstellen
vtd_sev_sprech_spiel <- vtd_sev_sprech_spiel %>%
  mutate(
    Sprechanteil = factor(
      Sprechanteil,
      levels = c(
        "0-15 Minuten",
        "16-30 Minuten",
        "31-45 Minuten",
        "46-60 Minuten",
        "61-75 Minuten",
        "76-90 Minuten",
        "91-105 Minuten",
        "106-120 Minuten",
        "Mehr als 120 Minuten"
      ),
      ordered = TRUE
    ),
    BA05_rank = as.numeric(Sprechanteil)
  )

# Kontrolle der Rangcodierung
vtd_sev_sprech_spiel %>%
  distinct(Sprechanteil, BA05_rank) %>%
  arrange(BA05_rank)

# Deskriptive Tabelle
vtd_sev_sprech_spiel_tab <- vtd_sev_sprech_spiel %>%
  group_by(Sprechanteil) %>%
  summarise(
    n = n(),
    Median = median(VTD_sev, na.rm = TRUE),
    Mittelwert = round(mean(VTD_sev, na.rm = TRUE), 2),
    SD = round(sd(VTD_sev, na.rm = TRUE), 2),
    Minimum = min(VTD_sev, na.rm = TRUE),
    Maximum = max(VTD_sev, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Sprechanteil)

vtd_sev_sprech_spiel_tab

# Spearman-Korrelation
spearman_vtd_sev_sprech_spiel <- cor.test(
  vtd_sev_sprech_spiel$BA05_rank,
  vtd_sev_sprech_spiel$VTD_sev,
  method = "spearman",
  exact = FALSE
)

spearman_vtd_sev_sprech_spiel

# -----------------------------------------------
# 21.9.3 VTD-Gesamtscore × Sprechanteil im Spiel
# -----------------------------------------------

vtd_gesamt_sprech_spiel <- daten_vtd_gesamt %>%
  filter(!is.na(BA05)) %>%
  mutate(
    BA05 = as.character(BA05)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA05", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA05" = "RESPONSE")
  ) %>%
  rename(Sprechanteil = MEANING)

# Reihenfolge festlegen und Rangcodierung erstellen
vtd_gesamt_sprech_spiel <- vtd_gesamt_sprech_spiel %>%
  mutate(
    Sprechanteil = factor(
      Sprechanteil,
      levels = c(
        "0-15 Minuten",
        "16-30 Minuten",
        "31-45 Minuten",
        "46-60 Minuten",
        "61-75 Minuten",
        "76-90 Minuten",
        "91-105 Minuten",
        "106-120 Minuten",
        "Mehr als 120 Minuten"
      ),
      ordered = TRUE
    ),
    BA05_rank = as.numeric(Sprechanteil)
  )

# Kontrolle der Rangcodierung
vtd_gesamt_sprech_spiel %>%
  distinct(Sprechanteil, BA05_rank) %>%
  arrange(BA05_rank)

# Deskriptive Tabelle
vtd_gesamt_sprech_spiel_tab <- vtd_gesamt_sprech_spiel %>%
  group_by(Sprechanteil) %>%
  summarise(
    n = n(),
    Median = median(VTD_gesamt, na.rm = TRUE),
    Mittelwert = round(mean(VTD_gesamt, na.rm = TRUE), 2),
    SD = round(sd(VTD_gesamt, na.rm = TRUE), 2),
    Minimum = min(VTD_gesamt, na.rm = TRUE),
    Maximum = max(VTD_gesamt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Sprechanteil)

vtd_gesamt_sprech_spiel_tab

# Spearman-Korrelation
spearman_vtd_gesamt_sprech_spiel <- cor.test(
  vtd_gesamt_sprech_spiel$BA05_rank,
  vtd_gesamt_sprech_spiel$VTD_gesamt,
  method = "spearman",
  exact = FALSE
)

spearman_vtd_gesamt_sprech_spiel

# -----------------------------------------------
# 21.10 VTD × Lautstärke im Training (BA04)
# Häufigkeit und Schweregrad getrennt
# -----------------------------------------------

# -----------------------------------------------
# 21.10.1 VTD-Häufigkeit × Lautstärke im Training
# -----------------------------------------------

vtd_freq_laut_training <- daten_vtd_freq %>%
  filter(!is.na(BA04)) %>%
  mutate(
    BA04 = as.character(BA04)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA04", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA04" = "RESPONSE")
  ) %>%
  rename(Lautstärke = MEANING)

# Reihenfolge festlegen und Rangcodierung erstellen
vtd_freq_laut_training <- vtd_freq_laut_training %>%
  mutate(
    Lautstärke = factor(
      Lautstärke,
      levels = c("Nie", "Selten", "Gelegentlich", "Oft", "Immer"),
      ordered = TRUE
    ),
    BA04_rank = as.numeric(Lautstärke)
  )

# Kontrolle
table(vtd_freq_laut_training$Lautstärke, useNA = "ifany")

# Kontrolle der Rangcodierung
vtd_freq_laut_training %>%
  distinct(Lautstärke, BA04_rank) %>%
  arrange(BA04_rank)

# Deskriptive Tabelle
vtd_freq_laut_training_tab <- vtd_freq_laut_training %>%
  group_by(Lautstärke) %>%
  summarise(
    n = n(),
    Median = median(VTD_freq, na.rm = TRUE),
    Mittelwert = round(mean(VTD_freq, na.rm = TRUE), 2),
    SD = round(sd(VTD_freq, na.rm = TRUE), 2),
    Minimum = min(VTD_freq, na.rm = TRUE),
    Maximum = max(VTD_freq, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Lautstärke)

vtd_freq_laut_training_tab

# Spearman-Korrelation
spearman_vtd_freq_laut_training <- cor.test(
  vtd_freq_laut_training$BA04_rank,
  vtd_freq_laut_training$VTD_freq,
  method = "spearman",
  exact = FALSE
)

spearman_vtd_freq_laut_training

# -----------------------------------------------
# 21.10.2 VTD-Schweregrad × Lautstärke im Training
# -----------------------------------------------

vtd_sev_laut_training <- daten_vtd_sev %>%
  filter(!is.na(BA04)) %>%
  mutate(
    BA04 = as.character(BA04)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA04", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA04" = "RESPONSE")
  ) %>%
  rename(Lautstärke = MEANING)

# Reihenfolge festlegen und Rangcodierung erstellen
vtd_sev_laut_training <- vtd_sev_laut_training %>%
  mutate(
    Lautstärke = factor(
      Lautstärke,
      levels = c("Nie", "Selten", "Gelegentlich", "Oft", "Immer"),
      ordered = TRUE
    ),
    BA04_rank = as.numeric(Lautstärke)
  )

# Kontrolle
table(vtd_sev_laut_training$Lautstärke, useNA = "ifany")

# Kontrolle der Rangcodierung
vtd_sev_laut_training %>%
  distinct(Lautstärke, BA04_rank) %>%
  arrange(BA04_rank)

# Deskriptive Tabelle
vtd_sev_laut_training_tab <- vtd_sev_laut_training %>%
  group_by(Lautstärke) %>%
  summarise(
    n = n(),
    Median = median(VTD_sev, na.rm = TRUE),
    Mittelwert = round(mean(VTD_sev, na.rm = TRUE), 2),
    SD = round(sd(VTD_sev, na.rm = TRUE), 2),
    Minimum = min(VTD_sev, na.rm = TRUE),
    Maximum = max(VTD_sev, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Lautstärke)

vtd_sev_laut_training_tab

# Spearman-Korrelation
spearman_vtd_sev_laut_training <- cor.test(
  vtd_sev_laut_training$BA04_rank,
  vtd_sev_laut_training$VTD_sev,
  method = "spearman",
  exact = FALSE
)

# -----------------------------------------------
# 21.10.3 VTD-Gesamtscore × Lautstärke im Training
# -----------------------------------------------

vtd_gesamt_laut_training <- daten_vtd_gesamt %>%
  filter(!is.na(BA04)) %>%
  mutate(
    BA04 = as.character(BA04)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA04", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA04" = "RESPONSE")
  ) %>%
  rename(Lautstärke = MEANING)

# Reihenfolge festlegen und Rangcodierung erstellen
vtd_gesamt_laut_training <- vtd_gesamt_laut_training %>%
  mutate(
    Lautstärke = factor(
      Lautstärke,
      levels = c("Nie", "Selten", "Gelegentlich", "Oft", "Immer"),
      ordered = TRUE
    ),
    BA04_rank = as.numeric(Lautstärke)
  )

# Kontrolle
table(vtd_gesamt_laut_training$Lautstärke, useNA = "ifany")

# Kontrolle der Rangcodierung
vtd_gesamt_laut_training %>%
  distinct(Lautstärke, BA04_rank) %>%
  arrange(BA04_rank)

# Deskriptive Tabelle
vtd_gesamt_laut_training_tab <- vtd_gesamt_laut_training %>%
  group_by(Lautstärke) %>%
  summarise(
    n = n(),
    Median = median(VTD_gesamt, na.rm = TRUE),
    Mittelwert = round(mean(VTD_gesamt, na.rm = TRUE), 2),
    SD = round(sd(VTD_gesamt, na.rm = TRUE), 2),
    Minimum = min(VTD_gesamt, na.rm = TRUE),
    Maximum = max(VTD_gesamt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Lautstärke)

vtd_gesamt_laut_training_tab

# Spearman-Korrelation
spearman_vtd_gesamt_laut_training <- cor.test(
  vtd_gesamt_laut_training$BA04_rank,
  vtd_gesamt_laut_training$VTD_gesamt,
  method = "spearman",
  exact = FALSE
)

spearman_vtd_gesamt_laut_training

# -----------------------------------------------
# 21.11 VTD × Lautstärke im Spiel (BA06)
# Häufigkeit und Schweregrad getrennt
# -----------------------------------------------

# -----------------------------------------------
# 21.11.1 VTD-Häufigkeit × Lautstärke im Spiel
# -----------------------------------------------

vtd_freq_laut_spiel <- daten_vtd_freq %>%
  filter(!is.na(BA06)) %>%
  mutate(
    BA06 = as.character(BA06)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA06", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA06" = "RESPONSE")
  ) %>%
  rename(Lautstärke = MEANING)

# Reihenfolge festlegen und Rangcodierung erstellen
vtd_freq_laut_spiel <- vtd_freq_laut_spiel %>%
  mutate(
    Lautstärke = factor(
      Lautstärke,
      levels = c("Nie", "Selten", "Gelegentlich", "Oft", "Immer"),
      ordered = TRUE
    ),
    BA06_rank = as.numeric(Lautstärke)
  )

# Kontrolle
table(vtd_freq_laut_spiel$Lautstärke, useNA = "ifany")

# Kontrolle der Rangcodierung
vtd_freq_laut_spiel %>%
  distinct(Lautstärke, BA06_rank) %>%
  arrange(BA06_rank)

# Deskriptive Tabelle
vtd_freq_laut_spiel_tab <- vtd_freq_laut_spiel %>%
  group_by(Lautstärke) %>%
  summarise(
    n = n(),
    Median = median(VTD_freq, na.rm = TRUE),
    Mittelwert = round(mean(VTD_freq, na.rm = TRUE), 2),
    SD = round(sd(VTD_freq, na.rm = TRUE), 2),
    Minimum = min(VTD_freq, na.rm = TRUE),
    Maximum = max(VTD_freq, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Lautstärke)

vtd_freq_laut_spiel_tab

# Spearman-Korrelation
spearman_vtd_freq_laut_spiel <- cor.test(
  vtd_freq_laut_spiel$BA06_rank,
  vtd_freq_laut_spiel$VTD_freq,
  method = "spearman",
  exact = FALSE
)

spearman_vtd_freq_laut_spiel

# -----------------------------------------------
# 21.11.2 VTD-Schweregrad × Lautstärke im Spiel
# -----------------------------------------------

vtd_sev_laut_spiel <- daten_vtd_sev %>%
  filter(!is.na(BA06)) %>%
  mutate(
    BA06 = as.character(BA06)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA06", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA06" = "RESPONSE")
  ) %>%
  rename(Lautstärke = MEANING)

# Reihenfolge festlegen und Rangcodierung erstellen
vtd_sev_laut_spiel <- vtd_sev_laut_spiel %>%
  mutate(
    Lautstärke = factor(
      Lautstärke,
      levels = c("Nie", "Selten", "Gelegentlich", "Oft", "Immer"),
      ordered = TRUE
    ),
    BA06_rank = as.numeric(Lautstärke)
  )

# Kontrolle
table(vtd_sev_laut_spiel$Lautstärke, useNA = "ifany")

# Kontrolle der Rangcodierung
vtd_sev_laut_spiel %>%
  distinct(Lautstärke, BA06_rank) %>%
  arrange(BA06_rank)

# Deskriptive Tabelle
vtd_sev_laut_spiel_tab <- vtd_sev_laut_spiel %>%
  group_by(Lautstärke) %>%
  summarise(
    n = n(),
    Median = median(VTD_sev, na.rm = TRUE),
    Mittelwert = round(mean(VTD_sev, na.rm = TRUE), 2),
    SD = round(sd(VTD_sev, na.rm = TRUE), 2),
    Minimum = min(VTD_sev, na.rm = TRUE),
    Maximum = max(VTD_sev, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Lautstärke)

vtd_sev_laut_spiel_tab

# Spearman-Korrelation
spearman_vtd_sev_laut_spiel <- cor.test(
  vtd_sev_laut_spiel$BA06_rank,
  vtd_sev_laut_spiel$VTD_sev,
  method = "spearman",
  exact = FALSE
)

spearman_vtd_sev_laut_spiel

# -----------------------------------------------
# 21.11.3 VTD-Gesamtscore × Lautstärke im Spiel
# -----------------------------------------------

vtd_gesamt_laut_spiel <- daten_vtd_gesamt %>%
  filter(!is.na(BA06)) %>%
  mutate(
    BA06 = as.character(BA06)
  ) %>%
  left_join(
    werte %>%
      filter(VAR == "BA06", RESPONSE != "-9") %>%
      select(RESPONSE, MEANING),
    by = c("BA06" = "RESPONSE")
  ) %>%
  rename(Lautstärke = MEANING)

# Reihenfolge festlegen und Rangcodierung erstellen
vtd_gesamt_laut_spiel <- vtd_gesamt_laut_spiel %>%
  mutate(
    Lautstärke = factor(
      Lautstärke,
      levels = c("Nie", "Selten", "Gelegentlich", "Oft", "Immer"),
      ordered = TRUE
    ),
    BA06_rank = as.numeric(Lautstärke)
  )

# Kontrolle
table(vtd_gesamt_laut_spiel$Lautstärke, useNA = "ifany")

# Kontrolle der Rangcodierung
vtd_gesamt_laut_spiel %>%
  distinct(Lautstärke, BA06_rank) %>%
  arrange(BA06_rank)

# Deskriptive Tabelle
vtd_gesamt_laut_spiel_tab <- vtd_gesamt_laut_spiel %>%
  group_by(Lautstärke) %>%
  summarise(
    n = n(),
    Median = median(VTD_gesamt, na.rm = TRUE),
    Mittelwert = round(mean(VTD_gesamt, na.rm = TRUE), 2),
    SD = round(sd(VTD_gesamt, na.rm = TRUE), 2),
    Minimum = min(VTD_gesamt, na.rm = TRUE),
    Maximum = max(VTD_gesamt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Lautstärke)

vtd_gesamt_laut_spiel_tab

# Spearman-Korrelation
spearman_vtd_gesamt_laut_spiel <- cor.test(
  vtd_gesamt_laut_spiel$BA06_rank,
  vtd_gesamt_laut_spiel$VTD_gesamt,
  method = "spearman",
  exact = FALSE
)

spearman_vtd_gesamt_laut_spiel

# -----------------------------------------------------------
# 22. Wunsch nach Stimmcoaching
# -----------------------------------------------------------
# Häufigkeiten für SB12
table(daten$SB12)

# Prozentwerte
prop.table(table(daten$SB12)) * 100

# Deskriptive Kennwerte für SB12
summary(daten$SB12)

mean(daten$SB12, na.rm = TRUE)
sd(daten$SB12, na.rm = TRUE)
median(daten$SB12, na.rm = TRUE)

# -----------------------------------------------------------
# 22.1 Tabelle:
# Wunsch nach Stimmtraining
# -----------------------------------------------------------

# Tabellenbasis erstellen

sb12_tabelle <- daten %>%
  filter(!is.na(SB12)) %>%
  count(SB12) %>%
  mutate(
    Prozent = n / sum(n) * 100,
    Antwort = case_when(
      SB12 == 1 ~ "trifft überhaupt nicht zu",
      SB12 == 2 ~ "trifft eher nicht zu",
      SB12 == 3 ~ "trifft teilweise zu",
      SB12 == 4 ~ "trifft eher zu",
      SB12 == 5 ~ "trifft voll zu"
    ),
    Antwort = factor(
      Antwort,
      levels = c(
        "trifft überhaupt nicht zu",
        "trifft eher nicht zu",
        "trifft teilweise zu",
        "trifft eher zu",
        "trifft voll zu"
      )
    )
  ) %>%
  arrange(Antwort) %>%
  select(Antwort, n, Prozent)

# Tabelle gestalten

sb12_gt <- sb12_tabelle %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Antwort = "Wunsch nach Stimmtraining",
    n = md("*n*"),
    Prozent = "%"
  ) %>%
  
  # Prozentwerte mit zwei Nachkommastellen
  
  fmt_number(
    columns = Prozent,
    decimals = 2,
    use_seps = FALSE
  ) %>%
  
  # Häufigkeiten ohne Nachkommastellen
  
  fmt_integer(
    columns = n,
    use_seps = FALSE
  ) %>%
  
  # Textspalte linksbündig
  
  cols_align(
    align = "left",
    columns = Antwort
  ) %>%
  
  # Zahlenspalten rechtsbündig
  
  cols_align(
    align = "right",
    columns = c(n, Prozent)
  ) %>%
  
  # Kompakte Spaltenbreiten
  
  cols_width(
    Antwort ~ px(240),
    n ~ px(60),
    Prozent ~ px(75)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Kategorie
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Antwort == "trifft voll zu"
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(375),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

sb12_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = sb12_gt,
  filename = "Tab_Wunsch_nach_Stimmtraining.png",
  zoom = 3,
  expand = 5
)
# -----------------------------------------------------------
# 22.2 Spearman-Korrelationen:
# FESS-Subskalen × Wunsch nach Stimmtraining
# -----------------------------------------------------------

# Datensatz vorbereiten: nur gültige Werte
daten_fess_sb12 <- daten_fess %>%
  filter(
    !is.na(SB12),
    SB12 != -9,
    !is.na(FESS_Beziehung),
    !is.na(FESS_Bewusstheit),
    !is.na(FESS_Emotion)
  )

# -----------------------------------------------------------
# 22.2.1 FESS Beziehung × Wunsch Stimmtraining
# -----------------------------------------------------------

cor.test(
  daten_fess_sb12$FESS_Beziehung,
  daten_fess_sb12$SB12,
  method = "spearman",
  exact = FALSE
)

# -----------------------------------------------------------
# 22.2.2 FESS Bewusstheit × Wunsch Stimmtraining
# -----------------------------------------------------------

cor.test(
  daten_fess_sb12$FESS_Bewusstheit,
  daten_fess_sb12$SB12,
  method = "spearman",
  exact = FALSE
)

# -----------------------------------------------------------
# 22.2.3 FESS Emotion × Wunsch Stimmtraining
# -----------------------------------------------------------

cor.test(
  daten_fess_sb12$FESS_Emotion,
  daten_fess_sb12$SB12,
  method = "spearman",
  exact = FALSE
)

# -----------------------------------------------------------
# 22.3 Spearman-Korrelation: Wunsch Stimmtraining X VHI-9i
# -----------------------------------------------------------

# Nur gültige Werte verwenden
daten_vhi_sb12 <- daten_vhi %>%
  filter(
    !is.na(VHI_gesamt),
    !is.na(SB12),
    SB12 != -9
  )

# Spearman-Korrelation berechnen
cor.test(
  daten_vhi_sb12$VHI_gesamt,
  daten_vhi_sb12$SB12,
  method = "spearman",
  exact = FALSE
)

# -----------------------------------------------------------
# 22.4 Wunsch nach Stimmtraining × VTD
# -----------------------------------------------------------

# -----------------------------------------------------------
# 22.4.1 Wunsch nach Stimmtraining × VTD-Häufigkeit
# -----------------------------------------------------------

# Datensatz vorbereiten
daten_vtd_freq_sb12 <- daten_vtd_freq %>%
  filter(
    !is.na(VTD_freq),
    !is.na(SB12),
    SB12 != -9
  )

# Spearman-Korrelation
cor_vtd_freq_sb12 <- cor.test(
  daten_vtd_freq_sb12$VTD_freq,
  daten_vtd_freq_sb12$SB12,
  method = "spearman",
  exact = FALSE
)

cor_vtd_freq_sb12

# -----------------------------------------------------------
# 22.4.2 Wunsch nach Stimmtraining × VTD-Schweregrad
# -----------------------------------------------------------

# Datensatz vorbereiten
daten_vtd_sev_sb12 <- daten_vtd_sev %>%
  filter(
    !is.na(VTD_sev),
    !is.na(SB12),
    SB12 != -9
  )

# Spearman-Korrelation
cor_vtd_sev_sb12 <- cor.test(
  daten_vtd_sev_sb12$VTD_sev,
  daten_vtd_sev_sb12$SB12,
  method = "spearman",
  exact = FALSE
)

cor_vtd_sev_sb12

# -----------------------------------------------------------
# 22.4.3 Wunsch nach Stimmtraining × VTD-Gesamtscore
# -----------------------------------------------------------

# Datensatz vorbereiten
daten_vtd_gesamt_sb12 <- daten_vtd_gesamt %>%
  filter(
    !is.na(VTD_gesamt),
    !is.na(SB12),
    SB12 != -9
  )

# Spearman-Korrelation
cor_vtd_gesamt_sb12 <- cor.test(
  daten_vtd_gesamt_sb12$VTD_gesamt,
  daten_vtd_gesamt_sb12$SB12,
  method = "spearman",
  exact = FALSE
)

cor_vtd_gesamt_sb12


# ----------------------------------------------------------
# 22.5 Tabellen und Effektplot zu Forschungsfrage 5
# Zusammenhang mit Wunsch nach Stimmtraining
# ----------------------------------------------------------

library(dplyr)
library(gt)
library(ggplot2)

# ----------------------------------------------------------
# 22.5.1 Datengrundlage für Tabellen und Plot
# ----------------------------------------------------------

ff5_ergebnisse <- tibble(
  Variable = c(
    "VTD-Gesamt",
    "VTD-Häufigkeit",
    "VTD-Schweregrad",
    "VHI-9i",
    "FESS-Beziehung",
    "FESS-Bewusstheit",
    "FESS-Emotion"
  ),
  
  Spearman_rho = c(
    0.418,
    0.388,
    0.435,
    0.336,
    -0.146,
    0.414,
    0.275
  ),
  
  p_Spearman = c(
    "<0,001",
    "<0,001",
    "<0,001",
    "0,001",
    "0,145",
    "<0,001",
    "0,005"
  ),
  
  Beta = c(
    0.390,
    0.360,
    0.380,
    0.320,
    -0.144,
    0.420,
    0.310
  ),
  
  p_Regression = c(
    "<0,001",
    "<0,001",
    "<0,001",
    "0,001",
    "0,151",
    "<0,001",
    "0,002"
  )
)

# ----------------------------------------------------------
# 22.5.2 Tabelle:
# Spearman-Korrelationen mit dem Wunsch nach Stimmtraining
# ----------------------------------------------------------

ff5_tabelle_spearman <- ff5_ergebnisse %>%
  mutate(
    `Spearman-ρ` = gsub("\\.", ",", sprintf("%.3f", Spearman_rho))
  ) %>%
  select(
    Variable,
    `Spearman-ρ`,
    p = p_Spearman
  )

# Tabelle gestalten

ff5_tabelle_spearman_gt <- ff5_tabelle_spearman %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Variable = "Variable",
    `Spearman-ρ` = md("*ρ*"),
    p = md("*p*")
  ) %>%
  
  # Erste Spalte linksbündig
  
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  
  # Ergebnisspalten zentrieren
  
  cols_align(
    align = "center",
    columns = c(`Spearman-ρ`, p)
  ) %>%
  
  # Kompakte Spaltenbreiten
  
  cols_width(
    Variable ~ px(190),
    `Spearman-ρ` ~ px(80),
    p ~ px(70)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Zeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Variable == "FESS-Emotion"
    )
  ) %>%
  
  # Anmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Angegeben sind Spearman-Rangkorrelationen (*ρ*) mit *p*-Werten."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(340),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

ff5_tabelle_spearman_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = ff5_tabelle_spearman_gt,
  filename = "Tab_FF5_Spearman_Stimmtraining.png",
  zoom = 3,
  expand = 5
)

# ----------------------------------------------------------
# 23. Sprechanteil Spiel vs. Training
# ----------------------------------------------------------
# Test: Wilcoxon-Vorzeichen-Rang-Test
# (gepaarte Stichproben)
# ----------------------------------------------------------

library(dplyr)

# ----------------------------------------------------------
# 23.1. Daten vorbereiten
# ----------------------------------------------------------

sprechanteil_vergleich <- daten %>%
  filter(!is.na(BA02), !is.na(BA05), !is.na(P006_bereinigt)) %>%
  mutate(
    
    # Trainerrolle
    Trainerrolle = recode(
      as.character(P006_bereinigt),
      "1" = "Cheftrainer:in",
      "2" = "Co-Trainer:in",
      "3" = "Torwarttrainer:in",
      "4" = "Andere"
    ),
    
    # --------------------------------------
    # BA02 = Training
    # Rangcodierung
    # --------------------------------------
    
    BA02_rank = case_when(
      BA02 == "6"  ~ 1,  # 0-15 Minuten
      BA02 == "2"  ~ 2,  # 16-30 Minuten
      BA02 == "3"  ~ 3,  # 31-45 Minuten
      BA02 == "9"  ~ 4,  # 46-60 Minuten
      BA02 == "4"  ~ 5,  # 61-75 Minuten
      BA02 == "8"  ~ 6,  # 76-90 Minuten
      BA02 == "10" ~ 7,  # 91-105 Minuten
      BA02 == "5"  ~ 8,  # 106-120 Minuten
      BA02 == "7"  ~ 9   # Mehr als 120 Minuten
    ),
    
    # --------------------------------------
    # BA05 = Spiel
    # Rangcodierung
    # --------------------------------------
    
    BA05_rank = case_when(
      BA05 == "7"  ~ 1,  # 0-15 Minuten
      BA05 == "2"  ~ 2,  # 16-30 Minuten
      BA05 == "3"  ~ 3,  # 31-45 Minuten
      BA05 == "10" ~ 4,  # 46-60 Minuten
      BA05 == "4"  ~ 5,  # 61-75 Minuten
      BA05 == "8"  ~ 6,  # 76-90 Minuten
      BA05 == "9"  ~ 7,  # 91-105 Minuten
      BA05 == "5"  ~ 8,  # 106-120 Minuten
      BA05 == "6"  ~ 9   # Mehr als 120 Minuten
    )
  )

# ----------------------------------------------------------
# 23.2. Gesamtvergleich:
# Spiel vs. Training
# ----------------------------------------------------------

wilcox_gesamt <- wilcox.test(
  sprechanteil_vergleich$BA05_rank,
  sprechanteil_vergleich$BA02_rank,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE
)

wilcox_gesamt

# ----------------------------------------------------------
# 23.3. Cheftrainer:innen
# Spiel vs. Training
# ----------------------------------------------------------

chef_daten <- sprechanteil_vergleich %>%
  filter(Trainerrolle == "Cheftrainer:in")

wilcox_chef <- wilcox.test(
  chef_daten$BA05_rank,
  chef_daten$BA02_rank,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE
)

wilcox_chef

# ----------------------------------------------------------
# 23.4. Co-Trainer:innen
# Spiel vs. Training
# ----------------------------------------------------------

co_daten <- sprechanteil_vergleich %>%
  filter(Trainerrolle == "Co-Trainer:in")

wilcox_co <- wilcox.test(
  co_daten$BA05_rank,
  co_daten$BA02_rank,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE
)

wilcox_co

# ----------------------------------------------------------
# 23.5. Torwarttrainer:innen
# Spiel vs. Training
# ----------------------------------------------------------

torwart_daten <- sprechanteil_vergleich %>%
  filter(Trainerrolle == "Torwarttrainer:in")

wilcox_torwart <- wilcox.test(
  torwart_daten$BA05_rank,
  torwart_daten$BA02_rank,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE
)

wilcox_torwart

# ----------------------------------------------------------
# 23.6. Andere Trainer:innen
# Spiel vs. Training
# ----------------------------------------------------------

andere_daten <- sprechanteil_vergleich %>%
  filter(Trainerrolle == "Andere")

wilcox_andere <- wilcox.test(
  andere_daten$BA05_rank,
  andere_daten$BA02_rank,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE
)

wilcox_andere

# ----------------------------------------------------------
# 23.7 Übersichtstabelle:
# Wilcoxon-Tests Sprechanteil Spiel vs. Training
# nach Trainerrolle
# ----------------------------------------------------------

tabelle_tests_sprech_spiel_training <- tibble(
  Gruppe = c(
    "Gesamt",
    "Cheftrainer:innen",
    "Co-Trainer:innen",
    "Torwarttrainer:innen",
    "Andere"
  ),
  
  n = c(
    nrow(sprechanteil_vergleich),
    nrow(chef_daten),
    nrow(co_daten),
    nrow(torwart_daten),
    nrow(andere_daten)
  ),
  
  Teststatistik = c(
    round(wilcox_gesamt$statistic, 1),
    round(wilcox_chef$statistic, 1),
    round(wilcox_co$statistic, 1),
    round(wilcox_torwart$statistic, 1),
    round(wilcox_andere$statistic, 1)
  ),
  
  p = c(
    ifelse(
      wilcox_gesamt$p.value < .001,
      "< .001",
      sprintf("%.3f", wilcox_gesamt$p.value)
    ),
    ifelse(
      wilcox_chef$p.value < .001,
      "< .001",
      sprintf("%.3f", wilcox_chef$p.value)
    ),
    ifelse(
      wilcox_co$p.value < .001,
      "< .001",
      sprintf("%.3f", wilcox_co$p.value)
    ),
    ifelse(
      wilcox_torwart$p.value < .001,
      "< .001",
      sprintf("%.3f", wilcox_torwart$p.value)
    ),
    ifelse(
      wilcox_andere$p.value < .001,
      "< .001",
      sprintf("%.3f", wilcox_andere$p.value)
    )
  )
)

# Tabelle gestalten

tabelle_tests_sprech_spiel_training_gt <- tabelle_tests_sprech_spiel_training %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Gruppe = "Trainer:innenrolle",
    n = md("*n*"),
    Teststatistik = md("*V*"),
    p = md("*p*")
  ) %>%
  
  # Feste, kompakte Spaltenbreiten
  
  cols_width(
    Gruppe ~ px(170),
    n ~ px(55),
    Teststatistik ~ px(75),
    p ~ px(70)
  ) %>%
  
  # Textspalte linksbündig
  
  cols_align(
    align = "left",
    columns = Gruppe
  ) %>%
  
  # Zahlenspalten rechtsbündig
  
  cols_align(
    align = "right",
    columns = c(n, Teststatistik, p)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Zeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Gruppe == "Andere"
    )
  ) %>%
  
  # Kurze Anmerkung zur gerichteten Testung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Gerichtete Wilcoxon-Vorzeichen-Rang-Tests für gepaarte Stichproben (Spiel > Training)."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(370),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

tabelle_tests_sprech_spiel_training_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_tests_sprech_spiel_training_gt,
  filename = "Tab_Wilcoxon_Sprechanteil_Spiel_vs_Training.png",
  zoom = 3,
  expand = 5
)

# ----------------------------------------------------------
# 24. Anteil erhöhte Lautstärke Spiel vs. Training
# ----------------------------------------------------------
# Test:
# Wilcoxon-Vorzeichen-Rang-Test
# gepaarte Stichproben
# ----------------------------------------------------------

library(dplyr)

# ----------------------------------------------------------
# 24.1. Daten vorbereiten
# ----------------------------------------------------------

lautstaerke_vergleich <- daten %>%
  filter(!is.na(BA04), !is.na(BA06), !is.na(P006_bereinigt)) %>%
  mutate(
    
    # Trainerrolle
    Trainerrolle = recode(
      as.character(P006_bereinigt),
      "1" = "Cheftrainer:in",
      "2" = "Co-Trainer:in",
      "3" = "Torwarttrainer:in",
      "4" = "Andere"
    ),
    
    Trainerrolle = factor(
      Trainerrolle,
      levels = c(
        "Cheftrainer:in",
        "Co-Trainer:in",
        "Torwarttrainer:in",
        "Andere"
      )
    ),
    
    # BA04 = Lautstärke Training
    BA04_rank = case_when(
      as.character(BA04) == "1" ~ 1,  # Nie
      as.character(BA04) == "2" ~ 2,  # Selten
      as.character(BA04) == "3" ~ 3,  # Gelegentlich
      as.character(BA04) == "4" ~ 4,  # Oft
      as.character(BA04) == "5" ~ 5   # Immer
    ),
    
    # BA06 = Lautstärke Spiel
    BA06_rank = case_when(
      as.character(BA06) == "1" ~ 1,  # Nie
      as.character(BA06) == "2" ~ 2,  # Selten
      as.character(BA06) == "3" ~ 3,  # Gelegentlich
      as.character(BA06) == "4" ~ 4,  # Oft
      as.character(BA06) == "5" ~ 5   # Immer
    )
  ) %>%
  filter(!is.na(BA04_rank), !is.na(BA06_rank))

# ----------------------------------------------------------
# 24.2. Gesamtvergleich:
# Lautstärke Spiel vs. Training
# ----------------------------------------------------------

wilcox_laut_gesamt <- wilcox.test(
  lautstaerke_vergleich$BA06_rank,
  lautstaerke_vergleich$BA04_rank,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE
)

wilcox_laut_gesamt

# ----------------------------------------------------------
# 24.3. Cheftrainer:innen:
# Lautstärke Spiel vs. Training
# ----------------------------------------------------------

chef_laut_daten <- lautstaerke_vergleich %>%
  filter(Trainerrolle == "Cheftrainer:in")

wilcox_laut_chef <- wilcox.test(
  chef_laut_daten$BA06_rank,
  chef_laut_daten$BA04_rank,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE
)

wilcox_laut_chef

# ----------------------------------------------------------
# 24.4. Co-Trainer:innen:
# Lautstärke Spiel vs. Training
# ----------------------------------------------------------

co_laut_daten <- lautstaerke_vergleich %>%
  filter(Trainerrolle == "Co-Trainer:in")

wilcox_laut_co <- wilcox.test(
  co_laut_daten$BA06_rank,
  co_laut_daten$BA04_rank,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE
)

wilcox_laut_co

# ----------------------------------------------------------
# 24.5. Torwarttrainer:innen:
# Lautstärke Spiel vs. Training
# ----------------------------------------------------------

torwart_laut_daten <- lautstaerke_vergleich %>%
  filter(Trainerrolle == "Torwarttrainer:in")

wilcox_laut_torwart <- wilcox.test(
  torwart_laut_daten$BA06_rank,
  torwart_laut_daten$BA04_rank,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE
)

wilcox_laut_torwart

# ----------------------------------------------------------
# 24.6. Andere Trainer:innen:
# Lautstärke Spiel vs. Training
# ----------------------------------------------------------

andere_laut_daten <- lautstaerke_vergleich %>%
  filter(Trainerrolle == "Andere")

wilcox_laut_andere <- wilcox.test(
  andere_laut_daten$BA06_rank,
  andere_laut_daten$BA04_rank,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE
)

wilcox_laut_andere

# ----------------------------------------------------------
# 24.7 Übersichtstabelle:
# Wilcoxon-Tests Lautstärke Spiel vs. Training
# nach Trainerrolle
# ----------------------------------------------------------

tabelle_tests_laut_spiel_training <- tibble(
  Gruppe = c(
    "Gesamt",
    "Cheftrainer:innen",
    "Co-Trainer:innen",
    "Torwarttrainer:innen",
    "Andere"
  ),
  
  n = c(
    nrow(lautstaerke_vergleich),
    nrow(chef_laut_daten),
    nrow(co_laut_daten),
    nrow(torwart_laut_daten),
    nrow(andere_laut_daten)
  ),
  
  Teststatistik = c(
    round(wilcox_laut_gesamt$statistic, 1),
    round(wilcox_laut_chef$statistic, 1),
    round(wilcox_laut_co$statistic, 1),
    round(wilcox_laut_torwart$statistic, 1),
    round(wilcox_laut_andere$statistic, 1)
  ),
  
  p = c(
    ifelse(
      wilcox_laut_gesamt$p.value < .001,
      "< .001",
      sprintf("%.3f", wilcox_laut_gesamt$p.value)
    ),
    ifelse(
      wilcox_laut_chef$p.value < .001,
      "< .001",
      sprintf("%.3f", wilcox_laut_chef$p.value)
    ),
    ifelse(
      wilcox_laut_co$p.value < .001,
      "< .001",
      sprintf("%.3f", wilcox_laut_co$p.value)
    ),
    ifelse(
      wilcox_laut_torwart$p.value < .001,
      "< .001",
      sprintf("%.3f", wilcox_laut_torwart$p.value)
    ),
    ifelse(
      wilcox_laut_andere$p.value < .001,
      "< .001",
      sprintf("%.3f", wilcox_laut_andere$p.value)
    )
  )
)

# Tabelle gestalten

tabelle_tests_laut_spiel_training_gt <- tabelle_tests_laut_spiel_training %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Gruppe = "Trainer:innenrolle",
    n = md("*n*"),
    Teststatistik = md("*V*"),
    p = md("*p*")
  ) %>%
  
  # Feste, kompakte Spaltenbreiten
  
  cols_width(
    Gruppe ~ px(170),
    n ~ px(55),
    Teststatistik ~ px(75),
    p ~ px(70)
  ) %>%
  
  # Textspalte linksbündig
  
  cols_align(
    align = "left",
    columns = Gruppe
  ) %>%
  
  # Zahlenspalten rechtsbündig
  
  cols_align(
    align = "right",
    columns = c(n, Teststatistik, p)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Zeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Gruppe == "Andere"
    )
  ) %>%
  
  # Kurze Anmerkung zur gerichteten Testung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Gerichtete Wilcoxon-Vorzeichen-Rang-Tests für gepaarte Stichproben (Spiel > Training)."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(370),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

tabelle_tests_laut_spiel_training_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_tests_laut_spiel_training_gt,
  filename = "Tab_Wilcoxon_Lautstaerke_Spiel_vs_Training.png",
  zoom = 3,
  expand = 5
)

# ==========================================================
# 25. SPRECHANTEILINDEX
# ==========================================================

library(dplyr)
library(psych)

# ----------------------------------------------------------
# 25.1 Sprechanteil Training und Spiel rangcodieren
# ----------------------------------------------------------

daten <- daten %>%
  mutate(
    BA02_rank = case_when(
      BA02 == 6  ~ 1,
      BA02 == 2  ~ 2,
      BA02 == 3  ~ 3,
      BA02 == 9  ~ 4,
      BA02 == 4  ~ 5,
      BA02 == 8  ~ 6,
      BA02 == 10 ~ 7,
      BA02 == 5  ~ 8,
      BA02 == 7  ~ 9,
      TRUE ~ NA_real_
    ),
    
    BA05_rank = case_when(
      BA05 == 7  ~ 1,
      BA05 == 2  ~ 2,
      BA05 == 3  ~ 3,
      BA05 == 10 ~ 4,
      BA05 == 4  ~ 5,
      BA05 == 8  ~ 6,
      BA05 == 9  ~ 7,
      BA05 == 5  ~ 8,
      BA05 == 6  ~ 9,
      TRUE ~ NA_real_
    )
  )

# ----------------------------------------------------------
# 25.2 Kontrolle der Rangcodierung
# ----------------------------------------------------------

table(daten$BA02, daten$BA02_rank, useNA = "ifany")
table(daten$BA05, daten$BA05_rank, useNA = "ifany")

range(daten$BA02_rank, na.rm = TRUE)
range(daten$BA05_rank, na.rm = TRUE)

# ----------------------------------------------------------
# 25.3 Korrelation: Sprechanteil Training × Spiel
# ----------------------------------------------------------

cor.test(
  daten$BA02_rank,
  daten$BA05_rank,
  method = "spearman",
  exact = FALSE
)

# ----------------------------------------------------------
# 25.4 Interne Konsistenz: Cronbachs Alpha
# ----------------------------------------------------------

alpha(
  daten %>%
    select(BA02_rank, BA05_rank) %>%
    na.omit()
)

# ----------------------------------------------------------
# 25.5 Sprechanteilindex als Summenindex bilden
# Wertebereich: 2–18
# ----------------------------------------------------------

daten <- daten %>%
  mutate(
    Sprechanteil_Index = BA02_rank + BA05_rank
  )

# ----------------------------------------------------------
# 25.6 Kontrolle und Deskription des Sprechanteilindex
# ----------------------------------------------------------

summary(daten$Sprechanteil_Index)
range(daten$Sprechanteil_Index, na.rm = TRUE)
sum(!is.na(daten$Sprechanteil_Index))
sd(daten$Sprechanteil_Index, na.rm = TRUE)

# ----------------------------------------------------------
# 25.7 Deskriptive Statistik Sprechanteilindex
# ----------------------------------------------------------

sprechanteil_deskriptiv <- daten %>%
  summarise(
    n = sum(!is.na(Sprechanteil_Index)),
    Mittelwert = round(mean(Sprechanteil_Index, na.rm = TRUE), 2),
    SD = round(sd(Sprechanteil_Index, na.rm = TRUE), 2),
    Median = median(Sprechanteil_Index, na.rm = TRUE),
    Minimum = min(Sprechanteil_Index, na.rm = TRUE),
    Maximum = max(Sprechanteil_Index, na.rm = TRUE)
  )

sprechanteil_deskriptiv

# ==========================================================
# 26. LAUTSTÄRKENINDEX
# ==========================================================

library(dplyr)
library(psych)

# ----------------------------------------------------------
# 26.1 Kontrolle der Codierung
# ----------------------------------------------------------

table(daten$BA04, useNA = "ifany")
table(daten$BA06, useNA = "ifany")

range(daten$BA04, na.rm = TRUE)
range(daten$BA06, na.rm = TRUE)

# ----------------------------------------------------------
# 26.2 Korrelation:
# Lautstärke Training × Lautstärke Spiel
# ----------------------------------------------------------

cor.test(
  daten$BA04,
  daten$BA06,
  method = "spearman",
  exact = FALSE
)

# ----------------------------------------------------------
# 26.3 Interne Konsistenz:
# Cronbachs Alpha
# ----------------------------------------------------------

alpha(
  daten %>%
    select(BA04, BA06) %>%
    na.omit()
)

# ----------------------------------------------------------
# 26.4 Lautstärkenindex als Summenindex bilden
# Wertebereich: 2–10
# ----------------------------------------------------------

daten <- daten %>%
  mutate(
    Lautstaerke_Index = BA04 + BA06
  )

# ----------------------------------------------------------
# 26.5 Kontrolle und Deskription
# ----------------------------------------------------------

summary(daten$Lautstaerke_Index)

range(daten$Lautstaerke_Index, na.rm = TRUE)

sum(!is.na(daten$Lautstaerke_Index))

sd(daten$Lautstaerke_Index, na.rm = TRUE)


# ----------------------------------------------------------
# 26.6 Deskriptive Statistik Lautstärkeindex
# ----------------------------------------------------------

lautstaerke_deskriptiv <- daten %>%
  summarise(
    n = sum(!is.na(Lautstaerke_Index)),
    Mittelwert = round(mean(Lautstaerke_Index, na.rm = TRUE), 2),
    SD = round(sd(Lautstaerke_Index, na.rm = TRUE), 2),
    Median = median(Lautstaerke_Index, na.rm = TRUE),
    Minimum = min(Lautstaerke_Index, na.rm = TRUE),
    Maximum = max(Lautstaerke_Index, na.rm = TRUE)
  )

lautstaerke_deskriptiv

# ==========================================================
# 27. Multiple Regressionsanalysen:
# Berufserfahrung + Sprechanteilindex + Lautstärkeindex
# --> VTD-Gesamtscore, VTD-Häufigkeit, VTD-Schweregrad
# ==========================================================

library(dplyr)
library(car)
library(lm.beta)

# ----------------------------------------------------------
# 27.1 Kontrolle: Sind alle Variablen vorhanden?
# ----------------------------------------------------------

c(
  "VTD_gesamt",
  "VTD_freq",
  "VTD_sev",
  "P007_07",
  "Sprechanteil_Index",
  "Lautstaerke_Index"
) %in% names(daten)

# ----------------------------------------------------------
# 27.2 Multiple Regression: VTD-Gesamtscore
# ----------------------------------------------------------

modell_vtd_gesamt <- lm(
  VTD_gesamt ~ P007_07 + Sprechanteil_Index + Lautstaerke_Index,
  data = daten
)

summary(modell_vtd_gesamt)

# standardisierte Beta-Koeffizienten
lm.beta(modell_vtd_gesamt)

# Multikollinearität
vif(modell_vtd_gesamt)

# Annahmenprüfung: Residualplots
par(mfrow = c(2, 2))
plot(modell_vtd_gesamt)
par(mfrow = c(1, 1))


# ----------------------------------------------------------
# 27.3 Multiple Regression: VTD-Häufigkeit
# ----------------------------------------------------------

modell_vtd_freq <- lm(
  VTD_freq ~ P007_07 + Sprechanteil_Index + Lautstaerke_Index,
  data = daten
)

summary(modell_vtd_freq)

# standardisierte Beta-Koeffizienten
lm.beta(modell_vtd_freq)

# Multikollinearität
vif(modell_vtd_freq)

# Annahmenprüfung: Residualplots
par(mfrow = c(2, 2))
plot(modell_vtd_freq)
par(mfrow = c(1, 1))


# ----------------------------------------------------------
# 27.4 Multiple Regression: VTD-Schweregrad
# ----------------------------------------------------------

modell_vtd_sev <- lm(
  VTD_sev ~ P007_07 + Sprechanteil_Index + Lautstaerke_Index,
  data = daten
)

summary(modell_vtd_sev)

# standardisierte Beta-Koeffizienten
lm.beta(modell_vtd_sev)

# Multikollinearität
vif(modell_vtd_sev)

# Annahmenprüfung: Residualplots
par(mfrow = c(2, 2))
plot(modell_vtd_sev)
par(mfrow = c(1, 1))

# ==========================================================
# 28. Multiple Regressionsanalyse:
# Berufserfahrung + Sprechanteilindex + Lautstärkeindex
# --> VHI-9i-Gesamtscore
# ==========================================================

library(dplyr)
library(car)
library(lm.beta)

# ----------------------------------------------------------
# 28.1 Kontrolle: Sind alle Variablen vorhanden?
# ----------------------------------------------------------

c(
  "VHI_gesamt",
  "P007_07",
  "Sprechanteil_Index",
  "Lautstaerke_Index"
) %in% names(daten)

# ----------------------------------------------------------
# 28.2 Multiple Regression: VHI-9i-Gesamtscore
# ----------------------------------------------------------

modell_vhi_gesamt <- lm(
  VHI_gesamt ~ P007_07 + Sprechanteil_Index + Lautstaerke_Index,
  data = daten
)

summary(modell_vhi_gesamt)

# standardisierte Beta-Koeffizienten
lm.beta(modell_vhi_gesamt)

# Multikollinearität
vif(modell_vhi_gesamt)

# Annahmenprüfung: Residualplots
par(mfrow = c(2, 2))
plot(modell_vhi_gesamt)
par(mfrow = c(1, 1))

# ==========================================================
# 29. Multiple Regressionsanalysen:
# Berufserfahrung + Sprechanteilindex + Lautstärkeindex
# --> FESS-Beziehung, FESS-Bewusstheit, FESS-Emotion
# ==========================================================

# ----------------------------------------------------------
# 29.1 Kontrolle: Sind alle Variablen vorhanden?
# ----------------------------------------------------------

c(
  "FESS_Beziehung",
  "FESS_Bewusstheit",
  "FESS_Emotion",
  "P007_07",
  "Sprechanteil_Index",
  "Lautstaerke_Index"
) %in% names(daten)

# ----------------------------------------------------------
# 29.2 Multiple Regression: FESS-Beziehung
# ----------------------------------------------------------

modell_fess_beziehung <- lm(
  FESS_Beziehung ~ P007_07 + Sprechanteil_Index + Lautstaerke_Index,
  data = daten
)

summary(modell_fess_beziehung)

# standardisierte Beta-Koeffizienten
lm.beta(modell_fess_beziehung)

# Multikollinearität
vif(modell_fess_beziehung)

# Annahmenprüfung: Residualplots
par(mfrow = c(2, 2))
plot(modell_fess_beziehung)
par(mfrow = c(1, 1))

# ----------------------------------------------------------
# 29.3 Multiple Regression: FESS-Bewusstheit
# ----------------------------------------------------------

modell_fess_bewusstheit <- lm(
  FESS_Bewusstheit ~ P007_07 + Sprechanteil_Index + Lautstaerke_Index,
  data = daten
)

summary(modell_fess_bewusstheit)

# standardisierte Beta-Koeffizienten
lm.beta(modell_fess_bewusstheit)

# Multikollinearität
vif(modell_fess_bewusstheit)

# Annahmenprüfung: Residualplots
par(mfrow = c(2, 2))
plot(modell_fess_bewusstheit)
par(mfrow = c(1, 1))

# ----------------------------------------------------------
# 29.4 Multiple Regression: FESS-Emotion
# ----------------------------------------------------------

modell_fess_emotion <- lm(
  FESS_Emotion ~ P007_07 + Sprechanteil_Index + Lautstaerke_Index,
  data = daten
)

summary(modell_fess_emotion)

# standardisierte Beta-Koeffizienten
lm.beta(modell_fess_emotion)

# Multikollinearität
vif(modell_fess_emotion)

# Annahmenprüfung: Residualplots
par(mfrow = c(2, 2))
plot(modell_fess_emotion)
par(mfrow = c(1, 1))

# ----------------------------------------------------------
# 30 Tabellen zu den multiplen Regressionsanalyse
# ----------------------------------------------------------

# ----------------------------------------------------------
# 30.1 Übersichtstabelle aller Regressionsanalysen
# ----------------------------------------------------------

regression_tabelle <- tibble(
  Praediktor = c(
    "Berufserfahrung",
    "Sprechanteilindex",
    "Lautstärkeindex"
  ),
  
  VTD = c(
    "−0,183 (0,068)",
    "0,132 (0,208)",
    "0,099 (0,333)"
  ),
  
  `VHI-9i` = c(
    "−0,112 (0,259)",
    "0,179 (0,088)",
    "−0,022 (0,827)"
  ),
  
  FESS_BE = c(
    "0,102 (0,304)",
    "−0,037 (0,722)",
    "0,060 (0,556)"
  ),
  
  FESS_BU = c(
    "−0,009 (0,929)",
    "0,121 (0,246)",
    "−0,042 (0,679)"
  ),
  
  FESS_SE = c(
    "−0,004 (0,968)",
    "0,195 (0,061)",
    "−0,081 (0,423)"
  )
)

# Tabelle gestalten

regression_tabelle_gt <- regression_tabelle %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Praediktor = "Prädiktor",
    VTD = "VTD",
    `VHI-9i` = "VHI-9i",
    FESS_BE = "Beziehung",
    FESS_BU = "Bewusstheit",
    FESS_SE = "Emotion"
  ) %>%
  
  # Erste Spalte linksbündig
  
  cols_align(
    align = "left",
    columns = Praediktor
  ) %>%
  
  # Ergebnisspalten zentrieren
  
  cols_align(
    align = "center",
    columns = c(VTD, `VHI-9i`, FESS_BE, FESS_BU, FESS_SE)
  ) %>%
  
  # Kompakte Spaltenbreiten
  
  cols_width(
    Praediktor ~ px(135),
    VTD ~ px(105),
    `VHI-9i` ~ px(105),
    FESS_BE ~ px(105),
    FESS_BU ~ px(110),
    FESS_SE ~ px(105)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Zeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Praediktor == "Lautstärkeindex"
    )
  ) %>%
  
  # Anmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Angegeben sind standardisierte Regressionskoeffizienten (*β*) mit *p*-Werten in Klammern. Beziehung = Beziehung zur eigenen Stimme; Bewusstheit = Bewusstheit im Umgang mit der Stimme; Emotion = Wechselwirkung zwischen Stimme und Emotion."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(665),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

regression_tabelle_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = regression_tabelle_gt,
  filename = "Tab_Regressionsanalysen_Uebersicht.png",
  zoom = 3,
  expand = 5
)

# ----------------------------------------------------------
# 30.2 Tabelle F2-F4:
# Modellgüte der multiplen Regressionsanalysen
# ----------------------------------------------------------

# 1. Regressionsmodelle berechnen

modell_vtd <- lm(
  VTD_gesamt ~ P007_07 + Sprechanteil_Index + Lautstaerke_Index,
  data = daten
)

modell_vhi <- lm(
  VHI_gesamt ~ P007_07 + Sprechanteil_Index + Lautstaerke_Index,
  data = daten
)

modell_fess_beziehung <- lm(
  FESS_Beziehung ~ P007_07 + Sprechanteil_Index + Lautstaerke_Index,
  data = daten
)

modell_fess_bewusstheit <- lm(
  FESS_Bewusstheit ~ P007_07 + Sprechanteil_Index + Lautstaerke_Index,
  data = daten
)

modell_fess_emotion <- lm(
  FESS_Emotion ~ P007_07 + Sprechanteil_Index + Lautstaerke_Index,
  data = daten
)

# 2. Modelle in Liste speichern

modelle <- list(
  "VTD-Gesamtscore" = modell_vtd,
  "VHI-9i" = modell_vhi,
  "FESS-Beziehung" = modell_fess_beziehung,
  "FESS-Bewusstheit" = modell_fess_bewusstheit,
  "FESS-Emotion" = modell_fess_emotion
)

# 3. Kennwerte extrahieren

ergebnisse <- lapply(names(modelle), function(name) {
  
  modell <- modelle[[name]]
  
  s <- summary(modell)
  
  fstat <- s$fstatistic
  
  p_modell <- pf(
    fstat[1],
    fstat[2],
    fstat[3],
    lower.tail = FALSE
  )
  
  vif_werte <- vif(modell)
  
  data.frame(
    Variable = name,
    
    F_Test = paste0(
      "F(",
      fstat[2],
      ", ",
      fstat[3],
      ") = ",
      round(fstat[1], 2)
    ),
    
    p = ifelse(
      p_modell < .001,
      "< .001",
      sprintf("%.3f", p_modell)
    ),
    
    R2 = round(s$r.squared, 3),
    
    Adj_R2 = round(s$adj.r.squared, 3),
    
    VIF_Bereich = paste0(
      round(min(vif_werte), 2),
      "–",
      round(max(vif_werte), 2)
    )
  )
  
}) %>%
  bind_rows()

# 4. Tabelle erstellen

tabelle_f2_f4 <- ergebnisse %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Variable = "Abhängige Variable",
    F_Test = md("*F*"),
    p = md("*p*"),
    R2 = md("*R²*"),
    Adj_R2 = md("adj. *R²*"),
    VIF_Bereich = "VIF-Bereich"
  ) %>%
  
  # Erste Spalte linksbündig
  
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  
  # Ergebnisspalten zentrieren
  
  cols_align(
    align = "center",
    columns = c(F_Test, p, R2, Adj_R2, VIF_Bereich)
  ) %>%
  
  # Kompakte Spaltenbreiten
  
  cols_width(
    Variable ~ px(155),
    F_Test ~ px(115),
    p ~ px(70),
    R2 ~ px(70),
    Adj_R2 ~ px(80),
    VIF_Bereich ~ px(95)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Zeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Variable == "FESS-Emotion"
    )
  ) %>%
  
  # Anmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Prädiktoren: Berufserfahrung, Sprechanteilindex und Lautstärkeindex. VIF = Variance Inflation Factor."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(585),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# 5. Im Viewer anzeigen

tabelle_f2_f4

# 6. Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_f2_f4,
  filename = "Tab_Modellguete_Regressionsanalysen.png",
  zoom = 3,
  expand = 5
)

# ----------------------------------------------------------
# 31 Tabelle zu F1: Deskriptive Kennwerte VTD, VHI-9i und FESS
# ----------------------------------------------------------

# 1. Itemnamen festlegen

vtd_freq_items <- paste0("SB05_0", 1:8)
vtd_sev_items  <- paste0("SB06_0", 1:8)
vtd_items <- c(vtd_freq_items, vtd_sev_items)

vhi_items <- c(
  "SB13_01", "SB13_02", "SB13_03", "SB13_04", "SB13_05",
  "SB13_06", "SB13_07", "SB13_08", "SB13_09"
)

fess_beziehung <- c(
  "SB02_05", "SB02_09", "SB03_01",
  "SB03_03", "SB03_06", "SB03_07"
)

fess_bewusstheit <- c(
  "SB02_01", "SB02_04", "SB02_06",
  "SB02_08", "SB03_02", "SB03_04"
)

fess_emotion <- c(
  "SB02_02", "SB02_03", "SB02_07",
  "SB03_05", "SB03_08"
)

fess_items <- c(
  fess_beziehung,
  fess_bewusstheit,
  fess_emotion
)

# 2. -9 als fehlend setzen

alle_items <- c(
  vtd_items,
  vhi_items,
  fess_items
)

for (item in alle_items) {
  daten[[item]][daten[[item]] == -9] <- NA
}

# 3. VTD-Gesamtscore berechnen

for(i in 1:8) {
  
  sb05 <- paste0("SB05_0", i)
  sb06 <- paste0("SB06_0", i)
  
  daten[[sb06]] <- ifelse(
    daten[[sb05]] == 1 & is.na(daten[[sb06]]),
    1,
    daten[[sb06]]
  )
}

daten_vtd_gesamt <- daten %>%
  mutate(
    across(
      all_of(vtd_items),
      ~ ifelse(!is.na(.), . - 1, NA_real_)
    )
  ) %>%
  filter(
    if_all(all_of(vtd_items), ~ !is.na(.))
  ) %>%
  mutate(
    VTD_gesamt = rowSums(
      select(., all_of(vtd_items))
    )
  )

alpha_vtd <- alpha(
  daten_vtd_gesamt[, vtd_items]
)$total$raw_alpha

# 4. VHI-9i berechnen

daten_vhi <- daten %>%
  mutate(
    across(
      all_of(vhi_items),
      ~ ifelse(!is.na(.), . - 1, NA_real_)
    )
  ) %>%
  filter(
    if_all(all_of(vhi_items), ~ !is.na(.))
  ) %>%
  mutate(
    VHI_gesamt = rowSums(
      select(., all_of(vhi_items))
    )
  )

alpha_vhi <- alpha(
  daten_vhi[, vhi_items]
)$total$raw_alpha

# 5. FESS-Subskalen berechnen

daten_fess <- daten %>%
  filter(
    if_all(all_of(fess_items), ~ !is.na(.))
  ) %>%
  mutate(
    FESS_Beziehung = rowSums(
      select(., all_of(fess_beziehung))
    ),
    FESS_Bewusstheit = rowSums(
      select(., all_of(fess_bewusstheit))
    ),
    FESS_Emotion = rowSums(
      select(., all_of(fess_emotion))
    )
  )

alpha_fess_beziehung <- alpha(
  daten_fess[, fess_beziehung]
)$total$raw_alpha

alpha_fess_bewusstheit <- alpha(
  daten_fess[, fess_bewusstheit]
)$total$raw_alpha

alpha_fess_emotion <- alpha(
  daten_fess[, fess_emotion]
)$total$raw_alpha

# 6. Deskriptive Kennwerte sammeln

tabelle_f1_daten <- data.frame(
  Skala = c(
    "VTD-Gesamtscore",
    "VHI-9i",
    "FESS-Beziehung",
    "FESS-Bewusstheit",
    "FESS-Emotion"
  ),
  
  n = c(
    nrow(daten_vtd_gesamt),
    nrow(daten_vhi),
    nrow(daten_fess),
    nrow(daten_fess),
    nrow(daten_fess)
  ),
  
  Theoretischer_Wertebereich = c(
    "0–96",
    "0–36",
    "6–30",
    "6–30",
    "5–25"
  ),
  
  M = c(
    mean(daten_vtd_gesamt$VTD_gesamt),
    mean(daten_vhi$VHI_gesamt),
    mean(daten_fess$FESS_Beziehung),
    mean(daten_fess$FESS_Bewusstheit),
    mean(daten_fess$FESS_Emotion)
  ),
  
  SD = c(
    sd(daten_vtd_gesamt$VTD_gesamt),
    sd(daten_vhi$VHI_gesamt),
    sd(daten_fess$FESS_Beziehung),
    sd(daten_fess$FESS_Bewusstheit),
    sd(daten_fess$FESS_Emotion)
  ),
  
  Median = c(
    median(daten_vtd_gesamt$VTD_gesamt),
    median(daten_vhi$VHI_gesamt),
    median(daten_fess$FESS_Beziehung),
    median(daten_fess$FESS_Bewusstheit),
    median(daten_fess$FESS_Emotion)
  ),
  
  Beobachteter_Wertebereich = c(
    paste0(
      min(daten_vtd_gesamt$VTD_gesamt),
      "–",
      max(daten_vtd_gesamt$VTD_gesamt)
    ),
    paste0(
      min(daten_vhi$VHI_gesamt),
      "–",
      max(daten_vhi$VHI_gesamt)
    ),
    paste0(
      min(daten_fess$FESS_Beziehung),
      "–",
      max(daten_fess$FESS_Beziehung)
    ),
    paste0(
      min(daten_fess$FESS_Bewusstheit),
      "–",
      max(daten_fess$FESS_Bewusstheit)
    ),
    paste0(
      min(daten_fess$FESS_Emotion),
      "–",
      max(daten_fess$FESS_Emotion)
    )
  ),
  
  Cronbach_Alpha = c(
    alpha_vtd,
    alpha_vhi,
    alpha_fess_beziehung,
    alpha_fess_bewusstheit,
    alpha_fess_emotion
  )
)

# 7. Zahlen runden

tabelle_f1_daten <- tabelle_f1_daten %>%
  mutate(
    M = round(M, 2),
    SD = round(SD, 2),
    Median = round(Median, 2),
    Cronbach_Alpha = round(Cronbach_Alpha, 2)
  )

# 8. Tabelle erstellen

tabelle_f1 <- tabelle_f1_daten %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Skala = "Skala",
    n = md("*n*"),
    Theoretischer_Wertebereich = md("Theoretischer<br>Wertebereich"),
    M = md("*M*"),
    SD = md("*SD*"),
    Median = "Median",
    Beobachteter_Wertebereich = md("Beobachteter<br>Wertebereich"),
    Cronbach_Alpha = md("Cronbachs *α*")
  ) %>%
  
  # Erste Spalte linksbündig
  
  cols_align(
    align = "left",
    columns = Skala
  ) %>%
  
  # Kennwerte zentrieren
  
  cols_align(
    align = "center",
    columns = c(
      n,
      Theoretischer_Wertebereich,
      M,
      SD,
      Median,
      Beobachteter_Wertebereich,
      Cronbach_Alpha
    )
  ) %>%
  
  # Kompakte Spaltenbreiten
  
  cols_width(
    Skala ~ px(150),
    n ~ px(45),
    Theoretischer_Wertebereich ~ px(105),
    M ~ px(60),
    SD ~ px(60),
    Median ~ px(70),
    Beobachteter_Wertebereich ~ px(110),
    Cronbach_Alpha ~ px(90)
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Zeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Skala == "FESS-Emotion"
    )
  ) %>%
  
  # Anmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* VTD = Vocal Tract Discomfort Scale; VHI-9i = Voice Handicap Index-9i; FESS = Fragebogen zur Erfassung des stimmlichen Selbstkonzepts."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(690),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(5),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# 9. Im Viewer anzeigen

tabelle_f1

# 10. Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_f1,
  filename = "Tab_F1_Deskriptive_Kennwerte_VTD_VHI_FESS.png",
  zoom = 3,
  expand = 5
)

# --------------------------------------------------------
# 32 Tabelle für F2:
# Berufserfahrung × VTD, VHI-9i und FESS
# --------------------------------------------------------

tabelle_berufserfahrung <- tibble(
  Instrument = c(
    "VTD",
    "VTD",
    "VTD",
    "VHI-9i",
    "FESS",
    "FESS",
    "FESS"
  ),
  
  Variable = c(
    "Gesamtscore",
    "Häufigkeit",
    "Schweregrad",
    "Gesamtscore",
    "Beziehung",
    "Bewusstheit",
    "Emotion"
  ),
  
  n = c(
    103,
    105,
    103,
    107,
    109,
    109,
    109
  ),
  
  rho = c(
    -0.205,
    -0.185,
    -0.189,
    -0.121,
    0.180,
    0.041,
    -0.053
  ),
  
  p = c(
    0.038,
    0.059,
    0.056,
    0.213,
    0.061,
    0.669,
    0.587
  )
)

# -----------------------------------------------
# Tabelle gestalten
# -----------------------------------------------

tabelle_berufserfahrung_gt <- tabelle_berufserfahrung %>%
  mutate(
    `Spearman-ρ` = sprintf("%.3f", rho),
    p = ifelse(
      p < .001,
      "< .001",
      sprintf("%.3f", p)
    )
  ) %>%
  select(
    Instrument,
    Variable,
    n,
    `Spearman-ρ`,
    p
  ) %>%
  gt(
    groupname_col = "Instrument"
  ) %>%
  
  # Zunächst alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Variable = "Variable",
    n = md("*n*"),
    `Spearman-ρ` = md("*ρ*"),
    p = md("*p*")
  ) %>%
  
  # Variable linksbündig ausrichten
  
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  
  # Ergebnisspalten zentrieren
  
  cols_align(
    align = "center",
    columns = c(n, `Spearman-ρ`, p)
  ) %>%
  
  # Kompakte Spaltenbreiten
  
  cols_width(
    Variable ~ px(150),
    n ~ px(55),
    `Spearman-ρ` ~ px(75),
    p ~ px(65)
  ) %>%
  
  # Gruppenüberschriften fett formatieren
  
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_row_groups()
  ) %>%
  
  # Variablen leicht einrücken
  
  tab_style(
    style = cell_text(
      indent = px(6)
    ),
    locations = cells_body(
      columns = Variable
    )
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Spaltenüberschriften
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Linie oberhalb jeder Gruppenüberschrift
  
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_row_groups()
  ) %>%
  
  # Anmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Angegeben sind Spearman-Rangkorrelationskoeffizienten (*ρ*) mit *p*-Werten."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    
    # Schrift
    
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    # Tabellenbreite
    
    table.width = px(345),
    
    # Kopfzeile
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    # Gruppenüberschriften
    
    row_group.padding = px(7),
    
    # Kompakte Datenzeilen
    
    data_row.padding = px(4),
    
    # Anmerkung
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    # Schwarze Abschlusslinie ganz unten
    
    table_body.border.bottom.style = "solid",
    table_body.border.bottom.width = px(1),
    table_body.border.bottom.color = "black",
    
    # Keine weiteren äußeren Rahmenlinien
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none"
  )

# Tabelle anzeigen

tabelle_berufserfahrung_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = tabelle_berufserfahrung_gt,
  filename = "Tab_F2_Berufserfahrung_VTD_VHI_FESS.png",
  zoom = 3,
  expand = 5
)

# ----------------------------------------------------------
# 33 Tabellen für F3
# ----------------------------------------------------------

# ----------------------------------------------------------
# 33.1 Sprechanteil im Training × stimmbezogene Variablen
# ----------------------------------------------------------

# ----------------------------------------------------------
# 33.1.1. Hilfsfunktion:
# n, Spearman-rho und p-Wert auslesen
# ----------------------------------------------------------

korrelation_auslesen <- function(variable, testobjekt, x, y) {
  
  tibble(
    Variable = variable,
    n = sum(complete.cases(x, y)),
    rho = unname(testobjekt$estimate),
    p_wert = testobjekt$p.value
  )
}

# ----------------------------------------------------------
# 33.1.2. Ergebnisse zusammenstellen
# ----------------------------------------------------------

f3_training_ergebnisse <- bind_rows(
  
  # Überschriftszeile VTD
  
  tibble(
    Variable = "VTD",
    n = NA_integer_,
    rho = NA_real_,
    p_wert = NA_real_
  ),
  
  korrelation_auslesen(
    variable = "Gesamtscore",
    testobjekt = spearman_vtd_gesamt_sprech_training,
    x = vtd_gesamt_sprech_training$BA02_rank,
    y = vtd_gesamt_sprech_training$VTD_gesamt
  ),
  
  korrelation_auslesen(
    variable = "Häufigkeit",
    testobjekt = spearman_vtd_freq_sprech_training,
    x = vtd_freq_sprech_training$BA02_rank,
    y = vtd_freq_sprech_training$VTD_freq
  ),
  
  korrelation_auslesen(
    variable = "Schweregrad",
    testobjekt = spearman_vtd_sev_sprech_training,
    x = vtd_sev_sprech_training$BA02_rank,
    y = vtd_sev_sprech_training$VTD_sev
  ),
  
  # Überschriftszeile VHI-9i
  
  tibble(
    Variable = "VHI-9i",
    n = NA_integer_,
    rho = NA_real_,
    p_wert = NA_real_
  ),
  
  korrelation_auslesen(
    variable = "Gesamtscore",
    testobjekt = spearman_vhi_sprech_training,
    x = vhi_sprech_training_spearman$BA02_rank,
    y = vhi_sprech_training_spearman$VHI_gesamt
  ),
  
  # Überschriftszeile FESS
  
  tibble(
    Variable = "FESS",
    n = NA_integer_,
    rho = NA_real_,
    p_wert = NA_real_
  ),
  
  korrelation_auslesen(
    variable = "Beziehung",
    testobjekt = spearman_fess_beziehung_sprech_training,
    x = fess_sprech_training_spearman$BA02_rank,
    y = fess_sprech_training_spearman$FESS_Beziehung
  ),
  
  korrelation_auslesen(
    variable = "Bewusstheit",
    testobjekt = spearman_fess_bewusstheit_sprech_training,
    x = fess_sprech_training_spearman$BA02_rank,
    y = fess_sprech_training_spearman$FESS_Bewusstheit
  ),
  
  korrelation_auslesen(
    variable = "Emotion",
    testobjekt = spearman_fess_emotion_sprech_training,
    x = fess_sprech_training_spearman$BA02_rank,
    y = fess_sprech_training_spearman$FESS_Emotion
  )
)

# Kontrolle der unformatierten Werte

f3_training_ergebnisse

# ----------------------------------------------------------
# 33.1.3. Zahlen für die Tabelle formatieren
# ----------------------------------------------------------

f3_training_tabelle <- f3_training_ergebnisse %>%
  mutate(
    
    # Leere Felder in den Überschriftszeilen
    
    n = ifelse(
      is.na(n),
      "",
      as.character(n)
    ),
    
    # rho mit drei Nachkommastellen und Dezimalkomma
    
    `Spearman-ρ` = ifelse(
      is.na(rho),
      "",
      sprintf("%.3f", rho)
    ),
    
    `Spearman-ρ` = gsub(
      "\\.",
      ",",
      `Spearman-ρ`
    ),
    
    # typografisches Minuszeichen
    
    `Spearman-ρ` = gsub(
      "-",
      "−",
      `Spearman-ρ`
    ),
    
    # p-Werte
    
    p = case_when(
      is.na(p_wert) ~ "",
      p_wert < .001 ~ "< 0,001",
      TRUE ~ gsub(
        "\\.",
        ",",
        sprintf("%.3f", p_wert)
      )
    )
  ) %>%
  select(
    Variable,
    n,
    `Spearman-ρ`,
    p
  )

# Kontrolle der formatierten Tabelle

f3_training_tabelle

# ----------------------------------------------------------
# 33.1.4. Tabelle mit gt gestalten
# ----------------------------------------------------------

f3_training_tabelle_gt <- f3_training_tabelle %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Variable = "Variable",
    n = md("*n*"),
    `Spearman-ρ` = md("*ρ*"),
    p = md("*p*")
  ) %>%
  
  # Variable linksbündig
  
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  
  # Ergebnisspalten zentrieren
  
  cols_align(
    align = "center",
    columns = c(n, `Spearman-ρ`, p)
  ) %>%
  
  # Kompakte Spaltenbreiten
  
  cols_width(
    Variable ~ px(150),
    n ~ px(55),
    `Spearman-ρ` ~ px(75),
    p ~ px(65)
  ) %>%
  
  # VTD, VHI-9i und FESS hervorheben
  
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_body(
      columns = Variable,
      rows = Variable %in% c("VTD", "VHI-9i", "FESS")
    )
  ) %>%
  
  # Untervariablen leicht einrücken
  
  tab_style(
    style = cell_text(
      indent = px(6)
    ),
    locations = cells_body(
      columns = Variable,
      rows = !Variable %in% c("VTD", "VHI-9i", "FESS")
    )
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Linie oberhalb von VHI-9i und FESS
  
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Variable %in% c("VHI-9i", "FESS")
    )
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Zeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Variable == "Emotion"
    )
  ) %>%
  
  # Anmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Angegeben sind Stichprobengrößen (*n*), Spearman-Rangkorrelationskoeffizienten (*ρ*) und zugehörige *p*-Werte."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(345),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(4),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

f3_training_tabelle_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = f3_training_tabelle_gt,
  filename = "Tab_F3_Sprechanteil_Training.png",
  zoom = 3,
  expand = 5
)
# ----------------------------------------------------------
# 33.2 Sprechanteil im Spiel × stimmbezogene Variablen
# ----------------------------------------------------------

# ----------------------------------------------------------
# 33.2.1. Hilfsfunktion:
# n, Spearman-rho und p-Wert auslesen
# ----------------------------------------------------------

korrelation_auslesen <- function(variable, testobjekt, x, y) {
  
  tibble(
    Variable = variable,
    n = sum(complete.cases(x, y)),
    rho = unname(testobjekt$estimate),
    p_wert = testobjekt$p.value
  )
}

# ----------------------------------------------------------
# 33.2.2. Ergebnisse zusammenstellen
# ----------------------------------------------------------

f3_spiel_ergebnisse <- bind_rows(
  
  # Überschriftszeile VTD
  
  tibble(
    Variable = "VTD",
    n = NA_integer_,
    rho = NA_real_,
    p_wert = NA_real_
  ),
  
  korrelation_auslesen(
    variable = "Gesamtscore",
    testobjekt = spearman_vtd_gesamt_sprech_spiel,
    x = vtd_gesamt_sprech_spiel$BA05_rank,
    y = vtd_gesamt_sprech_spiel$VTD_gesamt
  ),
  
  korrelation_auslesen(
    variable = "Häufigkeit",
    testobjekt = spearman_vtd_freq_sprech_spiel,
    x = vtd_freq_sprech_spiel$BA05_rank,
    y = vtd_freq_sprech_spiel$VTD_freq
  ),
  
  korrelation_auslesen(
    variable = "Schweregrad",
    testobjekt = spearman_vtd_sev_sprech_spiel,
    x = vtd_sev_sprech_spiel$BA05_rank,
    y = vtd_sev_sprech_spiel$VTD_sev
  ),
  
  # Überschriftszeile VHI-9i
  
  tibble(
    Variable = "VHI-9i",
    n = NA_integer_,
    rho = NA_real_,
    p_wert = NA_real_
  ),
  
  korrelation_auslesen(
    variable = "Gesamtscore",
    testobjekt = spearman_vhi_sprech_spiel,
    x = vhi_sprech_spiel_spearman$BA05_rank,
    y = vhi_sprech_spiel_spearman$VHI_gesamt
  ),
  
  # Überschriftszeile FESS
  
  tibble(
    Variable = "FESS",
    n = NA_integer_,
    rho = NA_real_,
    p_wert = NA_real_
  ),
  
  korrelation_auslesen(
    variable = "Beziehung",
    testobjekt = spearman_fess_beziehung_sprech_spiel,
    x = fess_sprech_spiel_spearman$BA05_rank,
    y = fess_sprech_spiel_spearman$FESS_Beziehung
  ),
  
  korrelation_auslesen(
    variable = "Bewusstheit",
    testobjekt = spearman_fess_bewusstheit_sprech_spiel,
    x = fess_sprech_spiel_spearman$BA05_rank,
    y = fess_sprech_spiel_spearman$FESS_Bewusstheit
  ),
  
  korrelation_auslesen(
    variable = "Emotion",
    testobjekt = spearman_fess_emotion_sprech_spiel,
    x = fess_sprech_spiel_spearman$BA05_rank,
    y = fess_sprech_spiel_spearman$FESS_Emotion
  )
)

# Kontrolle der unformatierten Werte

f3_spiel_ergebnisse

# ----------------------------------------------------------
# 33.2.3. Zahlen für die Tabelle formatieren
# ----------------------------------------------------------

f3_spiel_tabelle <- f3_spiel_ergebnisse %>%
  mutate(
    
    n = ifelse(
      is.na(n),
      "",
      as.character(n)
    ),
    
    `Spearman-ρ` = ifelse(
      is.na(rho),
      "",
      sprintf("%.3f", rho)
    ),
    
    `Spearman-ρ` = gsub(
      "\\.",
      ",",
      `Spearman-ρ`
    ),
    
    `Spearman-ρ` = gsub(
      "-",
      "−",
      `Spearman-ρ`
    ),
    
    p = case_when(
      is.na(p_wert) ~ "",
      p_wert < .001 ~ "< 0,001",
      TRUE ~ gsub(
        "\\.",
        ",",
        sprintf("%.3f", p_wert)
      )
    )
  ) %>%
  select(
    Variable,
    n,
    `Spearman-ρ`,
    p
  )

# ----------------------------------------------------------
# 33.2.4. Tabelle mit gt gestalten
# ----------------------------------------------------------

f3_spiel_tabelle_gt <- f3_spiel_tabelle %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Variable = "Variable",
    n = md("*n*"),
    `Spearman-ρ` = md("*ρ*"),
    p = md("*p*")
  ) %>%
  
  # Variable linksbündig
  
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  
  # Ergebnisspalten zentrieren
  
  cols_align(
    align = "center",
    columns = c(n, `Spearman-ρ`, p)
  ) %>%
  
  # Kompakte Spaltenbreiten
  
  cols_width(
    Variable ~ px(150),
    n ~ px(55),
    `Spearman-ρ` ~ px(75),
    p ~ px(65)
  ) %>%
  
  # VTD, VHI-9i und FESS hervorheben
  
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_body(
      columns = Variable,
      rows = Variable %in% c("VTD", "VHI-9i", "FESS")
    )
  ) %>%
  
  # Untervariablen leicht einrücken
  
  tab_style(
    style = cell_text(
      indent = px(6)
    ),
    locations = cells_body(
      columns = Variable,
      rows = !Variable %in% c("VTD", "VHI-9i", "FESS")
    )
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Linie oberhalb von VHI-9i und FESS
  
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Variable %in% c("VHI-9i", "FESS")
    )
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Zeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Variable == "Emotion"
    )
  ) %>%
  
  # Anmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Angegeben sind Stichprobengrößen (*n*), Spearman-Rangkorrelationskoeffizienten (*ρ*) und zugehörige *p*-Werte."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(345),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(4),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

f3_spiel_tabelle_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = f3_spiel_tabelle_gt,
  filename = "Tab_F3_Sprechanteil_Spiel.png",
  zoom = 3,
  expand = 5
)

# ----------------------------------------------------------
# 34 Tabellen für F4
# ----------------------------------------------------------

# ----------------------------------------------------------
# 34.1 Sprechen mit erhöhter Lautstärke im Training
# × stimmbezogene Variablen
# ----------------------------------------------------------

# ----------------------------------------------------------
# 34.1.1. Hilfsfunktion:
# n, Spearman-rho und p-Wert auslesen
# ----------------------------------------------------------

korrelation_auslesen <- function(variable, testobjekt, x, y) {
  
  tibble(
    Variable = variable,
    n = sum(complete.cases(x, y)),
    rho = unname(testobjekt$estimate),
    p_wert = testobjekt$p.value
  )
}

# ----------------------------------------------------------
# 34.1.2. Ergebnisse zusammenstellen
# ----------------------------------------------------------

f4_lautstaerke_training_ergebnisse <- bind_rows(
  
  # Überschriftszeile VTD
  
  tibble(
    Variable = "VTD",
    n = NA_integer_,
    rho = NA_real_,
    p_wert = NA_real_
  ),
  
  korrelation_auslesen(
    variable = "Gesamtscore",
    testobjekt = spearman_vtd_gesamt_laut_training,
    x = vtd_gesamt_laut_training$BA04_rank,
    y = vtd_gesamt_laut_training$VTD_gesamt
  ),
  
  korrelation_auslesen(
    variable = "Häufigkeit",
    testobjekt = spearman_vtd_freq_laut_training,
    x = vtd_freq_laut_training$BA04_rank,
    y = vtd_freq_laut_training$VTD_freq
  ),
  
  korrelation_auslesen(
    variable = "Schweregrad",
    testobjekt = spearman_vtd_sev_laut_training,
    x = vtd_sev_laut_training$BA04_rank,
    y = vtd_sev_laut_training$VTD_sev
  ),
  
  # Überschriftszeile VHI-9i
  
  tibble(
    Variable = "VHI-9i",
    n = NA_integer_,
    rho = NA_real_,
    p_wert = NA_real_
  ),
  
  korrelation_auslesen(
    variable = "Gesamtscore",
    testobjekt = spearman_vhi_laut_training,
    x = vhi_laut_training_spearman$BA04_rank,
    y = vhi_laut_training_spearman$VHI_gesamt
  ),
  
  # Überschriftszeile FESS
  
  tibble(
    Variable = "FESS",
    n = NA_integer_,
    rho = NA_real_,
    p_wert = NA_real_
  ),
  
  korrelation_auslesen(
    variable = "Beziehung",
    testobjekt = spearman_fess_beziehung_laut_training,
    x = fess_laut_training_spearman$BA04_rank,
    y = fess_laut_training_spearman$FESS_Beziehung
  ),
  
  korrelation_auslesen(
    variable = "Bewusstheit",
    testobjekt = spearman_fess_bewusstheit_laut_training,
    x = fess_laut_training_spearman$BA04_rank,
    y = fess_laut_training_spearman$FESS_Bewusstheit
  ),
  
  korrelation_auslesen(
    variable = "Emotion",
    testobjekt = spearman_fess_emotion_laut_training,
    x = fess_laut_training_spearman$BA04_rank,
    y = fess_laut_training_spearman$FESS_Emotion
  )
)

# Kontrolle der unformatierten Werte

f4_lautstaerke_training_ergebnisse

# ----------------------------------------------------------
# 34.1.3. Zahlen für die Tabelle formatieren
# ----------------------------------------------------------

f4_lautstaerke_training_tabelle <-
  f4_lautstaerke_training_ergebnisse %>%
  mutate(
    
    n = ifelse(
      is.na(n),
      "",
      as.character(n)
    ),
    
    `Spearman-ρ` = ifelse(
      is.na(rho),
      "",
      sprintf("%.3f", rho)
    ),
    
    `Spearman-ρ` = gsub(
      "\\.",
      ",",
      `Spearman-ρ`
    ),
    
    `Spearman-ρ` = gsub(
      "-",
      "−",
      `Spearman-ρ`
    ),
    
    p = case_when(
      is.na(p_wert) ~ "",
      p_wert < .001 ~ "< 0,001",
      TRUE ~ gsub(
        "\\.",
        ",",
        sprintf("%.3f", p_wert)
      )
    )
  ) %>%
  select(
    Variable,
    n,
    `Spearman-ρ`,
    p
  )

# Kontrolle der formatierten Tabelle

f4_lautstaerke_training_tabelle

# ----------------------------------------------------------
# 34.1.4. Tabelle mit gt gestalten
# ----------------------------------------------------------

f4_lautstaerke_training_tabelle_gt <-
  f4_lautstaerke_training_tabelle %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Variable = "Variable",
    n = md("*n*"),
    `Spearman-ρ` = md("*ρ*"),
    p = md("*p*")
  ) %>%
  
  # Variable linksbündig
  
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  
  # Ergebnisspalten zentrieren
  
  cols_align(
    align = "center",
    columns = c(n, `Spearman-ρ`, p)
  ) %>%
  
  # Kompakte Spaltenbreiten
  
  cols_width(
    Variable ~ px(150),
    n ~ px(55),
    `Spearman-ρ` ~ px(75),
    p ~ px(65)
  ) %>%
  
  # VTD, VHI-9i und FESS hervorheben
  
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_body(
      columns = Variable,
      rows = Variable %in% c("VTD", "VHI-9i", "FESS")
    )
  ) %>%
  
  # Untervariablen leicht einrücken
  
  tab_style(
    style = cell_text(
      indent = px(6)
    ),
    locations = cells_body(
      columns = Variable,
      rows = !Variable %in% c("VTD", "VHI-9i", "FESS")
    )
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Linie oberhalb von VHI-9i und FESS
  
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Variable %in% c("VHI-9i", "FESS")
    )
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Zeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Variable == "Emotion"
    )
  ) %>%
  
  # Anmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Angegeben sind Stichprobengrößen (*n*), Spearman-Rangkorrelationskoeffizienten (*ρ*) und zugehörige *p*-Werte."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(345),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(4),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

f4_lautstaerke_training_tabelle_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = f4_lautstaerke_training_tabelle_gt,
  filename = "Tab_F4_Lautstaerke_Training.png",
  zoom = 3,
  expand = 5
)

# ----------------------------------------------------------
# 34.2 Sprechen mit erhöhter Lautstärke im Spiel
# × stimmbezogene Variablen
# ----------------------------------------------------------

# ----------------------------------------------------------
# 34.2.1. Hilfsfunktion:
# n, Spearman-rho und p-Wert auslesen
# ----------------------------------------------------------

korrelation_auslesen <- function(variable, testobjekt, x, y) {
  
  tibble(
    Variable = variable,
    n = sum(complete.cases(x, y)),
    rho = unname(testobjekt$estimate),
    p_wert = testobjekt$p.value
  )
}

# ----------------------------------------------------------
# 34.2.2. Ergebnisse zusammenstellen
# ----------------------------------------------------------

f4_lautstaerke_spiel_ergebnisse <- bind_rows(
  
  # Überschriftszeile VTD
  
  tibble(
    Variable = "VTD",
    n = NA_integer_,
    rho = NA_real_,
    p_wert = NA_real_
  ),
  
  korrelation_auslesen(
    variable = "Gesamtscore",
    testobjekt = spearman_vtd_gesamt_laut_spiel,
    x = vtd_gesamt_laut_spiel$BA06_rank,
    y = vtd_gesamt_laut_spiel$VTD_gesamt
  ),
  
  korrelation_auslesen(
    variable = "Häufigkeit",
    testobjekt = spearman_vtd_freq_laut_spiel,
    x = vtd_freq_laut_spiel$BA06_rank,
    y = vtd_freq_laut_spiel$VTD_freq
  ),
  
  korrelation_auslesen(
    variable = "Schweregrad",
    testobjekt = spearman_vtd_sev_laut_spiel,
    x = vtd_sev_laut_spiel$BA06_rank,
    y = vtd_sev_laut_spiel$VTD_sev
  ),
  
  # Überschriftszeile VHI-9i
  
  tibble(
    Variable = "VHI-9i",
    n = NA_integer_,
    rho = NA_real_,
    p_wert = NA_real_
  ),
  
  korrelation_auslesen(
    variable = "Gesamtscore",
    testobjekt = spearman_vhi_laut_spiel,
    x = vhi_laut_spiel_spearman$BA06_rank,
    y = vhi_laut_spiel_spearman$VHI_gesamt
  ),
  
  # Überschriftszeile FESS
  
  tibble(
    Variable = "FESS",
    n = NA_integer_,
    rho = NA_real_,
    p_wert = NA_real_
  ),
  
  korrelation_auslesen(
    variable = "Beziehung",
    testobjekt = spearman_fess_beziehung_laut_spiel,
    x = fess_laut_spiel_spearman$BA06_rank,
    y = fess_laut_spiel_spearman$FESS_Beziehung
  ),
  
  korrelation_auslesen(
    variable = "Bewusstheit",
    testobjekt = spearman_fess_bewusstheit_laut_spiel,
    x = fess_laut_spiel_spearman$BA06_rank,
    y = fess_laut_spiel_spearman$FESS_Bewusstheit
  ),
  
  korrelation_auslesen(
    variable = "Emotion",
    testobjekt = spearman_fess_emotion_laut_spiel,
    x = fess_laut_spiel_spearman$BA06_rank,
    y = fess_laut_spiel_spearman$FESS_Emotion
  )
)

# Kontrolle der unformatierten Werte

f4_lautstaerke_spiel_ergebnisse

# ----------------------------------------------------------
# 34.2.3. Zahlen für die Tabelle formatieren
# ----------------------------------------------------------

f4_lautstaerke_spiel_tabelle <-
  f4_lautstaerke_spiel_ergebnisse %>%
  mutate(
    
    n = ifelse(
      is.na(n),
      "",
      as.character(n)
    ),
    
    `Spearman-ρ` = ifelse(
      is.na(rho),
      "",
      sprintf("%.3f", rho)
    ),
    
    `Spearman-ρ` = gsub(
      "\\.",
      ",",
      `Spearman-ρ`
    ),
    
    `Spearman-ρ` = gsub(
      "-",
      "−",
      `Spearman-ρ`
    ),
    
    p = case_when(
      is.na(p_wert) ~ "",
      p_wert < .001 ~ "< 0,001",
      TRUE ~ gsub(
        "\\.",
        ",",
        sprintf("%.3f", p_wert)
      )
    )
  ) %>%
  select(
    Variable,
    n,
    `Spearman-ρ`,
    p
  )

# Kontrolle der formatierten Tabelle

f4_lautstaerke_spiel_tabelle

# ----------------------------------------------------------
# 34.2.4. Tabelle mit gt gestalten
# ----------------------------------------------------------

f4_lautstaerke_spiel_tabelle_gt <-
  f4_lautstaerke_spiel_tabelle %>%
  gt() %>%
  
  # Alle automatisch erzeugten Tabellenlinien entfernen
  
  opt_table_lines(
    extent = "none"
  ) %>%
  
  # Spaltenüberschriften
  
  cols_label(
    Variable = "Variable",
    n = md("*n*"),
    `Spearman-ρ` = md("*ρ*"),
    p = md("*p*")
  ) %>%
  
  # Variable linksbündig
  
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  
  # Ergebnisspalten zentrieren
  
  cols_align(
    align = "center",
    columns = c(n, `Spearman-ρ`, p)
  ) %>%
  
  # Kompakte Spaltenbreiten
  
  cols_width(
    Variable ~ px(150),
    n ~ px(55),
    `Spearman-ρ` ~ px(75),
    p ~ px(65)
  ) %>%
  
  # VTD, VHI-9i und FESS hervorheben
  
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_body(
      columns = Variable,
      rows = Variable %in% c("VTD", "VHI-9i", "FESS")
    )
  ) %>%
  
  # Untervariablen leicht einrücken
  
  tab_style(
    style = cell_text(
      indent = px(6)
    ),
    locations = cells_body(
      columns = Variable,
      rows = !Variable %in% c("VTD", "VHI-9i", "FESS")
    )
  ) %>%
  
  # Schwarze Linien oberhalb und unterhalb der Kopfzeile
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Schwarze Linie oberhalb von VHI-9i und FESS
  
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Variable %in% c("VHI-9i", "FESS")
    )
  ) %>%
  
  # Schwarze Abschlusslinie unter der letzten Zeile
  
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      style = "solid",
      weight = px(1)
    ),
    locations = cells_body(
      rows = Variable == "Emotion"
    )
  ) %>%
  
  # Anmerkung
  
  tab_source_note(
    source_note = md(
      "*Anmerkung.* Angegeben sind Stichprobengrößen (*n*), Spearman-Rangkorrelationskoeffizienten (*ρ*) und zugehörige *p*-Werte."
    )
  ) %>%
  
  # Einheitliches Tabellendesign
  
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(12),
    
    table.width = px(345),
    
    column_labels.font.weight = "normal",
    column_labels.padding = px(6),
    
    data_row.padding = px(4),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(5),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    
    table_body.hlines.style = "none",
    table_body.hlines.width = px(0),
    
    table_body.border.bottom.style = "none"
  )

# Tabelle anzeigen

f4_lautstaerke_spiel_tabelle_gt

# Tabelle als hochauflösende PNG-Datei speichern

gtsave(
  data = f4_lautstaerke_spiel_tabelle_gt,
  filename = "Tab_F4_Lautstaerke_Spiel.png",
  zoom = 3,
  expand = 5
)