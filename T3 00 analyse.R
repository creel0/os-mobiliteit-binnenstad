# --- V1: Reden van bezoek (pipe-gescheiden, splitsen) -----------
cat("\n## V1 - Reden van bezoek\n")
n_v1 <- nrow(v1_gesplitst %>% distinct(id))  # unieke respondenten

v1_gesplitst <- df %>%
  filter(!is.na(v1_reden), v1_reden != "-") %>%
  mutate(reden = str_split(v1_reden, " \\| ")) %>%
  unnest(reden) %>%
  mutate(reden = str_trim(reden))

v1_freq <- v1_gesplitst %>%
  count(reden, name = "n") %>%
  mutate(pct = round(n / n_totaal * 100, 1)) %>%
  arrange(desc(n))

print(v1_freq)

ggplot(v1_freq, aes(x = reorder(reden, n), y = n)) +
  geom_col(fill = "#2E75B6") +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "V1 - Reden van bezoek binnenstad",
       subtitle = "Meerdere antwoorden mogelijk (max. 3)",
       x = "", y = "Aantal respondenten",
       caption = paste0("n = ", n_v1)) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(hjust = 0, color = "grey50"))

# --- V2: Bezoekfrequentie ---------------------------------------
cat("\n## V2 - Bezoekfrequentie\n")
n_v2 <- df %>% filter(!is.na(v2_frequentie)) %>% nrow()

# Vaste volgorde instellen
freq_volgorde <- c("Dagelijks of bijna dagelijks", "Wekelijks",
                   "Maandelijks", "Jaarlijks",
                   "Minder dan een keer per jaar", "Dit is mijn eerste bezoek")

df %>%
  filter(!is.na(v2_frequentie)) %>%
  mutate(v2_frequentie = factor(v2_frequentie, levels = freq_volgorde)) %>%
  count(v2_frequentie) %>%
  mutate(pct = round(n / n_totaal * 100, 1)) %>%
  print()

df %>%
  filter(!is.na(v2_frequentie)) %>%
  mutate(v2_frequentie = factor(v2_frequentie, levels = rev(freq_volgorde))) %>%
  count(v2_frequentie) %>%
  ggplot(aes(x = v2_frequentie, y = n)) +
  geom_col(fill = "#2E75B6") +
  geom_text(aes(label = paste0(round(n / n_totaal * 100, 0), "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "V2 - Hoe vaak bezoekt u de binnenstad?",
       x = "", y = "Aantal respondenten",
       caption = paste0("n = ", n_v2)) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(hjust = 0, color = "grey50"))

# --- V3: Vervoer naar binnenstad --------------------------------
cat("\n## V3 - Vervoer naar binnenstad\n")
maak_grafiek(df, `v3_verveor naar`, "V3 - Vervoerswijze naar de binnenstad")


# --- V4: Vervoer in binnenstad ----------------------------------
cat("\n## V4 - Vervoer in binnenstad\n")
maak_grafiek(df, v4_vervoer_in, "V4 - Vervoerswijze in de binnenstad")

# --- V5: Moeite met lopen ---------------------------------------
cat("\n## V5 - Moeite met lopen\n")
n_v5 <- df %>% filter(!is.na(v5_moeite_moeite_lopen)) %>% nrow()

moeite_volgorde <- c("Geen moeite met lopen", "Een beetje moeite met lopen",
                     "Ja, veel moeite met lopen", "Ik kan niet of nauwelijks lopen",
                     "Wil ik liever niet zeggen")

df %>%
  filter(!is.na(v5_moeite_moeite_lopen)) %>%
  mutate(v5_moeite_moeite_lopen = factor(v5_moeite_moeite_lopen,
                                         levels = rev(moeite_volgorde))) %>%
  count(v5_moeite_moeite_lopen) %>%
  ggplot(aes(x = v5_moeite_moeite_lopen, y = n)) +
  geom_col(fill = "#2E75B6") +
  geom_text(aes(label = paste0(round(n / n_totaal * 100, 0), "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "V5 - Moeite met lopen",
       x = "", y = "Aantal respondenten",
       caption = paste0("n = ", n_v5)) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(hjust = 0, color = "grey50"))

# --- V6: Autoluw gebied (schuifbalk) ----------------------------
n_v6 <- df %>% filter(!is.na(v6_autoluw)) %>% nrow()
cat("\n## V6 - Autoluw gebied\n")
cat("Gemiddelde score:", round(mean(df$v6_autoluw, na.rm = TRUE), 2), "\n")
cat("(Lager = alleen voetgangers | Hoger = ook fietsers welkom)\n")

ggplot(df %>% filter(!is.na(v6_autoluw)), aes(x = v6_autoluw)) +
  geom_histogram(binwidth = 1, fill = "#2E75B6", color = "white") +
  geom_vline(xintercept = mean(df$v6_autoluw, na.rm = TRUE),
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "V6 - Autoluw gebied: voetgangers vs. fietsers",
       subtitle = "Rode lijn = gemiddelde | Links = alleen voetgangers, Rechts = ook fietsers",
       x = "Score", y = "Aantal respondenten",
       caption = paste0("n = ", n_v6)) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(hjust = 0, color = "grey50"))

# --- V7: Vrijgekomen ruimte (schuifbalk) ------------------------
n_v7 <- df %>% filter(!is.na(v7_ruimte)) %>% nrow()

cat("\n## V7 - Vrijgekomen ruimte\n")
cat("Gemiddelde score:", round(mean(df$v7_ruimte, na.rm = TRUE), 2), "\n")
cat("(Lager = groen/terrassen | Hoger = lopen/fietsen)\n")

ggplot(df %>% filter(!is.na(v7_ruimte)), aes(x = v7_ruimte)) +
  geom_histogram(binwidth = 1, fill = "#70AD47", color = "white") +
  geom_vline(xintercept = mean(df$v7_ruimte, na.rm = TRUE),
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "V7 - Vrijgekomen ruimte: groen/terrassen vs. lopen/fietsen",
       subtitle = "Rode lijn = gemiddelde | Links = groen/terrassen, Rechts = lopen/fietsen",
       x = "Score", y = "Aantal respondenten",
       caption = paste0("n = ", n_v7)) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(hjust = 0, color = "grey50"))

# --- V8: Fietsstalling ------------------------------------------
cat("\n## V8 - Voorkeur fietsstalling\n")
maak_grafiek(df, v8_stalling_fiets, "V8 - Voorkeur fietsstalling")

# --- V9: Bushalte -----------------------------------------------
cat("\n## V9 - Voorkeur bushalte\n")
maak_grafiek(df, v9_bus, "V9 - Voorkeur locatie bushalte")


# --- V10: Open vraag --------------------------------------------
cat("\n## V10 - Open antwoorden\n")
cat("Aantal ingevuld:", sum(!is.na(df$v10_belangrijk_open)), "\n")

# Exporteren voor verdere analyse (bijv. in NimmAI)
write.csv(
  df %>%
    select(id, v10_belangrijk_open) %>%
    filter(!is.na(v10_belangrijk_open)),
  "open_antwoorden_v10.csv",
  row.names = FALSE
)
cat("Open antwoorden geëxporteerd naar: open_antwoorden_v10.csv\n")

