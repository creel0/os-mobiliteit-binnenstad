names(df)
unique(df$v1_reden)

# totaal aantal respondenten
n_totaal <- nrow(df)
cat("Aantal respondenten:", n_totaal, "\n")

# --- HULPFUNCTIE: frequentietabel + staafdiagram ----------------
maak_grafiek <- function(data, kolom, titel, kleur = "#2E75B6") {
  n_vraag <- data %>% filter(!is.na({{ kolom }})) %>% nrow()
  
  data %>%
    count({{ kolom }}) %>%
    filter(!is.na({{ kolom }})) %>%
    mutate(pct = round(n / n_totaal * 100, 1)) %>%
    arrange(desc(n)) %>%
    { print(.); . } %>%
    ggplot(aes(x = reorder({{ kolom }}, n), y = n)) +
    geom_col(fill = kleur) +
    geom_text(aes(label = paste0(round(n / n_totaal * 100, 0), "%")),
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = titel, x = "", y = "Aantal respondenten",
         caption = paste0("n = ", n_vraag)) +
    theme_minimal(base_size = 12) +
    theme(plot.caption = element_text(hjust = 0, color = "grey50"))
}
