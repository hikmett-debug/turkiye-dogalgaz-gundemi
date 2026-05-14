# ================================================================
# 04_alan_grafigi.R
# Turkiye Dogalgaz Ithalat ve Ihracati
# Kaynak: EPDK (2015-2024)
# Encoding: UTF-8
# ================================================================

library(ggplot2)
library(readxl)
library(dplyr)

# -- Sabitler -----------------------------------------------------
DOSYA <- "04_alan_grafigi_verisi.xlsx"
CIKTI <- "../Grafikler/04_alan_grafigi.png"
W <- 14; H <- 8; RES <- 300

R_IT <- "#003f88"
R_IH <- "#c1121f"

# -- Tema ---------------------------------------------------------
tema <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      plot.background    = element_rect(fill = "white", colour = NA),
      panel.background   = element_rect(fill = "white", colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "#E5E5E5", linewidth = 0.35),
      axis.line.x        = element_line(colour = "#BBBBBB", linewidth = 0.4),
      axis.ticks.x       = element_line(colour = "#BBBBBB", linewidth = 0.4),
      axis.ticks.y       = element_blank(),
      axis.title.x       = element_blank(),
      axis.title.y.left  = element_text(size = base - 2, colour = R_IT),
      axis.title.y.right = element_text(size = base - 2, colour = R_IH),
      axis.text          = element_text(size = base - 3, colour = "#555555"),
      axis.text.y.right  = element_text(colour = R_IH),
      plot.title         = element_text(size = base + 4, face = "bold",
                                        colour = "#111111", hjust = 0.5,
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(size = base - 2, colour = "#777777",
                                        hjust = 0.5, margin = margin(b = 14)),
      plot.caption       = element_text(size = base - 3, colour = "#999999",
                                        hjust = 0.5, face = "italic",
                                        margin = margin(t = 10)),
      legend.position      = "top",
      legend.justification = "center",
      legend.title         = element_blank(),
      legend.text          = element_text(size = base - 1),
      legend.key.width     = unit(1.8, "cm"),
      legend.key.height    = unit(0.35, "cm"),
      plot.margin          = margin(16, 12, 12, 16)
    )
}

# -- Veri ---------------------------------------------------------
df <- read_excel(DOSYA) |>
  mutate(across(everything(), as.numeric)) |>
  filter(!is.na(Yil), !is.na(Ithalat), !is.na(Ihracat))

faktor <- max(df$Ithalat) / max(df$Ihracat)
df     <- df |> mutate(Ihracat_sc = Ihracat * faktor)

# -- Grafik -------------------------------------------------------
p <- ggplot(df, aes(x = Yil)) +
  geom_area(aes(y = Ithalat,    fill   = "\u0130thalat"), alpha = 0.12) +
  geom_line(aes(y = Ithalat,    colour = "\u0130thalat"),
            linewidth = 1.8, lineend = "round") +
  geom_point(aes(y = Ithalat,   colour = "\u0130thalat"),
             size = 3.8, shape = 21, fill = "white", stroke = 2.2) +
  geom_area(aes(y = Ihracat_sc, fill   = "\u0130hracat"), alpha = 0.10) +
  geom_line(aes(y = Ihracat_sc, colour = "\u0130hracat"),
            linewidth = 1.8, lineend = "round", linetype = "dashed") +
  geom_point(aes(y = Ihracat_sc, colour = "\u0130hracat"),
             size = 3.8, shape = 21, fill = "white", stroke = 2.2) +
  scale_x_continuous(breaks = df$Yil, expand = expansion(mult = 0.03)) +
  scale_y_continuous(
    name   = "\u0130thalat (Milyon m\u00b3)",
    labels = function(x) formatC(x, format = "f", digits = 0,
                                  big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0.02, 0.09)),
    sec.axis = sec_axis(
      transform = ~ . / faktor,
      name      = "\u0130hracat (Milyon m\u00b3)",
      labels    = function(x) formatC(x, format = "f", digits = 0,
                                       big.mark = ".", decimal.mark = ",")
    )
  ) +
  scale_colour_manual(values = c("\u0130thalat" = R_IT, "\u0130hracat" = R_IH)) +
  scale_fill_manual(values   = c("\u0130thalat" = R_IT, "\u0130hracat" = R_IH)) +
  labs(
    title    = "T\u00fcrkiye Do\u011falgaz \u0130thalat ve \u0130hracat\u0131",
    subtitle = "Sol eksen: \u0130thalat  |  Sa\u011f eksen: \u0130hracat  |  Milyon m\u00b3  |  2015\u20132024",
    caption  = "Kaynak: EPDK (2015\u20132024)"
  ) +
  tema()

# -- Disa aktarim ------------------------------------------------
dir.create(dirname(CIKTI), showWarnings = FALSE, recursive = TRUE)
ggsave(CIKTI, p, width = W, height = H, dpi = RES, bg = "white")
message("Kaydedildi: ", CIKTI)
