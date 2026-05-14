# ================================================================
# 06_sutun_grafigi.R
# Kaynak Ulkelere Gore Dogalgaz Ithalati
# Kaynak: EPDK (2015-2024)
# Duzen: 4 sutun x 2 satir | Sabit Y ekseni | Legend sag alt
# Not: ggh4x paketi gereklidir -> install.packages("ggh4x")
# Encoding: UTF-8
# ================================================================

library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(ggh4x)

# -- Sabitler -----------------------------------------------------
DOSYA <- "06_sutun_grafigi_verisi.xlsx"
CIKTI <- "../Grafikler/06_sutun_grafigi.png"
W <- 14; H <- 8; RES <- 300

ULKE_SIRALI <- c("Rusya", "Iran", "Azerbaycan", "Cezayir",
                 "Nijerya", "ABD", "Misir")

ULKE_ETIKET <- c(
  "Rusya"      = "Rusya",
  "Iran"       = "\u0130ran",
  "Azerbaycan" = "Azerbaycan",
  "Cezayir"   = "Cezayir",
  "Nijerya"    = "Nijerya",
  "ABD"        = "ABD",
  "Misir"      = "M\u0131s\u0131r"
)

PALET <- c(
  "Rusya"      = "#003f88",
  "Iran"       = "#c1121f",
  "Azerbaycan" = "#006d6f",
  "Cezayir"   = "#5c4f9e",
  "Nijerya"    = "#e76f00",
  "ABD"        = "#028090",
  "Misir"      = "#b5830a"
)

# -- Tema ---------------------------------------------------------
tema <- function(base = 10) {
  theme_minimal(base_size = base) +
    theme(
      plot.background    = element_rect(fill = "white", colour = NA),
      panel.background   = element_rect(fill = "white", colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "#E5E5E5", linewidth = 0.3),
      axis.line.x        = element_line(colour = "#BBBBBB", linewidth = 0.3),
      axis.ticks.x       = element_line(colour = "#BBBBBB", linewidth = 0.3),
      axis.ticks.y       = element_blank(),
      axis.title.x       = element_blank(),
      axis.title.y       = element_text(size = base - 1, colour = "#555555"),
      axis.text.x        = element_text(size = base - 1, colour = "#555555",
                                        angle = 45, hjust = 1, vjust = 1),
      axis.text.y        = element_text(size = base - 1, colour = "#666666"),
      plot.title         = element_text(size = base + 6, face = "bold",
                                        colour = "#111111", hjust = 0.5,
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(size = base, colour = "#777777",
                                        hjust = 0.5, margin = margin(b = 12)),
      plot.caption       = element_text(size = base - 1, colour = "#999999",
                                        hjust = 0.5, face = "italic",
                                        margin = margin(t = 10)),
      strip.text         = element_blank(),
      strip.background   = element_blank(),
      legend.position      = c(0.97, 0.02),
      legend.justification = c("right", "bottom"),
      legend.direction     = "vertical",
      legend.title         = element_blank(),
      legend.text          = element_text(size = base - 0.5, colour = "#222222"),
      legend.key.size      = unit(0.55, "cm"),
      legend.key.spacing.y = unit(0.15, "cm"),
      legend.background    = element_rect(fill = "white", colour = "#CCCCCC",
                                          linewidth = 0.4),
      legend.margin        = margin(5, 8, 5, 8),
      panel.spacing.x      = unit(1.4, "lines"),
      panel.spacing.y      = unit(1.8, "lines"),
      plot.margin          = margin(16, 20, 14, 20)
    )
}

# -- Veri ---------------------------------------------------------
df_raw <- read_excel(DOSYA)
colnames(df_raw) <- trimws(gsub("\u00a0", "", colnames(df_raw)))

df_raw$Yil <- as.integer(suppressWarnings(as.numeric(
  trimws(gsub("[\u00a0']", "", as.character(df_raw$Yil)))
)))
df_raw <- df_raw[!is.na(df_raw$Yil), ]

DISLA <- c("Yil", "Toplam", "Diger")
for (col in setdiff(colnames(df_raw), DISLA)) {
  temiz <- gsub("[^0-9.]", "",
                trimws(gsub("[\u00a0']", "", as.character(df_raw[[col]]))))
  df_raw[[col]] <- suppressWarnings(as.numeric(temiz))
}

df_long <- df_raw |>
  select(-any_of(c("Toplam", "Diger"))) |>
  pivot_longer(-Yil, names_to = "Ulke", values_to = "Deger") |>
  mutate(
    Yil   = as.integer(Yil),
    Deger = as.numeric(Deger),
    Ulke  = recode(Ulke, "Iran" = "Iran", "Misir" = "Misir")
  ) |>
  filter(Ulke %in% ULKE_SIRALI) |>
  mutate(
    Ulke = factor(Ulke, levels = ULKE_SIRALI),
    YilF = factor(Yil, levels = sort(unique(Yil)))
  )

y_ust   <- max(df_long$Deger, na.rm = TRUE) * 1.08
y_kirik <- pretty(c(0, y_ust), n = 4)

# -- Grafik -------------------------------------------------------
p <- ggplot(df_long, aes(x = YilF, y = Deger, fill = Ulke)) +
  geom_col(width = 0.70, alpha = 0.92, na.rm = TRUE) +
  facet_wrap2(~ Ulke, ncol = 4, scales = "fixed", axes = "all",
              labeller = labeller(Ulke = ULKE_ETIKET)) +
  scale_fill_manual(values = PALET, labels = ULKE_ETIKET) +
  scale_y_continuous(
    breaks = y_kirik,
    limits = c(0, y_ust),
    labels = function(x) formatC(x, format = "f", digits = 0,
                                  big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1), ncol = 1)) +
  labs(
    title    = "Kaynak \u00dclkelere G\u00f6re Do\u011falgaz \u0130thalat\u0131",
    subtitle = "Y\u0131ll\u0131k miktar (Milyon m\u00b3)  |  2015\u20132024",
    y        = "\u0130thalat (Milyon m\u00b3)",
    caption  = "Kaynak: EPDK (2015\u20132024)"
  ) +
  tema()

# -- Disa aktarim ------------------------------------------------
dir.create(dirname(CIKTI), showWarnings = FALSE, recursive = TRUE)
ggsave(CIKTI, p, width = W, height = H, dpi = RES, bg = "white")
message("Kaydedildi: ", CIKTI)
