# ================================================================
# 02_daire_grafigi.R
# Sektorlere Gore Dogalgaz Nihai Tuketimi
# Kaynak: EPDK (2014-2024)
# Encoding: UTF-8
# ================================================================

library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

# -- Sabitler -----------------------------------------------------
DOSYA <- "02_daire_grafigi_verisi.xlsx"
CIKTI <- "../Grafikler/02_daire_grafigi.png"
W <- 14; H <- 8; RES <- 300

PALET <- c(
  "Donusum"       = "#003f88",
  "Konutlar"      = "#006d6f",
  "Sanayi"        = "#028090",
  "Hizmet"        = "#5c4f9e",
  "Enerji"        = "#c1121f",
  "Ulasim"        = "#e76f00",
  "Diger"         = "#adb5bd"
)

ETIKET <- c(
  "Donusum"  = "D\u00f6n\u00fc\u015f\u00fcm & \u00c7evrim",
  "Konutlar" = "Konutlar",
  "Sanayi"   = "Sanayi",
  "Hizmet"   = "Hizmet",
  "Enerji"   = "Enerji Sekt\u00f6r\u00fc",
  "Ulasim"   = "Ula\u015f\u0131m",
  "Diger"    = "Di\u011fer"
)

# -- Tema ---------------------------------------------------------
tema <- function(base = 13) {
  theme_void(base_size = base) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      plot.title       = element_text(size = base + 4, face = "bold",
                                      colour = "#111111", hjust = 0.5,
                                      margin = margin(b = 4)),
      plot.subtitle    = element_text(size = base - 2, colour = "#777777",
                                      hjust = 0.5, margin = margin(b = 12)),
      plot.caption     = element_text(size = base - 3, colour = "#999999",
                                      hjust = 0.5, face = "italic",
                                      margin = margin(t = 10)),
      legend.position  = "right",
      legend.title     = element_blank(),
      legend.text      = element_text(size = base - 1),
      legend.key.size  = unit(0.65, "cm"),
      legend.spacing.y = unit(0.2, "cm"),
      plot.margin      = margin(16, 12, 12, 16)
    )
}

# -- Veri ---------------------------------------------------------
df_raw <- read_excel(DOSYA) |>
  filter(!is.na(Yil), as.character(Yil) != "Genel_toplam") |>
  mutate(across(-Yil, \(x) suppressWarnings(as.numeric(as.character(x)))))

sektor_cols <- setdiff(names(df_raw), "Yil")

df_pie <- df_raw |>
  summarise(across(all_of(sektor_cols), \(x) mean(x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "Sektor", values_to = "Deger") |>
  mutate(
    Sektor = recode(Sektor,
      "Donusum_cevrim_sektoru" = "Donusum",
      "Enerji_sektoru"         = "Enerji",
      "Ulasim_sektoru"         = "Ulasim",
      "Sanayi_sektoru"         = "Sanayi",
      "Hizmet_sektoru"         = "Hizmet",
      "Konutlar"               = "Konutlar",
      "Diger"                  = "Diger"
    ),
    Yuzde      = Deger / sum(Deger) * 100,
    Legend_lbl = paste0(ETIKET[Sektor], "   ", round(Yuzde, 1), "%")
  ) |>
  arrange(desc(Yuzde)) |>
  mutate(Sektor = factor(Sektor, levels = rev(Sektor)))

# -- Grafik -------------------------------------------------------
p <- ggplot(df_pie, aes(x = 1, y = Yuzde / 100, fill = Sektor)) +
  geom_col(colour = "white", linewidth = 0.9, width = 1) +
  coord_polar(theta = "y", start = 0, direction = -1) +
  xlim(c(0.2, 1.8)) +
  scale_fill_manual(
    values = PALET,
    breaks = levels(df_pie$Sektor),
    labels = setNames(df_pie$Legend_lbl, as.character(df_pie$Sektor))
  ) +
  labs(
    title    = "Sekt\u00f6rlere G\u00f6re Do\u011falgaz Nihai T\u00fcketimi",
    subtitle = "D\u00f6nem ortalamas\u0131  |  Ktep  |  2014\u20132024",
    caption  = "Kaynak: EPDK (2014\u20132024)"
  ) +
  tema()

# -- Disa aktarim ------------------------------------------------
dir.create(dirname(CIKTI), showWarnings = FALSE, recursive = TRUE)
ggsave(CIKTI, p, width = W, height = H, dpi = RES, bg = "white")
message("Kaydedildi: ", CIKTI)
