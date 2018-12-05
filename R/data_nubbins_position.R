# Position des boutures au cours de l'expérience.

# Packages ---
SciViews::R
# Bouture position
position_bouture <- tribble(
  ~ localisation, ~ id, ~ condition, ~ respiro,
  "A0", 4, "control", "R1",
  "A0", 15, "control", NA,
  "A0", 2, "control", NA,
  "A0", 5, "control", "R2",
  "A1", 18, "hyposalin", NA,
  "A1", 13, "hyposalin", "R3",
  "A1", 14, "hyposalin", "R4",
  "A1", 8, "hyposalin", "R5",
  "A1", 17, "hyposalin", NA,
  "A1", 7, "hyposalin", NA,
  "A2", 19, "hypersalin", NA,
  "A2", 1, "hypersalin", NA,
  "A2", 3, "hypersalin", "R6",
  "A2", 20, "hypersalin", "R7",
  "A2", 11, "hypersalin", NA,
  "A2", 10, "hypersalin", "R8"
)

position_bouture$id <- as.factor(position_bouture$id)
write(position_bouture, file = "data/nubbins.rds", type = "rds", compress = "xz")
