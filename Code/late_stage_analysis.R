
#Welcome to a coding file used to create the figures in a submitted paper on late stage outsourcing

#The code is structured, as per the paper, with each 5 figures created in order

#The underlying data to this code is sometimes very large (many millions of rows)

#As such, it is currently not shared - but please find me at b.goodair@lse.ac.uk or search for Ben Goodair online and I would be happy to share underlying data





#### infographic The Stages of Outsourcing ####

library(ggplot2)


box_w   <- 2.6
gap     <- 0.5
box_h   <- 3.6
y_top   <- 4.0
y_bot   <- y_top - box_h

# geom_text does NOT auto-wrap to a box width — it draws each line exactly as
# given, so a long unwrapped sentence overflows the rect and bleeds into the
# next column. wrap_width is the character width to break lines at; tune this
# if you change box_w or font sizes below (roughly: box_w_inches * 14, minus
# a couple of characters for the left/right padding).
wrap_width <- 30
wrap_bullets <- function(bullets, width = wrap_width) {
  paste(sapply(bullets, function(b) paste(strwrap(b, width = width), collapse = "\n")),
        collapse = "\n\n")
}

boxes <- data.frame(
  step  = 1:3,
  x0    = c(0, box_w + gap, 2 * (box_w + gap)),
  title = c("1. Market legislation",
            "2. Tilting the playing field",
            "3. Path dependent lock-in")
)
boxes$x1 <- boxes$x0 + box_w
boxes$xc <- (boxes$x0 + boxes$x1) / 2

boxes$mechanism <- c(
  wrap_bullets(c("Purchaser-provider split; public bodies become commissioners.",
                 "Competitive contracting and tendering enforced.")),
  wrap_bullets(c("Financial incentives favour private provision.",
                 "Grants, contracts and bonuses apply only to private providers.")),
  wrap_bullets(c("Policy inaction as the private market dominates.",
                 "Commercial lobbying grows powerful as delivery is fully outsourced."))
)

boxes$example <- sapply(
  c("Example: Local Government Act 1988",
    "Example: Independent Sector Treatment Centres",
    "Example: UK social care, commission focused on funding reform"),
  function(x) paste(strwrap(x, width = wrap_width), collapse = "\n")
)

# Neutral, print-safe. No hue-only encoding — this is a sequence, not a
# categorical split, so a single grey plus rule-lines is enough.
fill_col   <- "grey96"
border_col <- "grey30"
rule_col   <- "grey70"
text_col   <- "grey15"

p <- ggplot() +
  # boxes
  geom_rect(data = boxes,
            aes(xmin = x0, xmax = x1, ymin = y_bot, ymax = y_top),
            fill = fill_col, colour = border_col, linewidth = 0.35) +
  # thin rule under each title
  geom_segment(data = boxes,
               aes(x = x0 + 0.15, xend = x1 - 0.15,
                   y = y_top - 0.75, yend = y_top - 0.75),
               colour = rule_col, linewidth = 0.3) +
  # title
  geom_text(data = boxes, aes(x = x0 + 0.15, y = y_top - 0.4, label = title),
            hjust = 0, vjust = 1, fontface = "bold",
            size = 3.4, colour = text_col,
            lineheight = 1.05) +
  # mechanism bullets (already wrapped to wrap_width by wrap_bullets() above)
  geom_text(data = boxes, aes(x = x0 + 0.15, y = y_top - 1.15, label = mechanism),
            hjust = 0, vjust = 1, size = 2.9, colour = text_col,
            lineheight = 1.15) +
  # example, set apart in italics at the foot of the box
  geom_text(data = boxes, aes(x = x0 + 0.15, y = y_bot + 0.5, label = example),
            hjust = 0, vjust = 0, size = 2.7, fontface = "italic",
            colour = "grey35", lineheight = 1.1) +
  # connecting arrows between boxes
  geom_segment(data = data.frame(x = boxes$x1[1:2] + 0.06,
                                 xend = boxes$x0[2:3] - 0.06,
                                 y = mean(c(y_top, y_bot)),
                                 yend = mean(c(y_top, y_bot))),
               aes(x = x, xend = xend, y = y, yend = yend),
               colour = border_col, linewidth = 0.4,
               arrow = arrow(length = unit(0.09, "in"), type = "closed")) +
  # early-stage / late-stage axis label above the boxes
  annotate("segment", x = 0, xend = max(boxes$x1), y = y_top + 0.55, yend = y_top + 0.55,
           colour = rule_col, linewidth = 0.3) +
  annotate("text", x = 0, y = y_top + 0.8, label = "Early-stage outsourcing",
           hjust = 0, size = 4, colour = "grey40") +
  annotate("text", x = max(boxes$x1), y = y_top + 0.8, label = "Late-stage outsourcing",
           hjust = 1, size = 4, colour = "grey40") +
  coord_cartesian(xlim = c(-0.1, max(boxes$x1) + 0.1),
                  ylim = c(y_bot - 0.1, y_top + 1.1), clip = "off") +
  theme_void(base_size = 11) #+
  #theme(plot.margin = margin(10, 10, 10, 10))

ggsave("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/care_home_mortality/Figures/flow_chart.png", 
       p, width = 9, height = 4.6, units = "in", dpi = 300, bg = "white")


#### Figure 1 – Firm cessation & provider deregistration rates (main paper)####

library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

cqc22     <- read.csv("~/Downloads/inspection and location data april 2025(in).csv",
                      stringsAsFactors = FALSE)

cessation <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Corporatisation_of_care_Big_Data/corporatisation_of_care/Data/companies_house_data_CQC_with_insolvency_full.csv",
                      stringsAsFactors = FALSE)

providers <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/providers_info_batch_all_2026.csv")

pad_company_number <- function(x) {
  x <- na_if(x, "")
  if_else(
    str_detect(x, "^[0-9]+$"),
    str_pad(x, width = 8, side = "left", pad = "0"),
    x
  )
}

cqc2 <- cqc22 %>%
  dplyr::rename(company_number = providercompanieshousenumber) %>%
  dplyr::distinct(locationid, provider_number, .keep_all = TRUE) %>%
  dplyr::mutate(company_number = pad_company_number(company_number))

firm_company_numbers <- cqc2 %>%
  pull(company_number) %>%
  unique() %>%
  na.omit()

message("Unique company numbers linked to care homes: ", comma(length(firm_company_numbers)))

# active beds by year, from each location's own start/end dates - this is the
# denominator: how many beds were actually open in the sector in a given year
active_beds_year <- cqc2 %>%
  filter(!is.na(year_location_start_2025)) %>%
  rowwise() %>%
  mutate(
    panel_end = if_else(is.na(year_location_end_2025), 2025L, as.integer(year_location_end_2025)),
    year      = list(seq(from = max(year_location_start_2025 + 1),
                         to   = min(panel_end, 2025L)))
  ) %>%
  unnest(cols = c(year)) %>%
  ungroup() %>%
  filter(year >= 2011, year <= 2025, !panel_end < year) %>%
  group_by(year) %>%
  summarise(active_beds = sum(carehomesbeds, na.rm = TRUE), .groups = "drop")

# historical total beds ever run by each company - a home that has already
# closed still counts here, so its beds stay "exposed" when the firm itself
# later ceases
carehomebeds <- cqc2 %>%
  filter(!is.na(year_location_start_2025)) %>%
  rowwise() %>%
  mutate(
    panel_end = if_else(is.na(year_location_end_2025), 2025L, as.integer(year_location_end_2025)),
    year      = list(seq(from = max(year_location_start_2025 + 1),
                         to   = min(panel_end, 2025L)))
  ) %>%
  unnest(cols = c(year)) %>%
  ungroup() %>%
  filter(year >= 2011, year <= 2025, !panel_end < year) %>%
  arrange(-year) %>%
  dplyr::mutate(replace_na(company_number, "non-corporation"))%>%
  group_by(next_group_id, company_number) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(company_number, carehomesbeds) %>%
  group_by(company_number) %>%
  summarise(carehomesbeds = sum(carehomesbeds, na.rm = TRUE), .groups = "drop")


firms <- cessation %>%
  filter(!is.na(company_number), company_number %in% firm_company_numbers) %>%
  arrange(company_number) %>%
  distinct(company_number, .keep_all = TRUE) %>%
  mutate(
    date_of_creation  = ymd(na_if(date_of_creation,  "")),
    date_of_cessation = ymd(na_if(date_of_cessation, "")),
    creation_year     = year(date_of_creation),
    cessation_year    = year(date_of_cessation)
  )

message("Firms (unique) selected: ",          comma(nrow(firms)))
message("Firms with any cessation date: ",    comma(sum(!is.na(firms$cessation_year))))

panel_firms <- firms %>%
  filter(!is.na(creation_year)) %>%
  rowwise() %>%
  mutate(
    panel_end = if_else(is.na(cessation_year), 2025L, as.integer(cessation_year)),
    year      = list(seq(from = max(creation_year, 2011L),
                         to   = min(panel_end, 2025L)))
  ) %>%
  unnest(cols = c(year)) %>%
  ungroup() %>%
  filter(year >= 2011, year <= 2025) %>%
  mutate(
    cessation_this_year = as.integer(!is.na(cessation_year) & year == cessation_year)
  ) %>%
  select(company_number, year, creation_year, cessation_year, cessation_this_year) %>%
  full_join(carehomebeds, by = "company_number") %>%
  filter(!is.na(carehomesbeds))

panel_providers <- providers %>%
  select(companiesHouseNumber, registrationDate, deregistrationDate) %>%
  distinct() %>%
  rename(
    company_number          = companiesHouseNumber,
    provider_registration   = registrationDate,
    provider_deregistration = deregistrationDate
  ) %>%
  mutate(
    company_number          = pad_company_number(company_number),
    provider_registration   = year(as.Date(provider_registration)),
    provider_deregistration = year(as.Date(provider_deregistration))
  ) %>%
  filter(!is.na(company_number)) %>%
  filter(!is.na(provider_registration)) %>%
  rowwise() %>%
  mutate(
    panel_end = if_else(is.na(provider_deregistration), 2025L, as.integer(provider_deregistration)),
    year      = list(seq(from = max(provider_registration, 2011L),
                         to   = min(panel_end, 2025L)))
  ) %>%
  unnest(cols = c(year)) %>%
  ungroup() %>%
  filter(year >= 2011, year <= 2025) %>%
  mutate(
    cessation_this_year = as.integer(!is.na(provider_deregistration) & year == provider_deregistration)
  ) %>%
  select(company_number, year, provider_registration, provider_deregistration, cessation_this_year) %>%
  full_join(carehomebeds, by = "company_number") %>%
  filter(!is.na(carehomesbeds))

# beds exposed to a firm cessation event this year, as a share of beds
# actually open in the sector this year
firm_year <- panel_firms %>%
  group_by(year) %>%
  summarise(beds_closed = sum(carehomesbeds * cessation_this_year, na.rm = TRUE), .groups = "drop") %>%
  left_join(active_beds_year, by = "year") %>%
  mutate(rate = beds_closed / active_beds)

# same logic for provider deregistrations
prov_year <- panel_providers %>%
  group_by(year) %>%
  summarise(beds_closed = sum(carehomesbeds * cessation_this_year, na.rm = TRUE), .groups = "drop") %>%
  left_join(active_beds_year, by = "year") %>%
  mutate(rate = beds_closed / active_beds)


#reg_inspection_closure

inspects <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/care_home_mortality/Data/complete inspection and location data_ben_may 2026.csv")


inspects <- inspects %>%
  dplyr::select(providercompanieshousenumber,carehomesbeds, next_group_id, closed_complete_year, closed_complete, overall, locationid)%>%
  dplyr::mutate(company_number = pad_company_number(providercompanieshousenumber),
                closed_complete = ifelse(closed_complete==0, NA, closed_complete),
                overall = ifelse(overall=="Outstanding", 4,
                                 ifelse(overall=="Good", 3,
                                        ifelse(overall=="Requires improvement", 2,
                                               ifelse(overall=="Requires Improvement", 2,
                                                      ifelse(overall=="Inadequate", 1, NA))))))%>%
  dplyr::group_by(next_group_id)%>%
  tidyr::fill(closed_complete, .direction = "updown") %>%
  dplyr::ungroup()%>%
  dplyr::group_by(locationid, closed_complete, company_number, carehomesbeds)%>%
  dplyr::summarise(overall = mean(overall, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(closed_complete = ifelse(is.na(closed_complete), 0, closed_complete))%>%
  dplyr::left_join(., firms)%>%
  dplyr::mutate(ceased   = factor(ifelse(!is.na(cessation_year), "Ceased", 
                                         ifelse(is.na(company_number), "Non-corporation", "Never Ceased")),
                                  levels = c( "Non-corporation", "Never Ceased", "Ceased")))

library(ggeffects)

one <- lm(overall~ceased, data = inspects, weights = carehomesbeds)

two <- lm(closed_complete~ceased, data = inspects, weights = carehomesbeds)


one <- ggpredict(one)
two <- ggpredict(two)

one <- one$ceased %>%
  tibble()

two <- two$ceased %>%
  tibble()



main_col     <- "#2b8cbe"
compare_cols <- c("Never Ceased" = "#4575b4", "Ceased" = "#d73027",  "Non-corporation" = "#006400")
x_breaks     <- seq(2011, 2025, 2)

theme_journal <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "grey92"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.title       = element_text(face = "bold", size = rel(1.0), hjust = 0),
    plot.caption     = element_text(size = rel(0.8), colour = "grey40"),
    axis.title       = element_text(size = rel(0.95)),
    axis.text        = element_text(size = rel(0.9), colour = "black"),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    strip.text       = element_text(face = "bold", size = rel(0.95)),
    strip.background = element_rect(fill = "grey95", colour = NA)
  )


sc_col <- "#b2182b"  # muted red used only for Southern Cross annotation

p_a <- ggplot(firm_year, aes(x = year, y = rate * 100)) +
  # Southern Cross: spike appears in 2013 (lagged legal dissolutions)
  annotate("rect",
           xmin = 2012.5, xmax = 2013.5, ymin = -Inf, ymax = Inf,
           fill = sc_col, alpha = 0.07) +
  geom_vline(xintercept = 2013, linetype = "dashed",
             colour = sc_col, linewidth = 0.45, alpha = 0.7) +
  geom_line(linewidth = 1.0, colour = main_col) +
  geom_point(shape = 21, size = 2.2, stroke = 0.6,
             fill = "white", colour = "black") +
  annotate("text",
           x = 2013.15, y = Inf,
           label = "Southern Cross\ncollapse",
           hjust = 0, vjust = 1.4,
           size = 2.9, colour = sc_col, fontface = "italic",
           lineheight = 0.9) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  labs(title = "A. Beds exposed to firm cessation",
       x = "Year", y = "Share of active beds (%)") +
  theme_journal

p_b <- ggplot(prov_year, aes(x = year, y = rate * 100)) +
  # Southern Cross: spike appears immediately in 2011 (provider deregistrations)
  annotate("rect",
           xmin = 2010.5, xmax = 2011.5, ymin = -Inf, ymax = Inf,
           fill = sc_col, alpha = 0.07) +
  geom_vline(xintercept = 2011, linetype = "dashed",
             colour = sc_col, linewidth = 0.45, alpha = 0.7) +
  geom_line(linewidth = 1.0, colour = main_col) +
  geom_point(shape = 21, size = 2.2, stroke = 0.6,
             fill = "white", colour = "black") +
  annotate("text",
           x = 2011.15, y = Inf,
           label = "Southern Cross\ncollapse",
           hjust = 0, vjust = 1.4,
           size = 2.9, colour = sc_col, fontface = "italic",
           lineheight = 0.9) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  labs(title = "B. Beds exposed to provider deregistration",
       x = "Year", y = "Share of active beds (%)") +
  theme_journal

p_c <- ggplot(one, aes(x =predicted , y = x, color = x)) +
  geom_point(alpha = 0.9, size = 2) +
  geom_errorbar(
    aes(xmin = conf.low, xmax =conf.high),
    width = 0.2, linewidth = 0.5
  ) +
  scale_color_manual(values = compare_cols) +
  labs(
    title = "C. Quality of homes by firm cessation",
    x = "Quality rating (1-4)", y = "Firm cessation"
  ) +
  coord_cartesian(xlim = c(1,4))+
  theme_journal +
  theme(
    axis.text.x      = element_text(angle = 0, hjust = 0.5),
    legend.position  = "none",
    panel.spacing    = unit(1, "lines")
  )

p_d <- ggplot(two,  aes(x =predicted , y = x, color = x)) +
  geom_point(alpha = 0.9, size = 2) +
  geom_errorbar(
    aes(xmin = conf.low, xmax =conf.high),
    width = 0.2, linewidth = 0.5
  ) +
  scale_color_manual(values = compare_cols) +
  labs(
    title = "D. Closure of homes by firm cessation",
    x = "Probability of closure", y = "Firm cessation"
  ) +
  coord_cartesian(xlim = c(0,1))+
  theme_journal +
  theme(
    axis.text.x      = element_text(angle = 0, hjust = 0.5),
    legend.position  = "none",
    panel.spacing    = unit(1, "lines")
  )

final_plot <- (p_a + p_b) / (p_c + p_d) +
  plot_layout(heights = c(1, 0.5)) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", colour = NA),
      plot.caption    = element_text(size = 9, hjust = 0, colour = "grey30",
                                     margin = margin(t = 10))
    )
  )

print(final_plot)

ggsave(
  "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/care_home_mortality/Figures/figure1.png",
  plot = final_plot, width = 12, height = 8, units = "in", dpi = 600, bg = "white"
)



#### Figure 2: Chain vs individual firm composition, openings, closures (2011-2025)####

library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(ggeffects)
library(zoo)

#  Uncomment if running standalone (i.e. Figure 1 block hasn't been run this session) 
# cqc22 <- read.csv("~/Downloads/inspection and location data april 2025(in).csv", stringsAsFactors = FALSE)
# cessation <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Corporatisation_of_care_Big_Data/corporatisation_of_care/Data/companies_house_data_CQC_with_insolvency_full.csv", stringsAsFactors = FALSE)
#
# pad_company_number <- function(x) {
#   x <- na_if(x, "")
#   if_else(str_detect(x, "^[0-9]+$"), str_pad(x, width = 8, side = "left", pad = "0"), x)
# }
#
# cqc2_f1 <- cqc22 %>%
#   rename(company_number = providercompanieshousenumber) %>%
#   distinct(locationid, provider_number, .keep_all = TRUE) %>%
#   mutate(company_number = pad_company_number(company_number))
#
# firm_company_numbers <- cqc2_f1 %>% pull(company_number) %>% unique() %>% na.omit()


# rolling window, in months, used to smooth every monthly series below - change this one number to change all of them
roll_window <- 12

# set to FALSE to skip the optional stacked ownership-composition chart
show_stacked_extra <- TRUE


cqc2 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/care_home_mortality/Data/complete inspection and location data_ben_may 2026.csv")

cqc2 <- cqc2 %>%
  dplyr::select(providercompanieshousenumber, carehomesbeds, brandname, next_group_id, ownership,
                closed_complete_year, closed_complete, year_location_start_2026, overall, locationid, providername,
                location_start_2026, location_end_2026) %>%
  dplyr::mutate(
    company_number  = pad_company_number(providercompanieshousenumber),
    closed_complete = ifelse(closed_complete == 0, NA, closed_complete),
    location_start_2026 = as.Date(location_start_2026, format = "%d%b%Y"),
    location_end_2026   = as.Date(location_end_2026, origin = "1960-01-01"),
    overall = ifelse(overall == "Outstanding", 4,
                     ifelse(overall == "Good", 3,
                            ifelse(overall %in% c("Requires improvement", "Requires Improvement"), 2,
                                   ifelse(overall == "Inadequate", 1, NA))))
  ) %>%
  dplyr::group_by(next_group_id) %>%
  tidyr::fill(closed_complete, .direction = "updown") %>%
  dplyr::ungroup()

firm_locations <- cqc2 %>%
  filter(!is.na(company_number)) %>%
  group_by(company_number) %>%
  summarise(n_locations = n_distinct(locationid, na.rm = TRUE))

# NOTE: table(cqc2$ownership) is worth checking - this assumes "Individual" ownership
# is coded either as the string "Individual" or as 1; adjust the values below to match
charity_status <- cessation %>%  
  filter(!is.na(company_number)) %>%  
  mutate(is_charity = company_type %in% c("private-limited-guarant-nsc",              
                                          "private-limited-guarant-nsc-limited-exemption")) %>%  
  mutate(    date_of_creation  = ymd(na_if(date_of_creation,  "")),    
             date_of_cessation = ymd(na_if(date_of_cessation, "")),    
             creation_year     = year(date_of_creation),    
             cessation_year    = year(date_of_cessation)  ) %>%  
  select(company_number, is_charity, company_type, date_of_creation, date_of_cessation, creation_year, cessation_year) %>%  
  distinct() 

# firm_category7 classification, in priority order:
#   Charity                  - company_type carries the charitable "limited exemption" guarantee code
#   Non-profit firm           - guarantee company (no share capital) without the charitable exemption
#   Public                    - plc / old public company
#   Independent/Partnership   - llp / limited partnership / partnership
#   Individual                - CQC ownership field flags the provider as an individual, not an organisation
#   Chain                     - branded operator running more than one home
#   Other firm                - any remaining registered company


  firm_type <- cqc2 %>%
  dplyr::select(company_number, brandname, providername, locationid, carehomesbeds, closed_complete_year, year_location_start_2026, ownership,
                location_start_2026, location_end_2026) %>%
  distinct() %>%
  left_join(firm_locations, by = "company_number") %>%
  left_join(charity_status, by = "company_number") %>%
  mutate(
    is_charity    =  ownership==2,
    is_nonprofit   = replace_na(company_type %in% c( "private-limited-guarant-nsc", "private-limited-guarant-nsc-limited-exemption"), FALSE),
    is_public      = ownership==1,
    is_partnership = ownership==0 & is.na(company_number),
    is_chain       = !is.na(brandname) & brandname != "-" & brandname != "",
    is_single     = (n_locations == 1) & !is_chain,
    firm_category7 = case_when(
      is_charity     ~ "Charity",
      is_nonprofit   ~ "Non-profit firm",
      is_public      ~ "Public",
      is_partnership ~ "Independent/Partnership",
      is_single  ~ "Single location",
      is_chain       ~ "Chain",
      TRUE           ~ "Other firm"
    )
  ) %>%
    dplyr::select(company_number, providername, locationid, carehomesbeds, n_locations, is_chain,is_single, firm_category7, brandname, date_of_creation, date_of_cessation, creation_year, cessation_year, closed_complete_year, year_location_start_2026, location_start_2026, location_end_2026)

# monthly location panel - only year-level start/close dates exist in this data, so
# each active year is expanded into its 12 months (Jan of the start year through Dec
# of the close year); if you have true month-level dates, swap them in here instead
  firm_type_monthly <- firm_type %>%
    rowwise() %>%
    dplyr::filter(!is.na(location_start_2026)) %>%
    mutate(
      start_date = floor_date(location_start_2026, "month"),
      end_date   = floor_date(if_else(is.na(location_end_2026), as.Date("2025-12-01"), location_end_2026), "month"),
      start_date = pmax(start_date, as.Date("2011-01-01")),
      end_date   = pmin(end_date, as.Date("2025-12-01"))
    ) %>%
    dplyr::filter(end_date >= start_date) %>%
    mutate(
      month = list(seq(start_date, end_date, by = "month"))
    ) %>%
    unnest(cols = c(month)) %>%
    ungroup() %>%
    filter(month >= as.Date("2011-01-01"), month <= as.Date("2025-12-01")) %>%
    mutate(
      chain_indicator      = as.integer(firm_category7 == "Chain"),
      individual_indicator = as.integer(firm_category7 == "Single location"),
      nonprofit_indicator  = as.integer(firm_category7 %in% c("Charity", "Non-profit firm"))
    ) %>%
    select(month, firm_category7, chain_indicator, individual_indicator, nonprofit_indicator, carehomesbeds)

# monthly bed share, per all beds active that month, then smoothed with a rolling mean
monthly_bed_share <- firm_type_monthly %>%
  group_by(month) %>%
  summarise(
    beds_chain      = sum(carehomesbeds * chain_indicator, na.rm = TRUE),
    beds_individual = sum(carehomesbeds * individual_indicator, na.rm = TRUE),
    beds_nonprofit  = sum(carehomesbeds * nonprofit_indicator, na.rm = TRUE),
    total_beds      = sum(carehomesbeds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(month) %>%
  mutate(
    rate_chain      = beds_chain / total_beds,
    rate_individual = beds_individual / total_beds,
    rate_nonprofit  = beds_nonprofit / total_beds,
    rate_chain_smooth      = zoo::rollmean(rate_chain, k = roll_window, fill = NA, align = "center"),
    rate_individual_smooth = zoo::rollmean(rate_individual, k = roll_window, fill = NA, align = "center"),
    rate_nonprofit_smooth  = zoo::rollmean(rate_nonprofit, k = roll_window, fill = NA, align = "center")
  )


inspects2 <- cqc2 %>%
  dplyr::group_by(locationid, closed_complete, company_number, carehomesbeds) %>%
  dplyr::summarise(overall = mean(overall, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(closed_complete = ifelse(is.na(closed_complete), 0, closed_complete)) %>%
  dplyr::left_join(firm_type %>%
                     dplyr::select(firm_category7, locationid), by = "locationid") %>%
  dplyr::mutate(
    firm_category7 = factor(firm_category7, levels = rev(c("Single location", "Independent/Partnership", "Other firm", "Non-profit firm", "Charity", "Public", "Chain")))  )

m_rating  <- lm(overall         ~ firm_category7, data = inspects2, weights = carehomesbeds)
m_closure <- lm(closed_complete ~ firm_category7, data = inspects2, weights = carehomesbeds)

pred_rating  <- ggpredict(m_rating)
pred_closure <- ggpredict(m_closure)

pred_rating  <- pred_rating$firm_category7  %>% tibble()
pred_closure <- pred_closure$firm_category7 %>% tibble()


main_col     <- "#2b8cbe"
compare_cols <- c(
  "Single location"              = "#FF8DA1",
  "Chain"                   = "#d73027",
  "Independent/Partnership" = "#e6ab02",
  "Non-profit firm"         = "#91cf60",
  "Charity"                 = "#2b8cbe",
  "Public"                  = "#7570b3",
  "Other firm"              = "#999999"
)
date_breaks_axis <- "2 years"

theme_journal <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "grey92"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.title       = element_text(face = "bold", size = rel(1.0), hjust = 0),
    plot.caption     = element_text(size = rel(0.8), colour = "grey40"),
    axis.title       = element_text(size = rel(0.95)),
    axis.text        = element_text(size = rel(0.9), colour = "black"),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    strip.text       = element_text(face = "bold", size = rel(0.95)),
    strip.background = element_rect(fill = "grey95", colour = NA)
  )

p_a <- ggplot(monthly_bed_share, aes(x = month, y = rate_chain_smooth * 100)) +
  geom_line(linewidth = 1.0, colour = main_col) +
  scale_x_date(date_breaks = date_breaks_axis, date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.01, 0.08))) +
  coord_cartesian(ylim = c(0, 80)) +
  labs(title = "A. Chain share of active beds", x = "Year", y = paste0(roll_window, "-month rolling share (%)")) +
  theme_journal

p_b <- ggplot(monthly_bed_share, aes(x = month, y = rate_individual_smooth * 100)) +
  geom_line(linewidth = 1.0, colour = main_col) +
  scale_x_date(date_breaks = date_breaks_axis, date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.01, 0.08))) +
  coord_cartesian(ylim = c(0, 80)) +
  labs(title = "B. Individual share of active beds", x = "Year", y = paste0(roll_window, "-month rolling share (%)")) +
  theme_journal

p_c_beds <- ggplot(monthly_bed_share, aes(x = month, y = rate_nonprofit_smooth * 100)) +
  geom_line(linewidth = 1.0, colour = main_col) +
  scale_x_date(date_breaks = date_breaks_axis, date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.01, 0.08))) +
  coord_cartesian(ylim = c(0, 80)) +
  labs(title = "C. Non-profit firm share of active beds", x = "Year", y = paste0(roll_window, "-month rolling share (%)")) +
  theme_journal

p_d <- ggplot(pred_rating, aes(x = predicted, y = x, color = x)) +
  geom_point(alpha = 0.9, size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, linewidth = 0.5) +
  scale_color_manual(values = compare_cols) +
  labs(title = "B. Inspection rating by firm type", x = "Quality rating (1-4)", y = "Firm type") +
  coord_cartesian(xlim = c(1, 4)) +
  theme_journal +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.position = "none",
        panel.spacing = unit(1, "lines"))

p_e <- ggplot(pred_closure, aes(x = predicted, y = x, color = x)) +
  geom_point(alpha = 0.9, size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, linewidth = 0.5) +
  scale_color_manual(values = compare_cols) +
  labs(title = "C. Closure likelihood by firm type", x = "Probability of closure", y = "Firm type") +
  coord_cartesian(xlim = c(0, 1)) +
  theme_journal +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.position = "none",
        panel.spacing = unit(1, "lines"))






  
  monthly_stack <- firm_type_monthly %>%
    group_by(month, firm_category7) %>%
    summarise(beds = sum(carehomesbeds, na.rm = TRUE), .groups = "drop") %>%
    group_by(month) %>%
    mutate(share = beds / sum(beds, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(firm_category7, month) %>%
    group_by(firm_category7) %>%
    mutate(share_smooth = zoo::rollmean(share, k = roll_window, fill = NA, align = "center")) %>%
    ungroup()
  
  monthly_stack <- monthly_stack %>%
    mutate(firm_category7 = factor(firm_category7, levels = c("Single location", "Independent/Partnership", "Other firm", "Non-profit firm", "Charity", "Public", "Chain")))
  
  p_stack <- ggplot(monthly_stack, aes(x = month, y = share_smooth * 100, fill = firm_category7)) +
    geom_area(position = "stack", colour = "white", linewidth = 0.15, alpha = 0.95) +
    scale_fill_manual(values = compare_cols, breaks = levels(monthly_stack$firm_category7)) +
    scale_x_date(date_breaks = date_breaks_axis, date_labels = "%Y", expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02)), breaks = seq(0, 100, 25)) +
    labs(title = "A. Ownership composition of active beds", x = "Year", y = paste0(roll_window, "-month rolling share (%)"), fill = "Ownership type") +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(alpha = 1))) +
    theme_journal
  
  print(p_stack)
  

  final_plot <- (p_stack) / (p_d + p_e) +
    plot_layout(heights = c(1, 0.7)) +
    plot_annotation(
      theme = theme(
        plot.background = element_rect(fill = "white", colour = NA),
        plot.caption    = element_text(size = 9, hjust = 0, colour = "grey30", margin = margin(t = 10))
      )
    )
  print(final_plot)
  
  ggsave(
    "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/care_home_mortality/Figures/figure2.png",
    plot = final_plot, width = 12, height = 8, units = "in", dpi = 600, bg = "white"
  )





#### Figure 3 Officer characteristics (2000-2018)####
  
  
  library(tidyverse)
  library(data.table)
  library(lubridate)
  library(scales)
  library(ggrepel)
  library(patchwork)
  
  jsp_theme <- theme_minimal(base_family = "serif", base_size = 12) +
    theme(
      plot.title = element_text(size = 13, face = "bold", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 10.5, colour = "grey30", margin = margin(b = 12)),
      plot.caption = element_text(size = 8.5, colour = "grey40", hjust = 0, margin = margin(t = 10)),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 9.5, colour = "grey20"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      legend.position = "none",
      plot.margin = margin(10, 55, 10, 10)
    )
  
  highlight_col <- "#B5241C"
  mute_col <- "grey70"
  
  # top half: composition of firms related to CQC-provider directors, real estate highlighted
  # assumes cqc2, carehomebeds and pad_company_number() already exist from the Figure 1 block
  
  Company_data <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/PhDing2020/Data/BasicCompanyDataAsOneFile-2019-07-01.csv")
  
  cqc_companies <- cqc2 %>%
    transmute(company_number = pad_company_number(company_number)) %>%
    filter(!is.na(company_number), company_number != "") %>%
    distinct()
  
  officers <- fread("~/Library/CloudStorage/OneDrive-Nexus365/Documents/PhDing2020/Data/ch_full_officers.tsv.gz") %>%
    filter(str_detect(`Officer Role`, regex("director", ignore_case = TRUE)))
  
  titles_pattern <- "\\b(?i)(mr|mrs|ms|miss|dr|prof|sir|dame|lady|lord|squire|rev|fr|capt|col|maj|lt|hon|judge|the rt hon)\\.?\\b"
  
  officers <- officers %>%
    mutate(across(everything(), ~ iconv(.x, from = "", to = "UTF-8"))) %>%
    mutate(
      CleanName = Name %>%
        str_replace_all("(?i)\\b(ltd|limited)\\b", "") %>%
        str_replace_all(",,+", ",") %>%
        str_replace_all("[\\[\\]\\(\\)<>]", "") %>%
        str_replace_all("[:@*\\\\+`]+", " ") %>%
        str_replace_all("\\s*&\\s*", " AND ") %>%
        str_replace_all("//+", " ") %>%
        str_squish()
    ) %>%
    filter(
      !str_detect(Name, regex("\\b(ltd|limited)\\b", ignore_case = TRUE)),
      !is.na(CleanName),
      CleanName != ""
    ) %>%
    mutate(
      Surname = str_to_upper(str_trim(str_extract(CleanName, "^[^,]+"))),
      Surname = ifelse(Name == ",MANNAN, Abdul", "MANNAN", Surname),
      Surname = Surname %>% str_replace_all("[-]", " ") %>% str_squish() %>% word(1) %>% str_to_upper(),
      Forenames = str_trim(str_remove(CleanName, "^[^,]+,")),
      Forenames = str_replace_all(Forenames, "^,+|,+$", "") %>% str_squish(),
      first_names = str_to_upper(str_extract(Forenames, "^[A-Za-zÀ-ÿ\\-']+")),
      first_names = case_when(
        is.na(first_names) | first_names == "" ~ str_to_upper(
          str_extract(Forenames, regex("\\b(LORD|DUKE|EARL|BARON|MARQUESS|MARQUIS|VISCOUNT|COUNTESS|HON|RT\\.?\\s*HON)\\b", ignore_case = TRUE))
        ),
        TRUE ~ first_names
      ),
      dob = na_if(`Date of Birth`, "N/A"),
      birth_month = as.integer(str_extract(str_extract(dob, "month':\\s*(\\d+)"), "\\d+")),
      second_names = str_to_upper(str_trim(str_extract(Forenames, "(?<=\\s)[A-Za-zÀ-ÿ\\-']+"))),
      YoB = str_extract(`Date of Birth`, "\\d{4}")
    ) %>%
    distinct(.keep_all = TRUE)
  
  officers <- officers %>%
    mutate(
      first_names = as.character(first_names), Surname = as.character(Surname),
      birth_month = as.character(birth_month), YoB = as.character(YoB),
      region = as.character(region), CompanyNumber = as.character(CompanyNumber),
      Appointed = as.character(Appointed), Resigned = as.character(Resigned)
    ) %>%
    mutate(
      across(c(first_names, Surname, birth_month, YoB, region),
             ~ .x %>% tidyr::replace_na("") %>% stringr::str_to_lower() %>%
               stringr::str_remove_all("[^[:alnum:]]+") %>% stringr::str_trim(),
             .names = "{.col}_norm"),
      key = paste(first_names_norm, Surname_norm, birth_month_norm, YoB_norm, region_norm, sep = "_"),
      appointed = suppressWarnings(as.Date(Appointed)),
      Resigned_clean = na_if(Resigned, "N/A") %>% na_if("NA") %>% na_if(""),
      resigned = suppressWarnings(if_else(is.na(Resigned_clean), as.Date(NA), as.Date(Resigned_clean))),
      CompanyNumber = pad_company_number(CompanyNumber)
    )
  
  officers_cqc <- officers %>%
    filter(CompanyNumber %in% cqc_companies$company_number) %>%
    select(CompanyNumber, key, first_names, Surname, birth_month, YoB, region, appointed, resigned, Occupation) %>%
    distinct() %>%
    mutate(appointed = as.Date(appointed), resigned = as.Date(resigned))
  
  related_officers <- officers %>%
    filter(!CompanyNumber %in% cqc_companies$company_number) %>%
    filter(!is.na(CompanyNumber), CompanyNumber != "") %>%
    filter(key %in% officers_cqc$key) %>%
    select(CompanyNumber, key, first_names, Surname, birth_month, YoB, region, appointed, resigned, Occupation) %>%
    distinct()
  
  related_company_type <- Company_data %>%
    transmute(
      CompanyNumber = pad_company_number(CompanyNumber),
      sic_1 = str_extract(SICCode.SicText_1, "^\\d{5}"), sic_1_label = str_trim(str_remove(SICCode.SicText_1, "^\\d{5}\\s*-\\s*")),
      sic_2 = str_extract(SICCode.SicText_2, "^\\d{5}"),
      sic_3 = str_extract(SICCode.SicText_3, "^\\d{5}"),
      sic_4 = str_extract(SICCode.SicText_4, "^\\d{5}")
    ) %>%
    mutate(
      health = str_detect(sic_1, "^(86|87|88)") | str_detect(sic_2, "^(86|87|88)") |
        str_detect(sic_3, "^(86|87|88)") | str_detect(sic_4, "^(86|87|88)"),
      valid_sic = (!is.na(sic_1) & sic_1 != "99999" & sic_1 != "None Supplied") |
        (!is.na(sic_2) & sic_2 != "99999" & sic_2 != "None Supplied") |
        (!is.na(sic_3) & sic_3 != "99999" & sic_3 != "None Supplied") |
        (!is.na(sic_4) & sic_4 != "99999" & sic_4 != "None Supplied"),
      company_type = case_when(health ~ "health-care", valid_sic ~ "non-health-care", TRUE ~ "unknown")
    ) %>%
    distinct(CompanyNumber, .keep_all = TRUE)
  
  related_officers <- related_officers %>%
    left_join(related_company_type %>% select(CompanyNumber, company_type), by = "CompanyNumber")
  
  cqc_officer_related <- officers_cqc %>%
    select(cqc_company_number = CompanyNumber, key, cqc_appointed = appointed, cqc_resigned = resigned) %>%
    inner_join(
      related_officers %>%
        select(related_company_number = CompanyNumber, key, company_type, related_appointed = appointed, related_resigned = resigned),
      by = "key", relationship = "many-to-many"
    ) %>%
    filter(related_company_number != cqc_company_number) %>%
    mutate(company_type = ifelse(is.na(company_type), "unknown", company_type)) %>%
    filter(!is.na(cqc_appointed), !is.na(related_appointed)) %>%
    mutate(
      cqc_active_start = pmax(year(cqc_appointed), 2001),
      cqc_active_end = pmin(year(if_else(is.na(cqc_resigned), as.Date("2018-12-31"), cqc_resigned)), 2018),
      related_active_start = pmax(year(related_appointed), 2001),
      related_active_end = pmin(year(if_else(is.na(related_resigned), as.Date("2018-12-31"), related_resigned)), 2018),
      overlap_start = pmax(cqc_active_start, related_active_start),
      overlap_end = pmin(cqc_active_end, related_active_end)
    ) %>%
    filter(overlap_start <= overlap_end)
  
  cqc_related_lookup_dedup <- cqc_officer_related %>%
    distinct(key, related_company_number, cqc_company_number) %>%
    group_by(key, related_company_number) %>%
    slice(1) %>%
    ungroup()
  
  cqc_officer_related_years <- cqc_officer_related %>%
    rowwise() %>%
    mutate(year = list(seq(overlap_start, overlap_end))) %>%
    ungroup() %>%
    unnest(year) %>%
    distinct(year, key, related_company_number, company_type) %>%
    left_join(cqc_related_lookup_dedup, by = c("key", "related_company_number"))
  
  non_productive_sic <- tribble(
    ~sic_5, ~sic_label,
    "64209", "Activities of other holding companies",
    "64200", "Activities of holding companies",
    "64191", "Banks",
    "64301", "Activities of investment trusts",
    "64302", "Activities of unit trusts",
    "64303", "Activities of venture and development capital companies",
    "64304", "Activities of open-ended investment companies",
    "64305", "Activities of property unit trusts",
    "64306", "Activities of real estate investment trusts",
    "64921", "Credit granting by non-deposit taking finance houses",
    "64929", "Other credit granting n.e.c.",
    "64991", "Security dealing on own account",
    "64999", "Financial intermediation n.e.c.",
    "66190", "Activities auxiliary to financial services",
    "66210", "Risk and damage evaluation",
    "66220", "Activities of insurance agents and brokers",
    "66300", "Fund management activities",
    "68100", "Buying and selling of own real estate",
    "68201", "Renting and operating of Housing Association real estate",
    "68209", "Other letting and operating of own or leased real estate",
    "68320", "Management of real estate on a fee or contract basis",
    "70100", "Activities of head offices",
    "70210", "Public relations and communications activities",
    "70221", "Financial management",
    "70229", "Management consultancy activities other than financial management",
    "82990", "Other business support service activities n.e.c.",
    "94990", "Activities of other membership organisations n.e.c.",
    "98000", "Residents property management",
    "99999", "Dormant Company",
    "74990", "Non-trading company"
  )
  
  related_sic_full <- cqc_officer_related_years %>%
    distinct(year, key, related_company_number, company_type) %>%
    left_join(
      Company_data %>%
        transmute(
          related_company_number = pad_company_number(CompanyNumber),
          sic_1 = str_extract(SICCode.SicText_1, "^\\d{5}"),
          sic_1_label = str_trim(str_remove(SICCode.SicText_1, "^\\d{5}\\s*-\\s*")),
          sic_2 = str_extract(SICCode.SicText_2, "^\\d{5}"),
          sic_3 = str_extract(SICCode.SicText_3, "^\\d{5}"),
          sic_4 = str_extract(SICCode.SicText_4, "^\\d{5}")
        ) %>%
        distinct(related_company_number, .keep_all = TRUE),
      by = "related_company_number"
    ) %>%
    mutate(
      is_non_productive = sic_1 %in% non_productive_sic$sic_5 | sic_2 %in% non_productive_sic$sic_5 |
        sic_3 %in% non_productive_sic$sic_5 | sic_4 %in% non_productive_sic$sic_5,
      sic_2digit = str_sub(sic_1, 1, 2)
    )
  
  related_sic_clean <- related_sic_full %>%
    filter(!sic_1 %in% c("99999", "74990"))
  
  related_sic_nonresi <- related_sic_clean %>%
    filter(sic_2digit != "87")
  
  top_sic2_nonresi <- related_sic_nonresi %>%
    filter(year >= 2010) %>%
    count(sic_2digit, sort = TRUE) %>%
    filter(!is.na(sic_2digit)) %>%
    slice_head(n = 10) %>%
    pull(sic_2digit)
  
  sic2_nonresi_labels <- related_sic_nonresi %>%
    filter(sic_2digit %in% top_sic2_nonresi) %>%
    distinct(sic_2digit, sic_1_label) %>%
    group_by(sic_2digit) %>%
    slice(1) %>%
    ungroup()
  
  sic2_nonresi_by_year <- related_sic_nonresi %>%
    filter(year >= 2010, sic_2digit %in% top_sic2_nonresi) %>%
    count(year, sic_2digit) %>%
    complete(year = 2010:2018, sic_2digit = top_sic2_nonresi, fill = list(n = 0)) %>%
    group_by(year) %>%
    mutate(pct_of_year = 100 * n / sum(n)) %>%
    ungroup() %>%
    left_join(sic2_nonresi_labels, by = "sic_2digit") %>%
    mutate(
      is_re = sic_2digit == "68"|sic_2digit == "64",
      sic_label_short = paste0(sic_2digit, " - ", str_trunc(sic_1_label, 40))
    )
  
  end_labels_nonresi <- sic2_nonresi_by_year %>% filter(year == max(year))
  
  p_top <- ggplot(sic2_nonresi_by_year, aes(x = year, y = pct_of_year, group = sic_2digit)) +
    geom_line(data = filter(sic2_nonresi_by_year, !is_re), colour = mute_col, linewidth = 0.6) +
    geom_point(data = filter(sic2_nonresi_by_year, !is_re), colour = mute_col, size = 1.6) +
    geom_line(data = filter(sic2_nonresi_by_year, is_re), colour = highlight_col, linewidth = 1.3) +
    geom_point(data = filter(sic2_nonresi_by_year, is_re), colour = highlight_col, size = 2.6) +
    geom_text_repel(
      data = end_labels_nonresi, aes(label = sic_label_short, colour = is_re),
      hjust = 0, nudge_x = 0.3, direction = "y", family = "serif", size = 3.1,
      segment.size = 0.3, segment.colour = "grey60", box.padding = 0.3, show.legend = FALSE
    ) +
    scale_colour_manual(values = c(`TRUE` = highlight_col, `FALSE` = "grey40")) +
    scale_x_continuous(breaks = 2010:2018, expand = expansion(mult = c(0.02, 0.16))) +
    scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0.02, 0.05))) +
    labs(
      x = NULL, y = "Share of related firms (%)",
      title = "A. Operations of firms linked to CQC-provider directors, 2010\u20132018",
      subtitle = "Top 10 two-digit SIC divisions, excluding residential care (SIC 87), dormant and non-trading companies"
    ) +
    theme_journal
  
  # bottom half: officer occupation characteristics (financier / carer share), harmonised style
  # assumes carehomebeds already exists if bed-weighting is added later - not applied here since
  # no bed data is joined in the source script, so these remain unweighted officer-year percentages
  
  cqc3_fig3 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Corporatisation_of_care_Big_Data/corporatisation_of_care/Data/ben_robustness check_location data 2.csv", stringsAsFactors = FALSE)
  cqc2_fig3_raw <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/complete inspection and location data_ben_feb2025v2.csv", stringsAsFactors = FALSE)
  insolvency_fig3 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Corporatisation_of_care_Big_Data/corporatisation_of_care/Data/companies_house_data_CQC_with_insolvency_full.csv", stringsAsFactors = FALSE)
  officers_related_fig3 <- read_csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Corporatisation_of_care_Big_Data/corporatisation_of_care/Data/health_officers_all_companies.csv")
  officers_health_fig3 <- read_csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/Corporatisation_of_care_Big_Data/corporatisation_of_care/Data/health_officers.csv")
  
  cqc_fig3 <- cqc3_fig3 %>%
    left_join(cqc2_fig3_raw %>% dplyr::select(providerid, providercompanieshousenumber) %>% distinct(), by = "providerid") %>%
    rename(company_number = providercompanieshousenumber) %>%
    left_join(insolvency_fig3, by = "company_number") %>%
    mutate(
      company_number = pad_company_number(company_number),
      closed_complete = ifelse(closed_complete == 0, NA, closed_complete)
    )
  
  norm <- function(x) {
    x %>% as.character() %>% replace_na("") %>% str_to_lower() %>%
      str_remove_all("[^[:alnum:]]+") %>% str_trim()
  }
  
  officers_fig3 <- officers_health_fig3 %>%
    filter(str_detect(`Officer Role`, regex("director", ignore_case = TRUE))) %>%
    mutate(
      across(c(first_names, Surname, birth_month, YoB, region), norm, .names = "{.col}_norm"),
      key = paste(first_names_norm, Surname_norm, birth_month_norm, YoB_norm, region_norm, sep = "_"),
      nhs = if_else(is.na(nhs_supplier) | nhs_supplier %in% c("NA", "N/A", "", "na"), 0L, 1L),
      appointed = as.Date(Appointed),
      Resigned_clean = na_if(Resigned, "N/A") %>% na_if("NA") %>% na_if(""),
      resigned = if_else(is.na(Resigned_clean), NA_Date_, as.Date(Resigned_clean))
    )
  
  related_officers_fig3 <- officers_related_fig3 %>%
    filter(!CompanyNumber %in% officers_health_fig3$CompanyNumber) %>%
    mutate(across(c(first_names, Surname, birth_month, YoB, region), norm, .names = "{.col}_norm")) %>%
    mutate(key = paste(first_names_norm, Surname_norm, birth_month_norm, YoB_norm, region_norm, sep = "_"))
  
  related_keys_fig3 <- unique(related_officers_fig3$key)
  
  multi_directors_fig3 <- officers_fig3 %>%
    dplyr::count(key) %>%
    filter(n > 4) %>%
    pull(key)
  
  officers_fig3 <- officers_fig3 %>%
    mutate(matched = key %in% related_keys_fig3, multi = key %in% multi_directors_fig3)
  
  cqc2check_fig3 <- cqc2_fig3_raw
  
  officerscqc <- officers_fig3 %>%
    dplyr::filter(CompanyNumber %in% cqc2check_fig3$providercompanieshousenumber) %>%
    dplyr::select(CompanyNumber, appointed, Resigned_clean, Occupation, matched, multi, key) %>%
    mutate(occupation_lower = str_squish(str_to_lower(Occupation)))
  
  officerscqc <- officerscqc %>%
    mutate(
      medic = str_detect(occupation_lower, paste(c(
        "medic", "doctor", "physician", "surgeon", "general pract", "\\bgp\\b", "psychiatr", "psycholog",
        "clinical", "dentist", "paramedic", "nurs", "\\brgn\\b", "\\bsrn\\b", "midwi", "pharmac", "therap",
        "physio", "physiotherap", "occupational therap", "radiograph", "optom", "podiatr", "chiropod",
        "dietician", "dietitian", "health", "carer", "social work", "social care", "support worker",
        "care worker", "care assistant", "md", "g p", "hospi", "healthcare", "health care", "matron"
      ), collapse = "|")),
      financier = str_detect(occupation_lower, paste(c(
        "accoun", "bookkeep", "book keeper", "audit", "financ", "financial", "cfo", "chief financial",
        "chief finance", "treasur", "capital", "bank", "invest", "investment", "investor", "fund",
        "portfolio", "wealth", "broker", "stockbroker", "actuar", "bursar", "economist", "busi",
        "commercial", "corporate", "entrepren", "enterpren", "propriet", "self employ", "manager",
        "management", "consult", "executive", "chief executive", "chief operating", "ceo", "coo",
        "managing director", "property manager"
      ), collapse = "|"))
    )
  
  officerscqc <- officerscqc %>%
    mutate(
      appointed = if_else(is.na(appointed), as.Date("2000-01-01"), appointed),
      resigned_date = if_else(is.na(Resigned_clean) | Resigned_clean == "", as.Date("2025-12-31"), as.Date(Resigned_clean)),
      appointed_year = year(appointed),
      resigned_year = year(resigned_date)
    )
  
  panel_officers <- officerscqc %>%
    rowwise() %>%
    mutate(panel_end = min(resigned_year, 2018L), year = list(seq(from = max(appointed_year, 2000L), to = panel_end))) %>%
    unnest(cols = c(year)) %>%
    ungroup() %>%
    filter(year >= 2001, year <= 2018) %>%
    mutate(
      matched_indicator = as.integer(matched),
      multi_indicator = as.integer(multi),
      medic_indicator = as.integer(medic),
      financier_indicator = as.integer(financier)
    )
  
  plotdf <- panel_officers %>%
    dplyr::select(year, medic_indicator, financier_indicator, matched_indicator, multi_indicator) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      medic_indicator = sum(medic_indicator, na.rm = TRUE),
      financier_indicator = sum(financier_indicator, na.rm = TRUE),
      multi_indicator = sum(multi_indicator, na.rm = TRUE),
      matched_indicator = sum(matched_indicator, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(panel_officers %>% dplyr::select(key, year) %>% dplyr::group_by(year) %>%
                       dplyr::summarise(total_officers = n_distinct(key)) %>% dplyr::ungroup(), by = "year") %>%
    dplyr::full_join(panel_officers %>% dplyr::filter(occupation_lower != "director") %>%
                       dplyr::select(key, year) %>% dplyr::group_by(year) %>%
                       dplyr::summarise(total_occupations = n_distinct(key)) %>% dplyr::ungroup(), by = "year") %>%
    dplyr::mutate(
      medic_indicator = medic_indicator / total_occupations * 100,
      multi_indicator = multi_indicator / total_officers * 100,
      matched_indicator = matched_indicator / total_officers * 100,
      financier_indicator = financier_indicator / total_occupations * 100
    )
  
  x_breaks <- seq(2001, 2018, 3)
  
  p_a <- ggplot(plotdf, aes(x = year, y = financier_indicator)) +
    geom_line(linewidth = 1.3, colour = highlight_col) +
    geom_point(size = 2.2, colour = highlight_col) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0.01, 0.05))) +
    labs(title = "B. Financier officers", x = "Year", y = "(%)") +
    theme_journal
  
  p_b <- ggplot(plotdf, aes(x = year, y = medic_indicator)) +
    geom_line(linewidth = 1.0, colour = mute_col) +
    geom_point(size = 2.2, colour = mute_col) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0.01, 0.05))) +
    labs(title = "C. Carer officers", x = "Year", y = "(%)") +
    theme_journal
  
  final_plot <-   p_top / (p_a | p_b) +
    plot_layout(heights = c(1.4, 1)) +
    plot_annotation(
      theme = theme(plot.caption = element_text(family = "serif", size = 8.5, colour = "grey40", hjust = 0))
    )
  
  
  
  
 
ggsave(
  "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/care_home_mortality/Figures/figure3_revised.png",
  plot = final_plot, width = 12, height = 8, units = "in", dpi = 600, bg = "white"
)






####ANALYSIS 4####

comm <- read.csv("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/adults_social_care_data/Final_data/commissioning_spend.csv")

full <- read.csv("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/adults_social_care_data/Final_data/expenditure.csv")

asc24 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/refs/heads/main/Raw_data/ASC-FR%20CSV%202023-24%20(Descriptions)%20V2.csv"))%>%
  dplyr::filter(FinanceType=="Expenditure",
                PrimarySupportReason=="99",
                AgeBand=="65 and Over")%>%
  dplyr::filter(SupportSetting=="99")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,FinanceDescription,PrimarySupportReason, ITEMVALUE)%>%
  dplyr::mutate(ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,FinanceDescription,PrimarySupportReason)%>%
  dplyr::summarise(ITEMVALUE=sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ITEMVALUE=ifelse(ITEMVALUE<0,0,ITEMVALUE),
                SupportSetting= "Total over 65",
                FinanceDescription = ifelse(FinanceDescription=="Own Provision", "In House",
                                            ifelse(FinanceDescription=="Provision by Others", "External",
                                                   ifelse(FinanceDescription=="99", "Total", NA))))%>%
  dplyr::rename(Sector=FinanceDescription)%>%
  dplyr::select(-PrimarySupportReason)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2024,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ITEMVALUE)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100,
                X=NA) %>%
  dplyr::ungroup()

asc25 <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/adults_social_care_data/Raw_data/adult-social-care-finance-report-2024-to-2025-data-tables-13-nov_TOTAL.csv", skip=5)%>%
  dplyr::select(LA.name,Short.term.care..65.and.over..2024.to.2025, Long.term.care..65.and.over..2024.to.2025)%>%
  mutate(
    Short_num = as.numeric(gsub("[£,]", "", Short.term.care..65.and.over..2024.to.2025)),
    Long_num  = as.numeric(gsub("[£,]", "", Long.term.care..65.and.over..2024.to.2025)),
    Total_expenditure = Short_num + Long_num
  )%>%
  dplyr::rename(DH_GEOGRAPHY_NAME = LA.name,
                Expenditure = Total_expenditure)%>%
  dplyr::mutate(SupportSetting = "Total over 65",
                Service=NA,
                Sector="Total",
                year=2025,
                X=NA,
                percent_sector=100)%>%
    dplyr::select(DH_GEOGRAPHY_NAME, year,SupportSetting, Service,X, Sector, percent_sector, Expenditure)

fully <- rbind(full, asc24, asc25) %>%
  dplyr::filter(SupportSetting=="Total over 65",
                Sector=="Total")%>%
  dplyr::mutate(DH_GEOGRAPHY_NAME = DH_GEOGRAPHY_NAME %>%
                  gsub('%20', " ",.)%>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  #gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  gsub(" THE", "",.)%>%
                  gsub("BEDFORD BOROUGH", "BEDFORD",.)%>%
                  str_trim())%>%
  dplyr::filter(!grepl("TOTAL", DH_GEOGRAPHY_NAME),
                DH_GEOGRAPHY_NAME!="ENGLAND",
                DH_GEOGRAPHY_NAME!="")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,year,  Expenditure)

ggplot(fully, aes(x=year, y=Expenditure))+
  geom_point()+
  geom_smooth()



comm <- comm %>%
  dplyr::filter(Sector=="Total")%>%
  dplyr::rename(commiss = Expenditure)%>%
  dplyr::select(DH_GEOGRAPHY_NAME, year, commiss,SupportSetting )


analysis_df <- full_join(comm, fully, by=c("year", "DH_GEOGRAPHY_NAME"))%>%
  dplyr::mutate(percent = as.numeric(commiss)/Expenditure*100,
                year_f = factor(year))


library(tidyverse)
library(scales)
library(patchwork)
library(broom)

# Data has already been prepared as analysis_df_comm and analysis_ass
# Filter to appropriate time periods
analysis_df_comm <- analysis_df %>%
  filter(SupportSetting == "commissiong_service_delivery",
         year >= 2015)  %>%
  dplyr::select(year, commiss, Expenditure)%>%
  dplyr::group_by(year)%>%
  dplyr::summarise(commiss = sum(commiss, na.rm=T),
                   Expenditure = sum(Expenditure, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(percent = commiss/Expenditure*100)

analysis_ass <- analysis_df %>%
  filter(SupportSetting == "assessment_care_management",
         year <= 2014)  # Assessment data ends 2014




theme_journal <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "grey92"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(face = "bold", size = rel(1.1), hjust = 0),
    plot.subtitle = element_text(size = rel(0.9), colour = "grey40", hjust = 0),
    plot.caption = element_text(size = rel(0.8), colour = "grey50", hjust = 0),
    axis.title = element_text(size = rel(0.95), face = "bold"),
    axis.text = element_text(size = rel(0.9), colour = "black"),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = rel(0.95), hjust = 0),
    strip.background = element_rect(fill = "grey95", colour = NA)
  )

# Color palette
main_col <- "#2b8cbe"
obs_col <- "#d73027"

p_commissioning <- ggplot() +
  # 95% CI ribbon
  # Predicted line
  geom_line(data = analysis_df_comm, 
            aes(x = year, y = percent), 
            linewidth = 1.2, colour = main_col) +
  geom_point(data = analysis_df_comm, 
             aes(x = year, y = percent),
             size = 2.5, colour = main_col) +
  # Observed means
  geom_point(data = analysis_df_comm, 
             aes(x = year, y = percent),
             shape = 21, size = 3.0, stroke = 1.2, 
             fill = "white", colour = obs_col) +
  geom_line(data = analysis_df_comm, 
            aes(x = year, y = percent),
            linetype = "dashed", alpha = 0.4, colour = obs_col) +
  # Scales
  scale_x_continuous(breaks = seq(2015, 2025, 2),
                     expand = c(0.02, 0.02)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.05, 0.1)),
                     limits = c(0, NA)) +
  # Labels
  labs(
    title = "Commissioning & Service Delivery",
    subtitle = "Proportion of total expenditure (2015-2025)",
    x = "Year",
    y = "% of Total Expenditure"
  ) +
  theme_journal

combined_expenditure <- p_commissioning

combined_expenditure <- combined_expenditure +
  plot_annotation(
    title = "Transaction costs over time",
    subtitle = "Trends in LA administrative and commissioning costs as proportion of over 65 spending",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle = element_text(size = 11, colour = "grey30", hjust = 0),
      
    )
  )

ggsave(
  "~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/care_home_mortality/Figures/figure4_revised.png",
  plot = combined_expenditure, width = 9, height = 5, units = "in", dpi = 600, bg = "white"
)

