library(readxl)
library(janitor)
library(tidyr)
library(dplyr)

years <- (2023:2025)

# this is a list of the UKPRN of all NI institutions so NI results can be
# isolated. Please note this does not include NI students at Open University
# as OU use one UKPRN for all campuses and so their inclusion would mean
# the inclusion of students domiciled outside NI

ni_ukprns <- c("10005343", 
               "10007807",
               "10008010",
               "10008026",
               "10020372",
               "10020633",
               "10020634",
               "10020680",
               "10020685",
               "10020699"
)


for (year in years) {
  assign(paste0("nss_", year),
         read.csv(paste0("//PR-DFE-FPASB01/ts_group/TEASB/Core Statistics Service/Higher Education/NSS/Public datasets/", year, "/NSS", year, "_fulldata.csv"))
         %>% 
           filter(UKPRN %in% ni_ukprns)) 
}

nss_2026 <- read_excel("//PR-DFE-FPASB01/ts_group/TEASB/Core Statistics Service/Higher Education/NSS/NSS 2026/NSS Results 2026.xlsx") %>% 
  filter(UKPRN %in% ni_ukprns)

nss <- bind_rows(
  mget(paste0("nss_", years)),
  .id = "year"
) %>%
  mutate(year = as.integer(sub("^nss_", "", year)))

## 1.1 Sex Chart by Q9 ----
library(tidyr)
library(ggplot2)

sex_data <- read.csv(
  "./outputs/sex_NSSQ9_crosstab_wide.csv"
)

sex_long <- pivot_longer(
  sex_data,
  cols = -Sex,
  names_to = "Response",
  values_to = "Count"
)

ggplot(
  sex_long,
  aes(
    x = Sex,
    y = Count,
    fill = Response
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "NSS Q9 Responses by Sex",
    x = NULL,
    y = "Percentage"
  ) +
  scale_fill_manual(
    values = c(
      "Very.well" = "#2E7D32",
      "Well" = "#81C784",
      "This.does.not.apply.to.me" = "#BDBDBD",
      "Not.very.well" = "#FF8A65",
      "Not.at.all.well" = "#C62828"
    )
  )
theme_minimal()

# 1.2 Deprivation Chart by Q9 ----
deprivation_data <- read.csv(
  "./outputs/deprivation_NSSQ9_crosstab_wide.csv"
)

deprivation_long <- pivot_longer(
  deprivation_data,
  cols = -Deprivation,
  names_to = "Response",
  values_to = "Count"
)

ggplot(
  deprivation_long,
  aes(
    x = Deprivation,
    y = Count,
    fill = Response
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "NSS Q9 Responses by Deprivation Quintile",
    x = NULL,
    y = "Percentage"
  ) +
  scale_fill_manual(
    values = c(
      "Very.well" = "#2E7D32",
      "Well" = "#81C784",
      "This.does.not.apply.to.me" = "#BDBDBD",
      "Not.very.well" = "#FF8A65",
      "Not.at.all.well" = "#C62828"
    )
  )
theme_minimal()

# 1.3 Disability Group Chart by Q9 ----
disability_data <- read.csv(
  "./outputs/disablegroup_NSSQ9_crosstab_wide.csv"
)

disability_long <- pivot_longer(
  disability_data,
  cols = -Disablegroup,
  names_to = "Response",
  values_to = "Count"
)

ggplot(
  disability_long,
  aes(
    x = Disablegroup,
    y = Count,
    fill = Response
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "NSS Q9 Responses by Disability Group",
    x = NULL,
    y = "Percentage"
  ) +
  scale_fill_manual(
    values = c(
      "Very.well" = "#2E7D32",
      "Well" = "#81C784",
      "This.does.not.apply.to.me" = "#BDBDBD",
      "Not.very.well" = "#FF8A65",
      "Not.at.all.well" = "#C62828"
    )
  )
theme_minimal()

# 1.4 Mode Chart by Q9 ----
library(tidyr)
library(ggplot2)

mode_data <- read.csv(
  "./outputs/mode_NSSQ9_crosstab_wide.csv"
)

mode_long <- pivot_longer(
  mode_data,
  cols = -Mode,
  names_to = "Response",
  values_to = "Count"
)

ggplot(
  mode_long,
  aes(
    x = Mode,
    y = Count,
    fill = Response
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Very.well" = "#2E7D32",
      "Well" = "#81C784",
      "This.does.not.apply.to.me" = "#BDBDBD",
      "Not.very.well" = "#FF8A65",
      "Not.at.all.well" = "#C62828"
    )
  ) +
  labs(
    title = "NSS Q9 Responses by Mode of Study",
    x = NULL,
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# 1.5 Level Chart by Q9 ----
library(tidyr)
library(ggplot2)

level_data <- read.csv(
  "./outputs/level_NSSQ9_crosstab_wide.csv"
)

level_long <- pivot_longer(
  level_data,
  cols = -Level,
  names_to = "Response",
  values_to = "Count"
)

ggplot(
  level_long,
  aes(
    x = Level,
    y = Count,
    fill = Response
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Very.well" = "#2E7D32",
      "Well" = "#81C784",
      "This.does.not.apply.to.me" = "#BDBDBD",
      "Not.very.well" = "#FF8A65",
      "Not.at.all.well" = "#C62828"
    )
  ) +
  labs(
    title = "NSS Q9 Responses by Level of Study",
    x = NULL,
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

## 2.1 Sex Chart by Q28 ----
library(tidyr)
library(ggplot2)

sex_data <- read.csv(
  "./outputs/sex_NSSQ28_crosstab_wide.csv"
)

sex_long <- pivot_longer(
  sex_data,
  cols = -Sex,
  names_to = "Response",
  values_to = "Count"
)

ggplot(
  sex_long,
  aes(
    x = Sex,
    y = Count,
    fill = Response
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Definitely.agree" = "#1B5E20",
      "Mostly.agree" = "#66BB6A",
      "Neither.agree.nor.disagree" = "#BDBDBD",
      "Mostly.disagree" = "#FF8A65",
      "Definitely.disagree" = "#C62828",
      "Not.applicable" = "#E0E0E0"
    ),
    labels = c(
      "Definitely agree",
      "Definitely disagree",
      "Mostly agree",
      "Mostly disagree",
      "Neither agree nor disagree",
      "Not applicable"
    )
  ) +
  labs(
    title = "NSS Q28 Responses by Sex",
    x = NULL,
    y = "Percentage",
    fill = "Response"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# 2.2 Deprivation Chart by Q28 ----
library(tidyr)
library(ggplot2)

deprivation_data <- read.csv(
  "./outputs/deprivation_NSSQ28_crosstab_wide.csv"
)

deprivation_long <- pivot_longer(
  deprivation_data,
  cols = -Deprivation,
  names_to = "Response",
  values_to = "Count"
)

ggplot(
  deprivation_long,
  aes(
    x = Deprivation,
    y = Count,
    fill = Response
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Definitely.agree" = "#1B5E20",
      "Mostly.agree" = "#66BB6A",
      "Neither.agree.nor.disagree" = "#BDBDBD",
      "Mostly.disagree" = "#FF8A65",
      "Definitely.disagree" = "#C62828",
      "Not.applicable" = "#E0E0E0"
    ),
    labels = c(
      "Definitely agree",
      "Definitely disagree",
      "Mostly agree",
      "Mostly disagree",
      "Neither agree nor disagree",
      "Not applicable"
    )
  ) +
  labs(
    title = "NSS Q28 Responses by Deprivation Quintile",
    x = NULL,
    y = "Percentage",
    fill = "Response"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

## 2.3 Disability Group by Q28 ----
library(tidyr)
library(ggplot2)

disability_data <- read.csv(
  "./outputs/disablegroup_NSSQ28_crosstab_wide.csv"
)

disability_long <- pivot_longer(
  disability_data,
  cols = -Disablegroup,
  names_to = "Response",
  values_to = "Count"
)

ggplot(
  disability_long,
  aes(
    x = Disablegroup,
    y = Count,
    fill = Response
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Definitely.agree" = "#1B5E20",
      "Mostly.agree" = "#66BB6A",
      "Neither.agree.nor.disagree" = "#BDBDBD",
      "Mostly.disagree" = "#FF8A65",
      "Definitely.disagree" = "#C62828",
      "Not.applicable" = "#E0E0E0"
    ),
    labels = c(
      "Definitely agree",
      "Definitely disagree",
      "Mostly agree",
      "Mostly disagree",
      "Neither agree nor disagree",
      "Not applicable"
    )
  ) +
  labs(
    title = "NSS Q28 Responses by Disability Group",
    x = NULL,
    y = "Percentage",
    fill = "Response"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# 2.4 Mode Chart by Q28 ----
library(tidyr)
library(ggplot2)

mode_data <- read.csv(
  "./outputs/mode_NSSQ28_crosstab_wide.csv"
)

mode_long <- pivot_longer(
  mode_data,
  cols = -Mode,
  names_to = "Response",
  values_to = "Count"
)

ggplot(
  mode_long,
  aes(
    x = Mode,
    y = Count,
    fill = Response
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Definitely.agree" = "#1B5E20",
      "Mostly.agree" = "#66BB6A",
      "Neither.agree.nor.disagree" = "#BDBDBD",
      "Mostly.disagree" = "#FF8A65",
      "Definitely.disagree" = "#C62828",
      "Not.applicable" = "#E0E0E0"
    )
  ) +
  labs(
    title = "NSS Q28 Responses by Mode of Study",
    x = NULL,
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# 2.5 Level Chart by Q28 ----
library(tidyr)
library(ggplot2)

level_data <- read.csv(
  "./outputs/level_NSSQ28_crosstab_wide.csv"
)

level_long <- pivot_longer(
  level_data,
  cols = -Level,
  names_to = "Response",
  values_to = "Count"
)

ggplot(
  level_long,
  aes(
    x = Level,
    y = Count,
    fill = Response
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Definitely.agree" = "#1B5E20",
      "Mostly.agree" = "#66BB6A",
      "Neither.agree.nor.disagree" = "#BDBDBD",
      "Mostly.disagree" = "#FF8A65",
      "Definitely.disagree" = "#C62828",
      "Not.applicable" = "#E0E0E0"
    )
  ) +
  labs(
    title = "NSS Q28 Responses by Level of Study",
    x = NULL,
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

## 3.1 Sex, Deprivation and Disability Group Chart Crosstabs by Q9 ----
ggplot(
  cross_tab,
  aes(
    x = NIM,
    y = PositivePct,
    fill = NSSSEX
  )
) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(PositivePct, 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  facet_wrap(~DISABLE_GROUP) +
  labs(
    title = "Positive Responses to NSS Q9",
    subtitle = "By Sex, Deprivation Quintile and Disability Group",
    x = "Deprivation Quintile",
    y = "Positive Responses (%)",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# 3.2 Sex, Deprivation and Disability Group Chart Crosstabs by Q28 ----
cross_tab$Positive <- cross_tab$Definitely.agree +
  cross_tab$Mostly.agree

cross_tab$Total <- rowSums(
  cross_tab[,4:9]
)

cross_tab$PositivePct <-
  100 * cross_tab$Positive / cross_tab$Total

ggplot(
  cross_tab,
  aes(
    x = NIM,
    y = PositivePct,
    fill = NSSSEX
  )
) +
  geom_col(position = "dodge") +
  facet_wrap(~DISABLE_GROUP) +
  labs(
    title = "Positive Responses to NSS Q28",
    x = "Deprivation Quintile",
    y = "Positive Responses (%)"
  ) +
  theme_minimal()

## 4.0 RAP Skeleton ----

# 5.1 Chart Specification Object ----
chart_specs <- tibble::tribble(
  ~file, ~group_var, ~title, ~question,
  
  # Q9
  
  "./outputs/sex_NSSQ9_crosstab_wide.csv",
  "Sex",
  "NSS Q9 Responses by Sex",
  "Q9",
  
  "./outputs/deprivation_NSSQ9_crosstab_wide.csv",
  "Deprivation",
  "NSS Q9 Responses by Deprivation Quintile",
  "Q9",
  
  "./outputs/disablegroup_NSSQ9_crosstab_wide.csv",
  "Disablegroup",
  "NSS Q9 Responses by Disability Group",
  "Q9",
  
  "./outputs/mode_NSSQ9_crosstab_wide.csv",
  "Mode",
  "NSS Q9 Responses by Mode of Study",
  "Q9",
  
  "./outputs/level_NSSQ9_crosstab_wide.csv",
  "Level",
  "NSS Q9 Responses by Level of Study",
  "Q9",
  
  # Q28
  
  "./outputs/sex_NSSQ28_crosstab_wide.csv",
  "Sex",
  "NSS Q28 Responses by Sex",
  "Q28",
  
  "./outputs/deprivation_NSSQ28_crosstab_wide.csv",
  "Deprivation",
  "NSS Q28 Responses by Deprivation Quintile",
  "Q28",
  
  "./outputs/disablegroup_NSSQ28_crosstab_wide.csv",
  "Disablegroup",
  "NSS Q28 Responses by Disability Group",
  "Q28",
  
  "./outputs/mode_NSSQ28_crosstab_wide.csv",
  "Mode",
  "NSS Q28 Responses by Mode of Study",
  "Q28",
  
  "./outputs/level_NSSQ28_crosstab_wide.csv",
  "Level",
  "NSS Q28 Responses by Level of Study",
  "Q28"
)

# 5.2 Updated Chart Function ----
create_response_chart <- function(
    file_path,
    group_var,
    title,
    question
) {
  
  data <- read.csv(file_path)
  
  data_long <- tidyr::pivot_longer(
    data,
    cols = -all_of(group_var),
    names_to = "Response",
    values_to = "Count"
  )
  
  if (question == "Q9") {
    
    colours <- c(
      "Very.well" = "#2E7D32",
      "Well" = "#81C784",
      "This.does.not.apply.to.me" = "#BDBDBD",
      "Not.very.well" = "#FF8A65",
      "Not.at.all.well" = "#C62828"
    )
    
  } else {
    
    colours <- c(
      "Definitely.agree" = "#1B5E20",
      "Mostly.agree" = "#66BB6A",
      "Neither.agree.nor.disagree" = "#BDBDBD",
      "Mostly.disagree" = "#FF8A65",
      "Definitely.disagree" = "#C62828",
      "Not.applicable" = "#E0E0E0"
    )
    
  }
  
  ggplot2::ggplot(
    data_long,
    ggplot2::aes(
      x = .data[[group_var]],
      y = Count,
      fill = Response
    )
  ) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::scale_y_continuous(
      labels = scales::percent
    ) +
    ggplot2::scale_fill_manual(
      values = colours
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Percentage"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom"
    )
  
}

# 5.3 Generate all Ten Charts Automatically ----
all_charts <- purrr::pmap(
  chart_specs,
  create_response_chart
)

# 5.4 Save all Charts Automatically ----
chart_specs <- chart_specs %>%
  dplyr::mutate(
    output_file = c(
      "q9_sex.png",
      "q9_deprivation.png",
      "q9_disability.png",
      "q9_mode.png",
      "q9_level.png",
      "q28_sex.png",
      "q28_deprivation.png",
      "q28_disability.png",
      "q28_mode.png",
      "q28_level.png"
    )
  )
``
for(i in seq_len(nrow(chart_specs))) {
  
  p <- create_response_chart(
    chart_specs$file[i],
    chart_specs$group_var[i],
    chart_specs$title[i],
    chart_specs$question[i]
  )
  
  ggsave(
    filename = file.path(
      "outputs",
      "charts",
      chart_specs$output_file[i]
    ),
    plot = p,
    width = 8,
    height = 6
  )
}

## 6.1 Full Function ----
create_crosstab_chart <- function(
    file_path,
    title,
    question
) {
  
  # Load data
  data <- read.csv(file_path)
  
  # Calculate positive response measure
  
  if (question == "Q9") {
    
    data$Positive <- data$Very.well +
      data$Well
    
    data$Total <- rowSums(
      data[, c(
        "Not.at.all.well",
        "Not.very.well",
        "This.does.not.apply.to.me",
        "Very.well",
        "Well"
      )]
    )
    
  }
  
  if (question == "Q28") {
    
    data$Positive <- data$Definitely.agree +
      data$Mostly.agree
    
    data$Total <- rowSums(
      data[, c(
        "Definitely.agree",
        "Definitely.disagree",
        "Mostly.agree",
        "Mostly.disagree",
        "Neither.agree.nor.disagree",
        "Not.applicable"
      )]
    )
    
  }
  
  # Calculate positivity percentage
  
  data$PositivePct <-
    (data$Positive / data$Total) * 100
  
  # Build chart
  
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = NIM,
      y = PositivePct,
      fill = NSSSEX
    )
  ) +
    
    ggplot2::geom_col(
      position = ggplot2::position_dodge(
        width = 0.9
      )
    ) +
    
    ggplot2::geom_text(
      ggplot2::aes(
        label = round(PositivePct, 1)
      ),
      position = ggplot2::position_dodge(
        width = 0.9
      ),
      vjust = -0.3,
      size = 3
    ) +
    
    ggplot2::facet_wrap(
      ~DISABLE_GROUP,
      nrow = 1
    ) +
    
    ggplot2::scale_y_continuous(
      limits = c(0, 100)
    ) +
    
    ggplot2::labs(
      title = title,
      x = "Deprivation Quintile",
      y = "Positive Responses (%)",
      fill = "Sex"
    ) +
    
    ggplot2::theme_minimal() +
    
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(
        face = "bold"
      ),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    )
  
}

# 6.2 Generate the Q9 Chart ----
library(ggplot2)

q9_crosstab <- create_crosstab_chart(
  "./outputs/sex_deprivation_disable_group_q9_wide.csv",
  "Positive Responses to NSS Q9",
  "Q9"
)

q9_crosstab

# 6.3 Generate the Q28 Chart ----
library(ggplot2)

q28_crosstab <- create_crosstab_chart(
  "./outputs/sex_deprivation_disable_group_q28_wide.csv",
  "Positive Responses to NSS Q28",
  "Q28"
)

q28_crosstab

# 6.4 Save Chart Q9 ----
ggsave(
  filename = "./outputs/charts/q9_crosstab.png",
  plot = q9_crosstab,
  width = 10,
  height = 6,
  dpi = 300
)

# 6.5 Save Chart Q28 ----
ggsave(
  filename = "./outputs/charts/q28_crosstab.png",
  plot = q28_crosstab,
  width = 10,
  height = 6,
  dpi = 300
)

# 6.6 RAP Automation ----
crosstab_specs <- tibble::tribble(
  ~file,
  ~title,
  ~question,
  ~output,
  
  "./outputs/sex_deprivation_disable_group_q9_wide.csv",
  "Positive Responses to NSS Q9",
  "Q9",
  "q9_crosstab.png",
  
  "./outputs/sex_deprivation_disable_group_q28_wide.csv",
  "Positive Responses to NSS Q28",
  "Q28",
  "q28_crosstab.png"
)

for(i in seq_len(nrow(crosstab_specs))) {
  
  p <- create_crosstab_chart(
    file_path = crosstab_specs$file[i],
    title = crosstab_specs$title[i],
    question = crosstab_specs$question[i]
  )
  
  ggsave(
    filename = file.path(
      "outputs",
      "charts",
      crosstab_specs$output[i]
    ),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
  
}
`