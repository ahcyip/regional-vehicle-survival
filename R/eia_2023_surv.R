library(readxl); library(tidyverse)
aeo_xlsx_surv <- read_excel("c:/users/ayip/downloads/AEO2023_LDVSurvivalCurvesVMT_NREL.XLSX", sheet = "Survival",
                            skip = 7, n_max = 23) %>%
  select(-"NA") %>%
  filter(region != "NA") %>%
  pivot_longer(cols = -c("vehicle_type", "region"), names_to = "Vintage", values_to = "survival") %>%
  mutate(Vintage = as.numeric(Vintage))

aeo_xlsx_surv %>%
  ggplot() +
  geom_line(aes(x=Vintage,y=survival,col=region,group=interaction(region, vehicle_type))) +
  facet_wrap(vars(vehicle_type)) +
  scale_y_continuous(breaks = seq(0,1.4,.2), limits = c(0,1.4))


aeo_xlsx_surv_natl <- aeo_xlsx_surv %>%
  filter(region == "natl") %>%
  group_by(vehicle_type) %>%
  arrange(vehicle_type, Vintage) %>%
  #mutate(natl_survival_change = survival - lag(survival)) %>%
  ungroup() %>%
  rename(natl_survival = survival) %>%
  select(-region)

aeo_xlsx_surv_2 <- aeo_xlsx_surv %>%
  left_join(aeo_xlsx_surv_natl, by = c("vehicle_type", "Vintage")) %>%
  group_by(vehicle_type, region) %>%
  mutate(#survival_less_natl_surv_change = survival - lead(natl_survival_change),
         survival_less_natl_surv = survival - natl_survival)

aeo_xlsx_surv_2 %>%
  ggplot() +
  geom_line(aes(x=Vintage,y=survival_less_natl_surv, col=region,group=interaction(region, vehicle_type))) +
  facet_wrap(vars(vehicle_type))
