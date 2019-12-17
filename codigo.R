library(imfr)
library(tidyverse)
library(gsynth)
library(Synth)
library(psData)
library(wbstats)
library(reshape2)
library(vdem)

#teste_m_ifs = imf_data(database_id = 'IFS', indicator = c(), country = "all", freq = "Q", start = 1990)




new_cache <- wbcache()

teste = wbsearch(pattern = "")


#### Chamando base

#Popula??o, mortalidade infantil, pib, gini, pobreza, school enrollment, agriculture % of gdp, Gross capital formation (% of gdp),
#Oil rents, Ease of doing business, Trade, Real Exchange rate

testinho = wb(indicator = c("SP.POP.TOTL","SP.DYN.IMRT.FE.IN", "SP.DYN.IMRT.MA.IN",
                            "NY.GDP.MKTP.PP.CD", "SI.POV.GINI", "SE.PRM.NENR", "SE.SEC.ENRR",
                            "SE.TER.ENRR", "SI.POV.NAHC", "NV.AGR.TOTL.ZS", "NE.GDI.TOTL.ZS",
                            "NV.IND.MANF.ZS", "NY.GDP.PETR.RT.ZS", "IC.BUS.EASE.XQ", 
                            "NE.TRD.GNFS.ZS", "PX.REX.REER"), startdate = 1970, enddate = 2020, country = "countries_only")



panel = dcast(testinho,formula = iso3c+date+iso2c+country~indicator,value.var = "value")

panel$GDP = panel$`GDP, PPP (current international $)`

panel_XXI <- panel %>% filter(date > 1999)


### chamando base VDEM

main_indices <- extract_vdem(section_number = 2)


vdem <- main_indices %>%
  filter(year > 1950) %>%
  select(vdem_country_text_id, year, v2x_polyarchy)


### Juntando as bases

panelzinho = merge(panel, vdem, by.x = c("iso3c", "date"), by.y = c("vdem_country_text_id", "year"), all.x = TRUE)


### Transforming variables 


panelpronto = panelzinho

panelpronto = panelpronto %>%
              group_by(country) %>%
              mutate(growth = c(NA,diff(GDP))/GDP)


panelpronto = panelpronto %>% 
  group_by(country) %>%
  mutate(politicalchange = c(NA,diff(`v2x_polyarchy`))/`v2x_polyarchy`)


hist(panelpronto$politicalchange)

panelpronto$growth = ifelse(is.na(panelpronto$growth), 0, panelpronto$growth)

panelpronto = panelpronto %>% 
  group_by(country) %>%
  mutate(cambio = c(NA,diff(`Real effective exchange rate index (2010 = 100)`))/`Real effective exchange rate index (2010 = 100)`)


panelpronto$tratamento = ifelse(panelpronto$politicalchange < -0.3, 1,0)

panelpronto$GDPcapita = panelpronto$GDP/panelpronto$`Population, total`

library(gsynth)
gsynth.out <- gsynth(GDPcapita ~  tratamento + `Population, total` + `Oil rents (% of GDP)`  + 
                     + `Manufacturing, value added (% of GDP)` + `Agriculture, forestry, and fishing, value added (% of GDP)` +
                     `Trade (% of GDP)` + `Gross capital formation (% of GDP)`, 
                     data = panelpronto,
                     index = c("country","date"), force = "two-way", 
                     CV = TRUE, r = c(0, 5), se = TRUE, 
                     inference = "parametric", nboots = 1000,
                     parallel = TRUE, na.rm = TRUE, min.T0 = 10, normalize = TRUE)

plot(gsynth.out)

plot(gsynth.out, type = 'counterfactual')


### Testinho

gsynth.out <- gsynth(growth ~  tratamento,   
                     data = panelpronto,
                     index = c("country","date"), force = "two-way", 
                     CV = TRUE, r = c(0, 5), se = TRUE, 
                     inference = "parametric", nboots = 1000,
                     parallel = TRUE, na.rm = TRUE, min.T0 = 10)







gsynth.out <- gsynth(cambio ~  tratamento + `Population, total` + `Oil rents (% of GDP)` + GDP  
                     + `Manufacturing, value added (% of GDP)` + `Agriculture, forestry, and fishing, value added (% of GDP)` +
                       `Trade (% of GDP)` + `Gross capital formation (% of GDP)`, 
                     data = panelpronto,
                     index = c("country","date"), force = "two-way", 
                     CV = TRUE, r = c(0, 5), se = TRUE, 
                     inference = "parametric", nboots = 1000,
                     parallel = TRUE, na.rm = TRUE, min.T0 = 10)


plot(gsynth.out)
plot(gsynth.out, type = 'counterfactual')




