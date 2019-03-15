setLib = function() {
  library(devtools)
  library(stringi)
  library(ggiraphExtra)
  library(tibble)
  library(ggiraph)
}

setLib()

library(kormaps2014)
kdata = kormaps2014::changeCode(korpop1)

colnames(kdata)

kdata <- kdata %>% rename(pop = 총인구_명)
kdata <- kdata %>% rename(area = 행정구역별_읍면동)

kdata$area = stringi::stri_enc_toutf8(kdata$area)

ggChoropleth(data=kdata,
             aes(fill = pop, map_id = code, tooltip = area),
             map = kormap1,
             interactive = T)
