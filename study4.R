head(chodata)
head(u)
library(grDevices)


ggplot(chodata, aes(map_id = state)) +
  geom_map(aes(fill = Murder), map = u) +
  expand_limits(x = u$long, y = u$lat) +
  labs(title = 'USA 살인', file = "살인") +
  scale_fill_gradient2('살인', low='blue')

tooltips = paste0(
  sprintf("<p><strong>%s</strong></p>", as.character(chodata$state)),
  '<table>',
  '  <tr>',
  '    <td>인구(만)</td>',
  sprintf('<td>%.0f</td>', chodata$UrbanPop * 10),
  '  </tr>',
  '  <tr>',
  '    <td>살인</td>',
  sprintf('<td>%.0f</td>', chodata$Murder),
  '  </tr>',
  '  <tr>',
  '    <td>폭력</td>',
  sprintf('<td>%.0f</td>', chodata$Assault),
  '  </tr>',
  '</table>' )

tooltips

onclick = sprintf("alert(\"%s\")", as.character(chodata$state))
onclick
library(ggplot2)

library(ggiraph)
library(dplyr)
library(ggiraphExtra)

gg_map = ggplot(chodata, aes(data = Murder, map_id = state)) +
  geom_map_interactive(
    aes(fill = Murder,
        data_id = state,
        tooltip = tooltips,
        onclick = onclick),
    map = u) +
  expand_limits(x = u$long, y = u$lat) +
  scale_fill_gradient2('a', low = 'red', high ="blue", mid ="green") +
  labs(title="usa a")

gg_map

library(stringi)
tooltips = stringi::stri_enc_toutf8(tooltips)

ggiraph(code = print(gg_map))
girafe(ggobj = gg_map)


