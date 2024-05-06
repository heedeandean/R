setwd('/Users/hj/Downloads/visualization2023-main/CH9/')

# 한글
#install.packages('showtext', type='binary')
library(showtext)
font_add_google("Nanum Gothic", "nanum")
showtext_auto()

# 웹 동적·대화형 시각화 I
# 1. 
#install.packages('ggvis')
library(ggvis)
library(tidyverse)
library(readxl)
library(zoo)

gdp <- read_excel('GDP_GR.xlsx')
head(gdp)
연도 <- seq(as.Date('1960-01-01'), as.Date('2022-01-01'), '3 months')
연도
gdp_gr <- cbind(연도, gdp[,4:5]) %>% filter(year(연도)>2000)
head(gdp_gr)
gdp_gr %>% ggvis(~연도) %>% 
  layer_lines(y=~전년동기대비, 
              opacity:=input_slider(0,1,label = '전년동기대비')) %>% # 명도를 슬라이드 바로 조정.
  layer_lines(y=~전기대비, stroke:=input_select(c('Red'='red', 'Blue'='blue', 'Green'='green'), label='전기대비 색')) %>% 
  add_axis('x', title='') %>% 
  add_axis('y', title='증감률')

####
# 2. 
library(shiny) # 대화형
#install.packages('ClusterR', type = 'binary')
library(ClusterR)
library(cluster)

names(iris) <- c('꽃잎 길이', '꽃잎 폭', '꽃받침 길이', '꽃받침 폭')

# UI(User Interface): 외관적 특정
ui <- fluidPage(
  titlePanel('붓꽃 k-means 군집화'),
  sidebarPanel(
    selectInput('xcol', 'X 변수', names(iris)[1:4]),
    selectInput('ycol', 'Y 변수', names(iris)[1:4], selected = names(iris)[[2]]),
    numericInput('clusters', '군집 수', 3, min=1, max=5)
    ),
  mainPanel(plotOutput('plot1'))
  )

server <- function(input, output) {
  selectedData <- reactive({iris[, c(input$xcol, input$ycol)]})
  clusters <- reactive({kmeans(selectedData(), input$clusters)})
  output$plot1 <- renderPlot({
    clusplot(selectedData(), clusters()$cluster,
             lines=0, shade=TRUE, color=TRUE, labels=4,
             plotchar=FALSE, span=TRUE,
             main=paste('군집수: ', input$clusters), sub='',
             xlab=names(selectedData())[1],
             ylab=names(selectedData())[2])
  })  
}

shinyApp(ui=ui, server=server)

####
# 3. 
#install.packages('googleVis')
library(googleVis)

# 3-1. 원그래프
options(encoding='utf-8')
x_n = c("농림어업", "광공업", "전기가스수도업", "건설업", "서비스업")
x <- c(34267.8, 482774.6, 43069.7,105632.3, 1106359.9)
dd <- data.frame(x_n, x)
dd

Pie2 <- gvisPieChart(dd, options=list(is3D=TRUE, title='2020년 생산구조(단위: %, 10억원)', width=500))
plot(Pie2)
plot(Pie2, browser=rstudioapi::viewer)

# 3-2. 지도
library(COVID19)

date1 <- '2022-05-31'
cov <- covid19(start = date1, end = date1, verbose = FALSE) %>% 
  select(date, confirmed, deaths, vaccines, population, iso_alpha_2) %>% 
  mutate(인구대비누적확진자비율=confirmed/population*100, 확진자대비사망자비율=deaths/population*100)

G5 <- gvisGeoChart(cov, 'iso_alpha_2', '인구대비누적확진자비율', '확진자대비사망자비율', options = list(dataMode='regions', width=1200, height=600))
plot(G5)


####
# 4.
#install.packages('devtools', type='binary')
library(devtools)
#install_github('ramnathv/rCharts', type='binary')
library(rCharts) # js 라이브러리 연결 - D3
library(reshape2)

gdp_s <- read.csv('gdp_s.csv', header = TRUE)
colnames(gdp_s)[2:9] <- substr(colnames(gdp_s), 2, 5)[2:9]
gdp_s1 <- melt(gdp_s, id='Industry')
head(gdp_s1)
colnames(gdp_s1)[2:3] <- c('Year', 'GDP Share')

hPlot(x='Industry', y='GDP Share', group='Year', type='column', data=gdp_s1)

####
# 5. 
# 모션 차트
# install.packages('WDI', type='binary')
# install.packages('plotly', type='binary')
library(WDI)
library(plotly)

inds <- c('NY.GDP.MKTP.CD', 'NY.GDP.PCAP.CD','SP.POP.65UP.TO.ZS')
indnams <- c("GDP", "gdpPercap",  "prop_pop_over65")
wdiData <- WDI(country = 'all', indicator = inds, 
               start = 1960, end = format(Sys.Date(), '%Y'),
               extra = TRUE)
head(wdiData)

colnum <- match(inds, names(wdiData))
colnum
names(wdiData)[colnum] <- indnams
WorldBank <- droplevels(subset(wdiData, !region %in% 'Aggregates'))
head(WorldBank) 

library(dplyr)
fig3 <- WorldBank %>% 
  plot_ly(
    x = ~prop_pop_over65, 
    y=~gdpPercap,
    size=~GDP, 
    color=~region, 
    frame=~year, 
    text=~country,
    hoverinfo='text', 
    type='scatter', 
    mode='markers'
  )
fig3

####
# 6.
#install.packages('remotes')
#library(remotes)
#remotes::install_github("bokeh/rbokeh")
library(rbokeh) # 복잡

data(iris)
figure(data = iris, title='Iris Data의 산점도') %>% 
  ly_points(Sepal.Length, Sepal.Width,
            color = Species, glyph = Species,
            hover = list(Sepal.Length, Sepal.Width))