# 지리 공간 데이터
# ⭐⭐⭐⭐⭐️CRS(coordinate reference system, 좌표 참조 시스템): 3차원 -> 2차원 평면 위치에 대응시키는 체계

setwd('/Users/hj/Downloads/Datasets_for_ch7')

# 1. raster: CRS 통일⭐️, 픽셀
# 1-1. stars 
#install.packages('stars', type='binary')
library(stars)
library(ggplot2)

HARV_elev <- read_stars('HARV_dsmCrop.tif')
class(HARV_elev)
HARV_elev

st_crs(HARV_elev) # CRS 출력

ggplot() + geom_stars(data=HARV_elev)
ggplot() + geom_stars(data=HARV_elev, aes(x=x, y=y, fill=HARV_dsmCrop.tif))
# => 두 결과는 동일.

ggplot() + geom_stars(data=HARV_elev) + scale_fill_viridis_c() + coord_equal()

# 범주화
library(dplyr)
HARV_elev2 <- HARV_elev %>% 
  mutate(elev.group=cut(HARV_dsmCrop.tif, breaks=c(300,350,400,450)))
HARV_elev2
ggplot() + geom_stars(data = HARV_elev2, aes(x=x, y=y, fill=elev.group)) + labs(fill='Elevation group') + coord_quickmap()

#
HARV_hill <- read_stars('HARV_DSMhill.tif')
class(HARV_hill)
HARV_hill
st_crs(HARV_hill)
ggplot() + geom_stars(data=HARV_hill) + coord_quickmap()

# 중첩
st_crs(HARV_elev) == st_crs(HARV_hill) # 같은 CRS를 사용하는지 확인.
ggplot() + 
  geom_stars(data=HARV_elev) + 
  geom_stars(data=HARV_hill, aes(x=x, y=y, alpha=HARV_DSMhill.tif)) + 
  scale_fill_viridis_c() +
  scale_alpha(range=c(0.15, 0.5), guide="none") # 범례 출력 X

# 
HARV_DTM_elev <- read_stars('HARV_dtmCrop.tif')
HARV_DTM_hill <- read_stars('HARV_DTMhill_WGS84.tif')
st_crs(HARV_DTM_elev) == st_crs(HARV_DTM_hill)

HARV_DTM_hill.2 <- st_warp(HARV_DTM_hill, HARV_DTM_elev)
st_crs(HARV_DTM_elev) == st_crs(HARV_DTM_hill.2)
ggplot() + 
  geom_stars(data=HARV_DTM_elev) + 
  geom_stars(data=HARV_DTM_hill.2, aes(x=x, y=y, alpha=HARV_DTMhill_WGS84.tif)) +
  scale_fill_viridis_c() +
  scale_alpha(range=c(0.15, 0.6), guide="none") 

# 세계지도
NE <- read_stars('NE1_50M_SR_W.tif')
NE # band 총 3개
ggplot() + 
  geom_stars(data=NE, downsample=10) + # 일부분 픽셀만 사용. 확대했기 때문에 해상도 낮아짐. => 1개 band만 이용. (데이터가 커서 시간이 오래 걸릴때 사용.)
  coord_quickmap()

ggplot() + geom_stars(data=NE, downsample=10) +
  coord_quickmap(xlim=c(100,150), ylim=c(25,50)) # 우리나라 부분 시각화

# 일부분만 잘라내서 해상도 높게 ⭐️
NE.cropped <- st_crop(NE, st_bbox(c(xmin=100, xmax=150, ymin=25, ymax=50), crs=st_crs(NE)))
ggplot() + geom_stars(data=NE.cropped) + coord_quickmap()

# 1-2. SpatRaster 
#install.packages('terra', type='binary')
library(terra)
t.NE <- rast('NE1_50M_SR_W.tif')
t.NE
class(t.NE)

plotRGB(t.NE)
plotRGB(t.NE, axes=T, mar=4) # 가로, 세로축 좌표 출력

t.NE.cropped <- crop(t.NE, ext(100, 150, 25, 50))
plotRGB(t.NE.cropped)

####
# 2. vector 
# 2-1. sf
library(sf)
NE.countries <- read_sf('ne_110m_admin_0_countries.shp')
NE.countries
class(NE.countries)
st_crs(NE.countries) # CRS 정보 출력.

ggplot() + geom_sf(data=NE.countries)

# 좌표축 조정.
ggplot() + geom_sf(data=NE.countries) + coord_sf(expand=FALSE) # 여분X, 꽉찬 지도
ggplot() + geom_sf(data=NE.countries) + coord_sf(xlim=c(100,150), ylim=c(30,55)) 
ggplot() + geom_sf(data=NE.countries) + coord_sf(crs=st_crs(2163)) # CRS 변경

table(NE.countries$INCOME_GRP)
ggplot() + geom_sf(data=NE.countries, aes(fill=INCOME_GRP))
ggplot() + geom_sf(data=NE.countries, aes(color=INCOME_GRP)) # 경계선 색

NE.countries2 <- NE.countries %>% 
  mutate(POP_GRP=as.factor(ifelse(POP_EST>=10^7, 1, 0)))
ggplot() + geom_sf(data=NE.countries2, aes(fill=POP_GRP))

# 2-2. 
#install.packages('geodata')
library(geodata)
gadm.kor <- gadm(country='KOR', level=1, path=getwd())
class(gadm.kor) # SpatVector
gadm.kor.sf <- st_as_sf(gadm.kor)
class(gadm.kor.sf) # sf
ggplot() + geom_sf(data=gadm.kor.sf)

table(gadm.kor.sf$NAME_1)
gadm.kor.sf2 <- gadm.kor.sf %>% 
  filter(NAME_1=='Seoul' | NAME_1=='Gyeonggi-do' | NAME_1=='Incheon')

ggplot() + geom_sf(data=gadm.kor.sf2, aes(fill=NAME_1))
ggplot(data=gadm.kor.sf2) + geom_sf() + geom_sf_label(aes(label=NAME_1))

# 2-3. 
# 한국 기찻길 데이터 시각화
osm.kor <- osm(country='KOR', var='railway', path=getwd())
class(osm.kor) # SpatVector
osm.kor.sf <- st_as_sf(osm.kor)
class(osm.kor.sf) # sf
ggplot() + geom_sf(data=osm.kor.sf, color='red')
ggplot() + geom_sf(data=gadm.kor.sf) + geom_sf(data=osm.kor.sf, color='red') # 중첩
# CRS 다르더라도, 자동으로 통일되어 그려짐⭐️ (cf. stars 객체 X)

####
# 3. raster + vector
# raster
seoul.env <- read_stars('eco_25k_376084_세계측지계.tif')
class(seoul.env)
ggplot() +
  geom_stars(data=seoul.env) +
  guides(fill=guide_legend(title='grade')) +
  scale_fill_viridis_d() +
  coord_equal()

# vector
kor.level3 <- gadm(country='KOR', level = 3, # 읍/면/동
                   path=getwd())
kor.level3.sf <- st_as_sf(kor.level3) 

st_crs(seoul.env) == st_crs(kor.level3.sf) # F

kor.level3.sf2 <- st_transform(kor.level3.sf, crs = st_crs(seoul.env)) # CRS 통일
kor.level3.sf3 <- st_crop(kor.level3.sf2, seoul.env) # 범위 통일

st_crs(seoul.env) == st_crs(kor.level3.sf3) # T

ggplot() +
  geom_stars(data=seoul.env) +
  guides(fill=guide_legend(title='grade')) +
  scale_fill_viridis_d() +
  geom_sf(data=kor.level3.sf3, fill=NA) + # 곂치지 않게 함.
  coord_sf()