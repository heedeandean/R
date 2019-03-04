# 1)
# 엑셀파일(melontop100.xlsx)의 첫번째 시트만 읽어,
# 마지막 합계 row는 삭제한 후, RData파일로 저장하시오.

mtx = read_excel('data/meltop100.xlsx', sheet = 1)
nrow(mtx)
mtx = mtx[-101,]
tail(mtx)
save(mtx, file = 'data/meltop100.rda')
rm(mtx)
load("data/meltop100.rda")
View(mtx)

# 2)
# 엑셀파일(melontop100.xlsx)을 csv형태로 저장한 후 RStudio에서 read하시오. 
# (read.table, read.csv모두 가능)

options(encoding='UTF-8')
mc = read.csv('data/mc.csv')
mc


# 3)
# 온도파일(temper.txt)을 읽어 연도, 기온, 구분자 컬럼만 남기시오.

td = read.fwf('data/temper.txt', header=F, width=c(15, 4, 68, 5, 1))
td$V1 = NULL
td$V3 = NULL
td

# -----------------------------------------------------------
# 1)
# 난반 남학생중에 국어와 수학 성적이 90점 초과인 학생의 학번과 전과목 성적을 국어 성적이 높은순(descending)으로 추출하시오.

km = d[(d$반 == '난' & d$성별 == '남' & d$국어 > 90 & d$수학 > 90), ]
km[order(-km$국어),]

# 2)
# data에 평균 변수(컬럼)을 추가하고, 
# 국어 성적이 80점 이상인 학생을 대상으로 반별 국어평균과 전체 평균을 구하시오.

d = read.csv("data/성적.csv")

d$평균 = (d[, 4] + d[, 5] + d[, 6] + d[, 7] + d[, 8]) / 5
d
k = d[d$국어 >= 80, ]
k
ban_k = aggregate(data=k, cbind(국어, 평균)~반, mean)
ban_k

# -----------------------------------------------------------
# 1)
# data$scout에서 스카우트가 아닌 학생을 제외하고 qplot을 그리시오.

# 1안.

sd = d[d$pass, ]
sd
qplot(sd$scout)

# 2안.

qplot(d[d$scout != '', ]$scout)



# 2)
# data에 학점 변수(컬럼)을 추가하시오.
# 범위 (A: 90이상, B:80이상, C: 70이상, D: 60이상, F: 60미만)

d$grade = ifelse(d$평균 >= 90, 'A', 
            ifelse(d$평균 >= 80, 'B', 
              ifelse(d$평균 >= 70, 'C',
                ifelse(d$평균 >= 60, 'D', 'F'))))

d[, c('평균', 'grade')]

# 3)
# 학점별 빈도 막대 그래프를 그리시오.

qplot(d$grade)

table(d$grade)











