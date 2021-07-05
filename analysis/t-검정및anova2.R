####################################################################
## 데이터 샘플링
####################################################################
# 1. 단순임의추출

# 2. 계통추출 

# 3. 집락추출 

# 4. 층화임의추출 


####################################################################
## T-검정(T-Test) : 두집단의 평균을 통계적으로 비교
####################################################################

## 1. 일표본 T-검정(One Sample T-Test)
# : 모집단의 평균과 표본의 평균을 비교.
# ex. 어떤 회사의 A과자의 평균무게가 30g이라고 알려져 있는데, 
# 왠지 더 적은 것 같아 표본을 뽑아 비교할 때 사용

# 표본의 크기
# 30 이상 : t-test
# 10-30 : 정규성검정 
# 10 이하 : 윌콕슨순위합검정(비모수적 방법)

# 귀무가설 : 실제 모집단의 평균 = m
# case1. 대립가설 : 실제 모집단의 평균 != m (주장) - 양측검정
# t.test(표본, mu=m)

# 단측검정
# case2. 대립가설 : 실제 모집단의 평균 < m (주장) 
# t.test(표본, mu=m, alternative='less')

# case3. 대립가설 : 실제 모집단의 평균 > m (주장)
# t.test(표본, mu=m, alternative='grater')

# ex. 
# 귀무가설 : 모집단의 평균 = 30
# 대립가설 : 모집단의 평균 < 30 (주장)
m <-  30
sample <- rnorm(30, 38, 3)

boxplot(sample)

# p-value가 0.05를 넘으므로, 귀무가설이 기각 X
t.test(sample, mu=m, alternative = 'less')


# 귀무가설 : A = B
# case1. 대립가설 : A != B (주장) 
# t.test(A표본, B표본)

# case2. 대립가설 : A < B (주장)
# t.test(A표본, B표본, alternative = 'less')

# case3. 대립가설 : A > B (주장)
# t.test(A표본, B표본, alternative = 'grater')

# 2표본 T-검정 : 두 집단의 평균 비교 
# 1) 대응표본 T-검정(Paired Sample T-Test) : 전 후를 비교 (ex. 약 처방 전 후 모발 개수)
before <- rnorm(40, 100000, 15000)
after <- before + rnorm(40, 3, 2)

boxplot(before, after, ann=FALSE, names = c('복용전', '복용후'), 
        ylim=c(0, 150000))
title(ylab = '모발양', main = '복용전후 비교')

t.test(before, after, alternative='less', paired = TRUE)



# 2) 독립표본 T-검정 (Independent Sample T-Test) : 서로 독립된 두 집단 비교 (ex. 남여 키)
# 2-1. levene's test 기각 -> 이분산 가정 T검정
# 2-2. levene's test 기각 X -> 등분산 가정 T검정

male_h <- rnorm(40, 170, 5)
female_h <- rnorm(40, 160, 5)

df <- data.frame(X = c(rep('M', length(male_h)), rep('F', length(female_h))),
                 Y = c(male_h, female_h))

boxplot(male_h, female_h, ann = FALSE)
boxplot(Y~X, df, ann = FALSE)

# 남여 순서 변경
df$X <- factor(df$X, levels = c('M', 'F'))
boxplot(Y~X, df, ann = FALSE)
title(xlab = '성별', ylab = '키', main = '남녀 키 비교')

# 등분산 검정
# install.packages('lawstat')
library(lawstat)

# p값이 0.05 이상이므로, 등분산 가정 가능
levene.test(df$Y, df$X, location = 'mean')

# 등분산 가정으로 진행하기 위해 equal=TRUE 옵션 설정
# p값이 0.05 보다 작으므로, 두 그룹 간 평균에 유의한 차이가 있음
t.test(male_h, female_h, var.equal = TRUE)  
t.test(Y~X, df, var.equal=TRUE)




####################################################################
## 교차분석 : 범주형자료인 두 변수간의 관계를 알아보기 위해 실시
####################################################################

## 1. 적합성검정


## 2. 독립성 검정


## 3. 동질성 검정


####################################################################
## 분산분석(ANOVA) : 두개이상의 다수집단간 평균을 비교.
####################################################################

## 1. 일원배치 분산분석 (One-way ANOVA)

## 2. 이원배치 분산분석(Two-way ANOVA)

####################################################################
## 상관분석
####################################################################
