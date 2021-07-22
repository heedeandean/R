# 순서가 있는 질적변수(ex. 덜매움, 매움, 아주매움) 수치화
# => 변수의 합이 1이 되도록
values <- c("low", "medium", "high")
dat <- data.frame(x = ordered(values, levels = values))
foo <- recipe(~ x, data = dat) %>% 
  step_dummy(x) %>% 
  prep() %>% 
  juice()

foo %>% 
  mutate(aa = rownames(.)) %>% 
  ggplot(aes(x=aa, y=x_2)) +
  geom_point()


