x = 1:100
d = sapply(x, sin)
a = (ifelse(d < -0.5, 'A', ifelse(d > 0.5, 'C', 'B')))
print(a)