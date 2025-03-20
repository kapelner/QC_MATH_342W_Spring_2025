pacman::p_load(data.table, ggplot2, HistData)

#problem 2

raw = datasets::HairEyeColor
raw = array(raw, dim=dim(raw), dimnames=dimnames(raw))

D = data.table(is_male = numeric(), eye_color = character(), hair_color = character())

for (k in 1 : 2){
  for (i in 1 : 4){
    for (j in 1 : 4){
      for (r in 1 : raw[i, j, k]){
        D = rbind(D, data.table(
          is_male = as.numeric(k == 1),
          eye_color = colnames(raw[, , k])[j],
          hair_color = rownames(raw[, , k])[i]
        ))
      }
    }
  }
}
rownames(D) = NULL
D$eye_color = factor(D$eye_color)
D$hair_color = factor(D$hair_color)
D = data.frame(D)

skimr::skim(D)
D$is_not_male = 1 - D$is_male
D[sample(1 : nrow(D), 10), ]

ncol(model.matrix(is_male ~ ., D))
summary(lm(is_male ~ ., D))

ncol(model.matrix(eye_color ~ ., D))


# problem 3


skimr::skim(HistData::Galton)
round(colMeans(HistData::Galton), 2)
mod = lm(child ~ parent, HistData::Galton)
round(summary(mod)$r.squared, 2)

#T/F r>0, r<0, r=0, sxy >0, sxy<0, sxy = 0, there is an association between x and y
round(with(HistData::Galton, cor(child, parent)), 2)
sqrt(0.21) * 2.52 / 1.79

summary(mod)$sigma

SST = 927*2.52^2
SSE = SST * (1 - .21)
sqrt(SSE / 926)


# problem 4
n = 200
D = data.table(x1 = runif(n, 0, 2), x2 = runif(n, 0, 2))
D[, y := factor(ifelse((x1 - 1)^2 + x2^2 < 1, 1, 0))]

ggplot(D) + geom_point(aes(x = x1, y = x2, shape = y ,size = y))

