---
title       : 非参数统计
subtitle    : 双样本检验
author      : yaleidu
job         : 为自己服务
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : prettify  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
github:
  user: badbye
  repo: Non-parameter.two
---

## 目录：

2.1 Brown-Mood中位数检验

2.2 Wilcoxon(Mann-Whitney)秩和检验

2.3 正态记分检验

2.4 成对数据检验

---

## 2.1 Brown-Mood中位数检验

双样本Brown-Mood中位数检验．

```r
brown = function(data, group) {
    if (length(unique(group)) != 2) 
        stop("group not valide")
    data.median = median(data)
    x = data[group == unique(group)[1]]
    y = data[group == unique(group)[2]]
    a = sum(x > data.median)
    b = sum(y > data.median)
    p = phyper(a, length(x), length(y), a + b)
    p.one = min(p, 1 - p)
    H1 = ifelse(median(x) > median(y), "Oneside test: M(1) > M(2)", "Oneside test: M(1) < M(2)")
    list(H1.one = H1, p.one = p.one, H1.two = "Twoside test: M(1) != M(2)", 
        p.two = 2 * p.one)
}
```


---
## 2.2 Wilcoxon(Mann-Whitney)秩和检验

类似与单样本wilcoxon检验，利用样本的相对大小信息．

适用范围：**两总体分布形状类似，无需对称**


```r
wilcoxon.two = function(data, group) {
    if (length(unique(group)) != 2) 
        stop("group not valide")
    cate = group == unique(group)[1]
    Wxy = sum(outer(data[cate], data[!cate], "-") > 0)
    Wyx = sum(outer(data[!cate], data[cate], "-") > 0)
    H1 = ifelse(median(data[cate]) > median(data[!cate]), "Oneside test: M(1) > M(2)", 
        "Oneside test: M(1) < M(2)")
    p.one = pwilcox(min(Wxy, Wyx), sum(cate), sum(1 - cate))
    list(H1.one = H1, p.one = p.one, H1.two = "Twoside test: M(1) != M(2)", 
        p.two = 2 * p.one)
}
```


---
## 2.3 正态记分检验

类似于单样本的正态记分检验．

```r
norm.two = function(data, group) {
    w = cbind(data, group)
    w = w[order(data), ]
    n = length(data)
    norm.score = qnorm((1:n)/(n + 1))
    w = cbind(w, 1:n, norm.score)
    cate = w[, 2] == unique(group)[1]
    T = sum(norm.score[cate])
    S = sqrt(sum(norm.score^2) * sum(cate) * sum(!cate)/n/(n - 1))
    p.one = min(pnorm(T/S), 1 - pnorm(T/S))
    H1 = ifelse(T > 0, "Oneside test: M(1) > M(2)", "Oneside test: M(1) < M(2)")
    list(H1.one = H1, p.one = p.one, H1.two = "Twoside test: M(1) != M(2)", 
        p.two = 2 * p.one)
}
```


---
## 2.4 成对数据检验

假定：　　　　
* 每对数据来自同一对象或比较类似的对象；
* 对和对之间独立；
* 都是连续变量；

```r
pair.test = function(x, y) {
    d = x - y
    r = rank(abs(d))
    w1 = sum(r[sign(d) == 1])
    w2 = sum(r[sign(d) == -1])
    p.one = psignrank(min(w1, w2), length(x))
    H1 = ifelse(w1 > w2, "Oneside test: M(x) > M(y)", "Oneside test: M(x) < M(y)")
    list(H1.one = H1, p.one = p.one, H1.two = "Twoside test: M(x) != M(y)", 
        p.two = 2 * p.one)
}
```

注: wilcox.test(x, y, paired=T)同样可以进行成对数据检验.
