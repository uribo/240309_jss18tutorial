library(ragg)
library(qrcode)
x <-
  qr_code("https://github.com/uribo/240309_jss18tutorial")

png("images/qrcode.png", width = 200, height = 200, res = 96)
plot(x)
dev.off()
