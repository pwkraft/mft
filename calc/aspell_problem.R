library(tmt)
library(devtools)

data(holbrook)

holbrook[1]
holbrook[21]

aspellCheck("Why doesn't this wrk. I mean really.", "fix")
aspellCheck("Why doesn't this wrk. I mean really. macwillaims", "fix")
aspellCheck(c("Test macwillaims","Why doesn't this wrk. I mean really"), "fix")
aspellCheck(c("Test macwillaims", holbrook[1]), "fix")



library(qdap)
x <- "Robots are evl creatres and deserv exterimanitation."
check_spelling(x)
correct(x)
preprocessed(m)
fixit <- attributes(m)$correct
fixit(mraja1spl$dialogue[1:75])
