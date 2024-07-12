library(testthat)
average<- function(x){
  
  if (any(is.na(x))){
    warning(("'x' contains one or more NA values."))
    return(NA)
  }
  if(!is.numeric(x)){
    stop("'x' must be a numeric vector.")
  }
  sum(x)/length(x)
}

#Fonksiyonun doğruluğunu test ediyor
test_that("'average' calculates mean", {
  expect_equal(average(c(1,2,3)),2)
  expect_equal(average(c(-1,-2,-3)),-2)
  expect_equal(average(c(-1,0,1)),0)
  expect_equal(average(c(0.1,0.5)),0.3, tolerance =1e-8)#0.3 aslında 0.29999999 gibi bir şey o yüzden tölerans noktası ekledik.
})

test_that("'average' warns about NAs in input",{
  expect_warning(average(c(1,NA,3)))
  expect_warning(average(c(NA,NA,NA)))
})


#TEST DRIVEN 
greet<-function(to){
  return(paste("hello,", to))
}


test_that("'greet' says hello to a user",{
  expect_equal(greet("Carter"), "hello, Carter")
})
#ÖNCE BUNU YAZDIK ŞİMDİ BU TESTİ GEÇİCEK KODU YAZICAZ

#BEHAVIOR DRIVEN

describe("greet()", {
  it("can say hello to a user",{
    name<-"Carter"
    expect_equal(greet(name), "hello, Carter")
  })
})






















