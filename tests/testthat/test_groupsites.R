wim.df <- load('./wim.df.R')
sample.data.1 <- load('./sample.data.1.R')
sample.data.30 <- load('./sample.data.30.R')
sample.data.405 <- load('./sample.data.405.R')

test_that('groupsites will assign values to all locations',{
    df <- groupsites(as.data.frame(sample.data.1),16,wim.df)
    expect_that(dim(df),equals(c(18,18)))
    ## expect_that(df$group) is not NA...
})

test_that('groupsites works without a wim data set',{

    df <- groupsites(as.data.frame(sample.data.405),16,wim.df)
    expect_that(sort(unique(df$group)),
                equals(as.factor(
                    c(772455,'wim.112.N','wim.13.N','wim.16.N','wim.60.N')
                )))

    df <- groupsites(as.data.frame(sample.data.405),16)
    expect_that(sort(unique(df$group)),
                equals(as.factor(
                  c(717799,718251,759427,771808,1214461,1218071)
                )))

})

test_that('specific VDS sites can be chosen',{

    priority.vds.sites <- sample.data.405[c(1,80,120),]

    df <- groupsites(as.data.frame(sample.data.405),16,priority.vds.sites)

    expect_that(is.element(priority.vds.sites$id,df$group)
                ,equals(c(TRUE,TRUE,TRUE)))


})
