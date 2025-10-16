# Global test setup
set.seed(123)
x <- rnorm(1000)
op.wn.4 <- StatOrdPattHxC::OPprob(x, emb = 4)
op.wn.5 <- StatOrdPattHxC::OPprob(x, emb = 5)


