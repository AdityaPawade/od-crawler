# TODO

- use an alternative safer Prelude
  - https://github.com/sdiehl/protolude
  - https://github.com/commercialhaskell/rio
  - https://github.com/kowainik/relude

- cleaner URI model https://github.com/mrkkrp/modern-uri

- optionally capture file size as well via HEAD requests

- graph max of the gauges

- property testing with Hedgehog https://github.com/hedgehogqa/haskell-hedgehog

- prevent cycles of links

- clean concurrent crawling tree exploration
  - define worker threads manually
    - http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html
    - https://www.fpcomplete.com/blog/2016/11/comparative-concurrency-with-haskell
  - use pooling lib
    - http://hackage.haskell.org/package/async-pool-0.9.0.2/docs/Control-Concurrent-Async-Pool.html#t:Async
    - https://stackage.org/lts-13.13/package/parallel-io-0.3.3

- profiling and optimization
  - http://book.realworldhaskell.org/read/profiling-and-optimization.html
  - Threadscope https://wiki.haskell.org/ThreadScope
  - ghc-events-analyze http://www.well-typed.com/blog/86/
  - try to write a benchmark with criterion https://github.com/bos/criterion