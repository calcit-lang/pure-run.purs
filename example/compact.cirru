
{} (:package |app)
  :configs $ {} (:init-fn |app.main/main!) (:reload-fn |app.main/reload!)
    :modules $ [] |core
    :version |0.0.1
  :files $ {}
    |app.main $ {}
      :ns $ quote
        ns app.main $ :require
          app.lib :refer $ v m-inc m-inc-2 m-count
          app.lib :as lib
      :defs $ {}
        |main! $ quote
          defn main! ()
            echo $ &+ 1 1
            echo "\"This is a demo"
            echo $ fibo 0
            echo $ fibo 5
            echo $ [] 1 2 3 4
            echo $ nth ([] 1 2 3 4) 1
            echo $ nth ([] 1 2 3 4) 10
            echo $ first ([] 1 2 3 4)
            echo $ last ([] 1 2 3 4)
            echo $ count ([] 1 2 3 4)
            echo $ &= 1 2
            echo $ &= (&+ 1 1) 2
            echo $ slice ([] 1 2 3 4) 1 3
            echo $ rest ([] 1 2 3 4)
            echo $ butlast ([] 1 2 3 4)
            &let
              a $ &+ 1 2
              echo a
            echo "\"import" v
            echo "\"local" w
            echo "\"import ns" lib/v
            echo "\"macro" $ m-inc 2
            echo "\"quasi macro" $ m-inc-2 3
            echo "\"quote splice" $ m-count (1 2 3 4)
        |fibo $ quote
          defn fibo (n) (; echo "\"calling fibo" n)
            if (&< n 2) 1 $ &+
              fibo $ &- n 1
              fibo $ &- n 2
        |w $ quote (def "\"TODO w" 10)
      :proc $ quote ()
      :configs $ {}
    |app.lib $ {}
      :ns $ quote (ns app.lib)
      :defs $ {}
        |v $ quote (def "\"TODO v" 1)
        |m-inc $ quote
          defmacro m-inc (x)
            [] &+ v $ [] &+ x 1
        |m-inc-2 $ quote
          defmacro m-inc-2 (x)
            quasiquote $ &+ (~ x)
              &+ (~ x) 2
        |m-count $ quote
          defmacro m-count (xs)
            quasiquote $ count
              [] $ ~@ xs
      :proc $ quote ()
      :configs $ {}
