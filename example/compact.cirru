
{} (:package |app)
  :configs $ {} (:init-fn |app.main/main!) (:reload-fn |app.main/reload!)
    :modules $ [] |core
    :version |0.0.1
  :files $ {}
    |app.test-macro $ {}
      :ns $ quote (ns app.test-macro)
      :defs $ {}
        |test-macro! $ quote
          defn test-macro! () (echo "\"Testing macro")
            echo "\"macro" $ m-inc 2
            echo "\"quasi macro" $ m-inc-2 3
            echo "\"quote splice" $ m-count (1 2 3 4)
            echo "\"out" $ do (echo "\"do 1") (echo "\"do 2")
            assert "\"try assert" $ &= 1 1
            assert-detect
              defn x (x) true
              , true
        |m-count $ quote
          defmacro m-count (xs)
            quasiquote $ count
              [] $ ~@ xs
        |m-inc $ quote
          defmacro m-inc (x)
            [] &+ v $ [] &+ x 1
        |m-inc-2 $ quote
          defmacro m-inc-2 (x)
            quasiquote $ &+ (~ x)
              &+ (~ x) 2
        |v $ quote (def v 1)
      :proc $ quote ()
      :configs $ {}
    |app.test-list $ {}
      :ns $ quote (ns app.test-list)
      :defs $ {}
        |test-list! $ quote
          defn test-list! () (echo "\"Testing List")
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
            echo "\"foldl" $ foldl ([] 1 2 3) 10 &+
            echo "\"map" $ map ([] 1 2 3) inc
            echo "\"concat" $ concat
              [] ([] 1 2 3) ([] 4 5 6)
            assert= (&- 4 1) (&+ 1 2)
      :proc $ quote ()
      :configs $ {}
    |app.lib $ {}
      :ns $ quote (ns app.lib)
      :defs $ {}
      :proc $ quote ()
      :configs $ {}
    |app.test-bool $ {}
      :ns $ quote (ns app.test-bool)
      :defs $ {}
        |test-bool! $ quote
          defn test-bool! () (echo "\"Testing bool") (echo true)
            echo $ &and true false
            echo $ &and true true
            echo $ &or false true
            echo $ &or true true
            echo (not true) (not false)
      :proc $ quote ()
      :configs $ {}
    |app.test-map $ {}
      :ns $ quote (ns app.test-map)
      :defs $ {}
      :proc $ quote ()
      :configs $ {}
    |app.test-fn $ {}
      :ns $ quote (ns app.test-fn)
      :defs $ {}
        |test-fn! $ quote
          defn test-fn! ()
            echo $ fibo 1
            echo $ fibo 5
            echo-list 1 2 3 4
            echo-list & $ [] 4 5 6 7
        |fibo $ quote
          defn fibo (n) (; echo "\"calling fibo" n)
            if (&< n 2) 1 $ &+
              fibo $ &- n 1
              fibo $ &- n 2
        |echo-list $ quote
          defn echo-list (& xs) (echo xs)
      :proc $ quote ()
      :configs $ {}
    |app.main $ {}
      :ns $ quote
        ns app.main $ :require
          app.test-macro :refer $ v m-inc m-inc-2 m-count
          app.test-macro :as lib-macro
          app.test-fn :refer $ test-fn!
          app.test-list :refer $ test-list!
          app.test-math :refer $ test-math!
          app.test-macro :refer $ test-macro!
          app.test-symbol :refer $ test-symbol!
          app.test-bool :refer $ test-bool!
      :defs $ {}
        |main! $ quote
          defn main! ()
            echo $ &+ 1 1
            echo "\"This is a demo"
            test-fn!
            test-list!
            test-math!
            test-macro!
            test-symbol!
            test-bool!
            &let
              a $ &+ 1 2
              assert= a 3
            echo "\"import" v
            echo "\"local" local-value
            echo "\"import ns" lib-macro/v
        |local-value $ quote (def local-value 10)
      :proc $ quote ()
      :configs $ {}
    |app.test-string $ {}
      :ns $ quote (ns app.test-string)
      :defs $ {}
      :proc $ quote ()
      :configs $ {}
    |app.test-symbol $ {}
      :ns $ quote (ns app.test-symbol)
      :defs $ {}
        |test-symbol! $ quote
          defn test-symbol! () (echo "\"Testing symbol")
            echo $ gensym "\"a"
            echo $ gensym "\"b"
            echo $ gensym
            reset-gensym-index!
            echo $ gensym "\"a"
            echo $ gensym "\"b"
      :proc $ quote ()
      :configs $ {}
    |app.test-math $ {}
      :ns $ quote (ns app.test-math)
      :defs $ {}
        |test-math! $ quote
          defn test-math! () (echo "\"Testing math")
            echo $ &= 1 2
            echo $ &= (&+ 1 1) 2
            assert= (&- 4 1) (&+ 1 2)
      :proc $ quote ()
      :configs $ {}
