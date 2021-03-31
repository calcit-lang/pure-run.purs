
{} (:package |app)
  :configs $ {} (:init-fn |app.main/main!) (:reload-fn |app.main/reload!)
    :modules $ [] |core
    :version |0.0.1
  :files $ {}
    |app.main $ {}
      :ns $ quote (ns app.main)
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
        |fibo $ quote
          defn fibo (n) (; echo "\"calling fibo" n)
            if (&< n 2) 1 $ &+
              fibo $ &- n 1
              fibo $ &- n 2
      :proc $ quote ()
      :configs $ {}
