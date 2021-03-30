
{} (:package |calcit)
  :configs $ {} (:init-fn |calcit.main/main!) (:reload-fn |calcit.main/reload!)
    :modules $ []
    :version |0.0.0
  :files $ {}
    |calcit.core $ {}
      :ns $ quote (ns calcit.core)
      :defs $ {}

        |first $ quote
          defn first (xs)
            nth xs 0

        |dec $ quote
          defn dec (x) (&- x 1)

        |inc $ quote
          defn inc (x) (&+ x 1)

        |last $ quote
          defn last (xs)
            if (&> (count xs) 0)
              nth xs (dec (count xs))
              , nil

        |rest $ quote
          defn rest (xs)
            slice xs 1

        |butlast $ quote
          defn butlast (xs)
            slice xs 0 (dec (count xs))

      :proc $ quote ()
      :configs $ {}