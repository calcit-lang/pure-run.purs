
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

        |def $ quote
          defmacro def (name v) v

        |assert= $ quote
          defmacro assert= (a b)
            quasiquote
              if (&= (~ a) (~ b)) nil
                &let nil
                  echo "|Left:   " (~ a)
                  echo "|     <= " (quote (~ a))
                  echo "|Right:  " (~ b)
                  echo "|     <= " (quote (~ b))
                  raise "|failed in assert="

        |assert $ quote
          defmacro assert (message expr)
            quasiquote
              if (~ expr) nil
                do
                  echo "|Failed:" (quote (~ expr))
                  raise "|failed in assert"

        |assert-detect $ quote
          defmacro assert-detect (f v)
            quasiquote
              if ((~ f) (~ v)) nil
                do
                  echo "|Failed:" (quote (~ f)) (quote (~ v))
                  raise "|failed in assert-detect"

        |do $ quote
          defmacro do (& xs)
            quasiquote
              &let nil (~@ xs)

      :proc $ quote ()
      :configs $ {}
