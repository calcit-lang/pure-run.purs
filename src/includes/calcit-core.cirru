
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
                  echo "|     <= " $ format-to-lisp (quote (~ b))
                  raise "|failed in assert="

        |assert-not= $ quote
          defmacro assert-not= (a b)
            quasiquote
              if (&= (~ a) (~ b))
                &let nil
                  echo "|Left:   " (~ a)
                  echo "|     <= " (quote (~ a))
                  echo "|Right:  " (~ b)
                  echo "|     <= " $ format-to-lisp (quote (~ b))
                  raise "|failed in assert="
                , nil

        |assert $ quote
          defmacro assert (message expr)
            quasiquote
              if (~ expr) nil
                do
                  echo "|Failed:" (~ message)
                  echo "|     <=" $ format-to-lisp (quote (~ expr))
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

        |not= $ quote
          defn not= (a b)
            not $ &= a b

        |identity $ quote
          defn identity (x) x


        |swap! $ quote
          defmacro swap! (r f & args)
            quasiquote
              reset! (~ r) $ (~ f) (deref (~ r)) (~@ args)

        |list? $ quote
          defn list? (x)
            &= :list (type-of x)

        |string? $ quote
          defn string? (x)
            &= :string (type-of x)

        |symbol? $ quote
          defn symbol? (x)
            &= :symbol (type-of x)

        |empty? $ quote
          defn empty? (xs)
            if (&= xs nil) true
              if (list? xs)
                &= 0 (count xs)
                if (string? xs)
                  &= 0 (count xs)
                  , false

        |format-to-lisp $ quote
          defn format-to-lisp (xs)
            if (symbol? xs)
              turn-string xs
              if (list? xs)
                &let
                  v $ join-str (map xs format-to-lisp) "| "
                  &str-concat (&str-concat "|(" v) "|)"
                &str xs

        |apply-args $ quote
          defmacro apply-args (args f)
            quasiquote
              (~ f) (~@ args)

        |fn $ quote
          defmacro fn (args & body)
            quasiquote
              defn fn% (~ args) (~@ body)

        |join-str $ quote
          defn join-str (xs sep)
            apply-args (| xs)
              fn (acc ys)
                if (empty? ys) acc
                  recur
                    if (empty? acc)
                      first ys
                      &str-concat (&str-concat acc sep) (first ys)
                    rest ys

        |odd? $ quote
          defn odd? (n)
            &= 1 (mod n 2)

        |even? $ quote
          defn even? (n)
            &= 0 (mod n 2)

        |case-default $ quote
          defmacro case-default (v r0 & xs)
            ; "TODO v should only eval once"
            if (empty? xs)
              quasiquote (~ r0)
              &let
                l0 $ first xs
                assert "|expected 2 items in pair" $ &= 2 (count l0)
                &let
                  v1 $ first l0
                  &let
                    r1 $ last l0
                    quasiquote
                      if
                        &= (~ v) (~ v1)
                        ~ r1
                        case-default (~ v) (~ r0) (~@ (rest xs))

        |nil? $ quote
          defn nil? (x)
            &= :nil (type-of x)

        |keys $ quote
          defn keys (x)
            map-kv x $ fn (k v) k

        |vals $ quote
          defn vals (x)
            map-kv x $ fn (k v) v

        |{} $ quote
          defmacro {} (& xs)
            assert "|{} expected pairs"
              every? xs $ fn (x)
                &and (list? x)
                  &= 2 (count x)
            quasiquote
              &{} (~@ (concat & xs))

        |fn? $ quote
          defn fn? (x)
            &= :fn (type-of x)

        |bool? $ quote
          defn bool? (x)
            &= :bool (type-of x)

        |every? $ quote
          defn every? (xs f)
            assert "|every? expected a list" (list? xs)
            assert "|every? expected a function" (fn? f)
            apply-args (xs)
              fn (ys)
                if (empty? ys) true
                  &let
                    y0 $ first ys
                    &let
                      v $ f y0
                      assert "|every? expected function returns a boolean" (bool? v)
                      if v (recur (rest ys)) false

        |&and $ quote
          defmacro &and (a b)
            quasiquote
              if (~ a) (~ b) false

        |&or $ quote
          defmacro &and (a b)
            quasiquote
              if (~ a) true (~ b)

        |cond $ quote
          defmacro cond (& pairs)
            if (empty? pairs) nil
              &let
                p0 $ first pairs
                assert
                  &str-concat "|pairs for cond: " (&str p0)
                  &and (list? p0) (&= 2 (count p0))
                &let
                  v $ first p0
                  &let
                    then $ last p0
                    if (&= v '_) then
                      quasiquote
                        if (~ v) (~ then) (cond (~@ (rest pairs)))

        |let[] $ quote
          defmacro let[]
            quasiquote 'TODO

        |case $ quote
          defmacro case (v & pairs)
            ; TODO symbols should only be evaled once
            if (empty? pairs) nil
              &let
                p0 $ first pairs
                assert
                  &str-concat "|pairs for cond: " (&str p0)
                  &and (list? p0) (&= 2 (count p0))
                &let (v1 $ first p0)
                  &let (r1 $ last p0)
                    quasiquote
                      if (&or (&= (~ v1) '_) (&= (~ v1) (~ v))) (~ r1)
                        case (~ v) (~@ (rest pairs))

        |+ $ quote
          defmacro + (x0 & xs)
            quasiquote-fold-tree x0 '&+ xs

        |* $ quote
          defmacro + (x0 & xs)
            quasiquote-fold-tree x0 '&* xs

        |- $ quote
          defmacro - (& xs)
            case (count xs)
              0 0
              1 $ [] &- 0 (first xs)
              _ $ quasiquote-fold-tree (first xs) &- (rest xs)

        |/ $ quote
          defmacro / (& xs)
            case (count xs)
              0 0
              1 $ [] &/ 1 (first xs)
              _ $ quasiquote-fold-tree (first xs) &/ (rest xs)

        |and $ quote
          defmacro and (x0 & xs)
            quasiquote-fold-tree x0 '&and xs

        |or $ quote
          defmacro or (x0 & xs)
            quasiquote-fold-tree x0 '&or xs

        |= $ quote
          defmacro = (x0 & xs)
            if (empty? xs) true
              quasiquote
                if
                  &= (~ x0) (~ (first xs))
                  = (~ x0) (~@ (rest xs))
                  , false

        |quasiquote-fold-tree $ quote
          defn quasiquote-fold-tree (x0 op xs)
            if (empty? xs) x0
              recur ([] op x0 (first xs)) op (rest xs)

      :proc $ quote ()
      :configs $ {}
