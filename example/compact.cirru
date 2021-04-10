
{} (:package |app)
  :configs $ {} (:init-fn |app.main/main!) (:reload-fn |app.main/reload!)
    :modules $ [] |core
    :version |0.0.2
  :files $ {}
    |app.test-ref $ {}
      :ns $ quote (ns app.test-ref)
      :defs $ {}
        |test-ref! $ quote
          defn test-ref! () (echo "\"Testing ref")
            &let
              r $ ref 0
              echo r
              assert= 0 $ deref r
              reset! r 2
              assert= 2 $ deref r
              swap! r inc
              assert= 3 $ deref r
              swap! r &+ 3
              assert= 6 $ deref r
      :proc $ quote ()
      :configs $ {}
    |app.test-macro $ {}
      :ns $ quote (ns app.test-macro)
      :defs $ {}
        |test-macro! $ quote
          defn test-macro! () (echo "\"Testing macro")
            assert= 4 $ m-inc 2
            assert= 8 $ m-inc-2 3
            assert= 4 $ m-count (1 2 3 4)
            assert "\"try assert" $ &= 1 1
            assert-detect
              defn x (x) true
              , true
            assert= "\"(count ([] 1 2 3))" $ format-to-lisp
              macroexpand-1 $ quote
                m-count $ 1 2 3
            assert= "\"(if (&= (&+ 1 2) 1) |one (case-default (&+ 1 2) |else (2 |two) (3 |three)))" $ format-to-lisp
              macroexpand-1 $ quote
                case-default (&+ 1 2) "\"else" (1 "\"one") (2 "\"two") (3 "\"three")
            assert= "\"three" $ case-default (&+ 1 2) "\"else" (1 "\"one") (2 "\"two") (3 "\"three")
            assert= 3 $ eval
              quote $ &+ 1 2
            assert= "\"b2" $ cond
                &> 2 11
                , |b1
              (&> 2 1) |b2
              true |b0
            assert= "\"b0" $ cond
                &> 2 11
                , |b1
              false |b2
              true |b0
            assert= "\"b0" $ cond
                &> 2 11
                , |b1
              false |b2
              _ |b0
            assert= "\"two" $ case 2 (1 "\"one") (2 "\"two") (_ "\"else")
            assert= "\"else" $ case 3 (1 "\"one") (2 "\"two") (_ "\"else")
            assert= 6 $ + 1 2 3
            assert= -4 $ - 1 2 3
            assert= -1 $ - 1
            assert= 24 $ * 1 2 3 4
            assert= 1 $ / 24 2 3 4
            assert= (&/ 1 2) (/ 2)
            assert= true $ and true true true
            assert= false $ and true true false
            assert= false $ and false false false
            assert= false $ and false true false
            assert= true $ = 1 1 1
            assert= false $ = 1 1 2
            assert=
              quote $ d
                c $ b a
              macroexpand $ quote (-> a b c d)
            assert=
              quote $ d (b a c)
              macroexpand $ quote
                -> a (b c) (d)
            &let
              square $ fn (x) (&* x x)
              assert= "\"9~now" $ -> 1 (+ 2) square &str (&str-concat "\"~now")
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
          defn test-list! () (echo "\"Testing list")
            assert= ([] 1 2 3 4) ([] 1 2 3 4)
            assert= 2 $ nth ([] 1 2 3 4) 1
            assert= nil $ nth ([] 1 2 3 4) 10
            assert= 1 $ first ([] 1 2 3 4)
            assert= 4 $ last ([] 1 2 3 4)
            assert= 4 $ count ([] 1 2 3 4)
            assert= true $ &= (&+ 1 1) 2
            assert=
              slice ([] 1 2 3 4) 1 3
              [] 2 3
            assert=
              rest $ [] 1 2 3 4
              [] 2 3 4
            assert=
              butlast $ [] 1 2 3 4
              [] 1 2 3
            assert=
              foldl ([] 1 2 3) 10 &+
              , 16
            assert=
              map ([] 1 2 3) inc
              [] 2 3 4
            assert=
              concat ([] 1 2 3) ([] 4 5 6)
              [] 1 2 3 4 5 6
            assert=
              concat ([] 1 2) ([] 3 4) ([] 5 6)
              [] 1 2 3 4 5 6
            assert= (&- 4 1) (&+ 1 2)
            assert= ([] 1 :a 3 4)
              assoc ([] 1 2 3 4) 1 :a
            assert= ([] 1 3 4)
              dissoc ([] 1 2 3 4) 1
            assert= ([] 4 5)
              map-maybe ([] 1 2 3 4 5)
                fn (x)
                  if (&> x 3) x nil
            assert= true $ every? ([] 2 3 4)
              fn (x) (&> x 0)
            assert= false $ every? ([] 2 3 4)
              fn (x) (&> x 2)
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
          defn test-bool! () (echo "\"Testing bool")
            assert= "\"true" $ &str true
            assert= false $ &and true false
            assert= true $ &and true true
            assert= true $ &or false true
            assert= true $ &or true true
            assert= false $ not true
            assert= true $ not false
      :proc $ quote ()
      :configs $ {}
    |app.test-file $ {}
      :ns $ quote (ns app.test-file)
      :defs $ {}
        |test-file! $ quote
          defn test-file! () (echo "\"Testing file") (write-file "\"output/demo.text" "\"this is a created file")
            echo "\"reading file:" $ read-file "\"output/demo.text"
      :proc $ quote ()
      :configs $ {}
    |app.test-map $ {}
      :ns $ quote (ns app.test-map)
      :defs $ {}
        |test-map! $ quote
          defn test-map! () (echo "\"Testing map")
            assert= (&{} :a 1 :b 2) (&{} :a 1 :b 2)
            &let
              a $ &{} :a 1 :b 2
              assert= (assoc a :c 2) (&{} :a 1 :b 2 :c 2)
              assert= (dissoc a :a) (&{} :b 2)
            assert= (&{} :a 1 :b 2 :c 3)
              &merge (&{} :a 1 :b 2) (&{} :c 3)
            assert= (&{} :a 1 :b 4)
              &merge (&{} :a 1 :b 2) (&{} :b 4)
            assert=
              [] ([] :a 1) ([] :b 2)
              to-pairs $ &{} :a 1 :b 2
            assert=
              [] ([] 1 :a) ([] 2 :b)
              map-kv (&{} :a 1 :b 2)
                fn (k v) ([] v k)
            assert= ([] :a :b)
              keys $ &{} :a 1 :b 2
            assert= ([] 1 2)
              vals $ &{} :a 1 :b 2
            assert= (&{} :d 4)
              map-maybe (&{} :a 1 :b 2 :c 3 :d 4)
                fn (k v)
                  if (&> v 3) ([] k v) nil
            assert= (&{} :a 1 :b 2)
              {} (:a 1) (:b 2)
      :proc $ quote ()
      :configs $ {}
    |app.test-effect $ {}
      :ns $ quote (ns app.test-effect)
      :defs $ {}
        |test-effect! $ quote
          defn test-effect! () (echo "\"Testing effect")
            echo "\"path" $ get-source-path
            echo "\"path" $ dirname (get-source-path)
            eval-commonjs-file
              join-path
                dirname $ get-source-path
                , "\"demo.js"
              , "\"showDemo"
      :proc $ quote ()
      :configs $ {}
    |app.test-fn $ {}
      :ns $ quote (ns app.test-fn)
      :defs $ {}
        |test-fn! $ quote
          defn test-fn! () (echo "\"Testing fn")
            assert= 1 $ fibo 1
            assert= 8 $ fibo 5
            echo-list 1 2 3 4
            echo-list & $ [] 4 5 6 7
            assert= fibo fibo
            assert-not= fibo &+
            echo $ recur 1 2 3
            assert= 10 $ re-sum 0 ([] 1 2 3 4)
            assert= 3 $ apply-args (1 2) &+
            assert= 10 $ apply-args
              0 $ [] 1 2 3 4
              fn (acc xs)
                if (empty? xs) acc $ recur
                  &+ acc $ first xs
                  rest xs
            &let
              f $ fn (a b) (&+ a b)
              assert= 7 $ f 3 4
        |fibo $ quote
          defn fibo (n) (; echo "\"calling fibo" n)
            if (&< n 2) 1 $ &+
              fibo $ &- n 1
              fibo $ &- n 2
        |echo-list $ quote
          defn echo-list (& xs) (echo "\"echo:" xs)
        |re-sum $ quote
          defn re-sum (acc xs)
            if (empty? xs) acc $ recur
              &+ acc $ first xs
              rest xs
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
          app.test-map :refer $ test-map!
          app.test-ref :refer $ test-ref!
          app.test-string :refer $ test-string!
          app.test-file :refer $ test-file!
          app.test-effect :refer $ test-effect!
      :defs $ {}
        |main! $ quote
          defn main! () (test-fn!) (test-list!) (test-math!) (test-macro!) (test-symbol!) (test-bool!) (test-map!) (test-ref!) (test-string!) (test-file!) (test-effect!)
            &let
              a $ &+ 1 2
              assert= a 3
            assert= 1 v
            assert= 10 local-value
            assert= 1 lib-macro/v
        |local-value $ quote (def local-value 10)
      :proc $ quote ()
      :configs $ {}
    |app.test-string $ {}
      :ns $ quote (ns app.test-string)
      :defs $ {}
        |test-string! $ quote
          defn test-string! () (echo "\"Testing String")
            assert= |ab $ &str-concat |a |b
            assert= "\"a_b_c" $ join-str ([] |a |b |c) |_
            assert= "\"(+ 1 2)" $ format-to-lisp
              quote $ + 1 2
            assert= ([] "\"a" "\"b" "\"c") (split "\"abc" "\"")
            assert= ([] "\"a" "\"b" "\"c") (split "\"a-b-c" "\"-")
            assert= "\"a b c" $ trim "\" a b c "
            assert= 0 $ str-find "\"abcde" "\"ab"
            assert= 1 $ str-find "\"abcde" "\"bc"
            assert= nil $ str-find "\"abcde" "\"ae"
            assert= true $ starts-with? "\"abcde" "\"ab"
            assert= false $ starts-with? "\"abcde" "\"bc"
            assert= true $ ends-with? "\"abcde" "\"de"
            assert= false $ ends-with? "\"abcde" "\"cd"
            assert= "\"a/b.c/d" $ join-path "\"a/b.c" "\"d"
            assert= "\"a" $ dirname "\"a/b"
      :proc $ quote ()
      :configs $ {}
    |app.test-symbol $ {}
      :ns $ quote (ns app.test-symbol)
      :defs $ {}
        |test-symbol! $ quote
          defn test-symbol! () (echo "\"Testing symbol") (reset-gensym-index!)
            assert= 'a__1 $ gensym "\"a"
            assert= 'b__2 $ gensym "\"b"
            assert= 'G__3 $ gensym
            reset-gensym-index!
            assert= 'a__1 $ gensym "\"a"
            assert= 'b__2 $ gensym "\"b"
            assert= :keyword $ type-of :a
            assert= :number $ type-of 1
      :proc $ quote ()
      :configs $ {}
    |app.test-math $ {}
      :ns $ quote (ns app.test-math)
      :defs $ {}
        |test-math! $ quote
          defn test-math! () (echo "\"Testing math")
            assert= false $ &= 1 2
            assert= true $ &= (&+ 1 1) 2
            assert= (&- 4 1) (&+ 1 2)
            assert= 2 $ mod 2 3
            assert= 0 $ mod 2 2
            assert= true $ odd? 3
            assert= false $ odd? 4
            assert= true $ even? 4
            assert= false $ even? 3
            assert= 81 $ pow 3 4
      :proc $ quote ()
      :configs $ {}
