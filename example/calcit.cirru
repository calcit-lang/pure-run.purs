
{}
  :users $ {}
    |u0 $ {} (:name |chen) (:id |u0) (:nickname |chen) (:avatar nil) (:password |d41d8cd98f00b204e9800998ecf8427e) (:theme :star-trail)
  :ir $ {} (:package |app)
    :files $ {}
      |app.main $ {}
        :ns $ {} (:type :expr) (:by |u0) (:at 1616315471762)
          :data $ {}
            |T $ {} (:type :leaf) (:by |u0) (:at 1616315471762) (:text |ns)
            |j $ {} (:type :leaf) (:by |u0) (:at 1616315471762) (:text |app.main)
        :defs $ {}
          |main! $ {} (:type :expr) (:by |u0) (:at 1616315475196)
            :data $ {}
              |T $ {} (:type :leaf) (:by |u0) (:at 1616315475196) (:text |defn)
              |j $ {} (:type :leaf) (:by |u0) (:at 1616315478731) (:text |main!)
              |r $ {} (:type :expr) (:by |u0) (:at 1616315475196)
                :data $ {}
              |v $ {} (:type :expr) (:by |u0) (:at 1616315480825)
                :data $ {}
                  |T $ {} (:type :leaf) (:by |u0) (:at 1616315481350) (:text |echo)
                  |j $ {} (:type :leaf) (:by |u0) (:at 1616315486529) (:text "|\"This is a demo")
        :proc $ {} (:type :expr) (:by |u0) (:at 1616315471762)
          :data $ {}
        :configs $ {}
  :configs $ {} (:reload-fn |app.main/reload!)
    :modules $ [] |core
    :output |src
    :port 6001
    :extension |.cljs
    :local-ui? false
    :init-fn |app.main/main!
    :compact-output? true
    :version |0.0.1
