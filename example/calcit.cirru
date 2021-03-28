
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
              |t $ {} (:type :expr) (:by |u0) (:at 1616739566523)
                :data $ {}
                  |T $ {} (:type :expr) (:by |u0) (:at 1616739543584)
                    :data $ {}
                      |j $ {} (:type :leaf) (:by |u0) (:at 1616915263362) (:text |&+)
                      |r $ {} (:type :leaf) (:by |u0) (:at 1616739547100) (:text |1)
                      |v $ {} (:type :leaf) (:by |u0) (:at 1616739547849) (:text |1)
                  |D $ {} (:type :leaf) (:by |u0) (:at 1616739568140) (:text |echo)
              |x $ {} (:type :expr) (:by |u0) (:at 1616914764140)
                :data $ {}
                  |T $ {} (:type :leaf) (:by |u0) (:at 1616914764590) (:text |echo)
                  |j $ {} (:type :expr) (:by |u0) (:at 1616914764975)
                    :data $ {}
                      |T $ {} (:type :leaf) (:by |u0) (:at 1616914765575) (:text |fibo)
                      |j $ {} (:type :leaf) (:by |u0) (:at 1616914765970) (:text |0)
              |y $ {} (:type :expr) (:by |u0) (:at 1616914767245)
                :data $ {}
                  |T $ {} (:type :leaf) (:by |u0) (:at 1616914767886) (:text |echo)
                  |j $ {} (:type :expr) (:by |u0) (:at 1616914768880)
                    :data $ {}
                      |T $ {} (:type :leaf) (:by |u0) (:at 1616914769687) (:text |fibo)
                      |j $ {} (:type :leaf) (:by |u0) (:at 1616914772031) (:text |5)
          |fibo $ {} (:type :expr) (:by |u0) (:at 1616914713780)
            :data $ {}
              |T $ {} (:type :leaf) (:by |u0) (:at 1616914713780) (:text |defn)
              |j $ {} (:type :leaf) (:by |u0) (:at 1616914713780) (:text |fibo)
              |r $ {} (:type :expr) (:by |u0) (:at 1616914713780)
                :data $ {}
                  |T $ {} (:type :leaf) (:by |u0) (:at 1616914716691) (:text |n)
              |v $ {} (:type :expr) (:by |u0) (:at 1616914717480)
                :data $ {}
                  |T $ {} (:type :leaf) (:by |u0) (:at 1616914718071) (:text |if)
                  |j $ {} (:type :expr) (:by |u0) (:at 1616914718329)
                    :data $ {}
                      |T $ {} (:type :leaf) (:by |u0) (:at 1616914722615) (:text |&<)
                      |j $ {} (:type :leaf) (:by |u0) (:at 1616914722925) (:text |n)
                      |r $ {} (:type :leaf) (:by |u0) (:at 1616914730288) (:text |2)
                  |r $ {} (:type :leaf) (:by |u0) (:at 1616914731628) (:text |1)
                  |v $ {} (:type :expr) (:by |u0) (:at 1616914752603)
                    :data $ {}
                      |T $ {} (:type :expr) (:by |u0) (:at 1616914732172)
                        :data $ {}
                          |T $ {} (:type :leaf) (:by |u0) (:at 1616914733656) (:text |fibo)
                          |j $ {} (:type :expr) (:by |u0) (:at 1616914737890)
                            :data $ {}
                              |T $ {} (:type :leaf) (:by |u0) (:at 1616914741367) (:text |&-)
                              |j $ {} (:type :leaf) (:by |u0) (:at 1616914738568) (:text |n)
                              |r $ {} (:type :leaf) (:by |u0) (:at 1616914738854) (:text |1)
                      |D $ {} (:type :leaf) (:by |u0) (:at 1616914757731) (:text |&+)
                      |j $ {} (:type :expr) (:by |u0) (:at 1616914732172)
                        :data $ {}
                          |T $ {} (:type :leaf) (:by |u0) (:at 1616914733656) (:text |fibo)
                          |j $ {} (:type :expr) (:by |u0) (:at 1616914737890)
                            :data $ {}
                              |T $ {} (:type :leaf) (:by |u0) (:at 1616914741367) (:text |&-)
                              |j $ {} (:type :leaf) (:by |u0) (:at 1616914738568) (:text |n)
                              |r $ {} (:type :leaf) (:by |u0) (:at 1616914755814) (:text |2)
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
