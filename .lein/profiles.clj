{:repl {:plugins [[cider/cider-nrepl "0.10.0-SNAPSHOT"]
                  [refactor-nrepl "2.0.0-SNAPSHOT" :exclusions [org.clojure/clojure]]
                  ; [lein-cljfmt "0.1.10"]
                  ]
        :dependencies [[alembic "0.3.2"]
                       [org.clojure/tools.nrepl "0.2.12"]
                       [org.clojure/clojure "1.7.0"]
                       ]}
 :user {:plugins [[jonase/eastwood "0.2.3" :exclusions [org.clojure/clojure]]]}

 :test {:plugins [[pjstadig/humane-test-output "0.7.0"]]}
 }
