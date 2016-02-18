{:repl {:plugins [
                  [cider/cider-nrepl "0.11.0-SNAPSHOT"]
                  [refactor-nrepl "2.2.0-SNAPSHOT" :exclusions [org.clojure/clojure]]
                  ]
        :dependencies [
                       [alembic "0.3.2"]
                       [org.clojure/tools.nrepl "0.2.12"]
                       ; [org.clojure/clojure "1.8.0"]
                       ]}
 :user {:plugins [[jonase/eastwood "0.2.3" :exclusions [org.clojure/clojure]]]}

 :test {:dependencies [[pjstadig/humane-test-output "0.7.1"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}
 }
