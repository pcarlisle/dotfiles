{:repl {:plugins [
                  [cider/cider-nrepl "0.13.0-SNAPSHOT"]
                  [refactor-nrepl "2.3.0-SNAPSHOT" :exclusions [org.clojure/clojure]]
                  ]
        :dependencies [
                       [alembic "0.3.2"]
                       [org.clojure/tools.nrepl "0.2.12"]
                       ; [org.clojure/clojure "1.8.0"]
                       ]
        :jvm-opts ["-Xmx512m"]
        }
 :user {:plugins [[jonase/eastwood "0.2.3" :exclusions [org.clojure/clojure]]
                  [venantius/yagni "0.1.4" :exclusions [org.clojure/clojure]]
                  ;; [io.aviso/pretty "0.1.26" :exclusions [org.clojure/clojure]]
                  ]
        ;; :dependencies [[io.aviso/pretty "0.1.26"]]
        }

 ;; :test {:dependencies [[pjstadig/humane-test-output "0.7.1"]]
 ;;        :injections [(require 'pjstadig.humane-test-output)
 ;;                     (pjstadig.humane-test-output/activate!)]

 }
