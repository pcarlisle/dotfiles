{:repl {:plugins [
                  [cider/cider-nrepl "0.17.0-SNAPSHOT" :exclusions [org.clojure/clojure]]
                  [refactor-nrepl "2.4.0-SNAPSHOT" :exclusions [org.clojure/clojure]]
                  ;; [com.billpiel/sayid "0.0.15"]
                  ]
        :dependencies [
                       [alembic "0.3.2"]
                       [org.clojure/tools.nrepl "0.2.13"]
                       ;; [org.clojure/clojure "1.8.0"]
                       ]
        :jvm-opts ["-Xms512m" "-Xmx512m"]
        }
 :user {:plugins [[jonase/eastwood "0.2.3" :exclusions [org.clojure/clojure]]
                  ; [venantius/yagni "0.1.4" :exclusions [org.clojure/clojure]]
                  ; [lein-ancient "0.6.10"]
                  [lein-eftest "0.5.0"]
                  ]
        :dependencies [
                       [spyscope "0.1.5"]
                       [eftest "0.5.0"]
                       ]
        :injections [(require 'spyscope.core)]
        :puppetserver-heap-size "5G"
        :eftest {
                 :test-warn-time 500}
        }

 ;; :test {:dependencies [[pjstadig/humane-test-output "0.7.1"]]
 ;;        :injections [(require 'pjstadig.humane-test-output)
 ;;                     (pjstadig.humane-test-output/activate!)]

 :ci {:plugins [[lein-pprint "1.1.1"]]}

 :pretty {
  :plugins [[io.aviso/pretty "0.1.33"]]
  :dependencies [[io.aviso/pretty "0.1.33"]]
}
 }
