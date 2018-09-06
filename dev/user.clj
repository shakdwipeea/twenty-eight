(ns user
  (:require [clojure.spec.alpha :as s]
            [snow.repl :as repl]
            [shakdwipeea.twenty-eight.app :refer [system-config]]
            [cider.nrepl :refer [cider-nrepl-handler]]
            [clojure.tools.nrepl.server :as nrepl]
            [shadow.cljs.devtools.server :as server]
            [shadow.cljs.devtools.api :as shadow]
            [eftest.runner :refer [find-tests run-tests]]))

(s/check-asserts true)

(do (require '[expound.alpha :as expound])
    (alter-var-root #'s/*explain-out* (constantly expound/printer)))


(defn cljs-repl []
  (cemerick.piggieback/cljs-repl :app))

(defn restart-systems! []
  (do (repl/stop!)
      (repl/start! system-config)))

#_(restart-systems!)

#_(cljs-repl)

(defn compile-cljs []
  (server/start!)
  (shadow/dev :app))

#_(compile-cljs)

#_(shadow/release :app)

#_("test"
   find-tests
   run-tests)

#_(run-tests (find-tests "test"))

(defn -main [& args]
  (println "Starting twenty-eight systems...")
  (repl/start! system-config)
  (repl/start-nrepl)
  (println "nrepl started")
  (run-tests (find-tests "test"))
  #_(server/start!)
  #_(shadow/dev :app))
