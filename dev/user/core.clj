(ns user.core
  (:require [clojure.spec.alpha :as s]
            [snow.repl :as repl]
            [shakdwipeea.twenty-eight.app :refer [system-config]]))

#_(s/check-asserts true)

#_(do (require '[expound.alpha :as expound])
      (set! s/*explain-out* expound/printer))


(defn cljs-repl []
  (cemerick.piggieback/cljs-repl :app))

(defn restart-systems! []
  (do (repl/stop!)
      (repl/start! system-config)))

#_(restart-systems!)

#_(cljs-repl)

#_(Compile-cljs)

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
  #_(server/start!)
  #_(shadow/dev :app))
