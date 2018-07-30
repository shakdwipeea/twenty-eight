(ns user
  (:require [clojure.spec.alpha :as s]
            [snow.repl :as repl]
            [snow.env :refer [read-edn]]
            [shakdwipeea.twenty-eight.app :refer [system-config]]
            [cider.nrepl :refer [cider-nrepl-handler]]
            [clojure.tools.nrepl.server :as nrepl]
            [shadow.cljs.devtools.server :as server]
            [shadow.cljs.devtools.api :as shadow]))

(s/check-asserts true)

#_(do (require '[expound.alpha :as expound])
      (set! s/*explain-out* expound/printer))


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

(defn -main [& args]
  (println "Starting twenty-eight systems...")
  (repl/start! system-config)
  (repl/start-nrepl)
  (println "nrepl started")
  (server/start!)
  (shadow/dev :app))
