(ns shakdwipeea.twenty-eight.app
    (:require [snow.comm.core :as comm]
              [clojure.java.io :as io]
              [snow.systems :as system]
              [compojure.core :refer [routes GET ANY]]
              [ring.util.http-response :as response]
              [ring.middleware.defaults :refer [wrap-defaults
                                                site-defaults
                                                api-defaults]]
              [com.stuartsierra.component :as component]
              (system.components
               [endpoint :refer [new-endpoint]]
               [middleware :refer [new-middleware]]
               [handler :refer [new-handler]]
               [immutant-web :refer [new-immutant-web]])))

(defn home-page []
  (-> "public/index.html"
     io/resource
     slurp
     response/ok
     (response/header "Content-Type" "text/html")))

(defn site [_]
  (routes
   (GET "/" [] (home-page))
   (ANY "*" [] (home-page))))

;; repl for investigation
#_(-> @snow.repl/state :snow.systems/repl)


(defn request-handler [request]
  (println "I handle requests. Now handling.. " request))

(defn system-config [config]
  [::site-endpoint (component/using (new-endpoint site)
                                    [::site-middleware])
   ::site-middleware (new-middleware {:middleware [[wrap-defaults site-defaults]]})
   ::handler (component/using (new-handler)
                              [::sente-endpoint ::site-endpoint ])
   ::sente-endpoint (component/using
                     (new-endpoint comm/sente-routes)
                     [::comm/comm ::site-middleware])
   ::comm/comm (comm/new-comm comm/event-msg-handler
                              comm/broadcast
                              request-handler)
   ::server (component/using (new-immutant-web :port (or (system/get-port config :http-port)
                                                        7700))
                             [::handler])])

