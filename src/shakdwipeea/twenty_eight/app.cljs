(ns shakdwipeea.twenty-eight.app
    (:require [snow.comm.core :as comm]
              [snow.router :as router]
              [reagent.core :as r]
              [re-frame.core :as rf]
              [stylefy.core :as stylefy :refer [use-style]]))

(def route-map {:home "/home"
                :sources "/about"})

(def loading-after {:width "40%"
                    :height "100vh"
                    :transition "all 0.3s ease-in"
                    :background "#374A67" })

(def home-container {:display "flex"
                     :flex-direction "column"
                     :justify-content "center"
                     :align-items "center"
                     :height "100vh"
                     })

(def form-container {:display "flex"
                     :flex-direction "column"
                     :justify-content "center"
                     :align-items "stretch"
                     :margin-top "70px"})

(def input-style {:text-align "center"
                  :background-color "#CB9CF2"
                  :height "30px"
                  :line-height "30px"
                  :border "1px solid purple"
                  :border-radius "2px"})

(def input-container-style {:padding "5px 0px 5px 0px"})

(defn home-page []
  [:div (stylefy/use-style home-container)
   [:h1 (use-style {:flex-grow "0"})  "twenty-eight"]
   [:div "Things are to be written."]
   [:div (use-style form-container)
    [:div "About"]]])

(defn page [route params]
  [:div (stylefy/use-style loading-after)
   (case route
     :voidwalker.home [home-page]
     :sources [:div "Sources"])])

(rf/reg-event-fx
 :navigate
 (fn [{:keys [db]} [_ page param]]
   (let [db (assoc db :page page :page-param param)]
     (case page
       {:db db}))))

(defn on-navigate
  "Function called on route change"
  [route params query]
  (rf/dispatch-sync [:navigate {:route route
                                :params params
                                :perform? false}])
  (r/render [page route params]
            (js/document.getElementById "root")))


(defn main []
  (comm/start!)
  (stylefy/init)
  (router/start! route-map on-navigate)
  (rf/dispatch [:navigate :home])
  (println "Well.. Hello there!!!!"))


(main)
