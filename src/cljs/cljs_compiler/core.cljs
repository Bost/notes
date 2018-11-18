(ns cljs_compiler.core
  (:require
   [re-frame.core :as re-frame]
   [notes.events :as events]
   [cljs.js :as cljs]))

(enable-console-print!)

(defn callback [{:keys [value error] :as prm}]
  (let [status (if error :error :ok)
        res (if error
              (.. error -cause -message)
              value)]
    [status res]))

(defn _compilation [s]
  (cljs/compile-str
   (cljs/empty-state)
   s
   (fn [prm] ;; callback-cljs-compile
     (let [[status res] (callback prm)]
       (re-frame/dispatch-sync [::events/cljs-compile [status res]])))))

(defn _evaluation-js [s]
  (cljs/eval-str
   (cljs/empty-state)
   s
   'test
   {:eval cljs/js-eval}
   (fn [prm] ;; callback-js-eval
     (let [[status res] (callback prm)]
       (re-frame/dispatch-sync [::events/js-eval
                                [status (.stringify js/JSON res nil 4)]])))))


(defn _evaluation-clj [s]
  (cljs/eval-str
   (cljs/empty-state)
   s
   'test
   {:eval cljs/js-eval}
   (fn [prm] ;; callback-cljs-eval
     (let [[status res] (callback prm)]
       (re-frame/dispatch-sync [::events/clj-eval
                                [status res]])))))

