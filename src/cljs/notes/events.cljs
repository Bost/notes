(ns notes.events
  (:require
   [re-frame.core :as re-frame]
   [notes.db :as db]
   [cljs_compiler.core :as compiler :refer [_compilation _evaluation-js _evaluation-clj]]))


(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-db
 ::set-re-pressed-example
 (fn [db [_ value]]
   (assoc db :re-pressed-example value)))

(re-frame/reg-event-db
 ::set-math
 (fn [db [id expr]]
   (assoc db [:math id] expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(re-frame/reg-event-db
 ::input-save
 (fn [db value]
   (assoc db :input value)))

(re-frame/reg-event-db
 ::cljs-compile
 (fn [db [ns [status res]]]
   (assoc db :compilation [ns [status res]])))

(re-frame/reg-event-db
 ::js-eval
 (fn [db [ns [status res]]]
   (assoc db :evaluation-js [ns [status res]])))

(re-frame/reg-event-db
 ::clj-eval
 (fn [db [ns [status res]]]
   (assoc db :evaluation-clj [ns [status res]])))
