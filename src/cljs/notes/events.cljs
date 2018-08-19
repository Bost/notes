(ns notes.events
  (:require
   [re-frame.core :as re-frame]
   [notes.db :as db]
   ))

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
