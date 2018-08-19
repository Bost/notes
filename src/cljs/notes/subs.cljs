(ns notes.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::math
 (fn [db id]
   (get-in [:math id] db)))

(re-frame/reg-sub
 ::name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 ::re-pressed-example
 (fn [db _]
   (:re-pressed-example db)))
