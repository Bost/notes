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

(re-frame/reg-sub
 ::compilation
 (fn [db _]
   #_(println "::compilation" db)
   (:compilation db)))

(re-frame/reg-sub
 ::evaluation-clj
 (fn [db _]
   #_(println "::evaluation-clj" db)
   (:evaluation-clj db)))

(re-frame/reg-sub
 ::evaluation-js
 (fn [db _]
   #_(println "::evaluation-js" db)
   (:evaluation-js db)))

