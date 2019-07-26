(ns notes.db)

(def default-db
  (conj
   {:name "re-frame"}
   {:render-math true}
   {:open-panels {"id1" true}}
   {:input ""}
   {:compilation ""}
   {:evaluation-js ""}
   {:evaluation-clj ""}))
