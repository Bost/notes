(ns notes.views
  (:require
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   [cljs_compiler.core :as compiler :refer [_compilation _evaluation-js _evaluation-clj]]
   [notes.events :as events]
   [notes.subs :as subs]
   [reagent.core :as reagent]
   [katex :as k :refer [render renderMathInElement renderToString]]
   [utils.core :refer [dbgv dbgi]]
   ))

(re-frame/reg-event-db
 :notes/toggle-render-math
 (fn [db [_ _]]
   (update-in db [:render-math] not)))

(re-frame/reg-event-db
  :notes/toggle-panel
  (fn [db [_ id]]
    (update-in db [:open-panels id] not)))

(re-frame/reg-sub
 :notes/render-math-state
 (fn [db [_ _]]
   ;; (.log js/console "db" (pr-str db))
   (get-in db [:render-math])))

(re-frame/reg-sub
  :notes/panel-state
  (fn [db [_ id]]
    ;; (.log js/console "db" (pr-str db))
    (get-in db [:open-panels id])))

(defn component [id content]
  #_(.log js/console "content" content)
  (let [s (reagent/atom 0)]
    (js/setInterval #(swap! s inc) 1000)
    (fn []
      [:div #_@s content])))

(defn ex [txt ktx]
  [:span {:class "m" :ktx (k/renderToString ktx) :txt txt} txt])

(def replacements
  [
   ["◦" "\\circ"]
   ["{" "\\{"]
   ["}" "\\}"]
   ["->" "\\rarr"]
   ["<-" "\\larr"]
   ["|" "\\mid"]
   ["•" "\\bullet"]
   ["~" "\\thicksim"]
   ;; this is a hack
   [" α " "~α~"]
   [" α" "~α"]])

(def replace-all
  (fn [exp replacements]
    (loop [rec-exp exp
           rec-replacements replacements
           acc 0]
      (if (or (> acc 100) ;; do not run forever if something's screwed
              (empty? rec-replacements))
        rec-exp
        (let [[src dst] (first rec-replacements)]
          (recur
           (clojure.string/replace rec-exp src dst)
           (rest rec-replacements)
           (inc acc)))))))

(defn e
  ([txt    ] (ex txt (replace-all txt replacements)))
  ([txt ktx] (ex txt ktx)))

(defn render-math [render? el]
  ;; see k/renderMathInElement, (k/render exp3 el)
  ;; (.log js/console (.getAttribute el "txt"))
  ;; (.log js/console (.getAttribute el "ktx"))
  (set! (.-innerHTML el)
        (if render?
          (.getAttribute el "ktx")
          (.getAttribute el "txt"))))

(defn doall-render-math []
  (let [render-math? @(re-frame/subscribe [:notes/render-math-state])]
    #_(.log js/console "render-math?" render-math?)
    (doall
     (let [elems (.getElementsByClassName js/document "m")]
       #_(.log js/console "render-math in" (count elems) "elems")
       (map #(render-math render-math? %) elems)))))

(defn ui [{:keys [id title content]}]
  #_(.log js/console "id" id)
  (let [open? @(re-frame/subscribe [:notes/panel-state id])
        title "title"
        content "content"]
    ;; (.log js/console "open?" open?)
    [:details [:summary title] content]))

(defn dispatch-keydown-rules []
  (re-frame/dispatch
   [::rp/set-keydown-rules
    {:event-keys [[[::events/set-re-pressed-example "Hello, world!"]
                   [{:which 72} ;; h
                    {:which 69} ;; e
                    {:which 76} ;; l
                    {:which 76} ;; l
                    {:which 79}]]] ;; o
     :clear-keys
     [[{:which 27}]]}])) ;; escape

(defn display-re-pressed-example []
  (let [re-pressed-example (re-frame/subscribe [::subs/re-pressed-example])]
    [:div

     [:p
      "Re-pressed is listening for keydown events. However, re-pressed
      won't trigger any events until you set some keydown rules."]

     [:div
      [:button
       {:on-click dispatch-keydown-rules}
       "set keydown rules"]]

     [:p
      [:span
       "After clicking the button, you will have defined a rule that
       will display a message when you type "]
      [:strong [:code "hello"]]
      [:span ". So go ahead, try it out!"]]

     (when-let [rpe @re-pressed-example]
       [:div
        {:style {:padding          "16px"
                 :background-color "lightgrey"
                 :border           "solid 1px grey"
                 :border-radius    "4px"
                 :margin-top       "16px"}}

        rpe])]))

(defn process-input [s]
  (re-frame/dispatch-sync [::events/input-save s])
  (_compilation s)
  (_evaluation-js s)
  (_evaluation-clj s))

(defn input-ui []
  [:div "input-ui"
   [:section
    [:textarea {:autoFocus true
                :onChange (fn [s]
                            (process-input
                             ;; two dots: clojurescript interop
                             (.. s -target -value)))}]]])

(defn compile-cljs-ui []
  (let [[ns [status result]] @(re-frame/subscribe [::subs/compilation])]
    [:div "compile-cljs" [:div result]]))

(defn evaluate-clj-ui []
  (let [[ns [status result]] @(re-frame/subscribe [::subs/evaluation-clj])]
    [:div "evaluate-clj" [:div result]]))

(defn evaluate-js-ui []
  (let [[ns [status result]] @(re-frame/subscribe [::subs/evaluation-js])]
    [:div "evaluate-js" [:div result]]))

(defn main-panel []
  #_[:div {:class "language-klipse"}
     [input-ui]
     [compile-cljs-ui]
     [evaluate-clj-ui]
     [evaluate-js-ui]
     #_"(identity 1)"]

  [:div
   [:button {:on-click (fn []
                         (doall-render-math)
                         (re-frame/dispatch [:notes/toggle-render-math]))}
    "(doall-render-math)"]
   (ui {:title "title" :content "content"})
   #_[display-re-pressed-example]])
