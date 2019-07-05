(defproject notes "0.1.0-SNAPSHOT"
  :dependencies
  [
   [org.clojure/clojure "1.10.1"]
   [org.clojure/clojurescript "1.10.520"]
   [reagent "0.8.1"]
   [re-frame "0.10.7"]
   [garden "1.3.9"]
   [ns-tracker "0.4.0"]
   [re-pressed "0.3.0"]
   [utils "0.0.0-21-0xd148"]
   ;; Domina is a jQuery inspired DOM manipulation library for ClojureScript. It
   ;; provides a functional, idiomatic Clojure interface to the DOM manipulation
   ;; facilities provided by the Google Closure library.
   [domina "1.0.3"]
   #_[cljsjs/katex "0.9.0-0"]
   [spyscope "0.1.6"]
   ]

  ;; Using: openjdk version "11.0.1" 2018-10-16; GraalVM-java doesn't work!
  :java-cmd "/usr/bin/java"

  :source-paths ["src/clj" "src/cljs"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"
                                    "resources/public/css"]

  :figwheel {:css-dirs ["resources/public/css"]}

  :garden
  {:builds [{:id           "screen"
             :source-paths ["src/clj"]
             :stylesheet   notes.css/screen
             :compiler     {:output-to     "resources/public/css/screen.css"
                            :pretty-print? true}}]}

  :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}

  :profiles
  {:dev
   {:dependencies [[binaryage/devtools "0.9.10"]
                   [figwheel-sidecar "0.5.19"]
                   [cider/piggieback "0.4.1"]]}}

  :cljsbuild
  {:builds
   [{:id           "dev"
     :source-paths ["src/cljs"]
     :figwheel     {:on-jsload            "notes.core/mount-root"}
     :compiler     {:main                 notes.core
                    :output-to            "resources/public/js/compiled/app.js"
                    :output-dir           "resources/public/js/compiled/out"
                    :asset-path           "js/compiled/out"
                    :source-map-timestamp true
                    :preloads             [devtools.preload]
                    :external-config      {:devtools/config
                                           {:features-to-install :all}}
                    :install-deps         true
                    :npm-deps {
                               :jsonfile    "5.0.0"  ;; might not be needed
                               :react       "16.8.6"
                               :react-dom   "16.8.6"
                               :katex       "0.10.1"
                               :react-katex "2.0.2"
                               }}}

    {:id           "min"
     :source-paths ["src/cljs"]
     :compiler     {:main            notes.core
                    :output-to       "resources/public/js/compiled/app.js"
                    :optimizations   :advanced
                    :closure-defines {goog.DEBUG false}
                    :pretty-print    false
                    :install-deps    true
                    :npm-deps {
                               :jsonfile    "5.0.0"  ;; might not be needed
                               :react       "16.8.6"
                               :react-dom   "16.8.6"
                               :katex       "0.10.1"
                               :react-katex "2.0.2"
                               }}}]})
