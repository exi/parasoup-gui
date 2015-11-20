(ns parasoup-gui.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [cljs.core.async :as as :refer [<! >! chan close! timeout]]
              [goog.net.XhrIo :as xhr]
              [re-com.core :as recom]
              [cljs-time.core :as time]
              [cljs-time.coerce :as timec]
              [clojure.string :as string]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant])
    (:require-macros
     [cljs.core.async.macros :refer [go go-loop alt!]]))

(defn GET [url]
  (let [ch (chan 1)]
    (xhr/send url
              (fn [event]
                (let [res (-> event .-target .getResponseText)
                      status (-> event .-target .getStatus)]
                  (go (when (= 200 status) (>! ch res))
                      (close! ch)))))
    ch))

(def url-fetch-wait-time 1000)
(def show-interval (atom 2000))
(defonce counter (atom 0))
(defonce start-time (time/now))
(defonce runtime (atom 0))
(defonce paused (atom false))
(defonce show-toggler (atom false))
(defonce url-channel (as/chan 5))
(defonce a-tag-channel (as/chan 5))
(defonce kb-interrupt-channel (as/chan (as/dropping-buffer 1)))
(defonce nodes-in-container (atom []))
(def max-nodes-in-container 10)

(defonce runtime-loop
  (go-loop []
    (reset! runtime (Math/floor (/ (- (timec/to-long (time/now))
                                      (timec/to-long start-time))
                                   1000)))
    (<! (timeout 1000))
    (recur)))

(defonce url-loop
  (go-loop []
    (if-let [url (<! (GET "http://parasoup.de/api/next"))]
      (>! url-channel url))
    (<! (timeout url-fetch-wait-time))
    (recur)))

(defn on-image-load [ready-chan a img]
  (-> img
      .hide
      (.css "float" "left"))
  (.remove (js/$ a))
  (go
    (>! a-tag-channel a)
    (>! ready-chan true)))

(defonce image-fetch-loop
  (go-loop []
    (let [url (<! url-channel)
          ready-chan (as/chan 1)
          timeout-chan (as/timeout 30000)
          img (-> (js/$ "<img />")
                  (.attr "src" url)
                  )
          a (-> (js/$ "<a />")
                (.attr "href" (string/replace url #"[a-zA-Z0-9\-]*\.parasoup\.de" "whatthepost.soup.io"))
                (.attr "target" "_blank"))]
      (.prepend a img)
      (.load img (partial on-image-load ready-chan a img))
      (let [[v c] (alts! [timeout-chan ready-chan])]
        (when (= c timeout-chan)
          (as/close! ready-chan)))
      (recur))))

(defn add-node-to-container [a]
  (-> (js/$ "#container")
      (.prepend a))
  (.slideDown (.find a "img") 500)
  (swap! counter inc)
  (swap! nodes-in-container conj a))

(defn remove-old-images []
  (let [to-be-removed (drop max-nodes-in-container @nodes-in-container)]
    (doseq [old-node to-be-removed]
      (.remove old-node))
    (reset! nodes-in-container (take max-nodes-in-container @nodes-in-container)))
  nil)

(defonce image-loop
  (go-loop [by-interrupt false]
    (when (or (not @paused)
              by-interrupt)
      (when-let [a (as/poll! a-tag-channel)]
        (add-node-to-container a)
        (remove-old-images)
        ))
    (let [[v c] (as/alts! [(timeout @show-interval)
                           kb-interrupt-channel])
          by-interrupt (= c kb-interrupt-channel)]
      (recur by-interrupt))))

(defn register-key-handlers []
  (let [next-keys #{
                   39 ;;right
                   40 ;;down
                   74 ;;j
                   76 ;;l
                   }
        invalid-keys (into #{
                             17 ;;ctrl
                             18 ;;tab
                             91 ;;win
                             32 33 34 35 36 38 144
                            }
                           (range 112 124))
        handler (fn [evt]
                  (let [key (or (.-which evt) (.-keyCode evt))]
                    (when-not (contains? invalid-keys key)
                      (cond (contains? next-keys key)(go (>! kb-interrupt-channel true))
                            :default (swap! paused not)))))]
    (-> (js/$ js/document)
        (.keydown handler))))

;; -------------------------
;; Views

(defn pause-indicator []
  [:div {:id "pause-indicator" :class (if @paused "paused" "")}])

(defn picture-delay-display []
  [:div {:class "col-md-1"}
   "picture delay: " @show-interval "ms"
   [recom/slider
    :model show-interval
    :min 1000
    :max 60000
    :step 100
    :width "100px"
    :style {"display" "inline-block"}
    :on-change #(reset! show-interval %)]
   ])

(defn help-text []
  [:div {:class "col-md-4"}
   "Pause with any key. Click image for search. You can trigger \"next picture\" with j, l, left/down arrow, even in pause mode. "
   [:a {:href "http://www.parasoup.de"} "click here for soup"]
   "."])

(defn status []
  [:div {:class "col-md-1"}
   [:div "Status: " (if (true? @paused) "paused" "unpaused")]
   [:div "Here for: " @runtime "s"]
   [:div "Watched " @counter " pictures."]])

(defn header []
  [:div {:id "effect" :class (if (true? @show-toggler) "" "closed")}
   [:h3 "Parasoup"]
   [:div {:class "row"}
    [:div {:class "col-md-3"}]
    [picture-delay-display]
    [status]
    [help-text]
    ]])

(defn home-page []
  [:div [:div {:id "toggler"
               :on-mouse-enter (fn [] (reset! show-toggler true))
               :on-mouse-leave (fn [] (reset! show-toggler false))
               }
         [pause-indicator]
         [header]]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root)
  (register-key-handlers))
