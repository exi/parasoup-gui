(ns parasoup-gui.prod
  (:require [parasoup-gui.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
