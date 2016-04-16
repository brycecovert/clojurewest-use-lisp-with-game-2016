(ns dialogue-tree.prod
  (:require [dialogue-tree.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
