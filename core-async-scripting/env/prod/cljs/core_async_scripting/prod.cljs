(ns core-async-scripting.prod
  (:require [core-async-scripting.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
