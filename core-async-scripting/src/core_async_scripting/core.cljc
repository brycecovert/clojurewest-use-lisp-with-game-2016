(ns core-async-scripting.core
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go-loop go]]
                      [core-async-scripting.core :refer [run-steps]]))
  #?(:cljs (:require [reagent.core :as reagent :refer [atom]]
                     [reagent.session :as session]
                     [clojure.zip :refer [edit up append-child zipper children right down node branch?]]
                     [cljs.core.async :refer [>! <! chan timeout alts! poll! put!]]
                     [secretary.core :as secretary :include-macros true])))

#?(:clj (defmacro run-steps [action-chan awake-chan state & forms]
          `(do
             ~@(for [form forms]
                `(do (~(symbol 'cljs.core.async/>!) ~action-chan ~form)
                     (reset! ~state (~(symbol 'cljs.core.async/<!) ~awake-chan)))))))
#?(:cljs
(do

(defprotocol IAction
  (begin-action [this current-state])
  (update-action [this current-state])
  (finished-action? [this current-state]))

(defn render [current-state]
  (let [{{:keys [action-chan awake-chan current-action] :as script} :script} current-state]
    (cond (not script)
          current-state

          (not current-action)
          (do
            (if-let [current-action (poll! action-chan)]
              (assoc-in (begin-action current-action current-state)
                        [:script :current-action] current-action)
              current-state))

          (finished-action? current-action current-state)
          (do
            (put! awake-chan current-state)
            (assoc-in (update-action current-action current-state)
                      [:script :current-action] nil))

          :else
          (update-action current-action current-state)))) 

(defn dist [[from-x from-y] [to-x to-y]]
  (Math/sqrt (+ (Math/pow (- from-x to-x) 2)
                (Math/pow (- from-y to-y) 2))))

(defn say [who msg]
  (let [start-time (atom nil)]
    (reify IAction
      (begin-action [this current-state]
        (reset! start-time (:total-time current-state))
        (assoc current-state :current-msg {:text msg
                                           :position
                                           [(+ 250 (get-in current-state [:entities who :position 0]))
                                            (get-in current-state [:entities who :position 1])]
                                           :color (get-in current-state [:entities who :color])}))
      (update-action [this current-state]
        (if (finished-action? this current-state)
          (dissoc current-state :current-msg)
          current-state))

      (finished-action? [this current-state]
        (> (- (:total-time current-state) @start-time)
           2000)))))

(defn step-towards [current-position destination speed]
  (let [[current-x current-y] current-position
        [dest-x dest-y] destination
        distance (dist current-position destination)
        [dx dy] [(- dest-x current-x) (- dest-y current-y )]]
    (if (<= distance speed)
      destination
      [(+ current-x (int (* speed (/ dx distance))))
       (+ current-y (int (* speed (/ dy distance))))])))

(defn walk-to [who destination & {:keys [speed] :or {speed 10}}]
  (reify IAction
    (begin-action [this current-state]
      current-state)

    (update-action [this current-state]
      (update-in current-state [:entities who :position]
                 step-towards destination speed))

    (finished-action? [this current-state]
      (let [current-position (get-in current-state [:entities who :position])
            distance (dist current-position destination)]
        (<= distance speed)))))

(defn remove-entity [who]
  (reify IAction
    (begin-action [this current-state]
      (update-in current-state [:entities] dissoc who))

    (update-action [this current-state]
      current-state)

    (finished-action? [this current-state]
      true)))

(defn wait [length]
  (let [start-time (atom nil)]
    (reify IAction
      (begin-action [this current-state]
        (reset! start-time (:total-time current-state))
        current-state)
      (update-action [this current-state]
        current-state)
      (finished-action? [this current-state]
        (> (- (:total-time current-state) @start-time)
           length)))))

(defn play-script [action-chan awake-chan state]
  (let [state (atom state)]
    (go
     (run-steps action-chan awake-chan state
                (walk-to :player [400 110])
                (wait 500)
                (say :player "Is anyone home?")
                (remove-entity :door)
                (wait 500)
                (walk-to :player [600 110])
                (say :player "Hi there Gandarf!")
                (if (= 0 (rand-int 2))
                  (say :wizard "No you again!")
                  (say :wizard "Oh no! Not you!"))))))

(defn play-script-long [action-chan awake-chan state]
  (let [state (atom state)]
    (go

     (>! action-chan (walk-to :player [400 110]))
     (reset! state (<! awake-chan))

     (>! action-chan (wait 500))
     (reset! state (<! awake-chan))

     (>! action-chan (say :player "Is anyone home?"))
     (reset! state (<! awake-chan))

     (>! action-chan (remove-entity :door))
     (reset! state (<! awake-chan))

     (>! action-chan (wait 500))
     (reset! state (<! awake-chan))

     (>! action-chan (walk-to :player [600 110]))
     (reset! state (<! awake-chan))

     (>! action-chan (say :player "Hi there Gandarf!"))
     (reset! state (<! awake-chan))

     (>! action-chan (if (= 0 (rand-int 2))
                       (say :wizard "No you again!")
                       (say :wizard "Oh no! Not you!")))
     (reset! state (<! awake-chan)))))

(def state (atom {:entities
                  {:player {:position [32 200]
                            :img "img/player.png"
                            :color "#080"}
                   :wizard {:position [800 30]
                            :img "img/wizard.png"
                            :color "#818"}
                   :door {:position [500 60]
                          :img "img/door.png"}}
                  :current-msg nil
                  :script nil
                  :total-time 0}))

(defn home-page []
  [:div.container [:h2 "core.async scripting"]
   [:a {:on-click (fn []
                    (let [action-chan (chan)
                          awake-chan (chan)]
                      (play-script-long action-chan awake-chan @state)
                      (swap! state #(assoc % :script {:action-chan action-chan :awake-chan awake-chan}))))}
    "Start script"]
   (when-let [{[x y] :position :keys [color text]}  (-> @state :current-msg)]
     [:div {:style {:left x
                    :top y
                    :padding "15px"
                    :border "8px dotted black"
                    :background-color "#CCC"
                    :color color
                    :position "absolute"
                    :z-index "5"}} text])
   [:div {:style {:position "relative"
                  :width "500px"
                  :height "100px"}}
    (for [[key {:keys [img position]}] (-> @state :entities)]
      ^{:key key} [:img {:src img
                         :style {:position "absolute"
                                 :left (str (first position) "px")
                                 :top (str (first (drop 1 position)) "px")}}])]])

(defonce render-loop
  (go-loop [msg (<! (timeout 10))]
           (reset! state (update-in (render @state)
                                    [:total-time] + 10))
           (recur (<! (timeout 10)))))

(defn mount-root []
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))))

