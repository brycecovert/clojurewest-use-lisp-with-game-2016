(ns dialogue-tree.core
  (:require-macros [cljs.core.async.macros :refer [go-loop go]] )
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [clojure.zip :refer [edit up append-child zipper children right down node branch?]]
            [cljs.core.async :refer [>! <! chan timeout]]
            [secretary.core :as secretary :include-macros true]))

(def state (atom {:history []
                  :zipper nil
                  :knows-about-sister? false
                  :busy false}))

(defonce say-chan (chan))

(defn say [who msg]
  (go
   (>! say-chan [who msg])
   (<! (timeout 1000))))

(defn respond [& msgs]
  (fn [m]
    (go
     (<! (say "Luke" m))
     (doseq [msg msgs]
       (<! (say "Obi-wan" msg))))))

(defn seen [z]
  (edit z assoc :seen? true))

(defn back [z] (-> z seen up))

(defn something-else [z] (-> z up seen up))

(def conversation-tree
  [{:heading "How did my father die?"
    :run (respond "A young Jedi named Darth Vader..."
                  "... who was a pupil of mine till he turned to evil..."
                  "Betrayed and murdered your father.")
    :choices [{:heading "I wish I knew him."
               :run (respond "He was a good man, and a good friend.")
               :next back}
              {:heading "But my father was a navigator."
               :run (respond "That's what your uncle told you."
                             "He was afraid you'd follow Obi-wan on some damn fool adventure.")
               :choices [{:heading "My uncle... how am I ever going to explain this?"
                          :run (respond "Learn about the Force, Luke.")
                          :next back}
                         {:heading "About that R2 unit..."
                          :run (respond "I seem to have recovered the message.")
                          :next back}
                         {:heading "Something else"
                          :run (respond "Okay.")
                          :next something-else}]}
              {:heading "Something else"
               :run (respond "Okay.")
               :next something-else}]}
   {:heading "You fought in the Clone Wars?"
    :run (respond "Yes, I was once a Jedi Knight, the same as your father.")
    :next back}
   {:heading "Goodbye"
    :run (respond "Goodbye.")
    :choices nil}])

(def names
  ["Aunt Beru" "Mon Mothma" "Shmi Skywalker" "Padme" "Maz Kanata" "Captain Phasma" "Zam Wesell", "Stass Allie" "Queen Apailana" "Depa Billaba" "Cordé"])

(def name-guessing-dialogue-tree
  [{:heading "Yoda spoke of another."
    :run (respond "The other is your twin sister.")
    :next (fn [z]
            (let [random-name-choices
                  (->> names
                       shuffle 
                       (take 3)
                       (map (fn [name]
                              {:heading (str "Is it... " name "?")
                               :run (respond "No, dummy.")
                               :next #(-> %
                                          (edit assoc :heading
                                                (str "Is it... " (rand-nth names) "?"))
                                          up)})))
                  leia-choice {:heading "Leia. Leia's my sister."
                               :run (respond "Your insight serves you well.")
                               :next something-else}
                  something-else-choice {:heading "It must have slipped my mind."
                                         :run (respond "Okay.")
                                         :next something-else}]
              (edit z assoc :choices (concat random-name-choices
                                                   (when (:knows-about-sister? @state)
                                                     [leia-choice])
                                                   [something-else-choice]))))}
   {:heading "Goodbye"
    :run (respond "Goodbye.")
    :choices nil}])

(defn make-zip [t]
  (zipper (some-fn :choices :next)
          :choices
          (fn [n c] (assoc n :choices c))
          {:choices t}))

(defn go-to-child [i z ]
  ((apply comp (flatten [(repeat i right) down])) z))

(defn choose-choice [state {:keys [run heading next] :or {run identity next identity}} index]
  (go
   (swap! state assoc :busy true)
   (<! (run heading))
   (swap! state (fn [s]
                  (-> s
                      (update-in [:zipper] (comp next
                                                 (partial go-to-child index)))
                      (assoc :busy false))))))

;; reagent app

(def css-transition-group
    (reagent/adapt-react-class js/React.addons.CSSTransitionGroup))

(defn home-page []
  [:div.container [:h2 "SCUMM-style dialogue"]
   [:a {:on-click (fn [] (swap! state assoc :zipper (make-zip conversation-tree) :history []))} "Star wars dialogue"]
   " / "
   [:a {:on-click (fn [] (swap! state assoc :zipper (make-zip name-guessing-dialogue-tree) :history []))} "Name guessing dialogue"]
   " - "
   [:a.small {:on-click (fn [] (swap! state assoc :knows-about-sister? true))} "Learn the secret."]
   [:img {:style {:float "right"}
          :src "img/obi.png"}]
   [:div.dialogue
    [:div.contents
     [css-transition-group {:transition-name "grow"}
      (for [[index [who message]] (map vector (range) (:history @state))]
        [:p [:b who]  ": " message])]]]
   [:div.choices
    [:img {:src "img/luke.png"}]
    [css-transition-group {:transition-name "appear"}
     (when-not (:busy @state)
       (for [[index {:keys [heading next run seen?] :or {next identity run identity} :as choice}]
             (map vector (range) (-> @state :zipper node :choices))]
         ^{:key index} [:div.choice
                        [:a {:href "#"
                             :on-click (fn [e]
                                         (.preventDefault e)
                                         (choose-choice state choice index))
                             :class (if seen? "seen" "")}
                         heading]]))]]])

(defn mount-root []
  (go-loop [msg (<! say-chan)]
           (swap! state update-in [:history] conj msg)
           (recur (<! say-chan)))

  (swap! state assoc :zipper (make-zip conversation-tree))
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
