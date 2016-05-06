(ns modern-cljs.login
  (:require [domina.core :refer [by-id value set-value!]]))

(declare validate-form)

(defn init []
  (if (and js/document 
           (.-getElementById js/document))
    (let [login-form (.getElementById js/document "loginForm")]
      (set! (.-onsubmit login-form) validate-form))))

(defn validate-form []
  (let [email (by-id "email")
        password (by-id "password")]
    (if (and (> (count (value email)) 0)
             (> (count (value password)) 0))
      true
      (do (js/alert "TRY AGAIN !!!")
          false))))

(set! (.-onload js/window) init)
