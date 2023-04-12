(ns my-nn.handlers
  (:require [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [my-nn.views :as views]))

(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (:remote-addr request)})

(defroutes app-routes
  (GET "/" [] (views/home-page))
  (GET "/a" [] handler)
  (GET "/user/:id" [id] (views/arg-page id))
  (GET "/add-user" [] (views/add-user-page))
  (POST "/add-user" {params :params} (views/add-user-result-page params))
  (route/resources "/")
  (route/not-found "<h1>Page not found</h1>"))

(def app
  ;; use #' prefix for REPL-friendly code
  (wrap-defaults #'app-routes site-defaults))

(comment
  ;; evaluate this def form to start the webapp via the REPL:
  ;; :join? false runs the web server in the background!
  (def server (jetty/run-jetty #'app {:port 3000 :join? false}))
  ;; evaluate this form to stop the webapp via the the REPL:
  (.stop server))

;; (defn -main []
;;   (jetty/run-jetty #'app {:port 3000}))
