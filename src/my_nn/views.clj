(ns my-nn.views
  (:require [hiccup.page :as page]
            [ring.util.anti-forgery :as util]))

(defn gen-page-head
  [title]
  [:head
   [:title (str "Locations: " title)]
   (page/include-css "/css/styles.css")])

(def header-links
  [:div#header-links
   "[ "
   [:a {:href "/"} "Home"]
   " | "
   [:a {:href "/add-location"} "Add a Location"]
   " | "
   [:a {:href "/all-locations"} "View All Locations"]
   " | "
   [:a {:href "/add-user"} "Add user"]
   " ]"])

(defn home-page []
  (page/html5
   (gen-page-head "Home")
   header-links
   [:h1 "Home"]
   [:p "Webapp to store and display some 2D (x,y) locations."]))

(defn arg-page
  [a-id]
  (page/html5
   (gen-page-head "Home")
   header-links
   [:h1 "An arg page"]
   [:p (str "An arg used: " a-id)]))

(defn add-user-page
  []
  (page/html5
   (gen-page-head "Add a Location")
   header-links
   [:div#a-card
    [:h1 "Add a Location"]
    [:form {:action "/add-user" :method "POST"}
     (util/anti-forgery-field) ; prevents cross-site scripting attacks
     [:p "x value: " [:input {:type "text" :name "x"}]]
     [:p "y value: " [:input {:type "text" :name "y"}]]
     [:p [:input {:type "submit" :value "submit location"}]]]]))

(defn add-user-result-page
  [{:keys [x y]}]
  (page/html5
   (gen-page-head "Added a Location")
   header-links
   [:h1 "Added a Location"]
   [:p "Added [" x ", " y "] (id: " "[TODO:GENERATE]" ") to the db. "
    [:a {:href (str "/user/" "[TODO: GENERATED USER NAME]")} "See for yourself"]
    "."]))

