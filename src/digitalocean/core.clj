(ns digitalocean.core
  (:import [java.net URLEncoder])
  (:require [cheshire.core :as json]
	    [org.httpkit.client :as http]))

(defonce endpoint "https://api.digitalocean.com/v2/")
(defn run-request
  "Utility method for making HTTP requests
   to the Digital Ocean API"
  [method url token & params]
  (let [all-params (into {} params)
        headers {"Content-Type" "application/json"
                 "Authorization" (str "Bearer " token)}
        mergeable (if (empty? all-params)
                    (hash-map)
                    {:body (json/encode all-params)})
        {:keys [status headers body error] :as resp}
          @(http/request
            (merge
              {:method method
               :url url
               :headers headers} mergeable))]
  (if (nil? error)
    (json/parse-string body true)
    {:error error})))

(defn normalize-url [url]
  (if (string? url)
    (URLEncoder/encode
      (clojure.string/lower-case url))
    url))

(defn resource-url
  "Helper function that builds url endpoints
   (resource-url :domains 1 2 3) =>
     https://api.digitalocean.com/v2/domains/1/2/3
  "
  [resource & parts]
  (let [nested-url-parts
         (apply str
           (interpose "/"
             (map normalize-url (into [] parts))))
        qualified-resource (name resource)]
    (str endpoint qualified-resource "/" nested-url-parts)))

(defn generic
  "The function does the bulk of the work in abstracting away repetitive
   REST like requests.
   i.e (generic :get :domains) => (fn [token] ;; domain fetching logic)"
  [method resource]
  (let [request-builder (fn [token url-identifiers & params]
                          (let [resource-endpoint
                            (-> (partial resource-url (name resource))
                                (apply url-identifiers))]
    (run-request method
                 resource-endpoint
                 token
                 (into {} params))))]
  (fn
    ([token]
      (request-builder token [] {}))
    ([token resource-identifier & params]
      (request-builder token [resource-identifier] (into {} params))))))

(def domains
  "Fetch all domains"
  (generic :get :domains))

(def get-domain
  "Get a single domain by name"
  domains)

(defn records
  "Return all records for a domain"
  [token domain]
  (run-request :get
    (resource-url (str "domains/" domain "/records"))
      token))

(def droplets
  "Get all droplets"
  (generic :get :droplets))

(def get-droplet
  "Get a single droplet by ID"
  droplets)

(def create-droplet
  "Create a new droplet"
  (generic :post :droplets))

(def delete-droplet
  "Delete a droplet by ID
  (delete-droplet <token> dropletID) => nil (if it has been deleted successfully)"
  (generic :delete :droplets))

(def images "Return all images"
  (generic :get :images))

(def get-image images)

(def ssh-keys "Get all account SSH keys"
  (generic :get "account/keys"))

(def get-key ssh-keys)

(def create-key
  "Create a new SSH key"
  (generic :post "account/keys"))

(def regions
  "Returns all Digital Ocean regions"
  (generic :get :regions))

(def sizes
  "Returns droplet sizes for Digital Ocean images"
  (generic :get :sizes))
