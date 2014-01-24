(ns digitalocean.core
  ^{:doc "A Clojure wrapper for the Digital Ocean API"
    :author "Owain Lewis"}
  (:require [cheshire.core :as json]
            [org.httpkit.client :as http]))

(defonce digital-ocean "https://api.digitalocean.com")

(defn env
  "Fetch an env var"
  [k] (System/getenv k))

(defn make-creds
  "Utility function for building a credentials map"
  [client api-key]
  { :client client :key api-key })

(defn creds []
  { :client (env "DO_CLIENT")
    :key    (env "DO_KEY") })

;; Helper methods
;; **************************************************************

(defn url-encode-params
  "Utility function for url encoding HTTP form params"
  [params-map]
  (into {}
    (for [[k v] params-map]
      [k (java.net.URLEncoder/encode v)])))

(defn make-query-params
  "Build query params"
  [client-id api-key & params]
  (let [base-params  (format "?client_id=%s&api_key=%s" client-id api-key)
        extra-params (apply str
                       (for [[k v] (url-encode-params (into {} params))]
                         (str "&" (if (keyword? k)
                                    (name k) k) "=" v)))]
    (if (clojure.string/blank? extra-params)
      base-params
      (format "%s%s" base-params extra-params))))

(defn url-with-params
  [endpoint client-id api-key & params]
  (let [query-params (make-query-params client-id api-key (into {} params))
        url (format "%s/%s%s" digital-ocean endpoint query-params)]
    url))

;; HTTP request
;; **************************************************************

(defn request
  "Make a simple request. We are only dealing with GET requests
   for this particular API"
  [endpoint client-id api-key & params]
  (let [url (url-with-params endpoint client-id api-key (into {} params))
        {:keys [status headers body error] :as resp} @(http/get url)]
    (if error
      {:error error}
      (json/parse-string body true))))

;; **************************************************************

(defmacro ->>>
  "Safe thread macro
   Returns err message if there is an error
   else applies arrow threading to the response"
  [response & forms]
  `(if-not (contains? ~response :error)
     (->> ~response ~@forms)
     ~response))

(defn get-for
  "Helper function/abstraction to reduce duplication"
  ([resource client-id api-key]
  (let [k (keyword resource)]
    (->>> (request resource client-id api-key)
          k))))

(defn enforce-params
  "Helper which throws assertion error if required params are
   missing"
  [params-map & keys]
  (let [f (partial contains? params-map)]
    (assert
      (every? true? (map f (into [] keys))))))

(defn simple-id-action
  "A helper function for id based urls i.e /droplets/:droplet_id/reboot"
  [target target-id action]
    (let [endpoint (format "%s/%s/%s" target target-id action)]
      (partial request endpoint)))

(defn pluralize
  "Helper function for pluralizing a string"
  [n s]
  (if (= 1 n) s (str s "s")))

(defmacro when-params
  "Takes a map {:a 1 :b 2} and a vector of required keys
   If any required keys are missing an exception is thrown
   else proceed with the computation"
  [subject-map keys & body]
  `(let [f# (partial contains? ~subject-map)]
     (if (every? true?
           (map f# ~keys))
       (do ~@body)
         (let [missing-params#
                (into []
                  (clojure.set/difference
                    (set ~keys)
                    (set (keys ~subject-map))))
               key-list# (apply str
                           (map name
                             (interpose " " ~keys)))
               msg# (format "Missing required %s %s"
                      (pluralize (count missing-params#) "param")
                        missing-params#)]
           (throw (Exception. msg#))))))

;; Droplets
;; ****************************************

(defn droplets
  "Returns all droplets"
  ([client-id api-key]
    (get-for "droplets" client-id api-key))
  ([creds]
    (apply droplets (vals creds))))

(defn droplets-with-status
  ([client-id api-key status]
    (filter (fn [droplet]
              (= (:status droplet)
                 (if (keyword status)
                   (name status)
                     status)))
      (droplets client-id api-key)))
  ([creds status]
    (let [[k t] ((juxt :client :key) creds)]
      (droplets-with-status k t status))))

(defn droplet
  "Get a single droplet"
  ([client-id api-key id]
  (->>> (request (str "droplets/" id) client-id api-key)
        :droplet))
  ([creds id]
    (apply droplet
      (conj (into [] (vals creds)) id))))

(defn lookup-droplet-ip
  ([client-id api-key droplet-id]
  (->> (droplet client-id api-key droplet-id)
       :ip_address)))

(defn droplet-by-name
  "Case sensitive name lookup"
  ([client-id api-key droplet-name]
    (let [droplets (droplets client-id api-key)]
      (reduce
        (fn [acc droplet]
          (if (= (:name droplet) droplet-name)
            (conj acc droplet)
              acc)) [] droplets)))
    ([creds droplet-name]
      (let [[k t] ((juxt :client :key) creds)]
        (droplet-by-name k t droplet-name))))

(defn new-droplet
  "Create a new Digital Ocean droplet. Droplets params is a simple map
   Required params
     :name
     :size_id
     :image_id
     :region_id"
  [client-id api-key droplet-params]
  (when (map? droplet-params)
    (enforce-params droplet-params :name :size_id :image_id :region_id)
    (request "droplets/new" client-id api-key
      droplet-params)))

(defn new-small-instance-test [client-id api-key]
    (new-droplet client-id api-key
      {:name "Demo"
       :size_id "66"
       :image_id "1989574"
       :region_id "1"}))

(defn droplet-url
  "Utility function to reduce duplication"
  [id action]
  (apply format "droplets/%s/%s" [id action]))

(defn generic-droplet-action [action args]
  (let [f (fn ([client-id api-key droplet-id]
                (let [url (droplet-url droplet-id action)]
                  (request url client-id api-key)))
              ([creds droplet-id]
                (let [[k t] ((juxt :client :key) creds)]
                  (request (droplet-url droplet-id action) k t))))]
    (apply f args)))

(defn reboot-droplet
  ([client-id api-key droplet-id]
    (request (droplet-url droplet-id "reboot")
      client-id  api-key))
  ([creds droplet-id]
    (let [[k t] ((juxt :client :key) creds)]
      (reboot-droplet k t droplet-id))))

(defn shutdown-droplet
  [& args]
  "Power off a Digital Ocean droplet"
  (generic-droplet-action "shutdown" (into [] args)))

(defn power-off-droplet
  "Power off a Digital Ocean droplet"
  [& args] (generic-droplet-action "power_off" (into [] args)))

(defn power-on-droplet
  "Power off a Digital Ocean droplet"
  [& args] (generic-droplet-action "power_on" (into [] args)))

;; Regions
;; ****************************************

(defn regions
  "Fetch all Digital Ocean regions"
  ([client-id api-key]
  (->>>
    (request "regions" client-id api-key)
    :regions))
  ([creds] (apply regions (vals creds))))

(defn region-ids
  "Returns all Digital Ocean region ids"
  ([client-id api-key]
    (regions client-id api-key))
  ([creds]
    (apply region-ids (vals creds))))

;; Images
;; ****************************************

(defn images
  "List all Digital Ocean images"
  ([client-id api-key]
    (get-for "images" client-id api-key))
  ([creds]
    (apply images (vals creds))))

(defn boot-server-from-existing-image [image-id]
  )

;; SSH Keys
;; ****************************************

(defn ssh-keys
  "Fetch all SSH keys for your account"
  ([client-id api-key]
    (get-for "ssh_keys" client-id api-key))
  ([creds]
    (apply ssh-keys (vals creds))))

;; Sizes
;; ****************************************

(defn sizes
  "Return all instance sizes"
  ([client-id api-key]
    (get-for "sizes" client-id api-key))
  ([creds]
    (apply sizes (vals creds))))

;; Domains
;; ****************************************

(defn domains
  "Return all domains for your digital ocean account"
  ([client-id api-key]
    (get-for "domains" client-id api-key))
  ([creds]
    (apply domains (vals creds))))

;; Events
;; ****************************************