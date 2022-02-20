(ns web.user
  (:require
   [cljc.java-time.instant :as inst]
   [crypto.password.bcrypt :as password]
   [web.utils :refer [md5]]))

(def user-keys
  [:_id :username :emailhash
   :isadmin :ismoderator :tournament-organizer
   :special :options :stats :has-api-keys :banned])

(def max-username-chars 20)
(def ascii-invalid-char-pattern #"[^\w.-]")
(def unicode-invalid-char-pattern #"[^\p{Alnum}\p{M}._-]")
(def invalid-leading-char-pattern #"^[^\p{Alnum}\p{M}_]+")
(def invalid-trailing-char-pattern #"[^\p{Alnum}\p{M}]+$")
(def repeated-special-char-pattern #"[-_.]{2,}")
(def patterns-username [
                        ascii-invalid-char-pattern,
                        unicode-invalid-char-pattern,
                        invalid-leading-char-pattern,
                        invalid-trailing-char-pattern,
                        repeated-special-char-pattern
                        ])

(defn valid-username?
  "Validate a username"
  [username]
  (if (<= (count username) max-username-chars)
    (every? (fn [re] (= (re-matches re username) false)) patterns-username)
    false
  )

(defn create-user
  "Create a new user map."
  [username password email & {:keys [isadmin]}]
  (let [registration-date (inst/now)]
    {:username         username
     :email            email
     :emailhash        (md5 email)
     :registrationDate registration-date
     :lastConnection   registration-date
     :password         (password/encrypt password)
     :isadmin          (or isadmin false)
     :options          {}}))

(defn active-user?
  "Returns the given user if it exists and is not banned"
  [user]
  (when (and user (not (:banned user)))
    user))

(defn visible-to-user
  "Returns true if user has not blocked other and other has not blocked user"
  [user other connected-users]
  (let [user-block-list (-> user :options :blocked-users (set))
        other-username (:username other)
        other-block-list (-> (get connected-users other-username)
                             :options
                             :blocked-users
                             (set))]
    (not (or (contains? user-block-list other-username)
             (contains? other-block-list (:username user))))))
